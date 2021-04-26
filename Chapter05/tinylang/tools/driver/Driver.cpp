#include "tinylang/Basic/Diagnostic.h"
#include "tinylang/Basic/Version.h"
#include "tinylang/CodeGen/CodeGenerator.h"
#include "tinylang/Parser/Parser.h"
#include "llvm/CodeGen/CommandFlags.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/WithColor.h"

using namespace llvm;
using namespace tinylang;

static codegen::RegisterCodeGenFlags CGF;

static llvm::cl::list<std::string>
    InputFiles(llvm::cl::Positional,
               llvm::cl::desc("<input-files>"));

static llvm::cl::opt<std::string>
    MTriple("mtriple",
            llvm::cl::desc("Override target triple for module"));

static llvm::cl::opt<bool>
    EmitLLVM("emit-llvm",
             llvm::cl::desc("Emit IR code instead of assembler"),
             llvm::cl::init(false));

static const char *Head = "tinylang - Tinylang compiler";

void printVersion(llvm::raw_ostream &OS) {
  OS << Head << " " << getTinylangVersion() << "\n";
  OS << "  Default target: "
     << llvm::sys::getDefaultTargetTriple() << "\n";
  std::string CPU(llvm::sys::getHostCPUName());
  OS << "  Host CPU: " << CPU << "\n";
  OS << "\n";
  OS.flush();
  llvm::TargetRegistry::printRegisteredTargetsForVersion(
      OS);
  exit(EXIT_SUCCESS);
}

llvm::TargetMachine *
createTargetMachine(const char *Argv0) {
  llvm::Triple Triple = llvm::Triple(
      !MTriple.empty()
          ? llvm::Triple::normalize(MTriple)
          : llvm::sys::getDefaultTargetTriple());

  llvm::TargetOptions TargetOptions =
      codegen::InitTargetOptionsFromCodeGenFlags(Triple);
  std::string CPUStr = codegen::getCPUStr();
  std::string FeatureStr = codegen::getFeaturesStr();

  std::string Error;
  const llvm::Target *Target =
      llvm::TargetRegistry::lookupTarget(codegen::getMArch(), Triple,
                                         Error);

  if (!Target) {
    llvm::WithColor::error(llvm::errs(), Argv0) << Error;
    return nullptr;
  }

  llvm::TargetMachine *TM = Target->createTargetMachine(
      Triple.getTriple(), CPUStr, FeatureStr, TargetOptions,
      llvm::Optional<llvm::Reloc::Model>(codegen::getRelocModel()));
  return TM;
}

bool emit(StringRef Argv0, llvm::Module *M,
          llvm::TargetMachine *TM,
          StringRef InputFilename) {
  CodeGenFileType FileType = codegen::getFileType();
  std::string OutputFilename;
  if (InputFilename == "-") {
    OutputFilename = "-";
  } else {
    if (InputFilename.endswith(".mod") ||
        InputFilename.endswith(".mod"))
      OutputFilename = InputFilename.drop_back(4).str();
    else
      OutputFilename = InputFilename.str();
    switch (FileType) {
    case CGFT_AssemblyFile:
      OutputFilename.append(EmitLLVM ? ".ll" : ".s");
      break;
    case CGFT_ObjectFile:
      OutputFilename.append(".o");
      break;
    case CGFT_Null:
      OutputFilename.append(".null");
      break;
    }
  }

  // Open the file.
  std::error_code EC;
  sys::fs::OpenFlags OpenFlags = sys::fs::OF_None;
  if (FileType == CGFT_AssemblyFile)
    OpenFlags |= sys::fs::OF_Text;
  auto Out = std::make_unique<llvm::ToolOutputFile>(
      OutputFilename, EC, OpenFlags);
  if (EC) {
    WithColor::error(llvm::errs(), Argv0) << EC.message() << '\n';
    return false;
  }

  legacy::PassManager PM;
  if (FileType == CGFT_AssemblyFile && EmitLLVM) {
    PM.add(createPrintModulePass(Out->os()));
  } else {
    if (TM->addPassesToEmitFile(PM, Out->os(), nullptr,
                                FileType)) {
      WithColor::error() << "No support for file type\n";
      return false;
    }
  }
  PM.run(*M);
  Out->keep();
  return true;
}

int main(int Argc, const char **Argv) {
  llvm::InitLLVM X(Argc, Argv);

  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmPrinters();
  InitializeAllAsmParsers();

  llvm::cl::SetVersionPrinter(&printVersion);
  llvm::cl::ParseCommandLineOptions(Argc, Argv, Head);

  if (codegen::getMCPU() == "help" ||
      std::any_of(codegen::getMAttrs().begin(), codegen::getMAttrs().end(),
                  [](const std::string &a) {
                    return a == "help";
                  })) {
    auto Triple = llvm::Triple(LLVM_DEFAULT_TARGET_TRIPLE);
    std::string ErrMsg;
    if (auto target = llvm::TargetRegistry::lookupTarget(
            Triple.getTriple(), ErrMsg)) {
      llvm::errs() << "Targeting " << target->getName()
                   << ". ";
      // this prints the available CPUs and features of the
      // target to stderr...
      target->createMCSubtargetInfo(Triple.getTriple(),
                                    codegen::getCPUStr(),
                                    codegen::getFeaturesStr());
    } else {
      llvm::errs() << ErrMsg << "\n";
      exit(EXIT_FAILURE);
    }
    exit(EXIT_SUCCESS);
  }

  llvm::TargetMachine *TM = createTargetMachine(Argv[0]);
  if (!TM)
    exit(EXIT_FAILURE);

  for (const auto &F : InputFiles) {
    llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>
        FileOrErr = llvm::MemoryBuffer::getFile(F);
    if (std::error_code BufferError =
            FileOrErr.getError()) {
      llvm::WithColor::error(llvm::errs(), Argv[0])
          << "Error reading " << F << ": "
          << BufferError.message() << "\n";
    }

    llvm::SourceMgr SrcMgr;
    DiagnosticsEngine Diags(SrcMgr);

    // Tell SrcMgr about this buffer, which is what the
    // parser will pick up.
    SrcMgr.AddNewSourceBuffer(std::move(*FileOrErr),
                              llvm::SMLoc());

    auto lexer = Lexer(SrcMgr, Diags);
    auto sema = Sema(Diags);
    auto parser = Parser(lexer, sema);
    auto *Mod = parser.parse();
    if (Mod && !Diags.numErrors()) {
      llvm::LLVMContext Ctx;
      if (CodeGenerator *CG = CodeGenerator::create(Ctx, TM)) {
        std::unique_ptr<llvm::Module> M = CG->run(Mod, F);
        if (!emit(Argv[0], M.get(), TM, F)) {
          llvm::WithColor::error(llvm::errs(), Argv[0]) << "Error writing output\n";
        }
        delete CG;
      }
    }
  }
}
