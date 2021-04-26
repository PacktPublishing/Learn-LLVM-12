#include "tinylang/AST/ASTContext.h"
#include "tinylang/Basic/Diagnostic.h"
#include "tinylang/Basic/Version.h"
#include "tinylang/CodeGen/CodeGenerator.h"
#include "tinylang/Parser/Parser.h"
#include "llvm/CodeGen/CommandFlags.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/WithColor.h"

#include "llvm/Passes/PassBuilder.h" // New
#include "llvm/Passes/PassPlugin.h" // New
#include "llvm/Analysis/AliasAnalysis.h" // New
#include "llvm/Analysis/TargetTransformInfo.h" // New

using namespace llvm;
using namespace tinylang;

static codegen::RegisterCodeGenFlags CGF;

static llvm::cl::list<std::string>
    InputFiles(cl::Positional, cl::desc("<input-files>"));

static cl::opt<std::string>
    MTriple("mtriple",
            cl::desc("Override target triple for module"));

static cl::opt<bool>
    EmitLLVM("emit-llvm",
             cl::desc("Emit IR code instead of assembler"),
             cl::init(false));

static cl::opt<signed char> OptLevel(
    cl::desc("Setting the optimization level:"),
    cl::ZeroOrMore,
    cl::values(
        clEnumValN(3, "O", "Equivalent to -O3"),
        clEnumValN(0, "O0", "Optimization level 0"),
        clEnumValN(1, "O1", "Optimization level 1"),
        clEnumValN(2, "O2", "Optimization level 2"),
        clEnumValN(3, "O3", "Optimization level 3"),
        clEnumValN(-1, "Os",
                   "Like -O2 with extra optimizations "
                   "for size"),
        clEnumValN(
            -2, "Oz",
            "Like -Os but reduces code size further")),
    cl::init(0));

static cl::opt<bool>
    DebugPM("debug-pass-manager", cl::Hidden,
            cl::desc("Print PM debugging information"));

static cl::opt<std::string> PassPipeline(
    "passes",
    cl::desc("A description of the pass pipeline"));

static cl::list<std::string> PassPlugins(
    "load-pass-plugin",
    cl::desc("Load passes from plugin library"));

static cl::opt<std::string> PipelineStartEPPipeline(
    "passes-ep-pipeline-start",
    cl::desc("Pipeline start extension point"));

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
      codegen::InitTargetOptionsFromCodeGenFlags(
          Triple);
  std::string CPUStr = codegen::getCPUStr();
  std::string FeatureStr = codegen::getFeaturesStr();

  std::string Error;
  const llvm::Target *Target =
      llvm::TargetRegistry::lookupTarget(
          codegen::getMArch(), Triple, Error);

  if (!Target) {
    llvm::WithColor::error(llvm::errs(), Argv0)
        << Error;
    return nullptr;
  }

  llvm::TargetMachine *TM = Target->createTargetMachine(
      Triple.getTriple(), CPUStr, FeatureStr,
      TargetOptions,
      llvm::Optional<llvm::Reloc::Model>(
          codegen::getRelocModel()));
  return TM;
}

std::string outputFilename(StringRef InputFilename) {
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
  return OutputFilename;
}

#define HANDLE_EXTENSION(Ext)                          \
  llvm::PassPluginLibraryInfo get##Ext##PluginInfo();
#include "llvm/Support/Extension.def"

bool emit(StringRef Argv0, llvm::Module *M,
          llvm::TargetMachine *TM,
          StringRef InputFilename) {

  // Create the optimization pipeline
  PassBuilder PB(TM);

  // Load requested pass plugins and let them register
  // pass builder callbacks
  for (auto &PluginFN : PassPlugins) {
    auto PassPlugin = PassPlugin::Load(PluginFN);
    if (!PassPlugin) {
      WithColor::error(errs(), Argv0)
          << "Failed to load passes from '" << PluginFN
          << "'. Request ignored.\n";
      continue;
    }

    PassPlugin->registerPassBuilderCallbacks(PB);
  }

#define HANDLE_EXTENSION(Ext)                          \
  get##Ext##PluginInfo().RegisterPassBuilderCallbacks( \
      PB);
#include "llvm/Support/Extension.def"

  LoopAnalysisManager LAM(DebugPM);
  FunctionAnalysisManager FAM(DebugPM);
  CGSCCAnalysisManager CGAM(DebugPM);
  ModuleAnalysisManager MAM(DebugPM);

  // Register the AA manager first so that our version
  // is the one used.
  FAM.registerPass(
      [&] { return PB.buildDefaultAAPipeline(); });

  // Register all the basic analyses with the managers.
  PB.registerModuleAnalyses(MAM);
  PB.registerCGSCCAnalyses(CGAM);
  PB.registerFunctionAnalyses(FAM);
  PB.registerLoopAnalyses(LAM);
  PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

  PB.registerPipelineStartEPCallback(
      [&PB,
       Argv0](ModulePassManager &PM,
              PassBuilder::OptimizationLevel Level) {
        if (auto Err = PB.parsePassPipeline(
                PM, PipelineStartEPPipeline)) {
          WithColor::error(errs(), Argv0)
              << "Could not parse pipeline "
              << PipelineStartEPPipeline.ArgStr << ": "
              << toString(std::move(Err)) << "\n";
        }
      });

  ModulePassManager MPM(DebugPM);

  if (!PassPipeline.empty()) {
    if (auto Err = PB.parsePassPipeline(
            MPM, PassPipeline)) {
      WithColor::error(errs(), Argv0)
          << toString(std::move(Err)) << "\n";
      return false;
    }
  } else {
    StringRef DefaultPass;
    switch (OptLevel) {
    case 0: DefaultPass = "default<O0>"; break;
    case 1: DefaultPass = "default<O1>"; break;
    case 2: DefaultPass = "default<O2>"; break;
    case 3: DefaultPass = "default<O3>"; break;
    case -1: DefaultPass = "default<Os>"; break;
    case -2: DefaultPass = "default<Oz>"; break;
    }
    if (auto Err = PB.parsePassPipeline(
            MPM, DefaultPass)) {
      WithColor::error(errs(), Argv0)
          << toString(std::move(Err)) << "\n";
      return false;
    }
  }

  // Open the file.
  std::error_code EC;
  sys::fs::OpenFlags OpenFlags = sys::fs::OF_None;
  CodeGenFileType FileType = codegen::getFileType();
  if (FileType == CGFT_AssemblyFile)
    OpenFlags |= sys::fs::OF_Text;
  auto Out = std::make_unique<llvm::ToolOutputFile>(
      outputFilename(InputFilename), EC, OpenFlags);
  if (EC) {
    WithColor::error(errs(), Argv0)
        << EC.message() << '\n';
    return false;
  }

  legacy::PassManager CodeGenPM;
  CodeGenPM.add(createTargetTransformInfoWrapperPass(
      TM->getTargetIRAnalysis()));
  if (FileType == CGFT_AssemblyFile && EmitLLVM) {
    CodeGenPM.add(createPrintModulePass(Out->os()));
  } else {
    if (TM->addPassesToEmitFile(CodeGenPM, Out->os(),
                                nullptr, FileType)) {
      WithColor::error()
          << "No support for file type\n";
      return false;
    }
  }

  MPM.run(*M, MAM);
  CodeGenPM.run(*M);
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
      std::any_of(codegen::getMAttrs().begin(),
                  codegen::getMAttrs().end(),
                  [](const std::string &a) {
                    return a == "help";
                  })) {
    auto Triple =
        llvm::Triple(LLVM_DEFAULT_TARGET_TRIPLE);
    std::string ErrMsg;
    if (auto target =
            llvm::TargetRegistry::lookupTarget(
                Triple.getTriple(), ErrMsg)) {
      llvm::errs() << "Targeting " << target->getName()
                   << ". ";
      // this prints the available CPUs and features of
      // the target to stderr...
      target->createMCSubtargetInfo(
          Triple.getTriple(), codegen::getCPUStr(),
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
      llvm::WithColor::error(errs(), Argv[0])
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
    auto ASTCtx = ASTContext(SrcMgr, F);
    auto sema = Sema(Diags);
    auto parser = Parser(lexer, sema);
    auto *Mod = parser.parse();
    if (Mod && !Diags.numErrors()) {
      llvm::LLVMContext Ctx;
      if (CodeGenerator *CG =
              CodeGenerator::create(Ctx, ASTCtx, TM)) {
        std::unique_ptr<llvm::Module> M = CG->run(Mod, F);
        if (!emit(Argv[0], M.get(), TM, F)) {
          llvm::WithColor::error(errs(), Argv[0])
              << "Error writing output\n";
        }
        delete CG;
      }
    }
  }
}
