#include "tinylang/CodeGen/CGModule.h"
#include "tinylang/CodeGen/CGProcedure.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/CommandLine.h"

using namespace tinylang;

static llvm::cl::opt<bool>
    Debug("g", llvm::cl::desc("Generate debug information"),
          llvm::cl::init(false));

CGModule::CGModule(ASTContext &ASTCtx, llvm::Module *M)
    : ASTCtx(ASTCtx), M(M) {
  initialize();
}

void CGModule::initialize() {
  VoidTy = llvm::Type::getVoidTy(getLLVMCtx());
  Int1Ty = llvm::Type::getInt1Ty(getLLVMCtx());
  Int32Ty = llvm::Type::getInt32Ty(getLLVMCtx());
  Int64Ty = llvm::Type::getInt64Ty(getLLVMCtx());
  Int32Zero =
      llvm::ConstantInt::get(Int32Ty, 0, /*isSigned*/ true);
}

llvm::Type *CGModule::convertType(TypeDeclaration *Ty) {
  if (llvm::Type *T = TypeCache[Ty])
    return T;

  if (llvm::isa<PervasiveTypeDeclaration>(Ty)) {
    if (Ty->getName() == "INTEGER")
      return Int64Ty;
    if (Ty->getName() == "BOOLEAN")
      return Int1Ty;
  } else if (auto *AliasTy =
                 llvm::dyn_cast<AliasTypeDeclaration>(Ty)) {
    llvm::Type *T = convertType(AliasTy->getType());
    return TypeCache[Ty] = T;
  } else if (auto *ArrayTy =
                 llvm::dyn_cast<ArrayTypeDeclaration>(Ty)) {
    llvm::Type *Component = convertType(ArrayTy->getType());
    Expr *Nums = ArrayTy->getNums();
    uint64_t NumElements = 5; // TODO Eval Nums
    llvm::Type *T =
        llvm::ArrayType::get(Component, NumElements);
    return TypeCache[Ty] = T;
  } else if (auto *RecordTy =
                 llvm ::dyn_cast<RecordTypeDeclaration>(
                     Ty)) {
    llvm::SmallVector<llvm::Type *, 4> Elements;
    for (const auto &F : RecordTy->getFields()) {
      Elements.push_back(convertType(F.getType()));
    }
    llvm::Type *T = llvm::StructType::create(
        Elements, RecordTy->getName(), false);
    return TypeCache[Ty] = T;
  }
  llvm::report_fatal_error("Unsupported type");
}

std::string CGModule::mangleName(Decl *D) {
  std::string Mangled;
  llvm::SmallString<16> Tmp;
  while (D) {
    llvm::StringRef Name = D->getName();
    Tmp.clear();
    Tmp.append(llvm::itostr(Name.size()));
    Tmp.append(Name);
    Mangled.insert(0, Tmp.c_str());
    D = D->getEnclosingDecl();
  }
  Mangled.insert(0, "_t");
  return Mangled;
}

llvm::GlobalObject *CGModule::getGlobal(Decl *D) {
  return Globals[D];
}

void CGModule::run(ModuleDeclaration *Mod) {
  this->Mod = Mod;
  for (auto *Decl : Mod->getDecls()) {
    if (auto *Var =
            llvm::dyn_cast<VariableDeclaration>(Decl)) {
      // Create global variables
      llvm::GlobalVariable *V = new llvm::GlobalVariable(
          *M, convertType(Var->getType()),
          /*isConstant=*/false,
          llvm::GlobalValue::PrivateLinkage, nullptr,
          mangleName(Var));
      Globals[Var] = V;
    } else if (auto *Proc =
                   llvm::dyn_cast<ProcedureDeclaration>(
                       Decl)) {
      CGProcedure CGP(*this);
      CGP.run(Proc);
    }
  }
}
