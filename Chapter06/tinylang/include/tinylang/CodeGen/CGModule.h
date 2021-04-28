#ifndef TINYLANG_CODEGEN_CGMODULE_H
#define TINYLANG_CODEGEN_CGMODULE_H

#include "tinylang/AST/AST.h"
#include "tinylang/AST/ASTContext.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

namespace tinylang {

class CGModule {
  ASTContext &ASTCtx;
  llvm::Module *M;

  ModuleDeclaration *Mod;

  llvm::DenseMap<TypeDeclaration *, llvm::Type *> TypeCache;

  // Repository of global objects.
  llvm::DenseMap<Decl *, llvm::GlobalObject *> Globals;

public:
  llvm::Type *VoidTy;
  llvm::Type *Int1Ty;
  llvm::Type *Int32Ty;
  llvm::Type *Int64Ty;
  llvm::Constant *Int32Zero;

public:
  CGModule(ASTContext &ASTCtx, llvm::Module *M);
  void initialize();

  ASTContext &getASTCtx() { return ASTCtx; }
  llvm::LLVMContext &getLLVMCtx() {
    return M->getContext();
  }
  llvm::Module *getModule() { return M; }
  ModuleDeclaration *getModuleDeclaration() { return Mod; }

  llvm::Type *convertType(TypeDeclaration *Ty);
  std::string mangleName(Decl *D);

  llvm::GlobalObject *getGlobal(Decl *);

  void run(ModuleDeclaration *Mod);
};
} // namespace tinylang
#endif
