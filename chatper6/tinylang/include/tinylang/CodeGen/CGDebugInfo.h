#ifndef TINYLANG_CODEGEN_CGDEBUGINFO_H
#define TINYLANG_CODEGEN_CGDEBUGINFO_H

#include "tinylang/AST/AST.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/GlobalVariable.h"

namespace tinylang {

class CGModule;

class CGDebugInfo {
  CGModule &CGM;
  llvm::DIBuilder DBuilder;
  llvm::DICompileUnit *CU;

  llvm::DenseMap<TypeDeclaration *, llvm::DIType *>
      TypeCache;

  llvm::SmallVector<llvm::DIScope *, 4> ScopeStack;

  llvm::DIScope *getScope();
  void openScope(llvm::DIScope *);
  unsigned getLineNumber(SMLoc Loc);

  llvm::DIType *getPervasiveType(TypeDeclaration *Ty);
  llvm::DIType *getAliasType(AliasTypeDeclaration *Ty);
  llvm::DIType *getArrayType(ArrayTypeDeclaration *Ty);
  llvm::DIType *getRecordType(RecordTypeDeclaration *Ty);

  llvm::DIType *getType(TypeDeclaration *Type);
  llvm::DISubroutineType *getType(ProcedureDeclaration *P);

public:
  CGDebugInfo(CGModule &CGM);

  void closeScope();

  void emitGlobalVariable(VariableDeclaration *Decl,
                          llvm::GlobalVariable *V);
  void emitProcedure(ProcedureDeclaration *Decl,
                     llvm::Function *Fn);
  void emitProcedureEnd(ProcedureDeclaration *Decl,
                        llvm::Function *Fn);
  llvm::DILocalVariable *
  emitParameterVariable(FormalParameterDeclaration *FP,
                        size_t Idx, llvm::Value *Val,
                        llvm::BasicBlock *BB);
  void emitValue(llvm::Value *Val,
                 llvm::DILocalVariable *Var, SMLoc Loc,
                 llvm::BasicBlock *BB);

  llvm::DebugLoc getDebugLoc(SMLoc Loc);

  void finalize();
};
} // namespace tinylang
#endif