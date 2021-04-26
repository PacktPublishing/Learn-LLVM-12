#ifndef TINYLANG_CODEGEN_CGPROCEDURE_H
#define TINYLANG_CODEGEN_CGPROCEDURE_H

#include "tinylang/AST/AST.h"
#include "tinylang/CodeGen/CGModule.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Value.h"

namespace llvm {
class Function;
}

namespace tinylang {

class CGProcedure {
  CGModule &CGM;
  llvm::IRBuilder<> Builder;

  llvm::BasicBlock *Curr;

  ProcedureDeclaration *Proc;
  llvm::FunctionType *Fty;
  llvm::Function *Fn;

  struct BasicBlockDef {
    // Maps the variable (or formal parameter) to its definition.
    llvm::DenseMap<Decl *, llvm::TrackingVH<llvm::Value>> Defs;
    // Set of incompleted phi instructions.
    llvm::DenseMap<llvm::PHINode *, Decl *> IncompletePhis;
    // Block is sealed, that is, no more predecessors will be added.
    unsigned Sealed : 1;

    BasicBlockDef() : Sealed(0) {}
  };

  llvm::DenseMap<llvm::BasicBlock *, BasicBlockDef> CurrentDef;

  void writeLocalVariable(llvm::BasicBlock *BB, Decl *Decl, llvm::Value *Val);
  llvm::Value *readLocalVariable(llvm::BasicBlock *BB, Decl *Decl);
  llvm::Value *readLocalVariableRecursive(llvm::BasicBlock *BB, Decl *Decl);
  llvm::PHINode *addEmptyPhi(llvm::BasicBlock *BB, Decl *Decl);
  void addPhiOperands(llvm::BasicBlock *BB, Decl *Decl,
                      llvm::PHINode *Phi);
  void optimizePhi(llvm::PHINode *Phi);
  void sealBlock(llvm::BasicBlock *BB);

  llvm::DenseMap<FormalParameterDeclaration *, llvm::Argument *> FormalParams;

  void writeVariable(llvm::BasicBlock *BB, Decl *Decl, llvm::Value *Val);
  llvm::Value *readVariable(llvm::BasicBlock *BB, Decl *Decl);

  llvm::Type *mapType(Decl *Decl);
  llvm::FunctionType *createFunctionType(ProcedureDeclaration *Proc);
  llvm::Function *createFunction(ProcedureDeclaration *Proc, llvm::FunctionType *FTy);
protected:
  void setCurr(llvm::BasicBlock *BB) {
    Curr = BB;
    Builder.SetInsertPoint(Curr);
  }

  llvm::Value *emitInfixExpr(InfixExpression *E);
  llvm::Value *emitPrefixExpr(PrefixExpression *E);
  llvm::Value *emitExpr(Expr *E);

  void emitStmt(AssignmentStatement *Stmt);
  void emitStmt(ProcedureCallStatement *Stmt);
  void emitStmt(IfStatement *Stmt);
  void emitStmt(WhileStatement *Stmt);
  void emitStmt(ReturnStatement *Stmt);
  void emit(const StmtList &Stmts);

public:
  CGProcedure(CGModule &CGM)
      : CGM(CGM), Builder(CGM.getLLVMCtx()),
        Curr(nullptr){};

  void run(ProcedureDeclaration *Proc);
  void run();
};
} // namespace tinylang
#endif