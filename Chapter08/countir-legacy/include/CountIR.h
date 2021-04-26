#ifndef COUNTIR_H
#define COUNTIR_H

#include "llvm/IR/PassManager.h"
#include "llvm/Pass.h"

class CountIRPass
    : public llvm::PassInfoMixin<CountIRPass> {
public:
  llvm::PreservedAnalyses
  run(llvm::Function &F,
      llvm::FunctionAnalysisManager &AM);
};

class CountIRLegacyPass : public llvm::FunctionPass {
public:
  static char ID;
  CountIRLegacyPass() : llvm::FunctionPass(ID) {}
  bool runOnFunction(llvm::Function &F) override;
  void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;
};

#endif
