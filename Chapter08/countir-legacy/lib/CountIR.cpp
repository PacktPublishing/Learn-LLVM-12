#include "CountIR.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/Debug.h"

// Run as:
// opt -load-pass-plugin=lib/CountIRPass.so
// -passes="countirpass" --stats ../a.ll

using namespace llvm;

#define DEBUG_TYPE "countir"

STATISTIC(NumOfInst, "Number of instructions.");
STATISTIC(NumOfBB, "Number of basic blocks.");

void runCounting(Function &F) {
  for (BasicBlock &BB : F) {
    ++NumOfBB;
    for (Instruction &I : BB) {
      (void)I;
      ++NumOfInst;
    }
  }
}

PreservedAnalyses
CountIRPass::run(Function &F, FunctionAnalysisManager &AM) {
  runCounting(F);
  return PreservedAnalyses::all();
}

bool CountIRLegacyPass::runOnFunction(Function &F) {
  runCounting(F);
  return false;
}

void CountIRLegacyPass::getAnalysisUsage(
    AnalysisUsage &AU) const {
  AU.setPreservesAll();
}

char CountIRLegacyPass::ID = 0;

static RegisterPass<CountIRLegacyPass>
    X("countir", "CountIR Pass");

bool PipelineParsingCB(
    StringRef Name, FunctionPassManager &FPM,
    ArrayRef<PassBuilder::PipelineElement>) {
  if (Name == "countir") {
    FPM.addPass(CountIRPass());
    return true;
  }
  return false;
}

extern "C" ::llvm::PassPluginLibraryInfo LLVM_ATTRIBUTE_WEAK
llvmGetPassPluginInfo() {
  return {LLVM_PLUGIN_API_VERSION, "CountIR", "v0.1",
          [](PassBuilder &PB) {
            PB.registerPipelineParsingCallback(
                [](StringRef Name, FunctionPassManager &FPM,
                   ArrayRef<PassBuilder::PipelineElement>) {
                  if (Name == "countir") {
                    FPM.addPass(CountIRPass());
                    return true;
                  }
                  return false;
                });
          }};
}