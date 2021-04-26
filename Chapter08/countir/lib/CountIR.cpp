#include "CountIR.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/Debug.h"

using namespace llvm;

#define DEBUG_TYPE "countir"

STATISTIC(NumOfInst, "Number of instructions.");
STATISTIC(NumOfBB, "Number of basic blocks.");

PreservedAnalyses
CountIRPass::run(Function &F, FunctionAnalysisManager &AM) {
  for (BasicBlock &BB : F) {
    ++NumOfBB;
    for (Instruction &I : BB) {
      (void)I;
      ++NumOfInst;
    }
  }
  return PreservedAnalyses::all();
}

bool PipelineParsingCB(
    StringRef Name, FunctionPassManager &FPM,
    ArrayRef<PassBuilder::PipelineElement>) {
  if (Name == "countir") {
    FPM.addPass(CountIRPass());
    return true;
  }
  return false;
}

void RegisterCB(PassBuilder &PB) {
  PB.registerPipelineParsingCallback(PipelineParsingCB);
}

extern "C" ::llvm::PassPluginLibraryInfo LLVM_ATTRIBUTE_WEAK
llvmGetPassPluginInfo() {
  return {LLVM_PLUGIN_API_VERSION, "CountIR", "v0.1",
          RegisterCB};
}

/* Alternative version using lambda function:
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
*/