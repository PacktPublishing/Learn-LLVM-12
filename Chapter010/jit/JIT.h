
#ifndef JIT_H
#define JIT_H

#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/IRTransformLayer.h"
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
#include "llvm/ExecutionEngine/Orc/Mangling.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/Orc/TargetProcessControl.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/Error.h"

class JIT {
private:
  std::unique_ptr<llvm::orc::TargetProcessControl> TPC;
  std::unique_ptr<llvm::orc::ExecutionSession> ES;
  llvm::DataLayout DL;
  llvm::orc::MangleAndInterner Mangle;

  std::unique_ptr<llvm::orc::RTDyldObjectLinkingLayer>
      ObjectLinkingLayer;
  std::unique_ptr<llvm::orc::IRCompileLayer>
      CompileLayer;
  std::unique_ptr<llvm::orc::IRTransformLayer>
      OptIRLayer;
  llvm::orc::JITDylib &MainJITDylib;

public:
  JIT(std::unique_ptr<llvm::orc::TargetProcessControl>
          TPCtrl,
      std::unique_ptr<llvm::orc::ExecutionSession>
          ExeS,
      llvm::DataLayout DataL,
      llvm::orc::JITTargetMachineBuilder JTMB)
      : TPC(std::move(TPCtrl)), ES(std::move(ExeS)),
        DL(std::move(DataL)), Mangle(*ES, DL),
        ObjectLinkingLayer(std::move(
            createObjectLinkingLayer(*ES, JTMB))),
        CompileLayer(std::move(createCompileLayer(
            *ES, *ObjectLinkingLayer,
            std::move(JTMB)))),
        OptIRLayer(std::move(
            createOptIRLayer(*ES, *CompileLayer))),
        MainJITDylib(
            ES->createBareJITDylib("<main>")) {
    MainJITDylib.addGenerator(llvm::cantFail(
        llvm::orc::DynamicLibrarySearchGenerator::
            GetForCurrentProcess(
                DL.getGlobalPrefix())));
  }

  ~JIT() {
    if (auto Err = ES->endSession())
      ES->reportError(std::move(Err));
  }

  static llvm::Expected<std::unique_ptr<JIT>>
  create() {
    auto SSP = std::make_shared<
        llvm::orc::SymbolStringPool>();
    auto TPC =
        llvm::orc::SelfTargetProcessControl::Create(
            SSP);
    if (!TPC)
      return TPC.takeError();
    llvm::orc::JITTargetMachineBuilder JTMB(
        (*TPC)->getTargetTriple());

    auto DL = JTMB.getDefaultDataLayoutForTarget();
    if (!DL)
      return DL.takeError();

    auto ES =
        std::make_unique<llvm::orc::ExecutionSession>(
            std::move(SSP));

    return std::make_unique<JIT>(
        std::move(*TPC), std::move(ES), std::move(*DL),
        std::move(JTMB));
  }

  static std::unique_ptr<
      llvm::orc::RTDyldObjectLinkingLayer>
  createObjectLinkingLayer(
      llvm::orc::ExecutionSession &ES,
      llvm::orc::JITTargetMachineBuilder &JTMB) {
    auto GetMemoryManager = []() {
      return std::make_unique<
          llvm::SectionMemoryManager>();
    };
    auto OLLayer = std::make_unique<
        llvm::orc::RTDyldObjectLinkingLayer>(
        ES, GetMemoryManager);
    if (JTMB.getTargetTriple().isOSBinFormatCOFF()) {
      OLLayer
          ->setOverrideObjectFlagsWithResponsibilityFlags(
              true);
      OLLayer
          ->setAutoClaimResponsibilityForObjectSymbols(
              true);
    }
    return std::move(OLLayer);
  }

  static std::unique_ptr<llvm::orc::IRCompileLayer>
  createCompileLayer(
      llvm::orc::ExecutionSession &ES,
      llvm::orc::RTDyldObjectLinkingLayer &OLLayer,
      llvm::orc::JITTargetMachineBuilder JTMB) {
    auto IRCompiler = std::make_unique<
        llvm::orc::ConcurrentIRCompiler>(
        std::move(JTMB));
    auto IRCLayer =
        std::make_unique<llvm::orc::IRCompileLayer>(
            ES, OLLayer, std::move(IRCompiler));
    return std::move(IRCLayer);
  }

  static std::unique_ptr<llvm::orc::IRTransformLayer>
  createOptIRLayer(
      llvm::orc::ExecutionSession &ES,
      llvm::orc::IRCompileLayer &CompileLayer) {
    auto OptIRLayer =
        std::make_unique<llvm::orc::IRTransformLayer>(
            ES, CompileLayer, optimizeModule);
    return std::move(OptIRLayer);
  }

  llvm::orc::JITDylib &getMainJITDylib() {
    return MainJITDylib;
  }

  llvm::Error addIRModule(
      llvm::orc::ThreadSafeModule TSM,
      llvm::orc::ResourceTrackerSP RT = nullptr) {
    if (!RT)
      RT = MainJITDylib.getDefaultResourceTracker();
    return OptIRLayer->add(RT, std::move(TSM));
  }

  llvm::Expected<llvm::JITEvaluatedSymbol>
  lookup(llvm::StringRef Name) {
    return ES->lookup({&MainJITDylib},
                      Mangle(Name.str()));
  }

  static llvm::Expected<llvm::orc::ThreadSafeModule>
  optimizeModule(
      llvm::orc::ThreadSafeModule TSM,
      const llvm::orc::MaterializationResponsibility
          &R) {
    TSM.withModuleDo([](llvm::Module &M) {
      bool DebugPM = false;
      llvm::PassBuilder PB(DebugPM);
      llvm::LoopAnalysisManager LAM(DebugPM);
      llvm::FunctionAnalysisManager FAM(DebugPM);
      llvm::CGSCCAnalysisManager CGAM(DebugPM);
      llvm::ModuleAnalysisManager MAM(DebugPM);
      FAM.registerPass(
          [&] { return PB.buildDefaultAAPipeline(); });
      PB.registerModuleAnalyses(MAM);
      PB.registerCGSCCAnalyses(CGAM);
      PB.registerFunctionAnalyses(FAM);
      PB.registerLoopAnalyses(LAM);
      PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);
      llvm::ModulePassManager MPM =
          PB.buildPerModuleDefaultPipeline(
              llvm::PassBuilder::OptimizationLevel::O2,
              DebugPM);
      MPM.run(M, MAM);
    });

    return std::move(TSM);
  }
};

#endif