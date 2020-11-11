#include "tinylang/CodeGen/CGDebugInfo.h"
#include "tinylang/CodeGen/CGModule.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"

using namespace tinylang;

CGDebugInfo::CGDebugInfo(CGModule &CGM)
    : CGM(CGM), DBuilder(*CGM.getModule()) {
  llvm::SmallString<128> Path(
      CGM.getASTCtx().getFilename());
  llvm::sys::fs::make_absolute(Path);

  llvm::DIFile *File = DBuilder.createFile(
      llvm::sys::path::filename(Path),
      llvm::sys::path::parent_path(Path));

  bool IsOptimzed = false;
  unsigned ObjCRunTimeVersion = 0;
  llvm::DICompileUnit::DebugEmissionKind EmissionKind =
      llvm::DICompileUnit::DebugEmissionKind::FullDebug;
  CU = DBuilder.createCompileUnit(
      llvm::dwarf::DW_LANG_Modula2, File, "tinylang",
      IsOptimzed, StringRef(), ObjCRunTimeVersion,
      StringRef(), EmissionKind);
}

llvm::DIScope *CGDebugInfo::getScope() {
  if (ScopeStack.empty())
    openScope(CU->getFile());
  return ScopeStack.back();
}

void CGDebugInfo::openScope(llvm::DIScope *Scope) {
  ScopeStack.push_back(Scope);
}

void CGDebugInfo::closeScope() { ScopeStack.pop_back(); }

unsigned CGDebugInfo::getLineNumber(SMLoc Loc) {
  return CGM.getASTCtx().getSourceMgr().FindLineNumber(Loc);
}

llvm::DIType *
CGDebugInfo::getPervasiveType(TypeDeclaration *Ty) {
  if (Ty->getName() == "INTEGER") {
    return DBuilder.createBasicType(
        Ty->getName(), 64, llvm::dwarf::DW_ATE_signed);
  }
  if (Ty->getName() == "BOOLEAN") {
    return DBuilder.createBasicType(
        Ty->getName(), 1, llvm::dwarf::DW_ATE_boolean);
  }
  llvm::report_fatal_error("Unsupported pervasive type");
}

llvm::DIType *
CGDebugInfo::getAliasType(AliasTypeDeclaration *Ty) {
  return DBuilder.createTypedef(
      getType(Ty->getType()), Ty->getName(), CU->getFile(),
      getLineNumber(Ty->getLocation()), getScope());
}

llvm::DIType *
CGDebugInfo::getArrayType(ArrayTypeDeclaration *Ty) {
  auto *ATy =
      llvm::cast<llvm::ArrayType>(CGM.convertType(Ty));
  const llvm::DataLayout &DL =
      CGM.getModule()->getDataLayout();

  Expr *Nums = Ty->getNums();
  uint64_t NumElements = 5; // TODO Eval Nums
  llvm::SmallVector<llvm::Metadata *, 4> Subscripts;
  Subscripts.push_back(
      DBuilder.getOrCreateSubrange(0, NumElements));
  return DBuilder.createArrayType(
      DL.getTypeSizeInBits(ATy) * 8,
      DL.getABITypeAlignment(ATy), getType(Ty->getType()),
      DBuilder.getOrCreateArray(Subscripts));
}

llvm::DIType *
CGDebugInfo::getRecordType(RecordTypeDeclaration *Ty) {
  llvm::DIType *T = nullptr;
  return T;
}

llvm::DIType *CGDebugInfo::getType(TypeDeclaration *Ty) {
  if (llvm::DIType *T = TypeCache[Ty])
    return T;

  if (llvm::isa<PervasiveTypeDeclaration>(Ty))
    return TypeCache[Ty] = getPervasiveType(Ty);
  else if (auto *AliasTy =
               llvm::dyn_cast<AliasTypeDeclaration>(Ty))
    return TypeCache[Ty] = getAliasType(AliasTy);
  else if (auto *ArrayTy =
               llvm::dyn_cast<ArrayTypeDeclaration>(Ty))
    return TypeCache[Ty] = getArrayType(ArrayTy);
  else if (auto *RecordTy =
               llvm ::dyn_cast<RecordTypeDeclaration>(Ty))
    return TypeCache[Ty] = getRecordType(RecordTy);
  llvm::report_fatal_error("Unsupported type");
  return nullptr;
}

llvm::DISubroutineType *
CGDebugInfo::getType(ProcedureDeclaration *P) {
  llvm::SmallVector<llvm::Metadata *, 4> Types;
  const llvm::DataLayout &DL =
      CGM.getModule()->getDataLayout();
  // Return type at index 0
  if (P->getRetType())
    Types.push_back(getType(P->getRetType()));
  else
    Types.push_back(DBuilder.createUnspecifiedType("void"));
  for (const auto *FP : P->getFormalParams()) {
    llvm::DIType *PT = getType(FP->getType());
    if (FP->isVar()) {
      llvm::Type *PTy = CGM.convertType(FP->getType());
      PT = DBuilder.createReferenceType(
          llvm::dwarf::DW_TAG_reference_type, PT,
          DL.getTypeSizeInBits(PTy) * 8,
          DL.getABITypeAlignment(PTy));
    }
    Types.push_back(PT);
  }
  return DBuilder.createSubroutineType(
      DBuilder.getOrCreateTypeArray(Types));
}

void CGDebugInfo::emitGlobalVariable(
    VariableDeclaration *Decl, llvm::GlobalVariable *V) {
  llvm::DIGlobalVariableExpression *GV =
      DBuilder.createGlobalVariableExpression(
          getScope(), Decl->getName(), V->getName(),
          CU->getFile(), getLineNumber(Decl->getLocation()),
          getType(Decl->getType()), false);
  V->addDebugInfo(GV);
}

void CGDebugInfo::emitProcedure(ProcedureDeclaration *Decl,
                                llvm::Function *Fn) {
  llvm::DISubroutineType *SubT = getType(Decl);
  llvm::DISubprogram *Sub = DBuilder.createFunction(
      getScope(), Decl->getName(), Fn->getName(),
      CU->getFile(), getLineNumber(Decl->getLocation()),
      SubT, getLineNumber(Decl->getLocation()),
      llvm::DINode::FlagPrototyped,
      llvm::DISubprogram::SPFlagDefinition);
  openScope(Sub);
  Fn->setSubprogram(Sub);
}

void CGDebugInfo::emitProcedureEnd(
    ProcedureDeclaration *Decl, llvm::Function *Fn) {
  if (Fn && Fn->getSubprogram())
    DBuilder.finalizeSubprogram(Fn->getSubprogram());
  closeScope();
}

llvm::DILocalVariable *CGDebugInfo::emitParameterVariable(
    FormalParameterDeclaration *FP, size_t Idx,
    llvm::Value *Val, llvm::BasicBlock *BB) {
  assert(llvm::isa<llvm::DILocalScope>(getScope()) &&
         "No local scope");
  llvm::DILocalVariable *Var =
      DBuilder.createParameterVariable(
          getScope(), FP->getName(), Idx, CU->getFile(),
          getLineNumber(FP->getLocation()),
          getType(FP->getType()));
  DBuilder.insertDbgValueIntrinsic(
      Val, Var, DBuilder.createExpression(),
      getDebugLoc(FP->getLocation()), BB);
  return Var;
}

void CGDebugInfo::emitValue(llvm::Value *Val,
                            llvm::DILocalVariable *Var,
                            SMLoc Loc,
                            llvm::BasicBlock *BB) {
  llvm::DebugLoc DLoc = getDebugLoc(Loc);
  llvm::Instruction *Instr =
      DBuilder.insertDbgValueIntrinsic(
          Val, Var, DBuilder.createExpression(), DLoc, BB);
  Instr->setDebugLoc(DLoc);
}

llvm::DebugLoc CGDebugInfo::getDebugLoc(SMLoc Loc) {
  std::pair<unsigned, unsigned> LineAndCol =
      CGM.getASTCtx().getSourceMgr().getLineAndColumn(Loc);
  llvm::DILocation *DILoc = llvm::DILocation::get(
      CGM.getLLVMCtx(), LineAndCol.first, LineAndCol.second,
      getScope());
  return llvm::DebugLoc(DILoc);
}

void CGDebugInfo::finalize() { DBuilder.finalize(); }