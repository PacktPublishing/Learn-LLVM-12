#include "tinylang/CodeGen/CGProcedure.h"
#include "llvm/IR/CFG.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Debug.h"

using namespace tinylang;

void CGProcedure::writeLocalVariable(llvm::BasicBlock *BB,
                                     Decl *Decl,
                                     llvm::Value *Val) {
  assert(BB && "Basic block is nullptr");
  assert(
      (llvm::isa<VariableDeclaration>(Decl) ||
       llvm::isa<FormalParameterDeclaration>(Decl)) &&
      "Declaration must be variable or formal parameter");
  assert(Val && "Value is nullptr");
  CurrentDef[BB].Defs[Decl] = Val;
}

llvm::Value *
CGProcedure::readLocalVariable(llvm::BasicBlock *BB,
                               Decl *Decl) {
  assert(BB && "Basic block is nullptr");
  assert(
      (llvm::isa<VariableDeclaration>(Decl) ||
       llvm::isa<FormalParameterDeclaration>(Decl)) &&
      "Declaration must be variable or formal parameter");
  auto Val = CurrentDef[BB].Defs.find(Decl);
  if (Val != CurrentDef[BB].Defs.end())
    return Val->second;
  return readLocalVariableRecursive(BB, Decl);
}

llvm::Value *CGProcedure::readLocalVariableRecursive(
    llvm::BasicBlock *BB, Decl *Decl) {
  llvm::Value *Val = nullptr;
  if (!CurrentDef[BB].Sealed) {
    // Add incomplete phi for variable.
    llvm::PHINode *Phi = addEmptyPhi(BB, Decl);
    CurrentDef[BB].IncompletePhis[Phi] = Decl;
    Val = Phi;
  } else if (auto *PredBB = BB->getSinglePredecessor()) {
    // Only one predecessor.
    Val = readLocalVariable(PredBB, Decl);
  } else {
    // Create empty phi instruction to break potential
    // cycles.
    llvm::PHINode *Phi = addEmptyPhi(BB, Decl);
    writeLocalVariable(BB, Decl, Phi);
    Val = addPhiOperands(BB, Decl, Phi);
  }
  writeLocalVariable(BB, Decl, Val);
  return Val;
}

llvm::PHINode *
CGProcedure::addEmptyPhi(llvm::BasicBlock *BB, Decl *Decl) {
  return BB->empty()
             ? llvm::PHINode::Create(mapType(Decl), 0, "",
                                     BB)
             : llvm::PHINode::Create(mapType(Decl), 0, "",
                                     &BB->front());
}

llvm::Value *CGProcedure::addPhiOperands(
    llvm::BasicBlock *BB, Decl *Decl, llvm::PHINode *Phi) {
  for (auto I = llvm::pred_begin(BB),
            E = llvm::pred_end(BB);
       I != E; ++I) {
    Phi->addIncoming(readLocalVariable(*I, Decl), *I);
  }
  return optimizePhi(Phi);
}

llvm::Value *CGProcedure::optimizePhi(llvm::PHINode *Phi) {
  llvm::Value *Same = nullptr;
  for (llvm::Value *V : Phi->incoming_values()) {
    if (V == Same || V == Phi)
      continue;
    if (Same && V != Same)
      return Phi;
    Same = V;
  }
  if (Same == nullptr)
    Same = llvm::UndefValue::get(Phi->getType());
  // Collect phi instructions using this one.
  llvm::SmallVector<llvm::PHINode *, 8> CandidatePhis;
  for (llvm::Use &U : Phi->uses()) {
    if (auto *P =
            llvm::dyn_cast<llvm::PHINode>(U.getUser()))
      if (P != Phi)
        CandidatePhis.push_back(P);
  }
  Phi->replaceAllUsesWith(Same);
  Phi->eraseFromParent();
  for (auto *P : CandidatePhis)
    optimizePhi(P);
  return Same;
}

void CGProcedure::sealBlock(llvm::BasicBlock *BB) {
  assert(!CurrentDef[BB].Sealed &&
         "Attempt to seal already sealed block");
  for (auto PhiDecl : CurrentDef[BB].IncompletePhis) {
    addPhiOperands(BB, PhiDecl.second, PhiDecl.first);
  }
  CurrentDef[BB].IncompletePhis.clear();
  CurrentDef[BB].Sealed = true;
}

void CGProcedure::writeVariable(llvm::BasicBlock *BB,
                                Decl *D, llvm::Value *Val) {
  if (auto *V = llvm::dyn_cast<VariableDeclaration>(D)) {
    if (V->getEnclosingDecl() == Proc)
      writeLocalVariable(BB, D, Val);
    else if (V->getEnclosingDecl() ==
             CGM.getModuleDeclaration()) {
      Builder.CreateStore(Val, CGM.getGlobal(D));
    } else
      llvm::report_fatal_error(
          "Nested procedures not yet supported");
  } else if (auto *FP =
                 llvm::dyn_cast<FormalParameterDeclaration>(
                     D)) {
    if (FP->isVar()) {
      Builder.CreateStore(Val, FormalParams[FP]);
    } else
      writeLocalVariable(BB, D, Val);
  } else
    llvm::report_fatal_error("Unsupported declaration");
}

llvm::Value *CGProcedure::readVariable(llvm::BasicBlock *BB,
                                       Decl *D,
                                       bool LoadVal) {
  if (auto *V = llvm::dyn_cast<VariableDeclaration>(D)) {
    if (V->getEnclosingDecl() == Proc)
      return readLocalVariable(BB, D);
    else if (V->getEnclosingDecl() ==
             CGM.getModuleDeclaration()) {
      auto *Global = CGM.getGlobal(D);
      if (!LoadVal)
        return Global;
      return Builder.CreateLoad(mapType(D), Global);
    } else
      llvm::report_fatal_error(
          "Nested procedures not yet supported");
  } else if (auto *FP =
                 llvm::dyn_cast<FormalParameterDeclaration>(
                     D)) {
    if (FP->isVar()) {
      if (!LoadVal)
        return FormalParams[FP];
      return Builder.CreateLoad(
          mapType(FP)->getPointerElementType(),
          FormalParams[FP]);
    } else
      return readLocalVariable(BB, D);
  } else
    llvm::report_fatal_error("Unsupported declaration");
}

llvm::Type *CGProcedure::mapType(Decl *Decl) {
  if (auto *FP = llvm::dyn_cast<FormalParameterDeclaration>(
          Decl)) {
    llvm::Type *Ty = CGM.convertType(FP->getType());
    if (FP->isVar())
      Ty = Ty->getPointerTo();
    return Ty;
  }
  if (auto *V = llvm::dyn_cast<VariableDeclaration>(Decl))
    return CGM.convertType(V->getType());
  return CGM.convertType(llvm::cast<TypeDeclaration>(Decl));
}

llvm::FunctionType *CGProcedure::createFunctionType(
    ProcedureDeclaration *Proc) {
  llvm::Type *ResultTy = CGM.VoidTy;
  if (Proc->getRetType()) {
    ResultTy = mapType(Proc->getRetType());
  }
  auto FormalParams = Proc->getFormalParams();
  llvm::SmallVector<llvm::Type *, 8> ParamTypes;
  for (auto FP : FormalParams) {
    llvm::Type *Ty = mapType(FP);
    ParamTypes.push_back(Ty);
  }
  return llvm::FunctionType::get(ResultTy, ParamTypes,
                                 /* IsVarArgs */ false);
}

llvm::Function *
CGProcedure::createFunction(ProcedureDeclaration *Proc,
                            llvm::FunctionType *FTy) {
  llvm::Function *Fn = llvm::Function::Create(
      Fty, llvm::GlobalValue::ExternalLinkage,
      CGM.mangleName(Proc), CGM.getModule());
  // Give parameters a name.
  size_t Idx = 0;
  for (auto I = Fn->arg_begin(), E = Fn->arg_end(); I != E;
       ++I, ++Idx) {
    llvm::Argument *Arg = I;
    FormalParameterDeclaration *FP =
        Proc->getFormalParams()[Idx];
    if (FP->isVar()) {
      llvm::AttrBuilder Attr;
      llvm::TypeSize Sz =
          CGM.getModule()->getDataLayout().getTypeStoreSize(
              CGM.convertType(FP->getType()));
      Attr.addDereferenceableAttr(Sz);
      Attr.addAttribute(llvm::Attribute::NoCapture);
      Arg->addAttrs(Attr);
    }
    Arg->setName(FP->getName());
  }
  return Fn;
}

llvm::Value *
CGProcedure::emitInfixExpr(InfixExpression *E) {
  llvm::Value *Left = emitExpr(E->getLeft());
  llvm::Value *Right = emitExpr(E->getRight());
  llvm::Value *Result = nullptr;
  switch (E->getOperatorInfo().getKind()) {
  case tok::plus:
    Result = Builder.CreateNSWAdd(Left, Right);
    break;
  case tok::minus:
    Result = Builder.CreateNSWSub(Left, Right);
    break;
  case tok::star:
    Result = Builder.CreateNSWMul(Left, Right);
    break;
  case tok::kw_DIV:
    Result = Builder.CreateSDiv(Left, Right);
    break;
  case tok::kw_MOD:
    Result = Builder.CreateSRem(Left, Right);
    break;
  case tok::equal:
    Result = Builder.CreateICmpEQ(Left, Right);
    break;
  case tok::hash:
    Result = Builder.CreateICmpNE(Left, Right);
    break;
  case tok::less:
    Result = Builder.CreateICmpSLT(Left, Right);
    break;
  case tok::lessequal:
    Result = Builder.CreateICmpSLE(Left, Right);
    break;
  case tok::greater:
    Result = Builder.CreateICmpSGT(Left, Right);
    break;
  case tok::greaterequal:
    Result = Builder.CreateICmpSGE(Left, Right);
    break;
  case tok::kw_AND:
    Result = Builder.CreateAnd(Left, Right);
    break;
  case tok::kw_OR:
    Result = Builder.CreateOr(Left, Right);
    break;
  case tok::slash:
    // Divide by real numbers not supported.
    LLVM_FALLTHROUGH;
  default:
    llvm_unreachable("Wrong operator");
  }
  return Result;
}

llvm::Value *
CGProcedure::emitPrefixExpr(PrefixExpression *E) {
  llvm::Value *Result = emitExpr(E->getExpr());
  switch (E->getOperatorInfo().getKind()) {
  case tok::plus:
    // Identity - nothing to do.
    break;
  case tok::minus:
    Result = Builder.CreateNeg(Result);
    break;
  case tok::kw_NOT:
    Result = Builder.CreateNot(Result);
    break;
  default:
    llvm_unreachable("Wrong operator");
  }
  return Result;
}

llvm::Value *CGProcedure::emitExpr(Expr *E) {
  if (auto *Infix = llvm::dyn_cast<InfixExpression>(E)) {
    return emitInfixExpr(Infix);
  } else if (auto *Prefix =
                 llvm::dyn_cast<PrefixExpression>(E)) {
    return emitPrefixExpr(Prefix);
  } else if (auto *Var = llvm::dyn_cast<Designator>(E)) {
    auto *Decl = Var->getDecl();
    llvm::Value *Val = readVariable(Curr, Decl);
    // With more languages features in place, here you
    // need to add array and record support.
    auto &Selectors = Var->getSelectors();
    for (auto I = Selectors.begin(), E = Selectors.end();
         I != E;
         /* no increment */) {
      if (auto *IdxSel =
              llvm::dyn_cast<IndexSelector>(*I)) {
        llvm::SmallVector<llvm::Value *, 4> IdxList;
        while (I != E) {
          if (auto *Sel =
                  llvm::dyn_cast<IndexSelector>(*I)) {
            IdxList.push_back(emitExpr(Sel->getIndex()));
            ++I;
          } else
            break;
        }
        Val = Builder.CreateInBoundsGEP(Val, IdxList);
        Val = Builder.CreateLoad(
            Val->getType()->getPointerElementType(), Val);
      } else if (auto *FieldSel =
                     llvm::dyn_cast<FieldSelector>(*I)) {
        llvm::SmallVector<llvm::Value *, 4> IdxList;
        while (I != E) {
          if (auto *Sel =
                  llvm::dyn_cast<FieldSelector>(*I)) {
            llvm::Value *V = llvm::ConstantInt::get(
                CGM.Int64Ty, Sel->getIndex());
            IdxList.push_back(V);
            ++I;
          } else
            break;
        }
        Val = Builder.CreateInBoundsGEP(Val, IdxList);
        Val = Builder.CreateLoad(
            Val->getType()->getPointerElementType(), Val);
      } else if (auto *DerefSel =
                     llvm::dyn_cast<DereferenceSelector>(
                         *I)) {
        Val = Builder.CreateLoad(
            Val->getType()->getPointerElementType(), Val);
        ++I;
      } else {
        llvm::report_fatal_error("Unsupported selector");
      }
    }

    return Val;
  } else if (auto *Const =
                 llvm::dyn_cast<ConstantAccess>(E)) {
    return emitExpr(Const->getDecl()->getExpr());
  } else if (auto *IntLit =
                 llvm::dyn_cast<IntegerLiteral>(E)) {
    return llvm::ConstantInt::get(CGM.Int64Ty,
                                  IntLit->getValue());
  } else if (auto *BoolLit =
                 llvm::dyn_cast<BooleanLiteral>(E)) {
    return llvm::ConstantInt::get(CGM.Int1Ty,
                                  BoolLit->getValue());
  }
  llvm::report_fatal_error("Unsupported expression");
}

void CGProcedure::emitStmt(AssignmentStatement *Stmt) {
  auto *Val = emitExpr(Stmt->getExpr());
  Designator *Desig = Stmt->getVar();
  auto &Selectors = Desig->getSelectors();
  if (Selectors.empty())
    writeVariable(Curr, Desig->getDecl(), Val);
  else {
    llvm::SmallVector<llvm::Value *, 4> IdxList;
    // First index for GEP.
    IdxList.push_back(
        llvm::ConstantInt::get(CGM.Int32Ty, 0));
    auto *Base =
        readVariable(Curr, Desig->getDecl(), false);
    for (auto I = Selectors.begin(), E = Selectors.end();
         I != E; ++I) {
      if (auto *IdxSel =
              llvm::dyn_cast<IndexSelector>(*I)) {
        IdxList.push_back(emitExpr(IdxSel->getIndex()));
      } else if (auto *FieldSel =
                     llvm::dyn_cast<FieldSelector>(*I)) {
        llvm::Value *V = llvm::ConstantInt::get(
            CGM.Int32Ty, FieldSel->getIndex());
        IdxList.push_back(V);
      } else {
        llvm::report_fatal_error("not implemented");
      }
    }
    if (!IdxList.empty()) {
      if (Base->getType()->isPointerTy()) {
        Base = Builder.CreateInBoundsGEP(Base, IdxList);
        Builder.CreateStore(Val, Base);
      }
      else {
        llvm::report_fatal_error("should not happen");
      }
    }
  }
}

void CGProcedure::emitStmt(ProcedureCallStatement *Stmt) {
  llvm::report_fatal_error("not implemented");
}

void CGProcedure::emitStmt(IfStatement *Stmt) {
  bool HasElse = Stmt->getElseStmts().size() > 0;

  // Create the required basic blocks.
  llvm::BasicBlock *IfBB = createBasicBlock("if.body");
  llvm::BasicBlock *ElseBB =
      HasElse ? createBasicBlock("else.body") : nullptr;
  llvm::BasicBlock *AfterIfBB =
      createBasicBlock("after.if");

  llvm::Value *Cond = emitExpr(Stmt->getCond());
  Builder.CreateCondBr(Cond, IfBB,
                       HasElse ? ElseBB : AfterIfBB);
  sealBlock(Curr);

  setCurr(IfBB);
  emit(Stmt->getIfStmts());
  if (!Curr->getTerminator()) {
    Builder.CreateBr(AfterIfBB);
  }
  sealBlock(Curr);

  if (HasElse) {
    setCurr(ElseBB);
    emit(Stmt->getElseStmts());
    if (!Curr->getTerminator()) {
      Builder.CreateBr(AfterIfBB);
    }
    sealBlock(Curr);
  }
  setCurr(AfterIfBB);
}

void CGProcedure::emitStmt(WhileStatement *Stmt) {
  // The basic block for the condition.
  llvm::BasicBlock *WhileCondBB;
  // The basic block for the while body.
  llvm::BasicBlock *WhileBodyBB =
      createBasicBlock("while.body");
  // The basic block after the while statement.
  llvm::BasicBlock *AfterWhileBB =
      createBasicBlock("after.while");

  if (Curr->empty()) {
    Curr->setName("while.cond");
    WhileCondBB = Curr;
  } else {
    WhileCondBB = createBasicBlock("while.cond");
    Builder.CreateBr(WhileCondBB);
    sealBlock(Curr);
    setCurr(WhileCondBB);
  }

  llvm::Value *Cond = emitExpr(Stmt->getCond());
  Builder.CreateCondBr(Cond, WhileBodyBB, AfterWhileBB);

  setCurr(WhileBodyBB);
  emit(Stmt->getWhileStmts());
  Builder.CreateBr(WhileCondBB);
  sealBlock(WhileCondBB);
  sealBlock(Curr);

  setCurr(AfterWhileBB);
}

void CGProcedure::emitStmt(ReturnStatement *Stmt) {
  if (Stmt->getRetVal()) {
    llvm::Value *RetVal = emitExpr(Stmt->getRetVal());
    Builder.CreateRet(RetVal);
  } else {
    Builder.CreateRetVoid();
  }
}

void CGProcedure::emit(const StmtList &Stmts) {
  for (auto *S : Stmts) {
    if (auto *Stmt = llvm::dyn_cast<AssignmentStatement>(S))
      emitStmt(Stmt);
    else if (auto *Stmt =
                 llvm::dyn_cast<ProcedureCallStatement>(S))
      emitStmt(Stmt);
    else if (auto *Stmt = llvm::dyn_cast<IfStatement>(S))
      emitStmt(Stmt);
    else if (auto *Stmt = llvm::dyn_cast<WhileStatement>(S))
      emitStmt(Stmt);
    else if (auto *Stmt =
                 llvm::dyn_cast<ReturnStatement>(S))
      emitStmt(Stmt);
    else
      llvm_unreachable("Unknown statement");
  }
}

void CGProcedure::run(ProcedureDeclaration *Proc) {
  this->Proc = Proc;
  Fty = createFunctionType(Proc);
  Fn = createFunction(Proc, Fty);

  llvm::BasicBlock *BB = createBasicBlock("entry");
  setCurr(BB);

  size_t Idx = 0;
  auto &Defs = CurrentDef[BB];
  for (auto I = Fn->arg_begin(), E = Fn->arg_end(); I != E;
       ++I, ++Idx) {
    llvm::Argument *Arg = I;
    FormalParameterDeclaration *FP =
        Proc->getFormalParams()[Idx];
    // Create mapping FormalParameter -> llvm::Argument
    // for VAR parameters.
    FormalParams[FP] = Arg;
    writeLocalVariable(Curr, FP, Arg);
  }

  for (auto *D : Proc->getDecls()) {
    if (auto *Var =
            llvm::dyn_cast<VariableDeclaration>(D)) {
      llvm::Type *Ty = mapType(Var);
      if (Ty->isAggregateType()) {
        llvm::Value *Val = Builder.CreateAlloca(Ty);
        writeLocalVariable(Curr, Var, Val);
      }
    }
  }

  auto Block = Proc->getStmts();
  emit(Proc->getStmts());
  if (!Curr->getTerminator()) {
    Builder.CreateRetVoid();
  }
  sealBlock(Curr);
}

void CGProcedure::run() {}
