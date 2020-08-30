#include "tinylang/Sema/Sema.h"
#include "llvm/Support/raw_ostream.h"

using namespace tinylang;

void Sema::enterScope(Decl *D) {
  CurrentScope = new Scope(CurrentScope);
  CurrentDecl = D;
}

void Sema::leaveScope() {
  assert(CurrentScope && "Can't leave non-existing scope");
  Scope *Parent = CurrentScope->getParent();
  delete CurrentScope;
  CurrentScope = Parent;
  CurrentDecl = CurrentDecl->getEnclosingDecl();
}

bool Sema::isOperatorForType(tok::TokenKind Op,
                             TypeDeclaration *Ty) {
  switch (Op) {
  case tok::plus:
  case tok::minus:
  case tok::star:
  case tok::kw_DIV:
  case tok::kw_MOD:
    return Ty == IntegerType;
  case tok::slash:
    return false; // REAL not implemented
  case tok::kw_AND:
  case tok::kw_OR:
  case tok::kw_NOT:
    return Ty == BooleanType;
  default:
    llvm_unreachable("Unknown operator");
  }
}

void Sema::checkFormalAndActualParameters(
    SMLoc Loc, const FormalParamList &Formals,
    const ExprList &Actuals) {
  if (Formals.size() != Actuals.size()) {
    Diags.report(Loc, diag::err_wrong_number_of_parameters);
    return;
  }
  auto A = Actuals.begin();
  for (auto I = Formals.begin(), E = Formals.end(); I != E;
       ++I, ++A) {
    FormalParameterDeclaration *F = *I;
    Expr *Arg = *A;
    if (F->getType() != Arg->getType())
      Diags.report(
          Loc,
          diag::
              err_type_of_formal_and_actual_parameter_not_compatible);
    if (F->isVar() && isa<VariableAccess>(Arg))
      Diags.report(Loc,
                   diag::err_var_parameter_requires_var);
  }
}

void Sema::initialize() {
  // Setup global scope.
  CurrentScope = new Scope();
  CurrentDecl = nullptr;
  IntegerType =
      new TypeDeclaration(CurrentDecl, SMLoc(), "INTEGER");
  BooleanType =
      new TypeDeclaration(CurrentDecl, SMLoc(), "BOOLEAN");
  TrueLiteral = new BooleanLiteral(true, BooleanType);
  FalseLiteral = new BooleanLiteral(false, BooleanType);
  TrueConst = new ConstantDeclaration(CurrentDecl, SMLoc(),
                                      "TRUE", TrueLiteral);
  FalseConst = new ConstantDeclaration(
      CurrentDecl, SMLoc(), "FALSE", FalseLiteral);
  CurrentScope->insert(IntegerType);
  CurrentScope->insert(BooleanType);
  CurrentScope->insert(TrueConst);
  CurrentScope->insert(FalseConst);
}

ModuleDeclaration *
Sema::actOnModuleDeclaration(SMLoc Loc, StringRef Name) {
  return new ModuleDeclaration(CurrentDecl, Loc, Name);
}

void Sema::actOnModuleDeclaration(
    ModuleDeclaration *ModDecl, SMLoc Loc, StringRef Name,
    DeclList &Decls, StmtList &Stmts) {
  if (Name != ModDecl->getName()) {
    Diags.report(Loc,
                 diag::err_module_identifier_not_equal);
    Diags.report(ModDecl->getLocation(),
                 diag::note_module_identifier_declaration);
  }
  ModDecl->setDecls(Decls);
  ModDecl->setStmts(Stmts);
}

void Sema::actOnImport(StringRef ModuleName,
                       IdentList &Ids) {
  Diags.report(SMLoc(), diag::err_not_yet_implemented);
}

void Sema::actOnConstantDeclaration(DeclList &Decls,
                                    SMLoc Loc,
                                    StringRef Name,
                                    Expr *E) {
  assert(CurrentScope && "CurrentScope not set");
  ConstantDeclaration *Decl =
      new ConstantDeclaration(CurrentDecl, Loc, Name, E);
  if (CurrentScope->insert(Decl))
    Decls.push_back(Decl);
  else
    Diags.report(Loc, diag::err_symbold_declared, Name);
}

void Sema::actOnVariableDeclaration(DeclList &Decls,
                                    IdentList &Ids,
                                    Decl *D) {
  assert(CurrentScope && "CurrentScope not set");
  if (TypeDeclaration *Ty = dyn_cast<TypeDeclaration>(D)) {
    for (auto I = Ids.begin(), E = Ids.end(); I != E; ++I) {
      SMLoc Loc = I->first;
      StringRef Name = I->second;
      VariableDeclaration *Decl = new VariableDeclaration(
          CurrentDecl, Loc, Name, Ty);
      if (CurrentScope->insert(Decl))
        Decls.push_back(Decl);
      else
        Diags.report(Loc, diag::err_symbold_declared, Name);
    }
  } else if (!Ids.empty()) {
    SMLoc Loc = Ids.front().first;
    Diags.report(Loc, diag::err_vardecl_requires_type);
  }
}

void Sema::actOnFormalParameterDeclaration(
    FormalParamList &Params, IdentList &Ids, Decl *D,
    bool IsVar) {
  assert(CurrentScope && "CurrentScope not set");
  if (TypeDeclaration *Ty = dyn_cast<TypeDeclaration>(D)) {
    for (auto I = Ids.begin(), E = Ids.end(); I != E; ++I) {
      SMLoc Loc = I->first;
      StringRef Name = I->second;
      FormalParameterDeclaration *Decl =
          new FormalParameterDeclaration(CurrentDecl, Loc,
                                         Name, Ty, IsVar);
      if (CurrentScope->insert(Decl))
        Params.push_back(Decl);
      else
        Diags.report(Loc, diag::err_symbold_declared, Name);
    }
  } else if (!Ids.empty()) {
    SMLoc Loc = Ids.front().first;
    Diags.report(Loc, diag::err_vardecl_requires_type);
  }
}

ProcedureDeclaration *
Sema::actOnProcedureDeclaration(SMLoc Loc, StringRef Name) {
  ProcedureDeclaration *P =
      new ProcedureDeclaration(CurrentDecl, Loc, Name);
  if (!CurrentScope->insert(P))
    Diags.report(Loc, diag::err_symbold_declared, Name);
  return P;
}

void Sema::actOnProcedureHeading(
    ProcedureDeclaration *ProcDecl, FormalParamList &Params,
    Decl *RetType) {
  ProcDecl->setFormalParams(Params);
  auto RetTypeDecl =
      dyn_cast_or_null<TypeDeclaration>(RetType);
  if (!RetTypeDecl && RetType)
    Diags.report(RetType->getLocation(),
                 diag::err_returntype_must_be_type);
  else
    ProcDecl->setRetType(RetTypeDecl);
}

void Sema::actOnProcedureDeclaration(
    ProcedureDeclaration *ProcDecl, SMLoc Loc,
    StringRef Name, DeclList &Decls, StmtList &Stmts) {

  if (Name != ProcDecl->getName()) {
    Diags.report(Loc, diag::err_proc_identifier_not_equal);
    Diags.report(ProcDecl->getLocation(),
                 diag::note_proc_identifier_declaration);
  }
  ProcDecl->setDecls(Decls);
  ProcDecl->setStmts(Stmts);
}

void Sema::actOnAssignment(StmtList &Stmts, SMLoc Loc,
                           Decl *D, Expr *E) {
  if (auto Var = dyn_cast<VariableDeclaration>(D)) {
    if (Var->getType() != E->getType()) {
      Diags.report(
          Loc, diag::err_types_for_operator_not_compatible,
          tok::getPunctuatorSpelling(tok::colonequal));
    }
    Stmts.push_back(new AssignmentStatement(Var, E));
  } else if (D) {
    // TODO Emit error
  }
}

void Sema::actOnProcCall(StmtList &Stmts, SMLoc Loc,
                         Decl *D, ExprList &Params) {
  if (auto Proc = dyn_cast<ProcedureDeclaration>(D)) {
    checkFormalAndActualParameters(
        Loc, Proc->getFormalParams(), Params);
    if (Proc->getRetType())
      Diags.report(
          Loc, diag::err_procedure_call_on_nonprocedure);
    Stmts.push_back(
        new ProcedureCallStatement(Proc, Params));
  } else if (D) {
    Diags.report(Loc,
                 diag::err_procedure_call_on_nonprocedure);
  }
}

void Sema::actOnIfStatement(StmtList &Stmts, SMLoc Loc,
                            Expr *Cond, StmtList &IfStmts,
                            StmtList &ElseStmts) {
  if (!Cond)
    Cond = FalseLiteral;

  if (Cond->getType() != BooleanType) {
    Diags.report(Loc, diag::err_if_expr_must_be_bool);
  }
  Stmts.push_back(
      new IfStatement(Cond, IfStmts, ElseStmts));
}

void Sema::actOnWhileStatement(StmtList &Stmts, SMLoc Loc,
                               Expr *Cond,
                               StmtList &WhileStmts) {
  if (!Cond)
    Cond = FalseLiteral;

  if (Cond->getType() != BooleanType) {
    Diags.report(Loc, diag::err_while_expr_must_be_bool);
  }
  Stmts.push_back(new WhileStatement(Cond, WhileStmts));
}

void Sema::actOnReturnStatement(StmtList &Stmts, SMLoc Loc,
                                Expr *RetVal) {
  auto *Proc = cast<ProcedureDeclaration>(CurrentDecl);
  if (Proc->getRetType() && !RetVal)
    Diags.report(Loc, diag::err_function_requires_return);
  else if (!Proc->getRetType() && RetVal)
    Diags.report(Loc, diag::err_procedure_requires_empty_return);
  else if (Proc->getRetType() && RetVal) {
    if (Proc->getRetType() != RetVal->getType())
      Diags.report(Loc, diag::err_function_and_return_type);
  }

  Stmts.push_back(new ReturnStatement(RetVal));
}

Expr *Sema::actOnExpression(Expr *Left, Expr *Right,
                            const OperatorInfo &Op) {
  // Relation
  if (!Left)
    return Right;
  if (!Right)
    return Left;

  if (Left->getType() != Right->getType()) {
    Diags.report(
        Op.getLocation(),
        diag::err_types_for_operator_not_compatible,
        tok::getPunctuatorSpelling(Op.getKind()));
  }
  bool IsConst = Left->isConst() && Right->isConst();
  return new InfixExpression(Left, Right, Op, BooleanType,
                             IsConst);
}

Expr *Sema::actOnSimpleExpression(Expr *Left, Expr *Right,
                                  const OperatorInfo &Op) {
  // Addition
  if (!Left)
    return Right;
  if (!Right)
    return Left;

  if (Left->getType() != Right->getType()) {
    Diags.report(
        Op.getLocation(),
        diag::err_types_for_operator_not_compatible,
        tok::getPunctuatorSpelling(Op.getKind()));
  }
  TypeDeclaration *Ty = Left->getType();
  bool IsConst = Left->isConst() && Right->isConst();
  if (IsConst && Op.getKind() == tok::kw_OR) {
    BooleanLiteral *L = dyn_cast<BooleanLiteral>(Left);
    BooleanLiteral *R = dyn_cast<BooleanLiteral>(Right);
    return L->getValue() || R->getValue() ? TrueLiteral
                                          : FalseLiteral;
  }
  return new InfixExpression(Left, Right, Op, Ty, IsConst);
}

Expr *Sema::actOnTerm(Expr *Left, Expr *Right,
                      const OperatorInfo &Op) {
  // Multiplication
  if (!Left)
    return Right;
  if (!Right)
    return Left;

  if (Left->getType() != Right->getType() ||
      !isOperatorForType(Op.getKind(), Left->getType())) {
    Diags.report(
        Op.getLocation(),
        diag::err_types_for_operator_not_compatible,
        tok::getPunctuatorSpelling(Op.getKind()));
  }
  TypeDeclaration *Ty = Left->getType();
  bool IsConst = Left->isConst() && Right->isConst();
  if (IsConst && Op.getKind() == tok::kw_AND) {
    BooleanLiteral *L = dyn_cast<BooleanLiteral>(Left);
    BooleanLiteral *R = dyn_cast<BooleanLiteral>(Right);
    return L->getValue() && R->getValue() ? TrueLiteral
                                          : FalseLiteral;
  }
  return new InfixExpression(Left, Right, Op, Ty, IsConst);
}

Expr *Sema::actOnPrefixExpression(Expr *E,
                                  const OperatorInfo &Op) {
  if (!E)
    return nullptr;

  if (!isOperatorForType(Op.getKind(), E->getType())) {
    Diags.report(
        Op.getLocation(),
        diag::err_types_for_operator_not_compatible,
        tok::getPunctuatorSpelling(Op.getKind()));
  }

  if (E->isConst() && Op.getKind() == tok::kw_NOT) {
    BooleanLiteral *L = dyn_cast<BooleanLiteral>(E);
    return L->getValue() ? FalseLiteral : TrueLiteral;
  }

  if (Op.getKind() == tok::minus) {
    bool Ambiguous = true;
    if (isa<IntegerLiteral>(E) || isa<VariableAccess>(E) ||
        isa<ConstantAccess>(E))
      Ambiguous = false;
    else if (auto *Infix = dyn_cast<InfixExpression>(E)) {
      tok::TokenKind Kind =
          Infix->getOperatorInfo().getKind();
      if (Kind == tok::star || Kind == tok::slash)
        Ambiguous = false;
    }
    if (Ambiguous) {
      Diags.report(Op.getLocation(),
                   diag::warn_ambigous_negation);
    }
  }

  return new PrefixExpression(E, Op, E->getType(),
                              E->isConst());
}

Expr *Sema::actOnIntegerLiteral(SMLoc Loc,
                                StringRef Literal) {
  uint8_t Radix = 10;
  if (Literal.endswith("H")) {
    Literal = Literal.drop_back();
    Radix = 16;
  }
  llvm::APInt Value(64, Literal, Radix);
  return new IntegerLiteral(Loc, llvm::APSInt(Value, false),
                            IntegerType);
}

Expr *Sema::actOnVariable(Decl *D) {
  if (!D)
    return nullptr;
  if (auto *V = dyn_cast<VariableDeclaration>(D))
    return new VariableAccess(V);
  else if (auto *P = dyn_cast<FormalParameterDeclaration>(D))
    return new VariableAccess(P);
  else if (auto *C = dyn_cast<ConstantDeclaration>(D)) {
    if (C == TrueConst)
      return TrueLiteral;
    if (C == FalseConst) {
      return FalseLiteral;
    }
    return new ConstantAccess(C);
  }
  return nullptr;
}

Expr *Sema::actOnFunctionCall(Decl *D, ExprList &Params) {
  if (!D)
    return nullptr;
  if (auto *P = dyn_cast<ProcedureDeclaration>(D)) {
    checkFormalAndActualParameters(
        D->getLocation(), P->getFormalParams(), Params);
    if (!P->getRetType())
      Diags.report(D->getLocation(),
                   diag::err_function_call_on_nonfunction);
    return new FunctionCallExpr(P, Params);
  }
  Diags.report(D->getLocation(),
               diag::err_function_call_on_nonfunction);
  return nullptr;
}

Decl *Sema::actOnQualIdentPart(Decl *Prev, SMLoc Loc,
                               StringRef Name) {
  if (!Prev) {
    if (Decl *D = CurrentScope->lookup(Name))
      return D;
  } else if (auto *Mod =
                 dyn_cast<ModuleDeclaration>(Prev)) {
    auto Decls = Mod->getDecls();
    for (auto I = Decls.begin(), E = Decls.end(); I != E;
         ++I) {
      if ((*I)->getName() == Name) {
        return *I;
      }
    }
  } else {
    llvm_unreachable("actOnQualIdentPart only callable "
                     "with module declarations");
  }
  Diags.report(Loc, diag::err_undeclared_name, Name);
  return nullptr;
}