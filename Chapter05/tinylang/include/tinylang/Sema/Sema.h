#ifndef TINYLANG_SEMA_SEMA_H
#define TINYLANG_SEMA_SEMA_H

#include "tinylang/AST/AST.h"
#include "tinylang/Basic/Diagnostic.h"
#include "tinylang/Sema/Scope.h"
#include <memory>

namespace tinylang {

class Sema {
  friend class EnterDeclScope;
  void enterScope(Decl *);
  void leaveScope();

  bool isOperatorForType(tok::TokenKind Op,
                         TypeDeclaration *Ty);

  void checkFormalAndActualParameters(
      SMLoc Loc, const FormalParamList &Formals,
      const ExprList &Actuals);

  Scope *CurrentScope;
  Decl *CurrentDecl;
  DiagnosticsEngine &Diags;

  TypeDeclaration *IntegerType;
  TypeDeclaration *BooleanType;
  BooleanLiteral *TrueLiteral;
  BooleanLiteral *FalseLiteral;
  ConstantDeclaration *TrueConst;
  ConstantDeclaration *FalseConst;

public:
  Sema(DiagnosticsEngine &Diags)
      : CurrentScope(nullptr), CurrentDecl(nullptr),
        Diags(Diags) {
    initialize();
  }

  void initialize();

  ModuleDeclaration *actOnModuleDeclaration(SMLoc Loc,
                                            StringRef Name);
  void actOnModuleDeclaration(ModuleDeclaration *ModDecl,
                              SMLoc Loc, StringRef Name,
                              DeclList &Decls,
                              StmtList &Stmts);
  void actOnImport(StringRef ModuleName, IdentList &Ids);
  void actOnConstantDeclaration(DeclList &Decls, SMLoc Loc,
                                StringRef Name, Expr *E);
  void actOnVariableDeclaration(DeclList &Decls,
                                IdentList &Ids, Decl *D);
  void
  actOnFormalParameterDeclaration(FormalParamList &Params,
                                  IdentList &Ids, Decl *D,
                                  bool IsVar);
  ProcedureDeclaration *
  actOnProcedureDeclaration(SMLoc Loc, StringRef Name);
  void actOnProcedureHeading(ProcedureDeclaration *ProcDecl,
                             FormalParamList &Params,
                             Decl *RetType);
  void actOnProcedureDeclaration(
      ProcedureDeclaration *ProcDecl, SMLoc Loc,
      StringRef Name, DeclList &Decls, StmtList &Stmts);
  void actOnAssignment(StmtList &Stmts, SMLoc Loc, Decl *D,
                       Expr *E);
  void actOnProcCall(StmtList &Stmts, SMLoc Loc, Decl *D,
                     ExprList &Params);
  void actOnIfStatement(StmtList &Stmts, SMLoc Loc,
                        Expr *Cond, StmtList &IfStmts,
                        StmtList &ElseStmts);
  void actOnWhileStatement(StmtList &Stmts, SMLoc Loc,
                           Expr *Cond,
                           StmtList &WhileStmts);
  void actOnReturnStatement(StmtList &Stmts, SMLoc Loc,
                            Expr *RetVal);

  Expr *actOnExpression(Expr *Left, Expr *Right,
                        const OperatorInfo &Op);
  Expr *actOnSimpleExpression(Expr *Left, Expr *Right,
                              const OperatorInfo &Op);
  Expr *actOnTerm(Expr *Left, Expr *Right,
                  const OperatorInfo &Op);
  Expr *actOnPrefixExpression(Expr *E,
                              const OperatorInfo &Op);
  Expr *actOnIntegerLiteral(SMLoc Loc, StringRef Literal);
  Expr *actOnVariable(Decl *D);
  Expr *actOnFunctionCall(Decl *D, ExprList &Params);
  Decl *actOnQualIdentPart(Decl *Prev, SMLoc Loc,
                           StringRef Name);
};

class EnterDeclScope {
  Sema &Semantics;

public:
  EnterDeclScope(Sema &Semantics, Decl *D)
      : Semantics(Semantics) {
    Semantics.enterScope(D);
  }
  ~EnterDeclScope() { Semantics.leaveScope(); }
};
} // namespace tinylang
#endif