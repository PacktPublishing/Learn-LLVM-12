
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendPluginRegistry.h"

using namespace clang;

namespace {
class NamingVisitor
    : public RecursiveASTVisitor<NamingVisitor> {
private:
  ASTContext &ASTCtx;

public:
  explicit NamingVisitor(CompilerInstance &CI)
      : ASTCtx(CI.getASTContext()) {}

  virtual bool VisitVarDecl(VarDecl *VD) {
    llvm::errs() << "Name: " << VD->getName() << "\n";
    return true;
  }

  virtual bool VisitFunctionDecl(FunctionDecl *FD) {
    std::string Name =
        FD->getNameInfo().getName().getAsString();
    assert(Name.length() > 0 &&
           "Unexpected empty identifier");
    char &First = Name.at(0);
    if (!(First >= 'a' && First <= 'z')) {
      DiagnosticsEngine &Diag = ASTCtx.getDiagnostics();
      unsigned ID = Diag.getCustomDiagID(
          DiagnosticsEngine::Warning,
          "Function name should start with "
          "lowercase letter");
      Diag.Report(FD->getLocation(), ID);
    }
    return true;
  }
};

class NamingASTConsumer : public ASTConsumer {
  std::unique_ptr<NamingVisitor> Visitor;

public:
  NamingASTConsumer(CompilerInstance &CI)
      : Visitor(std::make_unique<NamingVisitor>(CI)) {}

  void
  HandleTranslationUnit(ASTContext &ASTCtx) override {
    Visitor->TraverseDecl(
        ASTCtx.getTranslationUnitDecl());
  }
};

class PluginNamingAction : public PluginASTAction {
public:
  std::unique_ptr<ASTConsumer>
  CreateASTConsumer(CompilerInstance &CI,
                    StringRef file) override {
    return std::make_unique<NamingASTConsumer>(CI);
  }

  bool ParseArgs(const CompilerInstance &CI,
                 const std::vector<std::string> &args) override {
    return true;
  }

  PluginASTAction::ActionType getActionType() override {
    return AddAfterMainAction;
  }
};

} // namespace

static FrontendPluginRegistry::Add<PluginNamingAction>
    X("naming-plugin", "naming plugin");
