#include "CodeGen.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

namespace {
class ToIRVisitor : public ASTVisitor {
  Module *M;
  IRBuilder<> Builder;
  Type *VoidTy;
  Type *Int32Ty;
  Type *Int8PtrTy;
  Type *Int8PtrPtrTy;
  Constant *Int32Zero;

  Value *V;
  StringMap<Value *> nameMap;

public:
  ToIRVisitor(Module *M) : M(M), Builder(M->getContext()) {
    VoidTy = Type::getVoidTy(M->getContext());
    Int32Ty = Type::getInt32Ty(M->getContext());
    Int8PtrTy = Type::getInt8PtrTy(M->getContext());
    Int8PtrPtrTy = Int8PtrTy->getPointerTo();
    Int32Zero = ConstantInt::get(Int32Ty, 0, true);
  }

  void run(AST *Tree) {
    FunctionType *MainFty = FunctionType::get(
        Int32Ty, {Int32Ty, Int8PtrPtrTy}, false);
    Function *MainFn = Function::Create(
        MainFty, GlobalValue::ExternalLinkage, "main", M);
    BasicBlock *BB = BasicBlock::Create(M->getContext(),
                                        "entry", MainFn);
    Builder.SetInsertPoint(BB);

    Tree->accept(*this);

    FunctionType *CalcWriteFnTy =
        FunctionType::get(VoidTy, {Int32Ty}, false);
    Function *CalcWriteFn = Function::Create(
        CalcWriteFnTy, GlobalValue::ExternalLinkage,
        "calc_write", M);
    Builder.CreateCall(CalcWriteFnTy, CalcWriteFn, {V});

    Builder.CreateRet(Int32Zero);
  }

  virtual void visit(Factor &Node) override {
    if (Node.getKind() == Factor::Ident) {
      V = nameMap[Node.getVal()];
    } else {
      int intval;
      Node.getVal().getAsInteger(10, intval);
      V = ConstantInt::get(Int32Ty, intval, true);
    }
  };

  virtual void visit(BinaryOp &Node) override {
    Node.getLeft()->accept(*this);
    Value *Left = V;
    Node.getRight()->accept(*this);
    Value *Right = V;
    switch (Node.getOperator()) {
    case BinaryOp::Plus:
      V = Builder.CreateNSWAdd(Left, Right);
      break;
    case BinaryOp::Minus:
      V = Builder.CreateNSWSub(Left, Right);
      break;
    case BinaryOp::Mul:
      V = Builder.CreateNSWMul(Left, Right);
      break;
    case BinaryOp::Div:
      V = Builder.CreateSDiv(Left, Right);
      break;
    }
  };

  virtual void visit(WithDecl &Node) override {
    FunctionType *ReadFty =
        FunctionType::get(Int32Ty, {Int8PtrTy}, false);
    Function *ReadFn = Function::Create(
        ReadFty, GlobalValue::ExternalLinkage, "calc_read",
        M);
    for (auto I = Node.begin(), E = Node.end(); I != E;
         ++I) {
      StringRef Var = *I;

      // Create call to calc_read function.
      Constant *StrText = ConstantDataArray::getString(
          M->getContext(), Var);
      GlobalVariable *Str = new GlobalVariable(
          *M, StrText->getType(),
          /*isConstant=*/true, GlobalValue::PrivateLinkage,
          StrText, Twine(Var).concat(".str"));
      Value *Ptr = Builder.CreateInBoundsGEP(
          Str, {Int32Zero, Int32Zero}, "ptr");
      CallInst *Call =
          Builder.CreateCall(ReadFty, ReadFn, {Ptr});

      nameMap[Var] = Call;
    }

    Node.getExpr()->accept(*this);
  };
};
} // namespace

void CodeGen::compile(AST *Tree) {
  LLVMContext Ctx;
  Module *M = new Module("calc.expr", Ctx);
  ToIRVisitor ToIR(M);
  ToIR.run(Tree);
  M->print(outs(), nullptr);
}