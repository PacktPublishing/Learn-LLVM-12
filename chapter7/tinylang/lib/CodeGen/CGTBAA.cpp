
#include "tinylang/CodeGen/CGTBAA.h"
#include "tinylang/CodeGen/CGModule.h"
#include "llvm/IR/DataLayout.h"

using namespace tinylang;

CGTBAA::CGTBAA(CGModule &CGM)
      : CGM(CGM),
        MDHelper(llvm::MDBuilder(CGM.getLLVMCtx())),
        Root(nullptr) {}

llvm::MDNode *CGTBAA::getRoot() {
  if (!Root)
    Root = MDHelper.createTBAARoot("Simple tinylang TBAA");

  return Root;
}

llvm::MDNode *
CGTBAA::createScalarTypeNode(TypeDeclaration *Ty,
                             StringRef Name,
                             llvm::MDNode *Parent) {
  llvm::MDNode *N =
      MDHelper.createTBAAScalarTypeNode(Name, Parent);
  return MetadataCache[Ty] = N;
}

llvm::MDNode *CGTBAA::createStructTypeNode(
    TypeDeclaration *Ty, StringRef Name,
    llvm::ArrayRef<std::pair<llvm::MDNode *, uint64_t>>
        Fields) {
  llvm::MDNode *N =
      MDHelper.createTBAAStructTypeNode(Name, Fields);
  return MetadataCache[Ty] = N;
}

llvm::MDNode *CGTBAA::getTypeInfo(TypeDeclaration *Ty) {
  if (llvm::MDNode *N = MetadataCache[Ty])
    return N;

  if (auto *Pervasive =
          llvm::dyn_cast<PervasiveTypeDeclaration>(Ty)) {
    StringRef Name = Pervasive->getName();
    return createScalarTypeNode(Pervasive, Name, getRoot());
  }
  if (auto *Pointer =
          llvm::dyn_cast<PointerTypeDeclaration>(Ty)) {
    StringRef Name = "any pointer";
    return createScalarTypeNode(Pointer, Name, getRoot());
  }
  if (auto *Record =
          llvm::dyn_cast<RecordTypeDeclaration>(Ty)) {
    llvm::SmallVector<std::pair<llvm::MDNode *, uint64_t>,
                      4>
        Fields;
    auto *Rec =
        llvm::cast<llvm::StructType>(CGM.convertType(Record));
    const llvm::StructLayout *Layout =
        CGM.getModule()->getDataLayout().getStructLayout(Rec);

    unsigned Idx = 0;
    for (const auto &F : Record->getFields()) {
      uint64_t Offset = Layout->getElementOffset(Idx);
      Fields.emplace_back(getTypeInfo(F.getType()), Offset);
      ++Idx;
    }
    StringRef Name = CGM.mangleName(Record);
    return createStructTypeNode(Record, Name, Fields);
  }
  return nullptr;
}

llvm::MDNode *
CGTBAA::getAccessTagInfo(TypeDeclaration *Ty) {
  if (auto *Pointer =
          llvm::dyn_cast<PointerTypeDeclaration>(Ty)) {
    return getTypeInfo(Pointer->getType());
  }
  return nullptr;
}