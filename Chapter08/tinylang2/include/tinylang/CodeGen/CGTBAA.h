#ifndef TINYLANG_CODEGEN_CGTBAA_H
#define TINYLANG_CODEGEN_CGTBAA_H

#include "tinylang/AST/AST.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Metadata.h"

namespace tinylang {

class CGModule;

class CGTBAA {
  CGModule &CGM;

  // MDHelper - Helper for creating metadata.
  llvm::MDBuilder MDHelper;

  // The root node of the TBAA hierarchy
  llvm::MDNode *Root;

  llvm::DenseMap<TypeDeclaration *, llvm::MDNode *>
      MetadataCache;

  llvm::MDNode *createScalarTypeNode(TypeDeclaration *Ty,
                                     StringRef Name,
                                     llvm::MDNode *Parent);
  llvm::MDNode *createStructTypeNode(
      TypeDeclaration *Ty, StringRef Name,
      llvm::ArrayRef<std::pair<llvm::MDNode *, uint64_t>>
          Fields);

public:
  CGTBAA(CGModule &CGM);

  llvm::MDNode *getRoot();
  llvm::MDNode *getTypeInfo(TypeDeclaration *Ty);
  llvm::MDNode *getAccessTagInfo(TypeDeclaration *Ty);
};
} // namespace tinylang
#endif