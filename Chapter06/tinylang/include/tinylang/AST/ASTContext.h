#ifndef TINYLANG_AST_ASTCONTEXT_H
#define TINYLANG_AST_ASTCONTEXT_H

#include "tinylang/Basic/LLVM.h"
#include "llvm/Support/SourceMgr.h"

namespace tinylang {

class ASTContext {
  llvm::SourceMgr &SrcMgr;
  StringRef Filename;

public:
  ASTContext(llvm::SourceMgr &SrcMgr, StringRef Filename)
      : SrcMgr(SrcMgr), Filename(Filename) {}

  StringRef getFilename() { return Filename; }

  llvm::SourceMgr &getSourceMgr() { return SrcMgr; }
  const llvm::SourceMgr &getSourceMgr() const {
    return SrcMgr;
  }
};

} // namespace tinylang

#endif