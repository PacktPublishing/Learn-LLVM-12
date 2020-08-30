#ifndef TINYLANG_BASIC_LLVM_H
#define TINYLANG_BASIC_LLVM_H

#include "llvm/ADT/None.h"
#include "llvm/Support/Casting.h"

namespace llvm {
class SMLoc;
class SourceMgr;
template <typename T, typename A> class StringMap;
class StringRef;
class raw_ostream;
} // namespace llvm

namespace tinylang {
using llvm::cast;
using llvm::cast_or_null;
using llvm::dyn_cast;
using llvm::dyn_cast_or_null;
using llvm::isa;

using llvm::raw_ostream;
using llvm::SMLoc;
using llvm::SourceMgr;
using llvm::StringMap;
using llvm::StringRef;
} // namespace tinylang

#endif
