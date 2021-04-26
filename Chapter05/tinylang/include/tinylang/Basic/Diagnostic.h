#ifndef TINYLANG_BASIC_DIAGNOSTIC_H
#define TINYLANG_BASIC_DIAGNOSTIC_H

#include "tinylang/Basic/LLVM.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"
#include <utility>

namespace tinylang {

namespace diag {
enum {
#define DIAG(ID, Level, Msg) ID,
#include "tinylang/Basic/Diagnostic.def"
};
} // namespace diag

class DiagnosticsEngine {
  static const char *getDiagnosticText(unsigned DiagID);
  static SourceMgr::DiagKind
  getDiagnosticKind(unsigned DiagID);

  SourceMgr &SrcMgr;
  unsigned NumErrors;

public:
  DiagnosticsEngine(SourceMgr &SrcMgr)
      : SrcMgr(SrcMgr), NumErrors(0) {}

  unsigned numErrors() { return NumErrors; }

  template <typename... Args>
  void report(SMLoc Loc, unsigned DiagID,
              Args &&... Arguments) {
    std::string Msg =
        llvm::formatv(getDiagnosticText(DiagID),
                      std::forward<Args>(Arguments)...)
            .str();
    SourceMgr::DiagKind Kind = getDiagnosticKind(DiagID);
    SrcMgr.PrintMessage(Loc, Kind, Msg);
    NumErrors += (Kind == SourceMgr::DK_Error);
  }
};

} // namespace tinylang

#endif