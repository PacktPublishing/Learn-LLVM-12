#ifndef TINYLANG_LEXER_TOKEN_H
#define TINYLANG_LEXER_TOKEN_H

#include "tinylang/Basic/LLVM.h"
#include "tinylang/Basic/TokenKinds.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"

namespace tinylang {

class Lexer;

class Token {
  friend class Lexer;

  /// The location of the token.
  const char *Ptr;

  /// The length of the token.
  size_t Length;

  /// Kind - The actual flavor of token this is.
  tok::TokenKind Kind;

public:
  tok::TokenKind getKind() const { return Kind; }
  void setKind(tok::TokenKind K) { Kind = K; }

  /// is/isNot - Predicates to check if this token is a
  /// specific kind, as in "if (Tok.is(tok::l_brace))
  /// {...}".
  bool is(tok::TokenKind K) const { return Kind == K; }
  bool isNot(tok::TokenKind K) const { return Kind != K; }
  bool isOneOf(tok::TokenKind K1, tok::TokenKind K2) const {
    return is(K1) || is(K2);
  }
  template <typename... Ts>
  bool isOneOf(tok::TokenKind K1, tok::TokenKind K2,
               Ts... Ks) const {
    return is(K1) || isOneOf(K2, Ks...);
  }

  const char *getName() const {
    return tok::getTokenName(Kind);
  }

  SMLoc getLocation() const {
    return SMLoc::getFromPointer(Ptr);
  }
  size_t getLength() const { return Length; }

  StringRef getIdentifier() {
    assert(is(tok::identifier) &&
           "Cannot get identfier of non-identifier");
    return StringRef(Ptr, Length);
  }

  StringRef getLiteralData() {
    assert(isOneOf(tok::integer_literal,
                   tok::string_literal) &&
           "Cannot get literal data of non-literal");
    return StringRef(Ptr, Length);
  }
};

} // namespace tinylang
#endif