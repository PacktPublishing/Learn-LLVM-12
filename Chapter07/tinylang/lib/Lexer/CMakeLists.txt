set(LLVM_LINK_COMPONENTS support)

add_tinylang_library(tinylangLexer
  Lexer.cpp

  LINK_LIBS
  tinylangBasic
  )
