set(LLVM_LINK_COMPONENTS support)

add_tinylang_library(tinylangParser
  Parser.cpp

  LINK_LIBS
  tinylangBasic
  tinylangLexer
  )
