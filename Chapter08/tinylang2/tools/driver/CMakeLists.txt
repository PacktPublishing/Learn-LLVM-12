set(LLVM_LINK_COMPONENTS ${LLVM_TARGETS_TO_BUILD}
  AggressiveInstCombine Analysis AsmParser
  BitWriter CodeGen Core Coroutines IPO IRReader
  InstCombine Instrumentation MC ObjCARCOpts Remarks
  ScalarOpts Support Target TransformUtils Vectorize
  Passes)

add_tinylang_tool(tinylang Driver.cpp SUPPORT_PLUGINS)

target_link_libraries(tinylang
  PRIVATE tinylangBasic tinylangCodeGen
  tinylangLexer tinylangParser tinylangSema)
