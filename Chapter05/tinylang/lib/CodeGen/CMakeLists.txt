set(LLVM_LINK_COMPONENTS support)

add_tinylang_library(tinylangCodeGen
  CGModule.cpp
  CGProcedure.cpp
  CodeGenerator.cpp

  LINK_LIBS
  tinylangSema
  )
