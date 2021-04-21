set(LLVM_LINK_COMPONENTS support)

add_tinylang_library(tinylangCodeGen
  CGDebugInfo.cpp
  CGModule.cpp
  CGProcedure.cpp
  CGTBAA.cpp
  CodeGenerator.cpp

  LINK_LIBS
  tinylangSema
  )
