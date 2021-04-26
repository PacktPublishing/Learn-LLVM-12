set(LLVM_LINK_COMPONENTS support)

add_tinylang_library(tinylangSema
  Scope.cpp
  Sema.cpp

  LINK_LIBS
  tinylangBasic
  )
