set(LLVM_LINK_COMPONENTS
  Support
  )

add_tinylang_tool(tinylang
  Driver.cpp
  )

target_link_libraries(tinylang
  PRIVATE
  tinylangBasic
  )
