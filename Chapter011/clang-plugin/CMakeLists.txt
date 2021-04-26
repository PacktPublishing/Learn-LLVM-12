cmake_minimum_required(VERSION 3.13.4)
project(naminglugin)

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

list(APPEND CMAKE_MODULE_PATH ${LLVM_DIR})
include(ChooseMSVCCRT)

include(AddLLVM)
include(HandleLLVMOptions)
find_package(Clang REQUIRED)

include_directories("${LLVM_INCLUDE_DIR}" "${CLANG_INCLUDE_DIRS}")
add_definitions("${LLVM_DEFINITIONS}")

link_directories("${LLVM_LIBRARY_DIR}")

add_llvm_library(NamingPlugin MODULE NamingPlugin.cpp PLUGIN_TOOL clang)

if(LLVM_ENABLE_PLUGINS AND (WIN32 OR CYGWIN))
  set(LLVM_LINK_COMPONENTS
    Support
  )
  clang_target_link_libraries(NamingPlugin PRIVATE
    clangAST
    clangBasic
    clangFrontend
    clangLex
    )
endif()
