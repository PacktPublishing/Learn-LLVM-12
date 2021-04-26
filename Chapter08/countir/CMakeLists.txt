cmake_minimum_required(VERSION 3.4.3)
project(countirpass)

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

list(APPEND CMAKE_MODULE_PATH ${LLVM_DIR})
include(ChooseMSVCCRT)

include(AddLLVM)
include(HandleLLVMOptions)

include_directories("${LLVM_INCLUDE_DIR}")
add_definitions("${LLVM_DEFINITIONS}")

link_directories("${LLVM_LIBRARY_DIR}")

include_directories(BEFORE include)
add_subdirectory(lib)
