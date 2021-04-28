//===--- CodeGenerator.cpp - Modula-2 Language Code Generator ---*- C++ -*-===//
//
// Part of the M2Lang Project, under the Apache License v2.0 with
// LLVM Exceptions. See LICENSE file for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines the code generator implementation.
///
//===----------------------------------------------------------------------===//

#include "tinylang/CodeGen/CodeGenerator.h"
#include "tinylang/CodeGen/CGModule.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"

using namespace tinylang;

CodeGenerator *CodeGenerator::create(llvm::LLVMContext &Ctx, ASTContext &ASTCtx, llvm::TargetMachine *TM) {
  return new CodeGenerator(Ctx, ASTCtx, TM);
}

std::unique_ptr<llvm::Module> CodeGenerator::run(ModuleDeclaration *Mod, std::string FileName) {
  std::unique_ptr<llvm::Module> M = std::make_unique<llvm::Module>(FileName, Ctx);
  M->setTargetTriple(TM->getTargetTriple().getTriple());
  M->setDataLayout(TM->createDataLayout());
  CGModule CGM(ASTCtx, M.get());
  CGM.run(Mod);
  return M;
}
