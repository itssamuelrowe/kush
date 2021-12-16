/*
 * Copyright 2018-2020 Samuel Rowe
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

// Monday, March 16, 2020

#ifndef KUSH_COMPILER_COMPILER_H
#define KUSH_COMPILER_COMPILER_H

#include <kush/configuration.h>
#include <kush/error-handler.h>
#include <kush/symbol-loader.h>

#include <jtk/collection/list/ArrayList.h>
#include <jtk/collection/map/HashMap.h>
#include <jtk/log/Logger.h>

/******************************************************************************
 * Compiler                                                                   *
 ******************************************************************************/

typedef struct Module Module;

/**
 * @author Samuel Rowe
 * @since Kush 0.1
 */
struct Compiler {
    bool dumpTokens;
    bool dumpNodes;
    bool footprint;
    bool dumpInstructions;
    jtk_Logger_t* logger;
    jtk_ArrayList_t* inputFiles;
    int32_t currentFileIndex;
    ErrorHandler* errorHandler;
    Module** modules;
    uint8_t** packages;
    int32_t* packageSizes;
    SymbolLoader* symbolLoader;
    jtk_HashMap_t* repository;
    jtk_ArrayList_t* trash;
    bool coreApi;
    uint8_t* output;
    int32_t outputSize;
};

/**
 * @memberof Compiler
 */
typedef struct Compiler Compiler;

// Constructor

Compiler* newCompiler();

// Destructor

void deleteCompiler(Compiler* compiler);

// Compiler

bool compileEx(Compiler* compiler, char** arguments, int32_t length);
bool compile(Compiler* compiler);

#endif /* KUSH_COMPILER_COMPILER_H */
