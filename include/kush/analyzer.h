/*
 * Copyright 2017-2020 Samuel Rowe, Joel E. Rego
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

// Sunday, January 07, 2018

#ifndef KUSH_ANALYZER_H
#define KUSH_ANALYZER_H

#include <kush/configuration.h>
#include <kush/compiler.h>
#include <kush/scope.h>
#include <kush/context.h>

/*******************************************************************************
 * Analyzer                                                                    *
 *******************************************************************************/

/**
 * @class Analyzer
 * @author Samuel Rowe
 * @since Kush 0.1
 */
struct Analyzer {
    Compiler* compiler;
    const uint8_t* package;
    int32_t packageSize;
    Scope* scope;
    Function* function;
    int32_t index;
};

typedef struct Analyzer Analyzer;

Analyzer* newAnalyzer();
void deleteAnalyzer(Analyzer* self);
void defineSymbols(Analyzer* analyzer, Module* module);
void resetAnalyzer(Analyzer* analyzer);
void defineSymbols(Analyzer* analyzer, Module* module);
void resolveSymbols(Analyzer* analyzer, Module* module);

#endif /* KUSH_ANALYZER_H */
