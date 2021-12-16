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

#include <kush/configuration.h>
#include <kush/context.h>
#include <kush/compiler.h>
#include <kush/scope.h>

// Sunday, June 18 2020

/******************************************************************************
 * Generator                                                                  *
 ******************************************************************************/

struct Generator {
    Compiler* compiler;
    Scope* scope;
    FILE* output;
    int32_t index;
};

typedef struct Generator Generator;

Generator* newGenerator(Compiler* compiler);
void deleteGenerator(Generator* generator);
void generateC(Generator* generator, Module* module);
