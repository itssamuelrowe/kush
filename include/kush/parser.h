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

// Sunday, November 05, 2017

#ifndef KUSH_COMPILER_PARSER_PARSER_H
#define KUSH_COMPILER_PARSER_PARSER_H

#include <jtk/collection/list/ArrayList.h>

#include <kush/configuration.h>
#include <kush/token-stream.h>
#include <kush/context.h>

/*******************************************************************************
 * Parser                                                                      *
 *******************************************************************************/

struct Parser {
    Compiler* compiler;
    TokenStream* tokens;
    int32_t* followSet;
    int32_t followSetSize;
    int32_t followSetCapacity;
    bool recovery;
    bool placeholder;
};

typedef struct Parser Parser;

Parser* parserNew(Compiler* compiler, TokenStream* tokens);
void parserDelete(Parser* parser);
const char* getRuleName(ContextType type);
Module* parse(Parser* parser);
void resetParser(Parser* parser, TokenStream* tokens);

#endif /* KUSH_COMPILER_PARSER_PARSER_H */
