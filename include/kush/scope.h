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

// Saturday, November 25, 2017

#ifndef SCOPE_H
#define SCOPE_H

#include <jtk/collection/list/ArrayList.h>
#include <jtk/collection/map/HashMap.h>

#include <kush/configuration.h>

typedef struct Context Context;
typedef struct Module Module;
typedef struct Structure Structure;
typedef struct Function Function;
typedef struct Block Block;
typedef struct Symbol Symbol;

/*******************************************************************************
 * ScopeType                                                                   *
 *******************************************************************************/

enum ScopeType {
    SCOPE_MODULE,
    SCOPE_STRUCTURE,
    SCOPE_FUNCTION,
    SCOPE_LOCAL
};

typedef enum ScopeType ScopeType;

/*******************************************************************************
 * Scope                                                                       *
 *******************************************************************************/

#define isCompilationUnitScope(scope) ((scope)->type == SCOPE_MODULE)
#define isStructureScope(scope) ((scope)->type == SCOPE_STRUCTURE)
#define isFunctionScope(scope) ((scope)->type == SCOPE_FUNCTION)
#define isLocalScope(scope) ((scope)->type == SCOPE_LOCAL)

typedef struct Scope Scope;

struct Scope {
    ScopeType type;
    Scope* parent;
    Context* symbol;
    jtk_HashMap_t* symbols;
};

Scope* newScope(ScopeType type, Scope* parent, Context* symbol);
void deleteScope(Scope* scope);
Scope* scopeForModule(Module* module);
Scope* scopeForStructure(Scope* parent, Structure* structure);
Scope* scopeForFunction(Scope* parent, Function* function);
Scope* scopeForLocal(Scope* parent, Block* block);
void defineSymbol(Scope* scope, Symbol* symbol);
Symbol* resolveSymbol(Scope* scope, const uint8_t* name);
Symbol* resolveMember(Scope* scope, const uint8_t* name);

#endif /* SCOPE_H */
