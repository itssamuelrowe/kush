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

#include <jtk/core/CString.h>
#include <jtk/collection/stack/LinkedStack.h>
#include <jtk/core/CStringObjectAdapter.h>

#include <kush/scope.h>
#include <kush/context.h>

Scope* newScope(ScopeType type, Scope* parent, Context* symbol) {
    jtk_ObjectAdapter_t* stringObjectAdapter = jtk_CStringObjectAdapter_getInstance();

    Scope* scope = allocate(Scope, 1);
    scope->type = type;
    scope->parent = parent;
    scope->symbol = symbol;
    scope->symbols = jtk_HashMap_new(stringObjectAdapter, NULL);

    return scope;
}

void deleteScope(Scope* scope) {
    jtk_Assert_assertObject(scope, "The specified scope is null.");

    jtk_HashMap_delete(scope->symbols);
    deallocate(scope);
}

Scope* scopeForModule(Module* module) {
    return newScope(SCOPE_MODULE, NULL, NULL);
}

Scope* scopeForStructure(Scope* parent, Structure* structure) {
    return newScope(SCOPE_STRUCTURE, parent, (Context*)structure);
}

Scope* scopeForFunction(Scope* parent, Function* function) {
    return newScope(SCOPE_FUNCTION, parent, (Context*)function);
}

Scope* scopeForLocal(Scope* parent, Block* block) {
    return newScope(SCOPE_LOCAL, parent, (Context*)block);
}

void defineSymbol(Scope* scope, Symbol* symbol) {
    bool result = jtk_HashMap_putStrictly(scope->symbols, symbol->name, symbol);
    if (!result) {
        fprintf(stderr, "[internal error] defineSymbol() invoked to redefine a symbol.\n");
    }
}

Symbol* resolveSymbol(Scope* scope, const uint8_t* name) {
    Scope* current = scope;
    Symbol* result = NULL;
    while ((current != NULL) && (result == NULL)) {
        result = jtk_HashMap_getValue(current->symbols, (void*)name);
        current = current->parent;
    }
    return result;
}

Symbol* resolveMember(Scope* scope, const uint8_t* name) {
    return jtk_HashMap_getValue(scope->symbols, (void*)name);
}