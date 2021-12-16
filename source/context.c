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

// Sunday, June 07 2020

#include <jtk/core/CString.h>
#include <kush/context.h>

/* Delete only what was allocated in the constructor. */

/*******************************************************************************
 * Type                                                                        *
 *******************************************************************************/

Type* newType(uint8_t tag, bool indexable, bool accessible, bool callable,
    bool allocatable, Token* identifier) {
    Type* type = allocate(Type, 1);
    type->tag = tag;
    type->indexable = indexable;
    type->accessible = accessible;
    type->callable = callable;
    type->reference = allocatable;
    type->identifier = identifier;
    type->arrayTypes = jtk_ArrayList_new();

    return type;
}

void deleteType(Type* type) {
    deallocate(type);
}

/*******************************************************************************
 * Primitives                                                                  *
 *******************************************************************************/

Primitives primitives = {
    .boolean = {
        .tag = TYPE_BOOLEAN,
        .indexable = false,
        .accessible = false,
        .callable = false,
        .reference = false,
        .identifier = NULL,
        .arrayTypes = NULL
    },

    .i8 = {
        .tag = TYPE_INTEGER,
        .indexable = false,
        .accessible = false,
        .callable = false,
        .reference = false,
        .identifier = NULL,
        .arrayTypes = NULL,
        .integer = {
            .size = 1,
            .fullWidth = false
        }
    },

    .i16 = {
        .tag = TYPE_INTEGER,
        .indexable = false,
        .accessible = false,
        .callable = false,
        .reference = false,
        .identifier = NULL,
        .arrayTypes = NULL,
        .integer = {
            .size = 2,
            .fullWidth = false
        }
    },

    .i32 = {
        .tag = TYPE_INTEGER,
        .indexable = false,
        .accessible = false,
        .callable = false,
        .reference = false,
        .identifier = NULL,
        .arrayTypes = NULL,
        .integer = {
            .size = 4,
            .fullWidth = false
        }
    },

    .i64 = {
        .tag = TYPE_INTEGER,
        .indexable = false,
        .accessible = false,
        .callable = false,
        .reference = false,
        .identifier = NULL,
        .arrayTypes = NULL,
        .integer = {
            .size = 8,
            .fullWidth = false
        }
    },

    .ui8 = {
        .tag = TYPE_INTEGER,
        .indexable = false,
        .accessible = false,
        .callable = false,
        .reference = false,
        .arrayTypes = NULL,
        .integer = {
            .size = 1,
            .fullWidth = true
        }
    },

    .ui16 = {
        .tag = TYPE_INTEGER,
        .indexable = false,
        .accessible = false,
        .callable = false,
        .reference = false,
        .identifier = NULL,
        .arrayTypes = NULL,
        .integer = {
            .size = 2,
            .fullWidth = true
        }
    },

    .ui32 = {
        .tag = TYPE_INTEGER,
        .indexable = false,
        .accessible = false,
        .callable = false,
        .reference = false,
        .identifier = NULL,
        .arrayTypes = NULL,
        .integer = {
            .size = 4,
            .fullWidth = true
        }
    },

    .ui64 = {
        .tag = TYPE_INTEGER,
        .indexable = false,
        .accessible = false,
        .callable = false,
        .reference = false,
        .identifier = NULL,
        .arrayTypes = NULL,
        .integer = {
            .size = 8,
            .fullWidth = true
        }
    },



    .f32 = {
        .tag = TYPE_DECIMAL,
        .indexable = false,
        .accessible = false,
        .callable = false,
        .reference = false,
        .identifier = NULL,
        .arrayTypes = NULL,
        .decimal = {
            .size = 4
        }
    },

    .f64 = {
        .tag = TYPE_DECIMAL,
        .indexable = false,
        .accessible = false,
        .callable = false,
        .reference = false,
        .identifier = NULL,
        .arrayTypes = NULL,
        .decimal = {
            .size = 8
        }
    },

    .void_ = {
        .tag = TYPE_VOID,
        .indexable = false,
        .accessible = false,
        .callable = false,
        .reference = false,
        .identifier = NULL,
        .arrayTypes = NULL
    },

    .null = {
        .tag = TYPE_NULL,
        .indexable = false,
        .accessible = false,
        .callable = false,
        .reference = true,
        .identifier = NULL,
        .arrayTypes = NULL,
    },

    .string = {
        .tag = TYPE_STRING,
        .indexable = true,
        .accessible = true,
        .callable = false,
        .reference = true,
        .identifier = NULL,
        .arrayTypes = NULL,
    },

    .unknown = {
        .tag = TYPE_UNKONWN,
        .indexable = false,
        .accessible = false,
        .callable = false,
        .reference = false,
        .identifier = NULL,
        .arrayTypes = NULL,
    }
};

void initializePrimitives() {
    primitives.boolean.arrayTypes = jtk_ArrayList_new();
    primitives.i8.arrayTypes = jtk_ArrayList_new();
    primitives.i16.arrayTypes = jtk_ArrayList_new();
    primitives.i32.arrayTypes = jtk_ArrayList_new();
    primitives.i64.arrayTypes = jtk_ArrayList_new();
    primitives.ui8.arrayTypes = jtk_ArrayList_new();
    primitives.ui16.arrayTypes = jtk_ArrayList_new();
    primitives.ui32.arrayTypes = jtk_ArrayList_new();
    primitives.ui64.arrayTypes = jtk_ArrayList_new();
    primitives.f32.arrayTypes = jtk_ArrayList_new();
    primitives.f64.arrayTypes = jtk_ArrayList_new();
    primitives.string.arrayTypes = jtk_ArrayList_new();
}

void destroyPrimitives() {
    jtk_ArrayList_delete(primitives.boolean.arrayTypes);
    jtk_ArrayList_delete(primitives.i8.arrayTypes);
    jtk_ArrayList_delete(primitives.i16.arrayTypes);
    jtk_ArrayList_delete(primitives.i32.arrayTypes);
    jtk_ArrayList_delete(primitives.i64.arrayTypes);
    jtk_ArrayList_delete(primitives.ui8.arrayTypes);
    jtk_ArrayList_delete(primitives.ui16.arrayTypes);
    jtk_ArrayList_delete(primitives.ui32.arrayTypes);
    jtk_ArrayList_delete(primitives.ui64.arrayTypes);
    jtk_ArrayList_delete(primitives.f32.arrayTypes);
    jtk_ArrayList_delete(primitives.f64.arrayTypes);
    jtk_ArrayList_delete(primitives.string.arrayTypes);
}

/*******************************************************************************
 * Module                                                                      *
 *******************************************************************************/

Module* newModule() {
    Module* result = allocate(Module, 1);
    result->imports = jtk_ArrayList_new();
    result->functions = jtk_ArrayList_new();
    result->structures = jtk_ArrayList_new();

    return result;
}

void deleteModule(Module* self) {
    jtk_ArrayList_delete(self->imports);
    jtk_ArrayList_delete(self->functions);
    jtk_ArrayList_delete(self->structures);

    deallocate(self);
}

/*******************************************************************************
 * ImportDeclaration                                                           *
 *******************************************************************************/

ImportDeclaration* newImportDeclaration() {
    ImportDeclaration* result = allocate(ImportDeclaration, 1);
    result->tag = CONTEXT_IMPORT_DECLARATION;
    result->wildcard = false;
    result->identifiers = jtk_ArrayList_new();

    return result;
}

void deleteImportDeclaration(ImportDeclaration* self) {
    jtk_ArrayList_delete(self->identifiers);
    deallocate(self);
}

/*******************************************************************************
 * BinaryExpression                                                            *
 *******************************************************************************/

BinaryExpression* newBinaryExpression(ContextType tag) {
    BinaryExpression* result = allocate(BinaryExpression, 1);
    result->tag = tag;
    result->left =  NULL;
    result->others = jtk_ArrayList_new();
    return result;
}

void deleteBinaryExpression(BinaryExpression* self) {
    jtk_ArrayList_delete(self->others);
    deallocate(self);
}

/*******************************************************************************
 * ConditionalExpression                                                       *
 *******************************************************************************/

ConditionalExpression* newConditionalExpression() {
    ConditionalExpression* result = allocate(ConditionalExpression, 1);
    result->tag = CONTEXT_CONDITIONAL_EXPRESSION;
    result->hook = NULL;
    result->condition = NULL;
    result->then = NULL;
    result->otherwise = NULL;

    return result;
}

void deleteConditionalExpression(ConditionalExpression* self) {
    deallocate(self);
}

/*******************************************************************************
 * UnaryExpression                                                             *
 *******************************************************************************/

UnaryExpression* newUnaryExpression() {
    UnaryExpression* result = allocate(UnaryExpression, 1);
    result->tag = CONTEXT_UNARY_EXPRESSION;
    result->operator = NULL;
    result->expression = NULL;
    return result;
}

void deleteUnaryExpression(UnaryExpression* self) {
    deallocate(self);
}

/*******************************************************************************
 * PostfixExpression                                                           *
 *******************************************************************************/

PostfixExpression* newPostfixExpression() {
    PostfixExpression* result = allocate(PostfixExpression, 1);
    result->tag = CONTEXT_POSTFIX_EXPRESSION;
    result->primary = NULL;
    result->token = false;
    result->postfixParts = jtk_ArrayList_new();
    return result;
}

void deletePostfixExpression(PostfixExpression* self) {
    jtk_ArrayList_delete(self->postfixParts);
    deallocate(self);
}

/*******************************************************************************
 * MemberAccess                                                                *
 *******************************************************************************/

MemberAccess* newMemberAccess() {
    MemberAccess* result = allocate(MemberAccess, 1);
    result->tag = CONTEXT_MEMBER_ACCESS;
    result->identifier = NULL;
    return result;
}

void deleteMemberAccess(MemberAccess* self) {
    deallocate(self);
}

/*******************************************************************************
 * NewExpression                                                               *
 *******************************************************************************/

NewExpression* newNewExpression() {
    jtk_ArrayList_t* list = jtk_ArrayList_new();

    NewExpression* result = allocate(NewExpression, 1);
    result->tag = CONTEXT_NEW_EXPRESSION;
    result->variableType = NULL;
    result->type = NULL;
    result->entries = list;
    return result;
}

void deleteNewExpression(NewExpression* self) {
    jtk_ArrayList_delete(self->entries);
    deallocate(self);
}

/*******************************************************************************
 * ArrayExpression                                                              *
 *******************************************************************************/

ArrayExpression* newArrayExpression() {
    ArrayExpression* result = allocate(ArrayExpression, 1);
    result->tag = CONTEXT_ARRAY_EXPRESSION;
    result->expressions = jtk_ArrayList_new();
    result->token = NULL;
    result->type = NULL;
    return result;
}

void deleteArrayExpression(ArrayExpression* self) {
    jtk_ArrayList_delete(self->expressions);
    deallocate(self);
}

/*******************************************************************************
 * FunctionArguments                                                           *
 *******************************************************************************/

FunctionArguments* newFunctionArguments() {
    FunctionArguments* result = allocate(FunctionArguments, 1);
    result->tag = CONTEXT_FUNCTION_ARGUMENTS;
    result->parenthesis = NULL;
    result->expressions = jtk_ArrayList_new();
    return result;
}

void deleteFunctionArguments(FunctionArguments* self) {
    jtk_ArrayList_delete(self->expressions);
    deallocate(self);
}

/*******************************************************************************
 * Subscript                                                                   *
 *******************************************************************************/

Subscript* newSubscript() {
    Subscript* result = allocate(Subscript, 1);
    result->tag = CONTEXT_SUBSCRIPT;
    result->bracket = NULL;
    result->expression = NULL;
    return result;
}

void deleteSubscript(Subscript* self) {
    deallocate(self);
}

/*******************************************************************************
 * Block                                                                       *
 *******************************************************************************/

Block* newBlock() {
    Block* result = allocate(Block, 1);
    result->tag = CONTEXT_BLOCK;
    result->statements = jtk_ArrayList_new();
    result->scope = NULL;
    return result;
}

void deleteBlock(Block* self) {
    jtk_ArrayList_delete(self->statements);
    deallocate(self);
}

/*******************************************************************************
 * Function                                                                    *
 *******************************************************************************/

Function* newFunction(const uint8_t* name, int32_t nameSize, Token* identifier,
    jtk_ArrayList_t* parameters, Variable* variableParameter,
    Block* body, VariableType* returnVariableType) {
    Function* result = allocate(Function, 1);
    result->tag = CONTEXT_FUNCTION_DECLARATION;
    result->nameSize = nameSize;
    result->name = jtk_CString_newEx(name, nameSize);
    result->identifier = NULL;
    result->parameters = parameters;
    result->variableParameter = variableParameter;
    result->body = body;
    result->returnVariableType = returnVariableType;
    result->returnType = NULL;
    result->type = newType(TYPE_FUNCTION, false, false, true, false, identifier);
    result->scope = NULL;
    result->totalReferences = 0;

    // TODO: Probably move this to newType(), or some overloaded version of it?
    result->type->function = result;

    return result;
}

void deleteFunction(Function* self) {
    jtk_ArrayList_delete(self->parameters);
    deallocate(self);
}

/*******************************************************************************
 * Structure                                                            *
 *******************************************************************************/

Structure* newStructure(const uint8_t* name, int32_t nameSize,
    Token* identifier, jtk_ArrayList_t* variables) {
    Structure* result = allocate(Structure, 1);
    result->tag = CONTEXT_STRUCTURE_DECLARATION;
    result->nameSize = nameSize;
    result->name = jtk_CString_newEx(name, nameSize);
    result->identifier = identifier;
    result->declarations = variables;
    result->type = newType(TYPE_STRUCTURE, false, true, false, true, identifier);
    result->scope = NULL;

    // TODO: Probably move this to newType(), or some overloaded version of it?
    result->type->structure = result;

    return result;
}

void deleteStructure(Structure* self) {
    deallocate(self);
}

/*******************************************************************************
 * IfClause                                                                    *
 *******************************************************************************/

IfClause* newIfClause() {
    IfClause* result = allocate(IfClause, 1);
    // result->tag = ...;
    result->expression = NULL;
    result->body = NULL;
    result->token = NULL;
    return result;
}

void deleteIfClause(IfClause* self) {
    deallocate(self);
}

/*******************************************************************************
 * IfStatement                                                                 *
 *******************************************************************************/

IfStatement* newIfStatement() {
    IfStatement* result = allocate(IfStatement, 1);
    result->tag = CONTEXT_IF_STATEMENT;
    result->ifClause = NULL;
    result->elseIfClauses = jtk_ArrayList_new();
    result->elseClause = NULL;
    return result;
}

void deleteIfStatement(IfStatement* self) {
    jtk_ArrayList_delete(self->elseIfClauses);
    deallocate(self);
}

/*******************************************************************************
 * IterativeStatement                                                          *
 *******************************************************************************/

IterativeStatement* newIterativeStatement() {
    IterativeStatement* result = allocate(IterativeStatement, 1);
    result->tag = CONTEXT_ITERATIVE_STATEMENT;
    result->label = NULL;
    result->name = NULL;
    result->nameSize = 0;
    result->keyword = NULL;
    result->parameter = NULL;
    result->expression = NULL;
    result->body = NULL;
    return result;
}

void deleteIterativeStatement(IterativeStatement* self) {
    deallocate(self);
}

/*******************************************************************************
 * TryStatement                                                                *
 *******************************************************************************/

TryStatement* newTryStatement() {
    TryStatement* result = allocate(TryStatement, 1);
    result->tag = CONTEXT_TRY_STATEMENT;
    result->tryClause = NULL;
    result->catchClauses = jtk_ArrayList_new();
    result->finallyClause = NULL;
    return result;
}

void deleteTryStatement(TryStatement* self) {
    jtk_ArrayList_delete(self->catchClauses);
    deallocate(self);
}

/*******************************************************************************
 * CatchClause                                                                 *
 *******************************************************************************/

CatchClause* newCatchClause() {
    CatchClause* result = allocate(CatchClause, 1);
    result->captures = jtk_ArrayList_new();
    result->parameter = NULL;
    result->body = NULL;
    return result;
}

void deleteCatchClause(CatchClause* self) {
    jtk_ArrayList_delete(self->captures);
    deallocate(self);
}
/*******************************************************************************
 * VariableType                                                                *
 *******************************************************************************/

VariableType* newVariableType(Token* token, int32_t dimensions) {
    VariableType* self = allocate(VariableType, 1);
    self->token = token;
    self->dimensions = dimensions;

    return self;
}

void deleteVariableType(VariableType* self) {
    deallocate(self);
}

/*******************************************************************************
 * Variable                                                                    *
 *******************************************************************************/

Variable* newVariable(bool infer, bool constant, VariableType* variableType,
    const uint8_t* name, int32_t nameSize, Token* identifier,
    BinaryExpression* expression, Scope* parent) {
    Variable* result = allocate(Variable, 1);
    result->tag = CONTEXT_VARIABLE;
    result->nameSize = nameSize;
    result->name = jtk_CString_newEx(name, nameSize);
    result->infer = infer;
    result->constant = constant;
    result->variableType = variableType;
    result->type = NULL;
    result->identifier = identifier;
    result->expression = expression;
    result->index = -1;

    return result;
}

void deleteVariable(Variable* self) {
    deallocate(self);
}

/*******************************************************************************
 * VariableDeclaration                                                         *
 *******************************************************************************/

VariableDeclaration* newVariableDeclaration() {
    VariableDeclaration* result = allocate(VariableDeclaration, 1);
    result->tag = CONTEXT_VARIABLE_DECLARATION;
    result->variables = jtk_ArrayList_new();
    return result;
}

void deleteVariableDeclaration(VariableDeclaration* self) {
    jtk_ArrayList_delete(self->variables);
    deallocate(self);
}

/*******************************************************************************
 * ThrowStatement                                                              *
 *******************************************************************************/

ThrowStatement* newThrowStatement() {
    ThrowStatement* result = allocate(ThrowStatement, 1);
    result->tag = CONTEXT_THROW_STATEMENT;
    result->expression = NULL;
    return result;
}

void deleteThrowStatement(ThrowStatement* self) {
    deallocate(self);
}

/*******************************************************************************
 * ReturnStatement                                                             *
 *******************************************************************************/

ReturnStatement* newReturnStatement() {
    ReturnStatement* result = allocate(ReturnStatement, 1);
    result->tag = CONTEXT_RETURN_STATEMENT;
    result->expression = NULL;
    result->keyword = NULL;
    return result;
}

void deleteReturnStatement(ReturnStatement* self) {
    deallocate(self);
}

/*******************************************************************************
 * BreakStatement                                                              *
 *******************************************************************************/

BreakStatement* newBreakStatement() {
    BreakStatement* result = allocate(BreakStatement, 1);
    result->tag = CONTEXT_BREAK_STATEMENT;
    result->identifier = NULL;
    return result;
}

void deleteBreakStatement(BreakStatement* self) {
    deallocate(self);
}
