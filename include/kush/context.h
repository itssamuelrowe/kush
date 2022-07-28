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

// Sunday, May 31 2020

#ifndef KUSH_PARSER_CONTEXT_H
#define KUSH_PARSER_CONTEXT_H

#include <llvm-c/Core.h>

#include <jtk/collection/list/ArrayList.h>
#include <kush/token.h>
#include <kush/scope.h>

/*******************************************************************************
 * Type                                                                        *
 *******************************************************************************/

/* NOTE: The following constants are used as array index.
 * Therefore, ensure they are always sequential.
 */
#define TYPE_STRUCTURE 0
#define TYPE_INTEGER 1
#define TYPE_DECIMAL 2
#define TYPE_ARRAY 3
#define TYPE_VOID 4
#define TYPE_NULL 5
#define TYPE_STRING 6
#define TYPE_BOOLEAN 7
#define TYPE_FUNCTION 8
#define TYPE_ANY 9
#define TYPE_UNKNOWN 10

/* Variable layout for `any` type:
 *
 * [    type    ][    value    ]
 *  \             \
 *   8 bytes       8 bytes*
 *
 * The value size should be wide enough to hold pointers and
 * `i64` values.
 */
#define ANY_TYPE_HEADER 8
#define ANY_TYPE_SIZE (8 + ANY_TYPE_HEADER)

typedef struct Type Type;
typedef struct Structure Structure;

struct Type {
    uint8_t tag;
    bool indexable;
    bool accessible;
    bool callable;
    bool reference;
    Token* identifier;
    jtk_ArrayList_t* arrayTypes;
    LLVMTypeRef llvmType;
    LLVMValueRef llvmDefaultValue;
    union {
        struct {
            /**
             * Represents a structure that provides attributes for arrays.
             */
            Structure* array;

            /* Represents the actual type, without taking dimensions into
             * consideration. For example, i32[][][], base always
             * represents i32.
             */
            Type* base;

            /* Represents the component type, that is, a dimension one lesser
             * than the dimension of the current type. When the current dimenion
             * is 1, component and base are equal.
             */
            Type* component;

            /* The number of dimensions. */
            uint16_t dimensions;
        } array;
        struct {
            uint8_t size;
            bool fullWidth;
        } integer;
        struct {
            uint8_t size;
        } decimal;
        Structure* structure;
        Function* function;
    };
};

Type* newType(uint8_t tag, bool indexable, bool accessible, bool callable,
    bool allocatable, Token* identifier, LLVMTypeRef llvmType,
    LLVMValueRef llvmDefaultValue);
void deleteType(Type* type);
void printType(Type* type);

/*******************************************************************************
 * Primitives                                                                  *
 *******************************************************************************/

struct Primitives {
    Type any;
    Type boolean;
    Type i8;
    Type i16;
    Type i32;
    Type i64;
    Type ui8;
    Type ui16;
    Type ui32;
    Type ui64;
    Type f32;
    Type f64;
    Type void_;
    Type null;
    Type string;
    Type unknown;
};

typedef struct Primitives Primitives;

extern Primitives primitives;

void initializePrimitives(LLVMContextRef llvmContext);
void destroyPrimitives();

/*******************************************************************************
 * ContextType                                                                 *
 *******************************************************************************/

enum ContextType {

    CONTEXT_UNKNOWN,

    CONTEXT_MODULE,

    CONTEXT_IMPORT_DECLARATION,

    CONTEXT_FUNCTION_DECLARATION,

    CONTEXT_BLOCK,

    CONTEXT_VARIABLE_DECLARATION,

    CONTEXT_VARIABLE,

    CONTEXT_BREAK_STATEMENT,

    CONTEXT_RETURN_STATEMENT,

    CONTEXT_THROW_STATEMENT,

    CONTEXT_IF_STATEMENT,

    CONTEXT_ITERATIVE_STATEMENT,

    CONTEXT_TRY_STATEMENT,

    CONTEXT_CATCH_CLAUSE,

    CONTEXT_STRUCTURE_DECLARATION,

    CONTEXT_ASSIGNMENT_EXPRESSION,

    CONTEXT_CONDITIONAL_EXPRESSION,

    CONTEXT_LOGICAL_OR_EXPRESSION,

    CONTEXT_LOGICAL_AND_EXPRESSION,

    CONTEXT_INCLUSIVE_OR_EXPRESSION,

    CONTEXT_EXCLUSIVE_OR_EXPRESSION,

    CONTEXT_AND_EXPRESSION,

    CONTEXT_EQUALITY_EXPRESSION,

    CONTEXT_RELATIONAL_EXPRESSION,

    CONTEXT_SHIFT_EXPRESSION,

    CONTEXT_ADDITIVE_EXPRESSION,

    CONTEXT_MULTIPLICATIVE_EXPRESSION,

    CONTEXT_UNARY_EXPRESSION,

    CONTEXT_POSTFIX_EXPRESSION,

    CONTEXT_SUBSCRIPT,

    CONTEXT_FUNCTION_ARGUMENTS,

    CONTEXT_MEMBER_ACCESS,

    CONTEXT_NEW_EXPRESSION,

    CONTEXT_ARRAY_EXPRESSION
};

typedef enum ContextType ContextType;

/*******************************************************************************
 * Context                                                                     *
 *******************************************************************************/

struct Context {
    ContextType tag;
};

typedef struct Context Context;

/*******************************************************************************
 * Symbol                                                                      *
 *******************************************************************************/

struct Symbol {
    ContextType tag;
    uint8_t* name;
    int32_t nameSize;
};

/*******************************************************************************
 * VariableType                                                                *
 *******************************************************************************/

struct VariableType {
    Token* token;
    int32_t dimensions;
};

typedef struct VariableType VariableType;

VariableType* newVariableType(Token* token, int32_t dimensions);
void deleteVariableType(VariableType* self);

/*******************************************************************************
 * Module                                                                      *
 *******************************************************************************/

struct Module {
    ContextType tag;
	jtk_ArrayList_t* imports;
	jtk_ArrayList_t* functions;
    jtk_ArrayList_t* structures;
    Scope* scope;
};

typedef struct Module Module;

Module* newModule();
void deleteModule(Module* self);

/*******************************************************************************
 * ImportDeclaration                                                           *
 *******************************************************************************/

struct ImportDeclaration {
    ContextType tag;
    bool wildcard;
    jtk_ArrayList_t* identifiers;
};

typedef struct ImportDeclaration ImportDeclaration;

ImportDeclaration* newImportDeclaration();
void deleteImportDeclaration(ImportDeclaration* self);

/*******************************************************************************
 * BinaryExpression                                                            *
 *******************************************************************************/

typedef struct BinaryExpression BinaryExpression;

// TODO: Set resultType to NULL in new*().
struct BinaryExpression {
    ContextType tag;
    BinaryExpression* left;
    jtk_ArrayList_t* others;
    Type* type;
};

BinaryExpression* newBinaryExpression(ContextType tag);
void deleteBinaryExpression(BinaryExpression* self);

/*******************************************************************************
 * ConditionalExpression                                                       *
 *******************************************************************************/

typedef struct ConditionalExpression ConditionalExpression;

struct ConditionalExpression {
    ContextType tag;
    Token* hook;
    BinaryExpression* condition;
    BinaryExpression* then;
    ConditionalExpression* otherwise;
    Type* type;
};

ConditionalExpression* newConditionalExpression();
void deleteConditionalExpression(ConditionalExpression* self);

/*******************************************************************************
 * UnaryExpression                                                             *
 *******************************************************************************/

struct UnaryExpression {
    ContextType tag;
    Token* operator;
    Context* expression; // Unary expression or postfix expression
    Type* type;
};

typedef struct UnaryExpression UnaryExpression;

UnaryExpression* newUnaryExpression();
void deleteUnaryExpression(UnaryExpression* self);

/*******************************************************************************
 * PostfixExpression                                                           *
 *******************************************************************************/

struct PostfixExpression {
    ContextType tag;
    void* primary;
    bool token;
    jtk_ArrayList_t* postfixParts; // contexts
    Type* type;
};

typedef struct PostfixExpression PostfixExpression;

PostfixExpression* newPostfixExpression();
void deletePostfixExpression(PostfixExpression* self);

/*******************************************************************************
 * MemberAccess                                                                *
 *******************************************************************************/

struct MemberAccess {
    ContextType tag;
    Token* identifier;
    Type* previous;
    Type* type;
};

typedef struct MemberAccess MemberAccess;

MemberAccess* newMemberAccess();
void deleteMemberAccess(MemberAccess* self);

/*******************************************************************************
 * NewExpression                                                               *
 *******************************************************************************/

struct NewExpression {
    ContextType tag;
    VariableType* variableType;
    Type* type;
    union {
        jtk_ArrayList_t* entries;
        jtk_ArrayList_t* expressions;
    };
};

typedef struct NewExpression NewExpression;

NewExpression* newNewExpression();
void deleteNewExpression(NewExpression* self);

/*******************************************************************************
 * ArrayExpression                                                              *
 *******************************************************************************/

struct ArrayExpression {
    ContextType tag;
    jtk_ArrayList_t* expressions;
    Token* token;
    Type* type;
};

typedef struct ArrayExpression ArrayExpression;

ArrayExpression* newArrayExpression();
void deleteArrayExpression(ArrayExpression* self);

/*******************************************************************************
 * FunctionArguments                                                           *
 *******************************************************************************/

struct FunctionArguments {
    ContextType tag;
    Token* parenthesis;
    jtk_ArrayList_t* expressions;
    Type* type;
};

typedef struct FunctionArguments FunctionArguments;

FunctionArguments* newFunctionArguments();
void deleteFunctionArguments(FunctionArguments* self);

/*******************************************************************************
 * Subscript                                                                   *
 *******************************************************************************/

struct Subscript {
    ContextType tag;
    /**
     * The token is used when reporting errors, that is, to show the location.
     */
    Token* bracket;
    BinaryExpression* expression;
    /**
     * The type of the expression over which the subscript operator is applied.
     * For example, in `(Arrays:subarray<i32>(array, 0, 2))[0]` previous would be the
     * resolved type for `(Arrays:subarray<i32>(array, 0, 2))`, which is `i32[]`.
     * 
     * When an instance of `Subscript` is created, this attribute is initialized to `NULL`.
     * It is updated by the analyzer with an instance of `Type`.
     */
    Type* previous;
    Type* type;
};

typedef struct Subscript Subscript;

Subscript* newSubscript();
void deleteSubscript(Subscript* self);

/*******************************************************************************
 * Block                                                              *
 *******************************************************************************/

struct Block {
    ContextType tag;
    jtk_ArrayList_t* statements;
    Scope* scope;
};

typedef struct Block Block;

Block* newBlock();
void deleteBlock(Block* self);

/*******************************************************************************
 * Variable                                                                    *
 *******************************************************************************/

/**
 * Not part of the AST.
 */
struct Variable {
    ContextType tag;
    uint8_t* name;
    int32_t nameSize;
    bool infer;
    bool constant;
    bool parameter;
    VariableType* variableType;
    Type* type;
    Token* identifier;
    BinaryExpression* expression;
    int32_t index;
    LLVMValueRef llvmValue;
};

typedef struct Variable Variable;

Variable* newVariable(bool infer, bool constant, bool parameter,
    VariableType* variableType, const uint8_t* name, int32_t nameSize,
    Token* identifier, BinaryExpression* expression, Scope* parent);
void deleteVariable(Variable* variable);

/*******************************************************************************
 * Function                                                                    *
 *******************************************************************************/

struct Function {
    ContextType tag;
    uint8_t* name;
    int32_t nameSize;
    Token* identifier;
    jtk_ArrayList_t* parameters;
    Variable* variableParameter;
    Block* body;
    VariableType* returnVariableType;
    Type* returnType;
    Type* type;
    Scope* scope;
    int32_t totalReferences;
    LLVMValueRef llvmValue;
};

typedef struct Function Function;

Function* newFunction(const uint8_t* name, int32_t nameSize, Token* identifier,
    jtk_ArrayList_t* parameters, Variable* variableParameter,
    Block* body, VariableType* returnVariableType);
void deleteFunction(Function* self);

/*******************************************************************************
 * Structure                                                            *
 *******************************************************************************/

struct Structure {
    ContextType tag;
    uint8_t* name;
    int32_t nameSize;
    Token* identifier;
    // TODO: Rename variables to declarations.
    jtk_ArrayList_t* declarations;
    Type* type;
    Scope* scope;
    LLVMValueRef llvmConstructor;
};

typedef struct Structure Structure;

Structure* newStructure(const uint8_t* name, int32_t nameSize,
    Token* identifier, jtk_ArrayList_t* variables);
void deleteStructure(Structure* self);

/*******************************************************************************
 * IfClause                                                                    *
 *******************************************************************************/

struct IfClause {
    ContextType tag;
    BinaryExpression* expression;
    Block* body;
    Token* token;
};

typedef struct IfClause IfClause;

IfClause* newIfClause();
void deleteIfClause(IfClause* self);

/*******************************************************************************
 * IfStatement                                                                 *
 *******************************************************************************/

struct IfStatement {
    ContextType tag;
    IfClause* ifClause;
    jtk_ArrayList_t* elseIfClauses;
    Block* elseClause;
};

typedef struct IfStatement IfStatement;

IfStatement* newIfStatement();
void deleteIfStatement(IfStatement* self);

/*******************************************************************************
 * IterativeStatement                                                          *
 *******************************************************************************/

struct IterativeStatement {
    ContextType tag;
    uint8_t* name;
    int32_t nameSize;
    Token* label;
    Token* keyword;
    Token* parameter;
    BinaryExpression* expression;
    Block* body;
};

typedef struct IterativeStatement IterativeStatement;

IterativeStatement* newIterativeStatement();
void deleteIterativeStatement(IterativeStatement* self);

/*******************************************************************************
 * TryStatement                                                                *
 *******************************************************************************/

struct TryStatement {
    ContextType tag;
    Block* tryClause;
    jtk_ArrayList_t* catchClauses;
    Block* finallyClause;
};

typedef struct TryStatement TryStatement;

TryStatement* newTryStatement();
void deleteTryStatement(TryStatement* self);

/*******************************************************************************
 * CatchClause                                                                 *
 *******************************************************************************/

/**
 * Not part of the AST.
 */
struct CatchClause {
    jtk_ArrayList_t* captures;
    Variable* parameter;
    Block* body;
};

typedef struct CatchClause CatchClause;

CatchClause* newCatchClause();
void deleteCatchClause(CatchClause* self);

/*******************************************************************************
 * VariableDeclaration                                                         *
 *******************************************************************************/

struct VariableDeclaration {
    ContextType tag;
    jtk_ArrayList_t* variables;
};

typedef struct VariableDeclaration VariableDeclaration;

VariableDeclaration* newVariableDeclaration();
void deleteVariableDeclaration(VariableDeclaration* self);

/*******************************************************************************
 * ThrowStatement                                                              *
 *******************************************************************************/

struct ThrowStatement {
    ContextType tag;
    BinaryExpression* expression;
};

typedef struct ThrowStatement ThrowStatement;

ThrowStatement* newThrowStatement();
void deleteThrowStatement(ThrowStatement* self);

/*******************************************************************************
 * ReturnStatement                                                             *
 *******************************************************************************/

struct ReturnStatement {
    ContextType tag;
    BinaryExpression* expression;
    Token* keyword;
};

typedef struct ReturnStatement ReturnStatement;

ReturnStatement* newReturnStatement();
void deleteReturnStatement(ReturnStatement* self);

/*******************************************************************************
 * BreakStatement                                                              *
 *******************************************************************************/

struct BreakStatement {
    ContextType tag;
    Token* identifier;
};

typedef struct BreakStatement BreakStatement;

BreakStatement* newBreakStatement();
void deleteBreakStatement(BreakStatement* self);

#endif /* KUSH_PARSER_CONTEXT_H */