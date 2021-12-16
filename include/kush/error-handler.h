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

// Monday, March 12, 2018

#ifndef KUSH_COMPILER_SUPPORT_ERROR_HANDLER_H
#define KUSH_COMPILER_SUPPORT_ERROR_HANDLER_H

#include <jtk/collection/list/ArrayList.h>

#include <kush/configuration.h>
#include <kush/token.h>

/*******************************************************************************
 * ErrorCode                                                                   *
 *******************************************************************************/

/**
 * @class ErrorCode
 * @ingroup k_compiler_support
 * @author Samuel Rowe
 * @since Kush 0.1
 */
enum ErrorCode {
    ERROR_NONE = 0,

    // Lexcial Errors
    ERROR_UNTERMINATED_STRING_LITERAL,
    ERROR_UNTERMINATED_MULTI_LINE_COMMENT,
    ERROR_MALFORMED_UNICODE_CHARACTER_SEQUENCE,
    ERROR_INVALID_ESCAPE_SEQUENCE,
    ERROR_UNKNOWN_CHARACTER,
    ERROR_INVALID_INTEGER_LITERAL_PREFIX,
    ERROR_EXPECTED_DIGIT_AFTER_UNDERSCORE,
    ERROR_EXPECTED_LETTER_AFTER_COLON,

    // Syntax Errors
    ERROR_UNEXPECTED_TOKEN,
    ERROR_TRY_STATEMENT_EXPECTS_CATCH_OR_FINALLY,
    ERROR_VARIABLE_INITIALIZER_EXPECTED,
    ERROR_INVALID_LVALUE,

    // Semantic Errors

    // Errors related to binary expressions
    ERROR_CANNOT_COMBINE_EQUALITY_OPERATORS,
    ERROR_NON_CALLABLE_TYPE,
    ERROR_NON_INDEXABLE_TYPE,
    ERROR_NON_ACCESSIBLE_TYPE,

    // Errors related to unary expressions
    ERROR_INVALID_OPERAND,

    // Errors related to declaration
    ERROR_UNKNOWN_MODULE,
    ERROR_UNDECLARED_TYPE,
    ERROR_UNDECLARED_MEMBER,
    ERROR_UNDECLARED_IDENTIFIER,
    ERROR_UNDECLARED_LABEL,
    ERROR_REDECLARATION_AS_FUNCTION,
    ERROR_REDECLARATION_AS_PARAMETER,
    ERROR_REDECLARATION_AS_VARIABLE_PARAMETER,
    ERROR_REDECLARATION_AS_VARIABLE,
    ERROR_REDECLARATION_AS_CONSTANT,
    ERROR_REDECLARATION_AS_LABEL,
    ERROR_REDECLARATION_AS_LOOP_PARAMETER,
    ERROR_REDECLARATION_AS_CATCH_PARAMETER,
    ERROR_REDECLARATION_AS_STRUCTURE,
    ERROR_REDECLARATION_PREVIOUSLY_IMPORTED,
    ERROR_INVALID_TYPE,
    ERROR_INCOMPATIBLE_VARIABLE_INITIALIZER,

    // Errors related to types
    ERROR_EXPECTED_BOOLEAN_EXPRESSION,
    ERROR_EXPECTED_BOOLEAN_EXPRESSION_ON_LEFT,
    ERROR_EXPECTED_BOOLEAN_EXPRESSION_ON_RIGHT,
    ERROR_EXPECTED_INTEGER_EXPRESSION_ON_LEFT,
    ERROR_EXPECTED_INTEGER_EXPRESSION_ON_RIGHT,
    ERROR_INVALID_LEFT_OPERAND,
    ERROR_INVALID_RIGHT_OPERAND,
    ERROR_INCOMPATIBLE_OPERAND_TYPES,
    ERROR_EXPECTED_VARIABLE,
    ERROR_EXPECTED_LABEL,
    ERROR_INCOMPATIBLE_RETURN_VALUE,
    ERROR_INVALID_ARGUMENT_COUNT,
    ERROR_INCOMPATIBLE_ARGUMENT_TYPE,
    ERROR_ARRAY_MEMBERS_SHOULD_HAVE_SAME_TYPE,
    ERROR_EMPTY_ARRAY_INITIALIZER,
    ERROR_EXPECTED_STRUCTURE_NAME,
    ERROR_EXPECTED_INTEGER_EXPRESSION,

    // General Errors

    ERROR_CORRUPTED_MODULE,
    ERROR_INVALID_MODULE_VERSION,

    ERROR_COUNT
};

typedef enum ErrorCode ErrorCode;

/*******************************************************************************
 * Error                                                                       *
 *******************************************************************************/

struct Error {
    ErrorCode code;
    Token* token;
    TokenType expected;
};

typedef struct Error Error;

Error* errorNew(ErrorCode errorCode, Token* token);
Error* errorNewEx(ErrorCode errorCode, Token* token, TokenType expected);
void errorDelete(Error* error);

/*******************************************************************************
 * ErrorHandler                                                                *
 *******************************************************************************/

typedef struct Lexer Lexer;
typedef struct Parser Parser;

/* NOTE: The handlers should not maintain any reference to the origin
 * object that they receive. Because errors may persist beyond the lifetime
 * of an origin object.
 */

typedef void (*OnLexicalErrorFunction)(void* context, Lexer* lexer, Error* error);
typedef void (*OnSyntaxErrorFunction)(void* context, Parser* parser, Error* error, TokenType expected);
typedef void (*OnSemanticErrorFunction)(void* context, void* origin, Error* error);

typedef void (*OnGeneralErrorFunction)(void* context, void* origin, Error* error);

struct ErrorHandler {
    OnLexicalErrorFunction onLexicalError;
    OnSyntaxErrorFunction onSyntaxError;
    OnSemanticErrorFunction onSemanticError;
    OnGeneralErrorFunction onGeneralError;

    jtk_ArrayList_t* errors;
    void* context;
};

typedef struct ErrorHandler ErrorHandler;

ErrorHandler* newErrorHandler();
void deleteErrorHandler(ErrorHandler* handler);
void handleSyntaxError(ErrorHandler* handler,
    Parser* parser, ErrorCode errorCode, Token* token,
    TokenType expected);
void handleLexicalError(ErrorHandler* handler,
    Lexer* lexer, ErrorCode errorCode, Token* token);
void handleSemanticError(ErrorHandler* handler,
    void* origin, ErrorCode errorCode, Token* token);
void handleGeneralError(ErrorHandler* handler,
    void* origin, ErrorCode errorCode);

#endif /* KUSH_COMPILER_SUPPORT_ERROR_HANDLER_H */
