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

// Monday, March 11, 2018

#include <kush/error-handler.h>

/*******************************************************************************
 * Error                                                                       *
 *******************************************************************************/

// Constructor

Error* errorNew(ErrorCode code, Token* token) {
    return errorNewEx(code, token, TOKEN_UNKNOWN);
}

Error* errorNewEx(ErrorCode code, Token* token,
    TokenType expected) {
    Error* error = allocate(Error, 1);
    error->code = code;
    error->token = token;
    error->expected = expected;

    return error;
}

// Destructor

void errorDelete(Error* error) {
    jtk_Assert_assertObject(error, "The specified error is null.");

    deallocate(error);
}

/*******************************************************************************
 * ErrorHandler                                                                *
 *******************************************************************************/

// Constructor

ErrorHandler* newErrorHandler() {
    ErrorHandler* handler = allocate(ErrorHandler, 1);
    handler->onLexicalError = NULL;
    handler->onSyntaxError = NULL;
    handler->onSemanticError = NULL;
    handler->errors = jtk_ArrayList_new();
    handler->context = NULL;

    return handler;
}

// Destructor

void deleteErrorHandler(ErrorHandler* handler) {
    jtk_Assert_assertObject(handler, "The specified error handler is null.");

    int32_t errorCount = jtk_ArrayList_getSize(handler->errors);
    int32_t i;
    for (i = 0; i < errorCount; i++) {
        Error* error = (Error*)jtk_ArrayList_getValue(handler->errors, i);
        errorDelete(error);
    }
    jtk_ArrayList_delete(handler->errors);
    deallocate(handler);
}

void handleSyntaxError(ErrorHandler* handler,
    Parser* parser, ErrorCode errorCode, Token* token,
    TokenType expected) {
    jtk_Assert_assertObject(handler, "The specified error handler is null.");

    Error* error = errorNewEx(errorCode, token, expected);
    jtk_ArrayList_add(handler->errors, error);

    if (handler->onSyntaxError != NULL) {
        handler->onSyntaxError(handler->context, parser, error, expected);
    }
}

void handleLexicalError(ErrorHandler* handler,
    Lexer* lexer, ErrorCode errorCode, Token* token) {
    jtk_Assert_assertObject(handler, "The specified error handler is null.");

    Error* error = errorNew(errorCode, token);
    jtk_ArrayList_add(handler->errors, error);

    if (handler->onLexicalError != NULL) {
        handler->onLexicalError(handler->context, lexer, error);
    }
}

void handleSemanticError(ErrorHandler* handler,
    void* origin, ErrorCode errorCode, Token* token) {
    jtk_Assert_assertObject(handler, "The specified error handler is null.");

    Error* error = errorNew(errorCode, token);
    jtk_ArrayList_add(handler->errors, error);

    if (handler->onSemanticError != NULL) {
        handler->onSemanticError(handler->context, origin, error);
    }
}

void handleGeneralError(ErrorHandler* handler,
    void* origin, ErrorCode errorCode) {
    jtk_Assert_assertObject(handler, "The specified error handler is null.");

    Error* error = errorNew(errorCode, NULL);
    jtk_ArrayList_add(handler->errors, error);

    if (handler->onGeneralError != NULL) {
        handler->onGeneralError(handler->context, origin, error);
    }
}