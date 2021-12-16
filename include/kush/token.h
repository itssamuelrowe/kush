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

// Wedensday, October 18, 2017

#ifndef KUSH_TOKEN_H
#define KUSH_TOKEN_H

#include <kush/configuration.h>

/*******************************************************************************
 * TokenType                                                                   *
 *******************************************************************************/

enum TokenType {
    TOKEN_UNKNOWN,

    TOKEN_WHITESPACE,
    TOKEN_NEWLINE,
    TOKEN_END_OF_STREAM,

    TOKEN_EXCLAMATION_MARK_EQUAL,
    TOKEN_EXCLAMATION_MARK,

    TOKEN_AT,

    TOKEN_HASH,

    TOKEN_MODULUS_EQUAL,
    TOKEN_MODULUS,

    TOKEN_AMPERSAND_2,
    TOKEN_AMPERSAND_EQUAL,
    TOKEN_AMPERSAND,

    TOKEN_LEFT_PARENTHESIS,
    TOKEN_RIGHT_PARENTHESIS,

    TOKEN_ASTERISK_EQUAL,
    TOKEN_ASTERISK,

    TOKEN_PLUS_2,
    TOKEN_PLUS_EQUAL,
    TOKEN_PLUS,

    TOKEN_COMMA,

    TOKEN_DASH_2,
    TOKEN_ARROW,
    TOKEN_DASH_EQUAL,
    TOKEN_DASH,

    TOKEN_ELLIPSIS,
    TOKEN_DOT,

    TOKEN_SINGLE_LINE_COMMENT,
    TOKEN_MULTI_LINE_COMMENT,
    TOKEN_FORWARD_SLASH_EQUAL,
    TOKEN_FORWARD_SLASH,

    TOKEN_COLON,
    TOKEN_SEMICOLON,

    TOKEN_LEFT_ANGLE_BRACKET_2_EQUAL,
    TOKEN_LEFT_ANGLE_BRACKET_2,
    TOKEN_LEFT_ANGLE_BRACKET_EQUAL,
    TOKEN_LEFT_ANGLE_BRACKET,

    TOKEN_RIGHT_ANGLE_BRACKET_2_EQUAL,
    TOKEN_RIGHT_ANGLE_BRACKET_2,
    TOKEN_RIGHT_ANGLE_BRACKET_EQUAL,
    TOKEN_RIGHT_ANGLE_BRACKET,

    TOKEN_EQUAL_2,
    TOKEN_EQUAL,

    TOKEN_HOOK,

    TOKEN_LEFT_BRACE,
    TOKEN_RIGHT_BRACE,

    TOKEN_LEFT_SQUARE_BRACKET,
    TOKEN_RIGHT_SQUARE_BRACKET,

    TOKEN_CARET_EQUAL,
    TOKEN_CARET,

    TOKEN_VERTICAL_BAR_2,
    TOKEN_VERTICAL_BAR_EQUAL,
    TOKEN_VERTICAL_BAR,

    TOKEN_TILDE_EQUAL,
    TOKEN_TILDE,

    TOKEN_IDENTIFIER,

    /* Keywords */

    TOKEN_KEYWORD_BOOLEAN,
    TOKEN_KEYWORD_BREAK,
    TOKEN_KEYWORD_CATCH,
    TOKEN_KEYWORD_ELSE,
    TOKEN_KEYWORD_F32,
    TOKEN_KEYWORD_F64,
    TOKEN_KEYWORD_FALSE,
    TOKEN_KEYWORD_FINALLY,
    TOKEN_KEYWORD_FOR,
    TOKEN_KEYWORD_I16,
    TOKEN_KEYWORD_I32,
    TOKEN_KEYWORD_I64,
    TOKEN_KEYWORD_I8,
    TOKEN_KEYWORD_IF,
    TOKEN_KEYWORD_IMPORT,
    TOKEN_KEYWORD_LET,
    TOKEN_KEYWORD_NATIVE,
    TOKEN_KEYWORD_NEW,
    TOKEN_KEYWORD_NULL,
    TOKEN_KEYWORD_RETURN,
    TOKEN_KEYWORD_STRUCT,
    TOKEN_KEYWORD_STRING,
    TOKEN_KEYWORD_THIS,
    TOKEN_KEYWORD_THROW,
    TOKEN_KEYWORD_TRUE,
    TOKEN_KEYWORD_TRY,
    TOKEN_KEYWORD_UI16,
    TOKEN_KEYWORD_UI32,
    TOKEN_KEYWORD_UI64,
    TOKEN_KEYWORD_UI8,
    TOKEN_KEYWORD_VAR,
    TOKEN_KEYWORD_VOID,
    TOKEN_KEYWORD_WITH,
    TOKEN_KEYWORD_WHILE,

    TOKEN_INTEGER_LITERAL,
    TOKEN_STRING_LITERAL,
    TOKEN_FLOATING_POINT_LITERAL
};

typedef enum TokenType TokenType;

extern char tokenNames[][25];

/*******************************************************************************
 * TokenChannel                                                                *
 *******************************************************************************/

enum TokenChannel {
    TOKEN_CHANNEL_DEFAULT,
    TOKEN_CHANNEL_HIDDEN,
};

typedef enum TokenChannel TokenChannel;

/*******************************************************************************
 * Token                                                                       *
 *******************************************************************************/

/**
 * A token represents the smallest entity that appears
 * in a source code. Each token has two primary attributes:
 * a token type (symbol category) and the text associated
 * with it.
 */
struct Token {
    TokenChannel channel;
    TokenType type;
    uint8_t* text;
    int32_t length;
    int32_t startIndex;
    int32_t stopIndex;
    int32_t startLine;
    int32_t stopLine;
    int32_t startColumn;
    int32_t stopColumn;
    int32_t index;
    const char* file;
};

typedef struct Token Token;

Token* newToken(TokenChannel channel, TokenType type,
    const uint8_t* text, int32_t length, int32_t startIndex, int32_t stopIndex,
    int32_t startLine, int32_t stopLine, int32_t startColumn, int32_t stopColumn,
    const char* file);

void deleteToken(Token* token);

#endif /* KUSH_TOKEN_H */
