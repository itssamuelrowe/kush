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
#include <kush/token.h>

char tokenNames[][25] = {
    "<unknown>",

    "<whitespace>",
    "<newline>",
    "<eof>",

    "!=",
    "!",

    "@",

    "#",

    "%=",
    "%",

    "&&",
    "&=",
    "&",

    "(",
    ")",

    "*=",
    "*",

    "++",
    "+=",
    "+",

    ",",

    "--",
    "->",
    "-=",
    "-",

    "...",
    ".",

    "<single_line_comment>",
    "<multiline_comment>",
    "/=",
    "/",

    ":",
    ";",

    "<<=",
    "<<",
    "<=",
    "<",

    ">>=",
    ">>",
    ">=",
    ">",

    "==",
    "=",

    "?",

    "{",
    "}",

    "[",
    "]",

    "^=",
    "^",

    "||",
    "|=",
    "|",

    "~=",
    "~",

    "<identifier>",

    "boolean",
    "break",
    "catch",
    "else",
    "f32",
    "f64",
    "false",
    "finally",
    "for",
    "i16",
    "i32",
    "i64",
    "i8",
    "if",
    "import",
    "let",
    "native",
    "new",
    "null",
    "return",
    "struct",
    "string",
    "this",
    "throw",
    "true",
    "try",
    "ui16",
    "ui32",
    "ui64",
    "ui8",
    "var",
    "void",
    "with",
    "while",

    "<integer_literal>",
    "<string_literal>",
    "<floating_point_literal>"
};

Token* newToken(
    TokenChannel channel,
    TokenType type,
    const uint8_t* text,
    int32_t length,
    int32_t startIndex,
    int32_t stopIndex,
    int32_t startLine,
    int32_t stopLine,
    int32_t startColumn,
    int32_t stopColumn,
    const char* file) {
    Token* token = allocate(Token, 1);
    token->channel = channel;
    token->type = type;
    token->text = jtk_CString_newEx(text, length);
    token->length = length; // This is the length of the text representation!
    token->startIndex = startIndex;
    token->stopIndex = stopIndex;
    token->startLine = startLine;
    token->stopLine = stopLine;
    token->startColumn = startColumn;
    token->stopColumn = stopColumn;
    token->file = file;

    return token;
}

void deleteToken(Token* token) {
    jtk_Assert_assertObject(token, "The specified token is null.");
    jtk_CString_delete(token->text);
    deallocate(token);
}

/*******************************************************************************
 * TokenType                                                                   *
 *******************************************************************************/

// Modifiers

uint32_t k_TokenType_toModifiers(TokenType type) {
    uint32_t modifiers = 0;
    switch (type) {
    case TOKEN_KEYWORD_NATIVE: {
        // modifiers |= KUSH_MODIFIER_NATIVE;
        break;
    }
    }
    return modifiers;
}

// Assignment Operator

bool k_TokenType_isAssignmentOperator(TokenType type) {
    return (type == TOKEN_EQUAL) ||
           (type == TOKEN_MODULUS_EQUAL) ||
           (type == TOKEN_AMPERSAND_EQUAL) ||
           (type == TOKEN_ASTERISK_EQUAL) ||
           (type == TOKEN_PLUS_EQUAL) ||
           (type == TOKEN_DASH_EQUAL) ||
           (type == TOKEN_LEFT_ANGLE_BRACKET_2_EQUAL) ||
           (type == TOKEN_LEFT_ANGLE_BRACKET_2_EQUAL) ||
           (type == TOKEN_RIGHT_ANGLE_BRACKET_2_EQUAL) ||
           (type == TOKEN_VERTICAL_BAR_EQUAL);
}
