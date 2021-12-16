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

#include <jtk/collection/list/ArrayList.h>
#include <jtk/collection/queue/ArrayQueue.h>
#include <jtk/collection/stack/ArrayStack.h>
#include <jtk/io/InputStream.h>
#include <jtk/core/StringBuilder.h>
#include <jtk/core/CString.h>

#include <kush/lexer.h>
#include <kush/error-handler.h>

/*******************************************************************************
 * Lexer                                                                       *
 *******************************************************************************/

static void consume(Lexer* lexer);
static void destroyStaleTokens(Lexer* lexer);
static void emit(Lexer* lexer, Token* token);
static Token* createToken(Lexer* lexer);
static void onNewLine(Lexer* lexer);
static void binaryIntegerLiteral(Lexer* lexer);
static void octalIntegerLiteral(Lexer* lexer);
static void hexadecimalIntegerLiteral(Lexer* lexer);
static void decimalIntegerLiteral(Lexer* lexer);
static void integerLiteral(Lexer* lexer);


#define isBinaryPrefix(c) (c == 'b') || (c == 'B')

#define isBinaryDigit(c) (c == '0') || (c == '1')

#define isBinaryDigitOrUnderscore(c) \
    (c == '0') || (c == '1') || (c == '_')

#define isBasicEscapeSequence(c) \
    (c == 'b') || \
    (c == 'f') || \
    (c == 'n') || \
    (c == 'r') || \
    (c == 't') || \
    (c == '\\') || \
    (c == '\"') || \
    (c == '\'')

#define isDecimalDigit(c) (c >= '0') && (c <= '9')

#define isDecimalDigitOrUnderscore(c) isDecimalDigit(c) || (c == '_')

#define isIdentifierStart(c) \
    ((c >= 'a') && (c <= 'z')) || \
    ((c >= 'A') && (c <= 'Z'))

#define isIdentifierPart(c) \
    ((c >= 'a') && (c <= 'z')) || \
    ((c >= 'A') && (c <= 'Z')) || \
    ((c >= '0') && (c <= '9')) || \
    (c == '_')

#define isLetter(c) ((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z'))

#define isLetterOrDigit(c) isLetter(c) || isDecimalDigit(c)

#define isHexadecimalPrefix(c) (c == 'x') || (c == 'X')

#define isHexadecimalDigit(c) \
    isDecimalDigit(c) || ((c >= 'a') && (c <= 'f')) || ((c >= 'A') && (c <= 'F'))

#define isHexadecimalDigitOrUnderscore(c) isHexadecimalDigit(c) || (c == '_')

#define isOctalDigit(c) (c >= '0') && (c <= '7')

#define isOctalPrefix(c) (c == 'c') || (c == 'C')

#define isOctalDigitOrUnderscore(c) isOctalDigit(c) || (c == '_')

#define isIntegerSuffix(c) (c == 'l') || (c == 'L')

/* Constructor */

Lexer* lexerNew(Compiler* compiler) {
    /* The constructor invokes consume() to initialize
     * the LA(1) character. Therefore, we assign negative values
     * to certain attributes.
     */

    Lexer* lexer = allocate(Lexer, 1);
    lexer->compiler = compiler;
    lexer->inputStream = NULL;
    lexer->la1 = 0;
    lexer->index = -1;
    lexer->line = 1;
    lexer->column = -1;
    lexer->startIndex = 0;
    lexer->startLine = 0;
    lexer->startColumn = 0;
    lexer->hitEndOfStream = false;
    lexer->token = NULL;
    lexer->channel = TOKEN_CHANNEL_DEFAULT;
    lexer->text = jtk_StringBuilder_new();
    lexer->type = TOKEN_UNKNOWN;
    lexer->tokens = jtk_ArrayQueue_new();
    lexer->errorCode = ERROR_NONE;

    return lexer;
}

/* Destructor */

/* The lexer may have unretrieved tokens in the buffer.
* This function is responsible for the destruction of
* such tokens.
*/
void destroyStaleTokens(Lexer* lexer) {
    int32_t size = jtk_ArrayList_getSize(lexer->tokens->m_list);
    int32_t i;
    for (i = 0; i < size; i++) {
        Token* token = (Token*)jtk_ArrayList_getValue(lexer->tokens->m_list, i);
        deleteToken(token);
    }
}

void lexerDelete(Lexer* lexer) {
    jtk_Assert_assertObject(lexer, "The specified lexer is null.");

    jtk_StringBuilder_delete(lexer->text);
    jtk_ArrayQueue_delete(lexer->tokens);
    deallocate(lexer);
}

/* Create Token */

Token* createToken(Lexer* lexer) {
    uint8_t* text = jtk_CString_newEx(lexer->text->m_value, lexer->text->m_size); // jtk_StringBuilder_toCString(lexer->text);
    int32_t length = jtk_StringBuilder_getSize(lexer->text);

    Compiler* compiler = lexer->compiler;
    const char* file = jtk_ArrayList_getValue(compiler->inputFiles,
                       compiler->currentFileIndex);
    Token* token =
        newToken(
            lexer->channel,
            lexer->type,
            text,
            length,
            lexer->startIndex,    /* inclusive */
            lexer->index,         /* exclusive */
            lexer->startLine,     /* inclusive */
            lexer->line,          /* inclusive */
            lexer->startColumn,   /* inclusive */
            lexer->column,        /* inclusive */
            file
        );

    /* Destroy the text; not required anymore. */
    jtk_CString_delete(text);

    return token;
}

void onNewLine(Lexer* lexer) {
    lexer->line++;
    lexer->column = 1;
}

bool isInputStart(Lexer* lexer) {
    return (lexer->startLine == 0) && (lexer->startColumn == 0);
}

void consume(Lexer* lexer) {
    jtk_StringBuilder_appendCodePoint(lexer->text, lexer->la1);

    lexer->index++;
    lexer->column++;
    /* NOTE: We could have used lexer->index >= "length of input stream"
     * Unfortunately, the flexible design of both the lexer and input stream fails
     * to provide a method to determine the stream length in advance.
     *
     * NOTE: The getAvailable() function is only temporarily used. However, the
     * working of this function is not finalized. Therefore, the following expression
     * may be subjected to changes.
     */
    if (!jtk_InputStream_isAvailable(lexer->inputStream)) {
        lexer->la1 = KUSH_END_OF_STREAM;
    }
    else {
        lexer->la1 = jtk_InputStream_read(lexer->inputStream);
    }
}

void emit(Lexer* lexer, Token* token) {
    lexer->token = token;
    jtk_ArrayQueue_enqueue(lexer->tokens, token);
}

/* NOTE: The design for the integer literal was adopted from Java 8's lexer. The actual rules
* were borrowed and adopted from the grammars-v4/Java8 repository in ANTLR's GitHub profile.
*
* One of the advantages of hand written lexers over generated lexers is that the code may be
* optimized with the domain knowledge. This is exactly what I have done here. In other words,
* the following grammar disallows underscores at the very end of an integer literal.
* To support this, the designer has taken a longer path. However, I have simply used a
* variable called previous, which stores the last character consumed by the integer literal
* recognizing code, to prevent such inappropriate occurrences. After the simplified code
* executes, the previous variable is tested for an underscore. If the test succeeds, a lexical
* error is generated; otherwise, the integer literal is emitted by the lexer.
*
* IntegerLiteral
* : BinaryIntegerLiteral
* | OctalIntegerLiteral
* | HexadecimalIntegerLiteral
* | DecimalIntegerLiteral
* ;
*
* BinaryIntegerLiteral
* :    '0' [bB] BinaryNumeral IntegerTypeSuffix?
* ;
*
* DecimalIntegerLiteral
* :    DecimalNumeral IntegerTypeSuffix?
* ;
*
*
* OctalIntegerLiteral
* :    '0' [cC] OctalNumeral IntegerTypeSuffix?
* ;
*
* HexadecimalIntegerLiteral
* :    '0' [xX] HexadecimalNumeral IntegerTypeSuffix?
* ;
*
* IntegerTypeSuffix
* :    [lL]
* ;
*
* -----------------------------------------------------
*
* BinaryNumeral
* :    BinaryDigit (BinaryDigitsAndUnderscores? BinaryDigit)?
* ;
*
* BinaryDigit
* :    [01]
* ;
*
* BinaryDigitsAndUnderscores
* :    BinaryDigitOrUnderscore+
* ;
*
* BinaryDigitOrUnderscore
* :    BinaryDigit
* |    '_'
* ;
*
* -----------------------------------------------------
*
* OctalNumeral
* :    OctalDigit (OctalDigitsAndUnderscores? OctalDigit)?
* ;
*
* OctalDigit
* :    [0-7]
* ;
*
* OctalDigitsAndUnderscores
* :    OctalDigitOrUnderscore+
* ;
*
* OctalDigitOrUnderscore
* :    OctalDigit
* |    '_'
* ;
*
* -----------------------------------------------------
*
* DecimalNumeral
* :    '0'
* |    DecimalNonZeroDigit (Digits? Undscores Digits)
* ;
*
* DecimalDigits
* :    Digit (DecimalDigitsAndUnderscores? DecimalDigit)?
* ;
*
* DecimalDigit
* :    '0'
* |    NonZeroDigit
* ;
*
* DecimalNonZeroDigit
* :    [1-9]
* ;
*
* DecimalDigitsAndUnderscores
* :    DecimalDigitOrUnderscore+
* ;
*
* DecimalDigitOrUnderscore
* :    DecimalDigit
* |    '_'
* ;
*
* -----------------------------------------------------
*
* HexadecimalNumeral
* :    HexadecimalDigit (HexadecimalDigitsAndUnderscores? HexadecimalDigit)?
* ;
*
* HexadecimalDigit
* :    [0-9a-f-A-F]
* ;
*
* HexadecimalDigitsAndUnderscores
* :    HexadecimalDigitOrUnderscore+
* ;
*
* HexadecimalDigitOrUnderscore
* :    HexadecimalDigit
* |    '_'
* ;
*
*/

void binaryIntegerLiteral(Lexer* lexer) {
    /* Consume and discard the binary prefix character. */
    consume(lexer);

    if (isBinaryDigit(lexer->la1)) {
        /* Consume and discard the binary digit character. */
        consume(lexer);

        if (isBinaryDigitOrUnderscore(lexer->la1)) {
            uint8_t previous = '\0';
            while (isBinaryDigitOrUnderscore(lexer->la1)) {
                previous = lexer->la1;

                /* Consume and discard a binary digit or an underscore
                    * character.
                    */
                consume(lexer);
            }

            if (previous == '_') {
                lexer->errorCode = ERROR_EXPECTED_DIGIT_AFTER_UNDERSCORE;
                /* Consume and discard the invalid character. */
                consume(lexer);
            }
        }
    }
    else {
        lexer->errorCode = ERROR_EXPECTED_DIGIT_AFTER_UNDERSCORE;
        /* Consume and discard the invalid character. */
        consume(lexer);
    }
}

void octalIntegerLiteral(Lexer* lexer) {
    /* Consume and discard the octal prefix character. */
    consume(lexer);

    if (isOctalDigit(lexer->la1)) {
        /* Consume and discard the octal digit character. */
        consume(lexer);

        if (isOctalDigitOrUnderscore(lexer->la1)) {
            uint8_t previous = '\0';
            while (isOctalDigitOrUnderscore(lexer->la1)) {
                previous = lexer->la1;

                /* Consume and discard a octal digit or an underscore
                    * character.
                    */
                consume(lexer);
            }

            if (previous == '_') {
                lexer->errorCode = ERROR_EXPECTED_DIGIT_AFTER_UNDERSCORE;
                /* Consume and discard the invalid character. */
                consume(lexer);
            }
        }
    }
    else {
        lexer->errorCode = ERROR_EXPECTED_DIGIT_AFTER_UNDERSCORE;
        /* Consume and discard the invalid character. */
        consume(lexer);
    }
}

void hexadecimalIntegerLiteral(Lexer* lexer) {
    /* Consume and discard the binary prefix character. */
    consume(lexer);

    if (isHexadecimalDigit(lexer->la1)) {
        /* Consume and discard the hexadecimal digit character. */
        consume(lexer);

        if (isHexadecimalDigitOrUnderscore(lexer->la1)) {
            uint8_t previous = '\0';
            while (isHexadecimalDigitOrUnderscore(lexer->la1)) {
                previous = lexer->la1;

                /* Consume and discard a binary digit or an underscore
                    * character.
                    */
                consume(lexer);
            }

            if (previous == '_') {
                lexer->errorCode = ERROR_EXPECTED_DIGIT_AFTER_UNDERSCORE;
                /* Consume and discard the invalid character. */
                consume(lexer);
            }
        }
    }
    else {
        lexer->errorCode = ERROR_EXPECTED_DIGIT_AFTER_UNDERSCORE;
        /* Consume and discard the invalid character. */
        consume(lexer);
    }
}

void decimalIntegerLiteral(Lexer* lexer) {
    /* Consume and discard the decimal digit character. */
    consume(lexer);

    if (isDecimalDigit(lexer->la1)) {
        /* Consume and discard the decimal digit character. */
        consume(lexer);

        if (isDecimalDigitOrUnderscore(lexer->la1)) {
            uint8_t previous = '\0';
            while (isDecimalDigitOrUnderscore(lexer->la1)) {
                previous = lexer->la1;

                /* Consume and discard the decimal digit or underscore
                    * character.
                    */
                consume(lexer);
            }

            if (previous == '_') {
                lexer->errorCode = ERROR_EXPECTED_DIGIT_AFTER_UNDERSCORE;
                /* Consume and discard the invalid character. */
                consume(lexer);
            }
        }
    }
    else if (lexer->la1 == '_') {
        do {
            /* Consume and discard the '_' character. */
            consume(lexer);
        }
        while (lexer->la1 == '_');

        if (isDecimalDigitOrUnderscore(lexer->la1)) {
            uint8_t previous = '\0';
            while (isDecimalDigitOrUnderscore(lexer->la1)) {
                previous = lexer->la1;

                /* Consume and discard the decimal digit or underscore
                    * character.
                    */
                consume(lexer);
            }

            if (previous == '_') {
                lexer->errorCode = ERROR_EXPECTED_DIGIT_AFTER_UNDERSCORE;
                /* Consume and discard the invalid character. */
                consume(lexer);
            }
        }
        else {
            lexer->errorCode = ERROR_EXPECTED_DIGIT_AFTER_UNDERSCORE;
            /* Consume and discard the invalid character. */
            consume(lexer);
        }
    }
}

/* Here is the simplified grammar which allows underscore characters at the
 * end of the integer literal. This grammar has simplified the decimal integer
 * literal rule, too. This type of integer literals are the last to be recognized,
 * given the order in which they are written. This allows us to implement
 * the longest-match-wins behaviour, the same technique that helps us differentiate
 * keywords and identifiers with similar prefixes.
 *
 * PS: I am not sure what the "longest-match-wins behaviour" is actually called. :")
 *
 * IntegerLiteral
 * : BinaryIntegerLiteral
 * | OctalIntegerLiteral
 * | HexadecimalIntegerLiteral
 * | DecimalIntegerLiteral
 * ;
 *
 * BinaryIntegerLiteral
 * :    '0' [bB] BinaryNumeral IntegerTypeSuffix?
 * ;
 *
 * DecimalIntegerLiteral
 * :    DecimalNumeral IntegerTypeSuffix?
 * ;
 *
 * OctalIntegerLiteral
 * :    '0' [cC] OctalNumeral IntegerTypeSuffix?
 * ;
 *
 * HexadecimalIntegerLiteral
 * :    '0' [xX] HexadecimalNumeral IntegerTypeSuffix?
 * ;
 *
 * IntegerTypeSuffix
 * :    [lL]
 * ;
 *
 * -----------------------------------------------------
 *
 * BinaryNumeral
 * :    BinaryDigit BinaryDigitOrUnderscore*
 * ;
 *
 * BinaryDigitOrUnderscore
 * :    BinaryDigit
 * |    '_'
 * ;
 *
 * BinaryDigit
 * :    [01]
 * ;
 *
 * -----------------------------------------------------
 *
 * OctalNumeral
 * :    OctalDigit OctalDigitOrUnderscore*
 * ;
 *
 * OctalDigit
 * :    [0-7]
 * ;
 *
 * OctalDigitOrUnderscore
 * :    OctalDigit
 *  |    '_'
 * ;
 *
 * -----------------------------------------------------
 *
 * HexadecimalNumeral
 * :    HexadecimalDigit HexadecimalDigitOrUnderscore*
 * ;
 *
 * HexadecimalDigit
 * :    [0-9a-f-A-F]
 * ;
 *
 * HexadecimalDigitOrUnderscore
 * :    HexadecimalDigit
 * |    '_'
 * ;
 *
 * -----------------------------------------------------
 *
 * DecimalNumeral
 * :    DecimalDigit DecimalDigitOrUnderscore*
 * ;
 *
 * DecimalDigit
 * :    [0-9]
 * ;
 *
 * DecimalDigitOrUnderscore
 * :    DecimalDigit
 * |    '_'
 * ;
 *
 */
void integerLiteral(Lexer* lexer) {
    /* The lexer has recognized an integer literal. */
    lexer->type = TOKEN_INTEGER_LITERAL;

    if (lexer->la1 == '0') {
        /* Consume and discard the '0' character. */
        consume(lexer);

        if (isBinaryPrefix(lexer->la1)) {
            binaryIntegerLiteral(lexer);
        }
        else if (isOctalPrefix(lexer->la1)) {
            octalIntegerLiteral(lexer);
        }
        else if (isHexadecimalPrefix(lexer->la1)) {
            hexadecimalIntegerLiteral(lexer);
        }
        else if (isDecimalDigit(lexer->la1) || lexer->la1 == '_') {
            decimalIntegerLiteral(lexer);
        }
        else if (isLetter(lexer->la1)) {
            lexer->errorCode = ERROR_INVALID_INTEGER_LITERAL_PREFIX;
        }
    }
    else {
        decimalIntegerLiteral(lexer);
    }
}

/*
 * ALGORITHM
 * ---------
 *
 * Check for a buffered token. If found, remove it from the buffer
 * and return it to the user.
 *
 * The lexer checks if an end of stream was encountered within
 * a block, indicated when the end of stream character is encountered
 * when the indentation depth is greater than 0. If so, a newline
 * token is emitted. This arbitrary token serves as the end of
 * a statement. After which, the lexer emits dedentation tokens
 * as needed. In order to retrieve all the tokens generated in the
 * above steps multiple calls to nextToken() are required. Therefore,
 * the lexer buffers up tokens. The buffered tokens are appropriately
 * produced before the user as described previously.
 *
 * A single lexical recognition may result in multiple errors. For example,
 * when recognizing a string literal we may encounter a number of malformed
 * escape sequences and an unexpected end of stream.
 *
 * When the lambda operator is encountered, the lexer marks a flag that indicates
 * the encouter. If the lexer encounters a left brace after such an encounter,
 * a flag which allows indentation is pushed onto the stack. When the lexer encouters
 * a left brace without encountering the lambda operator, a flag which disallows
 * indentation is pushed onto the stack. When the right brace is encountered, the
 * stack is popped. Thus, the compiler recognizes the following function
 * as valid.
 *
 * function main()
 *     var array = [
 *         'Samuel Rowe',
 *         'Nikola Tesla',
 *         'Bill Gates',
 *         'Isaac Newton',
 *         'Keanu Reeves'
 *     ]
 *     array.forEach(lambda (item, index) {
 *             if index.isEven()
 *                 print('[even] ' + index + '->' + item)
 *             else
 *                 print('[odd] ' + index + '->' + item)
 *         }
 *     )
 */
Token* nextToken(Lexer* lexer) {
    jtk_Assert_assertObject(lexer, "The specified lexer is null.");

    Compiler* compiler = lexer->compiler;
    const char* file = jtk_ArrayList_getValue(compiler->inputFiles,
                       compiler->currentFileIndex);

    /* The lexer does not bother to recognize a token
     * from the input stream unless necessary.
     */
    if (jtk_ArrayQueue_isEmpty(lexer->tokens)) {
        /* We don't exit the loop until
         * -- We have a token.
         * -- We have reached the end of the stream.
         * -- We have encountered an error. (Interestingly, this condition
         *    is not explicitly checked because errorneous token recognition
         *    too generate tokens!)
         */
        loopEntry: {
            lexer->token = NULL;
            lexer->type = TOKEN_UNKNOWN;
            jtk_StringBuilder_clear(lexer->text);
            lexer->channel = TOKEN_CHANNEL_DEFAULT;
            lexer->startIndex = lexer->index;
            lexer->startLine = lexer->line;
            lexer->startColumn = lexer->column;
            lexer->errorCode = ERROR_NONE;


            switch (lexer->la1) {
            case KUSH_END_OF_STREAM : {
                /* The data required for the creating the end-of-stream token.
                 */
                lexer->type = TOKEN_END_OF_STREAM;
                lexer->hitEndOfStream = true;
                break;
            }

            case ' '  : {
                if (lexer->la1 == ' ') {
                    do {
                        consume(lexer);
                    }
                    while (lexer->la1 == ' ');

                    /* This token belongs to the WHITESPACE rule. */
                    lexer->type = TOKEN_WHITESPACE;
                    lexer->channel = TOKEN_CHANNEL_HIDDEN;
                }
                break;
            }

            case '\r' :
            case '\n' : {
                if (lexer->la1 == '\r') {
                    consume(lexer);
                    /* Optionally, the carriage return character may be
                        * followed by a newline character.
                        */
                    if (lexer->la1 == '\n') {
                        consume(lexer);

                        /* Update information such as the current line,
                            * current column, etc.
                            */
                        onNewLine(lexer);
                    }
                }
                else {
                    consume(lexer);

                    /* Update information such as the current line,
                        * current column, etc.
                        */
                    onNewLine(lexer);
                }

                /*
                 * NOTE: The lexer is creating a custom token here.
                 *       Therefore, we directly invoke newToken().
                 */
                Token* newlineToken = newToken(
                    TOKEN_CHANNEL_HIDDEN,
                    TOKEN_NEWLINE,
                    "\n",
                    1,
                    lexer->startIndex,    /* inclusive */
                    lexer->index,         /* exclusive */
                    lexer->startLine,     /* inclusive */
                    lexer->line,          /* inclusive */
                    lexer->startColumn,   /* inclusive */
                    lexer->column,        /* inclusive */
                    file
                );
                emit(lexer, newlineToken);
                /* The rule action has taken care of generating
                 * tokens. The lexer can confidently skip any other
                 * token producing operations.
                 */
                goto loopEntry;
            }

            /* EXCLAMATION_MARK_EQUAL
             * :    '!='
             * ;
             *
             * EXCLAMATION_MARK
             * :    '!'
             * ;
             */
            case '!' : {
                consume(lexer);

                if (lexer->la1 == '=') {
                    consume(lexer);
                    lexer->type = TOKEN_EXCLAMATION_MARK_EQUAL;
                }
                else {
                    lexer->type = TOKEN_EXCLAMATION_MARK;
                }

                break;
            }

            /* AT
             * :    '@'
             * ;
             */
            case '@' : {
                /* Consume and discard the '@' character. */
                consume(lexer);
                /* The lexer has recognized the '@' token. */
                lexer->type = TOKEN_AT;
                break;
            }

            /* HASH
             * :    '#'
             * ;
             */
            case '#' : {
                /* Consume and discard the '#' character. */
                consume(lexer);
                /* The lexer has recognized the '#' token. */
                lexer->type = TOKEN_HASH;

                break;
            }

            /* MODULUS_EQUAL
             * :    '%='
             * ;
             *
             * MODULUS
             * :    '%'
             * ;
             */
            case '%' : {
                consume(lexer);

                if (lexer->la1 == '=') {
                    consume(lexer);
                    lexer->type = TOKEN_MODULUS_EQUAL;
                }
                else {
                    lexer->type = TOKEN_MODULUS;
                }

                break;
            }

            /* AMPERSAND_2
             * :    '&&'
             * ;
             *
             * AMPERSAND_EQUAL
             * :    '&='
             * ;
             *
             * AMPERSAND
             * :    '&'
             * ;
             */
            case '&' : {
                consume(lexer);

                if (lexer->la1 == '&') {
                    consume(lexer);
                    lexer->type = TOKEN_AMPERSAND_2;
                }
                else if (lexer->la1 == '=') {
                    consume(lexer);
                    lexer->type = TOKEN_AMPERSAND_EQUAL;
                }
                else {
                    lexer->type = TOKEN_AMPERSAND;
                }

                break;
            }

            /* LEFT_PARENTHESIS
             * :    '('
             * ;
             */
            case '(' : {
                /* Consume and discard the '(' character. */
                consume(lexer);
                /* The lexer has recognized the '(' token. */
                lexer->type = TOKEN_LEFT_PARENTHESIS;
                break;
            }

            /* RIGHT_PARENTHESIS
             * :    ')'
             * ;
             */
            case ')' : {
                /* Consume and discard the '(' character. */
                consume(lexer);
                /* The lexer has recognized the '(' token. */
                lexer->type = TOKEN_RIGHT_PARENTHESIS;
                break;
            }

            /* ASTERISK_2_EQUAL
             * :    '**='
             * ;
             *
             * ASTERISK_2
             * :    '**'
             * ;
             *
             * ASTERISK_EQUAL
             * :    '*='
             * ;
             *
             * ASTERISK
             * :    '*'
             * ;
             */
            case '*' : {
                /* Consume and discard the '*' character. */
                consume(lexer);

                if (lexer->la1 == '=') {
                    /* Consume and discard the '=' character. */
                    consume(lexer);
                    /* The lexer has recognized the '*=' token. */
                    lexer->type = TOKEN_ASTERISK_EQUAL;
                }
                else {
                    lexer->type = TOKEN_ASTERISK;
                }

                break;
            }

            /* PLUS_2
             * :    '++'
             * ;
             *
             * PLUS_EQUAL
             * :    '+='
             * ;
             *
             * PLUS
             * :    '+'
             * ;
             */
            case '+' : {
                /* Consume and discard the '+' character. */
                consume(lexer);

                if (lexer->la1 == '+') {
                    /* Consume and discard the '+' character. */
                    consume(lexer);
                    /* The lexer has recognized the '++' token. */
                    lexer->type = TOKEN_PLUS_2;
                }
                else if (lexer->la1 == '=') {
                    /* Consume and discard the '=' character. */
                    consume(lexer);
                    /* The lexer has recognized the '+=' token. */
                    lexer->type = TOKEN_PLUS_EQUAL;
                }
                else {
                    lexer->type = TOKEN_PLUS;
                }

                break;
            }

            /* COMMA
             * :    ','
             * ;
             */
            case ',' : {
                /* Consume and discard the ',' character. */
                consume(lexer);
                /* The lexer has recognized the ',' token. */
                lexer->type = TOKEN_COMMA;
                break;
            }

            /* DASH_2
             * :    '--'
             * ;
             *
             * ARROW
             * :    '->'
             * ;
             *
             * DASH_EQUAL
             * :    '-='
             * ;
             *
             * DASH
             * :    '-'
             * ;
             */
            case '-' : {
                /* Consume and discard the '-' character. */
                consume(lexer);

                if (lexer->la1 == '-') {
                    /* Consume and discard the '-' character. */
                    consume(lexer);
                    /* The lexer has recognized the '--' token. */
                    lexer->type = TOKEN_DASH_2;
                }
                else if (lexer->la1 == '>') {
                    /* Consume and discard the '>' character. */
                    consume(lexer);
                    /* The lexer has recognized the '->' token. */
                    lexer->type = TOKEN_ARROW;
                }
                else if (lexer->la1 == '=') {
                    /* Consume and discard the '=' character. */
                    consume(lexer);
                    /* The lexer has recognized the '-=' token. */
                    lexer->type = TOKEN_DASH_EQUAL;
                }
                else {
                    /* The lexer has recognized the '-' token. */
                    lexer->type = TOKEN_DASH;
                }

                break;
            }

            /* ELLIPSIS
             * :    '...'
             * ;
             *
             * DOT_2
             * :    '..'
             * ;
             *
             * DOT
             * :    '.'
             * ;
             */
            case '.' : {
                /* Consume and discard the '.' character. */
                consume(lexer);

                if (lexer->la1 == '.') {
                    /* Consume and discard the '.' character. */
                    consume(lexer);

                    if (lexer->la1 == '.') {
                        /* Consume and discard the '.' character. */
                        consume(lexer);
                        /* The lexer has recognized the '...' token. */
                        lexer->type = TOKEN_ELLIPSIS;
                    }
                    else {
                        printf("[internal error] Two consecutive dots cannot be recognized right now. Please consider using spacing.\n");
                    }
                }
                else {
                    /* The lexer has recognized the '.' token. */
                    lexer->type = TOKEN_DOT;
                }

                break;
            }

            /* SINGLE_LINE_COMMENT
             * :    '//' ~[\r\n]* -> channel(hidden)
             * ;
             *
             * MULTI_LINE_COMMENT
             * :    '/*' .*? '*''/' -> channel(hidden)
             * ;
             *
             * FORWARD_SLASH_EQUAL
             * :    '/='
             * ;
             *
             * FORWARD_SLASH
             * :    '/'
             * ;
             */
            case '/' : {
                /* Consume and discard the '/' character. */
                consume(lexer);

                if (lexer->la1 == '/') {
                    /* Consume and discard the '/' character. */
                    consume(lexer);

                    while ((lexer->la1 != '\n') && (lexer->la1 != KUSH_END_OF_STREAM)) {
                        /* Consume and discard the unknown character. */
                        consume(lexer);
                    }

                    /* We consume the terminating character, regardless
                     * of it being newline or end-of-stream.
                     */
                    consume(lexer);

                    /* Update information such as the current line,
                     * current column, etc.
                     */
                    if (lexer->la1 == '\n') {
                        onNewLine(lexer);
                    }

                    /* The lexer has recognized a single line comment. */
                    lexer->type = TOKEN_SINGLE_LINE_COMMENT;
                    /* The single-line comment token should be produced on the
                     * hidden channel. Otherwise, the parser will have a hard
                     * time ignoring redundant single-line comment tokens.
                     */
                    lexer->channel = TOKEN_CHANNEL_HIDDEN;
                }
                else if (lexer->la1 == '*') {
                    /* Consume and discard the '*' token. */
                    consume(lexer);

                    do {
                        while (lexer->la1 != '*') {
                            if (lexer->la1 == KUSH_END_OF_STREAM) {
                                lexer->errorCode = ERROR_UNTERMINATED_MULTI_LINE_COMMENT;
                                break;
                            }

                            /* Update information such as the current line,
                             * current column, etc.
                             */
                            if (lexer->la1 == '\n') {
                                onNewLine(lexer);
                            }

                            /* Consume and discard the unknown character. */
                            consume(lexer);
                        }

                        /* The following condition is checked to exit the
                         * outer loop. We do not have to report the error.
                         * Because the error was reported in the inner loop.
                         */
                        if (lexer->la1 == KUSH_END_OF_STREAM) {
                            break;
                        }

                        /* Here, we are currently processing the '*' character.
                         * Therefore, we consume it.
                         */
                        consume(lexer);
                    }
                    while (lexer->la1 != '/');

                    if (lexer->la1 == '/') {
                        /* At this point, we are indeed processing the '/'
                         * character. Therefore, we consume it.
                         */
                        consume(lexer);
                    }
                    /*
                    else {
                        ... UNTERMINATED COMMENT ...
                    }
                    */

                    /* The lexer has recognized the multi-line comment. */
                    lexer->type = TOKEN_MULTI_LINE_COMMENT;
                    /* The multi-line comment token should be produced on the
                     * hidden channel. Otherwise, the parser will have a hard
                     * time ignoring redundant multi-line comment tokens.
                     */
                    lexer->channel = TOKEN_CHANNEL_HIDDEN;
                }
                else if (lexer->la1 == '=') {
                    /* Consume and discard the '=' character. */
                    consume(lexer);
                    /* The lexer has recognized the '/=' token. */
                    lexer->type = TOKEN_FORWARD_SLASH_EQUAL;
                }
                else {
                    /* The lexer has recognized the '/' token. */
                    lexer->type = TOKEN_FORWARD_SLASH;
                }

                break;
            }

            /*
             * COLON
             * :    ':'
             * ;
             */
            case ':' : {
                /* Consume and discard the ':' character. */
                consume(lexer);
                /* The lexer has recognized the ':' token. */
                lexer->type = TOKEN_COLON;

                break;
            }

            /* SEMICOLON
             * :    ';'
             * ;
             */
            case ';' : {
                /* Consume and discard the ';' character. */
                consume(lexer);
                /* The lexer has recognized the ';' token. */
                lexer->type = TOKEN_SEMICOLON;
                break;
            }

            /* LEFT_ANGLE_BRACKET_2_EQUAL
             * :    '<<='
             * ;
             *
             * LEFT_ANGLE_BRACKET_2
             * :    '<<'
             * ;
             *
             * LEFT_ANGLE_BRACKET_EQUAL
             * :    '<='
             * ;
             *
             * LEFT_ANGLE_BRACKET
             * :    '<'
             * ;
             */
            case '<' : {
                /* Consume and discard the '<' character. */
                consume(lexer);

                if (lexer->la1 == '<') {
                    /* Consume and discard the '<' character. */
                    consume(lexer);

                    if (lexer->la1 == '=') {
                        /* Consume and discard the '=' character. */
                        consume(lexer);
                        /* The lexer has recognized the '<<=' token. */
                        lexer->type = TOKEN_LEFT_ANGLE_BRACKET_2_EQUAL;
                    }
                    else {
                        /* The lexer has recognized the '<<' token. */
                        lexer->type = TOKEN_LEFT_ANGLE_BRACKET_2;
                    }
                }
                else if (lexer->la1 == '=') {
                    /* Consume and discard the '=' character. */
                    consume(lexer);
                    /* The lexer has recognized the '<=' token. */
                    lexer->type = TOKEN_LEFT_ANGLE_BRACKET_EQUAL;
                }
                else {
                    /* The lexer has recognized the '<' token. */
                    lexer->type = TOKEN_LEFT_ANGLE_BRACKET;
                }

                break;
            }

            /* RIGHT_ANGLE_BRACKET_2_EQUAL
             * :    '>>='
             * ;
             *
             * RIGHT_ANGLE_BRACKET_2
             * :    '>>'
             * ;
             *
             * RIGHT_ANGLE_BRACKET_EQUAL
             * :    '>='
             * ;
             *
             * RIGHT_ANGLE_BRACKET
             * :    '>'
             * ;
             */
            case '>' : {
                /* Consume and discard the '>' character. */
                consume(lexer);

                if (lexer->la1 == '>') {
                    /* Consume and discard the '>' character. */
                    consume(lexer);

                    if (lexer->la1 == '>') {
                        /* Consume and discard the '>' character. */
                        consume(lexer);
                    }
                    else if (lexer->la1 == '=') {
                        /* Consume and discard the '=' character. */
                        consume(lexer);
                        /* The lexer has recognized the '>>=' token. */
                        lexer->type = TOKEN_RIGHT_ANGLE_BRACKET_2_EQUAL;
                    }
                    else {
                        /* The lexer has recognized the '>>' token. */
                        lexer->type = TOKEN_RIGHT_ANGLE_BRACKET_2;
                    }
                }
                else if (lexer->la1 == '=') {
                    /* Consume and discard the '=' character. */
                    consume(lexer);
                    /* The lexer has recognized the '>=' token. */
                    lexer->type = TOKEN_RIGHT_ANGLE_BRACKET_EQUAL;
                }
                else {
                    /* The lexer has recognized the '>' token. */
                    lexer->type = TOKEN_RIGHT_ANGLE_BRACKET;
                }

                break;
            }

            /* EQUAL_2
             * :    '=='
             * ;
             *
             * EQUAL
             * :    '='
             * ;
             */
            case '=' : {
                /* Consume and discard the '=' character. */
                consume(lexer);

                if (lexer->la1 == '=') {
                    /* Consume and discard the '=' character. */
                    consume(lexer);
                    /* The lexer has recognized the '==' token. */
                    lexer->type = TOKEN_EQUAL_2;
                }
                else {
                    /* The lexer has recognized the '=' token. */
                    lexer->type = TOKEN_EQUAL;
                }

                break;
            }

            /* HOOK
             * :    '?'
             * ;
             */
            case '?' : {
                /* Consume and discard the '?' character. */
                consume(lexer);
                /* The lexer has recognized the '?' token. */
                lexer->type = TOKEN_HOOK;
                break;
            }

            /* LEFT_BRACE
             * :    '{'
             * ;
             */
            case '{' : {
                /* Consume and discard the '{' character. */
                consume(lexer);
                /* The lexer has recognized the '{' token. */
                lexer->type = TOKEN_LEFT_BRACE;
                break;
            }

            /* RIGHT_BRACE
             * :    '}'
             * ;
             */
            case '}' : {
                /* Consume and discard the '}' character. */
                consume(lexer);
                /* The lexer has recognized the '}' token. */
                lexer->type = TOKEN_RIGHT_BRACE;
                break;
            }

            /* LEFT_SQUARE_BRACKET
             * :    '['
             * ;
             */
            case '[' : {
                /* Consume and discard the '[' character. */
                consume(lexer);
                /* The lexer has recognized the '[' token. */
                lexer->type = TOKEN_LEFT_SQUARE_BRACKET;
                break;
            }

            /* RIGHT_SQUARE_BRACKET
             * :    ']'
             * ;
             */
            case ']' : {
                /* Consume and discard the ']' character. */
                consume(lexer);
                /* The lexer has recognized the ']' token. */
                lexer->type = TOKEN_RIGHT_SQUARE_BRACKET;
                break;
            }

            /* CARET_EQUAL
             * :    '^='
             * ;
             *
             * CARET
             * :    '^'
             * ;
             */
            case '^' : {
                /* Consume and discard the '^' character. */
                consume(lexer);

                if (lexer->la1 == '=') {
                    /* Consume and discard the '=' character. */
                    consume(lexer);
                    /* The lexer has recognized the '^=' token. */
                    lexer->type = TOKEN_CARET_EQUAL;
                }
                else {
                    /* The lexer has recognized the '^' token. */
                    lexer->type = TOKEN_CARET;
                }

                break;
            }

            /* VERTICAL_BAR_2
             * :    '||'
             * ;
             *
             * VERTICAL_BAR_EQUAL
             * :    '|='
             * ;
             *
             * VERTICAL_BAR
             * :    '|'
             * ;
             */
            case '|' : {
                /* Consume and discard the '|' character. */
                consume(lexer);

                if (lexer->la1 == '|') {
                    /* Consume and discard the '|' character. */
                    consume(lexer);
                    /* The lexer has recognized the '||' token. */
                    lexer->type = TOKEN_VERTICAL_BAR_2;
                }
                else if (lexer->la1 == '=') {
                    /* Consume and discard the '=' character. */
                    consume(lexer);
                    /* The lexer has recognized the '|=' token. */
                    lexer->type = TOKEN_VERTICAL_BAR_EQUAL;
                }
                else {
                    /* The lexer has recognized the '|' token. */
                    lexer->type = TOKEN_VERTICAL_BAR;
                }

                break;
            }

            /* TILDE_EQUAL
             * :    '~='
             * ;
             *
             * TILDE
             * :    '~'
             * ;
             */
            case '~' : {
                /* Consume and discard the '~' character. */
                consume(lexer);

                if (lexer->la1 == '=') {
                    /* Consume and discard the '=' character. */
                    consume(lexer);
                    /* The lexer has recognized the '~=' token. */
                    lexer->type = TOKEN_TILDE_EQUAL;
                }
                else {
                    /* The lexer has recognized the '~' token. */
                    lexer->type = TOKEN_TILDE;
                }

                break;
            }

            /* STRING_LITERAL
             * :    '"' STRING_CHARACTER* '"'
             * |    '\'' STRING_CHARACTER* '\''
             * ;
             *
             * STRING_CHARACTER
             * :    ~["'\\]
             * |    ESCAPE_SEQUENCE
             * ;
             *
             * ESCAPE_SEQUENCE
             * :    '\\' [btnfr"'\\]
             * |    '\\' 'u' HEXADECIMAL_DIGIT HEXADECIMAL_DIGIT HEXADECIMAL_DIGIT HEXADECIMAL_DIGIT
             * ;
             *
             * HEXADECIMAL_DIGIT
             * :    [0-9a-fA-F]
             * ;
             */
            case '"':
            case '\'' : {
                uint8_t terminator = lexer->la1;

                /* Consume and discard the character which marks the
                 * beginning of this string.
                 */
                consume(lexer);

                while (lexer->la1 != terminator) {
                    if ((lexer->la1 == KUSH_END_OF_STREAM) || (lexer->la1 == '\n')) {
                        lexer->errorCode = ERROR_UNTERMINATED_STRING_LITERAL;
                        break;
                    }
                    else if (lexer->la1 == '\\') {
                        /* Consume and discard the '\' character. */
                        consume(lexer);

                        if (isBasicEscapeSequence(lexer->la1)) {
                            /* Consume and discard the character which represents
                             * a basic escape sequence.
                             */
                            consume(lexer);
                        }
                        else if (lexer->la1 == 'u') {
                            /* Consume and discard the 'u' character. */
                            consume(lexer);

                            int32_t i;
                            for (i = 0; i < 4; i++) {
                                if (isHexadecimalDigit(lexer->la1)) {
                                    /* Consume and discard the hexadecimal digit character. */
                                    consume(lexer);
                                }
                                else {
                                    lexer->errorCode = ERROR_MALFORMED_UNICODE_CHARACTER_SEQUENCE;
                                    break;
                                }
                            }
                        }
                        else {
                            lexer->errorCode = ERROR_INVALID_ESCAPE_SEQUENCE;

                            /* Consume and discard the unknown escape sequence. */
                            consume(lexer);
                        }
                    }
                    else {
                        /* Consume and discard a character in the string literal. */
                        consume(lexer);
                    }
                }

                if (lexer->la1 == terminator) {
                    /* At this point, we are processing the terminating
                     * double or single quote character. Therefore,
                     * consume and discard it.
                     */
                    consume(lexer);
                }
                else {
                    /* Most likely, we encountered an immature end of line or stream. */
                }

                /* The lexer has recognized a string literal. */
                lexer->type = TOKEN_STRING_LITERAL;

                break;
            }

            default : {

                /* IDENTIFIER
                 * :    LETTER LETTER_OR_DIGIT*
                 * ;
                 */
                if (isIdentifierStart(lexer->la1)) {
                    /* Consume and discard the first letter. */
                    consume(lexer);


                    while (isIdentifierPart(lexer->la1)) {
                        /* Consume and discard the consecutive letter
                         * or digit character.
                         */
                        consume(lexer);
                    }

                    if (lexer->la1 == ':') {
                        consume(lexer);

                        if (isIdentifierStart(lexer->la1)) {
                            do {
                                consume(lexer);
                            }
                            while (isIdentifierPart(lexer->la1));
                        }
                        else {
                            lexer->errorCode = ERROR_EXPECTED_LETTER_AFTER_COLON;
                            consume(lexer);
                        }
                    }
                    uint8_t* text = lexer->text->m_value; // jtk_StringBuilder_toCString(lexer->text);
                    int32_t length = lexer->text->m_size; // lexer->index - lexer->startIndex;

                    /* TODO: Find a better solution. Given we have access to a sorted
                     * list of keywords, a good idea would be to implement a binary
                     * search here.
                     *
                     * Another solution, would be to use a hash map with token
                     * text as key and token type as values. This is obviously
                     * more efficient than the current algorithm. Unfortunately,
                     * I neither have patience nor time.
                     *
                     * The following algorithm tries to prevent redundant comparisons
                     * between strings (as many as possible).
                     */
                    lexer->type = TOKEN_IDENTIFIER;
                    switch (text[0]) {
                    case 'b' : {
                        if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_BOOLEAN], 7)) {
                            lexer->type = TOKEN_KEYWORD_BOOLEAN;
                        }
                        else if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_BREAK], 5)) {
                            lexer->type = TOKEN_KEYWORD_BREAK;
                        }
                        break;
                    }

                    case 'c': {
                        if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_CATCH], 5)) {
                            lexer->type = TOKEN_KEYWORD_CATCH;
                        }
                        break;
                    }

                    case 'e' : {
                        if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_ELSE], 4)) {
                            lexer->type = TOKEN_KEYWORD_ELSE;
                        }
                        break;
                    }

                    case 'f' : {
                        if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_F32], 3)) {
                            lexer->type = TOKEN_KEYWORD_F32;
                        }
                        else if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_F64], 3)) {
                            lexer->type = TOKEN_KEYWORD_F64;
                        }
                        else if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_FALSE], 5)) {
                            lexer->type = TOKEN_KEYWORD_FALSE;
                        }
                        else if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_FINALLY], 7)) {
                            lexer->type = TOKEN_KEYWORD_FINALLY;
                        }
                        else if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_FOR], 3)) {
                            lexer->type = TOKEN_KEYWORD_FOR;
                        }
                        break;
                    }

                    case 'i' : {
                        if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_I16], 3)) {
                            lexer->type = TOKEN_KEYWORD_I16;
                        }
                        else if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_I32], 3)) {
                            lexer->type = TOKEN_KEYWORD_I32;
                        }
                        else if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_I64], 3)) {
                            lexer->type = TOKEN_KEYWORD_I64;
                        }
                        else if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_I8], 2)) {
                            lexer->type = TOKEN_KEYWORD_I8;
                        }
                        else if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_IF], 2)) {
                            lexer->type = TOKEN_KEYWORD_IF;
                        }
                        else if (jtk_CString_equals(text, length, tokenNames[TOKEN_KEYWORD_IMPORT], 6)) {
                            lexer->type = TOKEN_KEYWORD_IMPORT;
                        }
                        break;
                    }

                    case 'l' : {
                        if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_LET], 3)) {
                            lexer->type = TOKEN_KEYWORD_LET;
                        }
                        break;
                    }
                    case 'n' : {
                        if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_NATIVE], 6)) {
                            lexer->type = TOKEN_KEYWORD_NATIVE;
                        }
                        else if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_NEW], 3)) {
                            lexer->type = TOKEN_KEYWORD_NEW;
                        }
                        else if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_NULL], 4)) {
                            lexer->type = TOKEN_KEYWORD_NULL;
                        }
                        break;
                    }
                    case 'r' : {
                        if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_RETURN], 6)) {
                            lexer->type = TOKEN_KEYWORD_RETURN;
                        }
                        break;
                    }
                    case 's' : {
                        if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_STRUCT], 6)) {
                            lexer->type = TOKEN_KEYWORD_STRUCT;
                        }
                        else if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_STRING], 6)) {
                            lexer->type = TOKEN_KEYWORD_STRING;
                        }
                        break;
                    }
                    case 't' : {
                        if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_THIS], 4)) {
                            lexer->type = TOKEN_KEYWORD_THIS;
                        }
                        else if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_THROW], 5)) {
                            lexer->type = TOKEN_KEYWORD_THROW;
                        }
                        else if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_TRUE], 4)) {
                            lexer->type = TOKEN_KEYWORD_TRUE;
                        }
                        else if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_TRY], 3)) {
                            lexer->type = TOKEN_KEYWORD_TRY;
                        }
                        break;
                    }
                    case 'u' : {
												if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_UI16], 4)) {
                            lexer->type = TOKEN_KEYWORD_UI16;
                        }
                        else if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_UI32], 4)) {
                            lexer->type = TOKEN_KEYWORD_UI32;
                        }
                        else if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_UI64], 4)) {
                            lexer->type = TOKEN_KEYWORD_UI64;
                        }
                        else if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_UI8], 3)) {
                            lexer->type = TOKEN_KEYWORD_UI8;
                        }
                        break;

										}
                    case 'v' : {
                        if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_VAR], 3)) {
                            lexer->type = TOKEN_KEYWORD_VAR;
                        }
                        else if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_VOID], 4)) {
                            lexer->type = TOKEN_KEYWORD_VOID;
                        }
                        break;
                    }
                    case 'w' : {
                        if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_WITH], 4)) {
                            lexer->type = TOKEN_KEYWORD_WITH;
                        }
                        else if (jtk_CString_equals(text, length, tokenNames[(int32_t)TOKEN_KEYWORD_WHILE], 5)) {
                            lexer->type = TOKEN_KEYWORD_WHILE;
                        }
                        break;
                    }
                    }

                    /* Destroy the text; not required anymore. */
                    // jtk_CString_delete(text);
                }
                else if (isDecimalDigit(lexer->la1)) {
                    integerLiteral(lexer);

                    /* Check for integer type suffix. */

                    /* All integers in KUSH occupy 64-bits. */
                    // if (isIntegerSuffix(lexer->la1)) {
                    /* Consume and discard the integer suffix character. */
                    // consume(lexer);
                    // }
                    // else if (isLetter(lexer->la1)) {
                    /* Consume and discard the invalid suffix. */
                    // consume(lexer);

                    // printf("[error] Invalid integer literal suffix\n");
                    // }
                }
                else {
                    lexer->errorCode = ERROR_UNKNOWN_CHARACTER;

                    /* Consume and discard the unknown character. */
                    consume(lexer);
                    /* The lexer has encountered an unrecognized character. */
                    lexer->type = TOKEN_UNKNOWN;
                }
                break;
            }
            }
        }

        Token* newToken = createToken(lexer);
        emit(lexer, newToken);

        /* Unlike the parser, the lexer does not support error recovery strategies.
         * Therefore, all types of errors are collectively recorded at this point.
         */
        if (lexer->errorCode != ERROR_NONE) {
            handleLexicalError(lexer->compiler->errorHandler,
                                              lexer, lexer->errorCode, newToken);
        }
    }

    Token* next = (Token*)jtk_ArrayQueue_peek(lexer->tokens);
    jtk_ArrayQueue_dequeue(lexer->tokens);
    return next;
}

// Reset

void resetLexer(Lexer* lexer, jtk_InputStream_t* inputStream) {
    jtk_Assert_assertObject(lexer, "The specified lexer is null.");
    jtk_Assert_assertObject(inputStream, "The specified input stream is null.");

    destroyStaleTokens(lexer);

    lexer->inputStream = inputStream;
    lexer->la1 = 0;
    lexer->index = -1;
    lexer->line = 1;
    lexer->column = -1;
    lexer->startIndex = 0;
    lexer->startLine = 0;
    lexer->startColumn = 0;
    lexer->hitEndOfStream = false;
    lexer->token = NULL;
    lexer->channel = TOKEN_CHANNEL_DEFAULT;
    lexer->type = TOKEN_UNKNOWN;
    lexer->errorCode = ERROR_NONE;

    jtk_StringBuilder_clear(lexer->text);
    jtk_ArrayQueue_clear(lexer->tokens);

    consume(lexer);
}
