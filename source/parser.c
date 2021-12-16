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
#include <jtk/collection/Pair.h>
#include <jtk/collection/array/Arrays.h>
#include <kush/parser.h>

/*
 * The parser analyzes the syntactic structure of the input token sequence.
 * Here, we implement an LL(k) recursive-descent parser. As of this writing,
 * the parser needs to lookahead 3 tokens at maximum to choose certain rules,
 * making k equal 3.
 *
 * -----------------------------------------------------------------------------
 * How is the abstract syntax tree (AST) constructed?
 * -----------------------------------------------------------------------------
 *
 * Each rule allocates a context of a specific type, which forms an AST node.
 * A context has specific data related for a given rule. It is recognized by
 * the `tag` attribute. All contexts can be casted to the `Context`
 * structure and vice versa.
 *
 * -----------------------------------------------------------------------------
 * How are contexts and rule specific data destroyed?
 * -----------------------------------------------------------------------------
 *
 * Every rule is represented by a context. Any object allocated within a rule
 * should be immediately attached to the context. The function destroyAST()
 * should be invoked on the root context, which is usually an instance
 * of the Module structure. However, destroyAST() is perfectly capable of
 * handling other contexts as roots, too. It implements the visitor pattern
 * and traverses down the AST destroying each node it encounters and all its
 * descendants.
 *
 * -----------------------------------------------------------------------------
 * How does the parser recover from syntax errors?
 * -----------------------------------------------------------------------------
 *
 * When the parser encounters an invalid input, the current rule cannot continue,
 * so the parser recovers by skipping tokens until a resynchronized state is
 * achived. The control is then returned to the calling rule.
 * This technique is known as the panic mode strategy.
 *
 * The trick here is to discard tokens only until the lookahead token is
 * something that the parent rule of the current rule expects. For example,
 * if there is a syntax error within a throw statement, the parser discards
 * tokens until a semicolon token or other relevant token is encountered.
 *
 * When the parser encounters an error, it switches to the recovery mode.
 * This prevents the parser from reporting errors during resynchronization.
 * The parser remains in the recovery mode until it encounters an expected
 * token.
 */

/*******************************************************************************
 * Parser                                                                      *
 *******************************************************************************/

// Recover

static void recover(Parser* parser);
static void reportAndRecover(Parser* parser, TokenType expected);
static bool ensureFollowSetSpace(Parser* parser, int32_t capacity);
static void pushFollowToken(Parser* parser, TokenType type);
static void popFollowToken(Parser* parser);

// Consume

static Token* consumeAndYield(Parser* parser);

// Match

static int32_t matchEx(Parser* parser, TokenType* types, int32_t count);
static Token* matchAndYieldEx(Parser* parser, const TokenType* types, int32_t count,
    int32_t* index);
static Token* matchAndYield(Parser* parser, TokenType type);

// Rules

static bool followVariableDeclaration(Parser* parser);

// Rules

static Module* parseModule(Parser* parser);
static ImportDeclaration* parseImportDeclaration(Parser* parser);
static VariableType* parseTypeEx(Parser* parser, bool includeVoid);
static VariableType* parseType(Parser* parser);
static VariableType* parseReturnType(Parser* parser);
static Function* parseFunctionDeclaration(Parser* parser, uint32_t modifiers);
static void parseFunctionParameters(Parser* parser, jtk_ArrayList_t* fixedParameters,
    Variable** variableParameter);
static Block* parseBlock(Parser* parser);
static Context* parseSimpleStatement(Parser* parser);
static VariableDeclaration* parseVariableDeclaration(Parser* parser, bool onlyVariable,
    bool allowInitializer);
void parseVariableDeclarator(Parser* parser, bool infer, bool constant,
    VariableType* variableType, jtk_ArrayList_t* variables);
static BreakStatement* parseBreakStatement(Parser* parser);
static ReturnStatement* parseReturnStatement(Parser* parser);
static ThrowStatement* parseThrowStatement(Parser* parser);
static Context* parseCompoundStatement(Parser* parser);
static IfStatement* parseIfStatement(Parser* parser);
static IfClause* parseIfClause(Parser* parser);
static IfClause* parseElseIfClause(Parser* parser);
static IterativeStatement* parseIterativeStatement(Parser* parser);
static TryStatement* parseTryStatement(Parser* parser);
static CatchClause* parseCatchClause(Parser* parser);
static Structure* parseStructureDeclaration(Parser* parser);
static jtk_ArrayList_t* parseExpressions(Parser* parser);
static BinaryExpression* parseExpression(Parser* parser);
static BinaryExpression* parseAssignmentExpression(Parser* parser);
static ConditionalExpression* parseConditionalExpression(Parser* parser);
static BinaryExpression* parseLogicalOrExpression(Parser* parser);
static BinaryExpression* parseLogicalAndExpression(Parser* parser);
static BinaryExpression* parseInclusiveOrExpression(Parser* parser);
static BinaryExpression* parseExclusiveOrExpression(Parser* parser);
static BinaryExpression* parseAndExpression(Parser* parser);
static BinaryExpression* parseEqualityExpression(Parser* parser);
static BinaryExpression* parseRelationalExpression(Parser* parser);
static BinaryExpression* parseShiftExpression(Parser* parser);
static BinaryExpression* parseAdditiveExpression(Parser* parser);
static BinaryExpression* parseMultiplicativeExpression(Parser* parser);
static UnaryExpression* parseUnaryExpression(Parser* parser);
static PostfixExpression* parsePostfixExpression(Parser* parser);
static FunctionArguments* parseFunctionArguments(Parser* parser);
static Subscript* parseSubscript(Parser* parser);
static MemberAccess* parseMemberAccess(Parser* parser);
static void* parsePrimaryExpression(Parser* parser, bool* token);
static NewExpression* parseNewExpression(Parser* parser);
static jtk_Pair_t* parseInitializerEntry(Parser* parser);
static ArrayExpression* parseArrayExpression(Parser* parser);

// static const char* ruleNames[] = {
//     "module",
//     "importDeclaration",
//     "functionDeclaration",
//     "block",
//     "variableDeclaration",
//     "breakStatement",
//     "returnStatement",
//     "throwStatement",
//     "ifStatement",
//     "iterativeStatement",
//     "tryStatement",
//     "structureDeclaration",
//     "assignmentExpression",
//     "conditionalExpression",
//     "logicalOrExpression",
//     "logicalAndExpression",
//     "inclusiveOrExpression",
//     "exclusiveOrExpression",
//     "andExpression",
//     "equalityExpression",
//     "relationalExpression",
//     "shiftExpression",
//     "additiveExpression",
//     "multiplicativeExpression",
//     "unaryExpression",
//     "postfixExpression",
//     "subscript",
//     "functionArguments",
//     "memberAccess"
//     "newExpression",
//     "arrayExpression"
// };


// TODO: Replace matchAndYield with match and consumeAndYield with consume!
#define la(parser, count) k_TokenStream_la((parser)->tokens, (count))
#define consume(parser) consumeToken((parser)->tokens)
#define match(parser, type) matchAndYield((parser), type)
#define lt(parser, count) k_TokenStream_lt((parser)->tokens, (count))

// Recover

void recover(Parser* parser) {
    /* The parser is now in recovery mode; flag other parts of the parser. */
    parser->recovery = true;

    if (parser->followSetSize > 0) {
        Token* lt1 = lt(parser, 1);
        /* The parser tries to recover until a token from the follow set or
         * the end-of-stream token is encountered.
         */
        while (lt1->type != TOKEN_END_OF_STREAM) {
            /* When searching for a follow token, the parser prioritizes tokens
             * that are expected by the nearest rule in the rule invocation
             * stack. This is why, the linear search algorithm is applied in a
             * reverse fashion over the follow set.
             */
            int32_t i;
            // for (i = parser->followSetSize - 1; i >= 0; i--) {
                // Look only for the latest follow token.
                if (lt1->type == (TokenType)parser->followSet[parser->followSetSize - 1]) {
                    /* A token from the follow set was encountered. The parser
                     * may have resynchronized with the input.
                     */
                    goto afterDiscard;
                }
            // }
            /* Consume and discard the current token. */
            consume(parser);
            /* Update the lookahead token. */
            lt1 = lt(parser, 1);
        }
        afterDiscard:
            ;
    }
}

void reportAndRecover(Parser* parser, TokenType expected) {
    /* Do not report the error if the parser is in recovery mode. Otherwise,
    * duplicate syntax errors will be reported to the end user.
    */
    if (!parser->recovery) {
        Token* lt1 = lt(parser, 1);
        Compiler* compiler = parser->compiler;
        ErrorHandler* errorHandler = compiler->errorHandler;
        handleSyntaxError(errorHandler, parser,
            ERROR_UNEXPECTED_TOKEN, lt1, expected);
    }

    /* Try to resychronize the parser with the input. */
    recover(parser);
}

bool ensureFollowSetSpace(Parser* parser, int32_t capacity) {
    jtk_Assert_assertObject(parser, "The specified parser is null.");

    bool result = false;
    if (capacity > 0) {
        int32_t currentCapacity = parser->followSetCapacity;
        int32_t minimumCapacity = (parser->followSetSize + capacity);
        int32_t requiredCapacity = minimumCapacity - currentCapacity;
        if (requiredCapacity > 0) {
            int32_t newCapacity = (currentCapacity * 2) + 2;
            if ((newCapacity - minimumCapacity) < 0) {
                newCapacity = minimumCapacity;
            }

            if (newCapacity < 0) {
                if (minimumCapacity < 0) {
                    /* Report an error, the requested capacity is too big. */
                    printf("[internal error] The requested capacity is too big. The parser has run out of memory.\n");
                }
                else {
                    /* Fall back, the new capacity was recommened by this function. */
                    newCapacity = JTK_INTEGER_MAX_VALUE;
                }
            }

            /* Do not allocate a new buffer if an out-of-memory error was
             * reported.
             */
            if (newCapacity > 0) {
                int32_t* temporary = parser->followSet;
                parser->followSet = (int32_t*)jtk_Arrays_copyOfEx_i(
                    parser->followSet, parser->followSetSize, newCapacity, 0,
                    false);
                parser->followSetCapacity = newCapacity;
                deallocate(temporary);
            }
        }
    }
    return result;
}

void pushFollowToken(Parser* parser, TokenType type) {
    jtk_Assert_assertObject(parser, "The specified parser is null.");

    /* Make sure that the set is large enough to hold another token type. */
    ensureFollowSetSpace(parser, parser->followSetSize + 1);
    /* Insert the follow token to the set. */
    parser->followSet[parser->followSetSize] = (int32_t)type;
    /* Increment the size of the follow set. */
    parser->followSetSize++;
}

void popFollowToken(Parser* parser) {
    jtk_Assert_assertTrue(parser->followSetSize > 0, "The follow set is empty.");

    parser->followSetSize--;
}

/* Consume */

Token* consumeAndYield(Parser* parser) {
    Token* lt1 = lt(parser, 1);
    consume(parser);

    return lt1;
}

/* Match */

// int32_t matchEx(Parser* parser, TokenType* types, int32_t count) {
//     jtk_Assert_assertObject(parser, "The specified parser is null.");

//     int32_t index;
//     matchAndYieldEx(parser, types, count, &index);
//     return index;
// }

Token* matchAndYieldEx(Parser* parser, const TokenType* types, int32_t count,
    int32_t* index) {
    jtk_Assert_assertObject(parser, "The specified parser is null.");
    jtk_Assert_assertTrue(count > 0, "The specified count is invalid.");

    Token* lt1 = lt(parser, 1);
    *index = -1;
    int32_t i;
    for (i = 0; i < count; i++) {
        if (lt1->type == types[i]) {
            *index = i;

            /* The token expected by the parser was found. If we the parser is
            * in error recovery, turn it off.
            */
            parser->recovery = false;

            /* The token stream prohibts consumption of end-of-stream
             * token.
             */
            if (lt1->type != TOKEN_END_OF_STREAM) {
                consume(parser);
            }
        }
    }

    if (*index == -1) {
        reportAndRecover(parser, types[0]);
        lt1 = NULL;
    }

    return lt1;
}

Token* matchAndYield(Parser* parser, TokenType type) {
    jtk_Assert_assertObject(parser, "The specified parser is null.");

    Token* lt1 = lt(parser, 1);
    if (lt1->type == type) {
        /* The token expected by the parser was found. If the parser is
         * in error recovery, turn it off.
         */
        parser->recovery = false;

        /* The token stream prohibts consumption of end-of-stream
         * token.
         */
        if (lt1->type != TOKEN_END_OF_STREAM) {
            consume(parser);
        }
    }
    else {
        reportAndRecover(parser, type);
    }
    return lt1;
}

// Rules

#define isReturnType(token) \
    (token == TOKEN_KEYWORD_VOID) || \
    (token == TOKEN_KEYWORD_BOOLEAN) || \
    (token == TOKEN_KEYWORD_I8) || \
    (token == TOKEN_KEYWORD_I16) || \
    (token == TOKEN_KEYWORD_I32) || \
    (token == TOKEN_KEYWORD_I64) || \
    (token == TOKEN_KEYWORD_UI8) || \
    (token == TOKEN_KEYWORD_UI16) || \
    (token == TOKEN_KEYWORD_UI32) || \
    (token == TOKEN_KEYWORD_UI64) || \
    (token == TOKEN_KEYWORD_F32) || \
    (token == TOKEN_KEYWORD_F64) || \
    (token == TOKEN_KEYWORD_STRING) || \
    (token == TOKEN_IDENTIFIER)

#define isComponentFollow(token) \
    (token == TOKEN_KEYWORD_STRUCT) || isReturnType(token)

// TODO: Add the string keyword.
#define isType(token) \
    (token == TOKEN_KEYWORD_BOOLEAN) || \
    (token == TOKEN_KEYWORD_I8) || \
    (token == TOKEN_KEYWORD_I16) || \
    (token == TOKEN_KEYWORD_I32) || \
    (token == TOKEN_KEYWORD_I64) || \
    (token == TOKEN_KEYWORD_UI8) || \
    (token == TOKEN_KEYWORD_UI16) || \
    (token == TOKEN_KEYWORD_UI32) || \
    (token == TOKEN_KEYWORD_UI64) || \
    (token == TOKEN_KEYWORD_F32) || \
    (token == TOKEN_KEYWORD_F64) || \
    (token == TOKEN_KEYWORD_STRING) || \
    (token == TOKEN_IDENTIFIER)

// expressionStatement (includes IDENTIFIER, which may lead to variableDeclaration, too!)
#define isSimpleStatementFollow(type) \
    (type == TOKEN_KEYWORD_VAR) || \
    (type == TOKEN_KEYWORD_LET) || \
    (type == TOKEN_KEYWORD_BREAK) || \
    (type == TOKEN_KEYWORD_RETURN) || \
    (type == TOKEN_KEYWORD_THROW) || \
    isType(type) || \
    isExpressionFollow(type)

#define isCompoundStatementFollow(type) \
    (type == TOKEN_KEYWORD_IF) || \
    (type == TOKEN_HASH) || \
    (type == TOKEN_KEYWORD_WHILE) || \
    (type == TOKEN_KEYWORD_FOR) || \
    (type == TOKEN_KEYWORD_TRY)

#define isStatementFollow(type) isSimpleStatementFollow(type) || isCompoundStatementFollow(type)

#define isExpressionFollow(type) isUnaryExpressionFollow(type)

/*
 * assignmentOperator
 * :    '='
 * |    '*='
 * |    '/='
 * |    '%='
 * |    '+='
 * |    '-='
 * |    '<<='
 * |    '>>='
 * |    '>>>='
 * |    '&='
 * |    '^='
 * |    '|='
 * ;
 */
#define isAssignmentOperator(type) \
    (type == TOKEN_EQUAL) || \
    (type == TOKEN_ASTERISK_EQUAL) || \
    (type == TOKEN_FORWARD_SLASH_EQUAL) || \
    (type == TOKEN_MODULUS_EQUAL) || \
    (type == TOKEN_PLUS_EQUAL) || \
    (type == TOKEN_DASH_EQUAL) || \
    (type == TOKEN_LEFT_ANGLE_BRACKET_2_EQUAL) || \
    (type == TOKEN_RIGHT_ANGLE_BRACKET_2_EQUAL) || \
    (type == TOKEN_AMPERSAND_EQUAL) || \
    (type == TOKEN_CARET_EQUAL) || \
    (type == TOKEN_VERTICAL_BAR_EQUAL)

/*
 * equalityOperator
 * :	'=='
 * |	'!='
 * ;
 */
#define isEqualityOperator(type) \
    (type == TOKEN_EQUAL_2) || (type == TOKEN_EXCLAMATION_MARK_EQUAL)

/*
 * relationalOperator
 * :	'<'
 * |	'>'
 * |	'<='
 * |	'>='
 * |	'is'
 * ;
 */
#define isRelationalOperator(type) \
    (type == TOKEN_LEFT_ANGLE_BRACKET) || \
    (type == TOKEN_RIGHT_ANGLE_BRACKET) || \
    (type == TOKEN_LEFT_ANGLE_BRACKET_EQUAL) || \
    (type == TOKEN_RIGHT_ANGLE_BRACKET_EQUAL)

/*
 * shiftOperator
 * :	'<<'
 * |	'>>'
 * |	'>>>'
 * ;
 */
#define isShiftOperator(type) \
    (type == TOKEN_LEFT_ANGLE_BRACKET_2) || \
    (type == TOKEN_RIGHT_ANGLE_BRACKET_2)

/*
 * additiveOperator
 * :	'+'
 * |	'-'
 * ;
 */
#define isAdditiveOperator(type) \
    (type == TOKEN_PLUS) || (type == TOKEN_DASH)

/*
 * multiplicativeOperator
 * :	'*'
 * |	'/'
 * |	'%'
 * ;
 */
#define isMultiplicativeOperator(type) \
    (type == TOKEN_ASTERISK) || \
    (type == TOKEN_FORWARD_SLASH) || \
    (type == TOKEN_MODULUS)

#define isUnaryExpressionFollow(type) \
    isUnaryOperator(type) || isPostfixExpressionFollow(type)

#define isPostfixExpressionFollow(type) isPrimaryExpressionFollow(type)

/*
 * unaryOperator
 * :    '+'
 * |    '-'
 * |    '~'
 * |    '!'
 * // |    '++'
 * // |    '--'
 * ;
 */
#define isUnaryOperator(type) \
    (type == TOKEN_PLUS) || \
    (type == TOKEN_DASH) || \
    (type == TOKEN_TILDE) || \
    (type == TOKEN_EXCLAMATION_MARK)

#define isPostfixPartFollow(type) \
    (type == TOKEN_LEFT_SQUARE_BRACKET) || \
    (type == TOKEN_LEFT_PARENTHESIS) || \
    (type == TOKEN_DOT) || \
    (type == TOKEN_PLUS_2) || \
    (type == TOKEN_DASH_2)

#define isPrimaryExpressionFollow(type) \
    isLiteral(type) || \
    (type == TOKEN_KEYWORD_THIS) || \
    (type == TOKEN_IDENTIFIER) || \
    (type == TOKEN_LEFT_PARENTHESIS) || \
    (type == TOKEN_KEYWORD_NEW) || \
    (type == TOKEN_LEFT_SQUARE_BRACKET) || \
    (type == TOKEN_LEFT_ANGLE_BRACKET) || \
    (type == TOKEN_KEYWORD_NEW)

#define isLiteral(type) \
    (type == TOKEN_INTEGER_LITERAL) || \
    (type == TOKEN_FLOATING_POINT_LITERAL) || \
    (type == TOKEN_KEYWORD_TRUE) || \
    (type == TOKEN_KEYWORD_FALSE) || \
    (type == TOKEN_STRING_LITERAL) || \
    (type == TOKEN_KEYWORD_NULL)

/*
 * compilationUnit
 * :	NEWLINE*
 *      importDeclaration*
 *      componentDeclaration*
 *      EOF
 * ;
 *
 * componentDeclaration
 * :    functionDeclaration
 * |    structureDeclaration
 * ;
 */
Module* parseModule(Parser* parser) {
	/* Create the context of this rule. */
	Module* context = newModule();

	/* Zero or more import declarations may occur in the source code.
	 * Therefore, repeatedly parse import declarations as long as
	 * LA(1) is the 'import' keyword.
	 *
	 * The following code represents: importDeclaration*
	 */
    while (la(parser, 1) == TOKEN_KEYWORD_IMPORT) {
		ImportDeclaration* importDeclaration = parseImportDeclaration(parser);
		jtk_ArrayList_add(context->imports, importDeclaration);
    }

    while (isComponentFollow(la(parser, 1))) {
        if (la(parser, 1) == TOKEN_KEYWORD_STRUCT) {
            Structure* structure = parseStructureDeclaration(parser);
            jtk_ArrayList_add(context->structures, structure);
        }
        else {
            Function* function = parseFunctionDeclaration(parser, 0);
            jtk_ArrayList_add(context->functions, function);
        }
    }

	/* We are expecting the 'end of stream' token here. */
    match(parser, TOKEN_END_OF_STREAM);

    return context;
}

/*
 * importDeclaration
 * :    'import' importTarget ';'
 * ;
 *
 * TODO:
 *
 * importTarget
 * :    IDENTIFIER ('.' IDENTIFIER)* ('.' '*' | ('as' IDENTIFIER))? ';'
 * ;
 *
 * The following function combines both the rules. This measure was
 * taken to avoid redundant nodes in the AST.
 */
ImportDeclaration* parseImportDeclaration(Parser* parser) {
	ImportDeclaration* context = newImportDeclaration();

	/* An import statement begins with the 'import' keyword. Therefore,
	 * we are expecting it.
	 */
    match(parser, TOKEN_KEYWORD_IMPORT);

    /* If importTarget fails, discard tokens until the newline token is
     * encountered.
     */
    pushFollowToken(parser, TOKEN_SEMICOLON);

	/* The user is expected to specify at least, one identifier.
	 * Consume it. The consumed identifier saved for later inspection.
	 */
    Token* identifier = matchAndYield(parser, TOKEN_IDENTIFIER);
	jtk_ArrayList_add(context->identifiers, identifier);

	/* Optionally, the user may specify more identifiers (with each identifier
	 * separated by the '.' token. Therefore, we repeatedly consume the '.' and
	 * identifier tokens as long as LA(1) is the '.' token and LA(2) is the
	 * identifier token.
	 */
    while ((la(parser, 1) == TOKEN_DOT) &&
           (k_TokenStream_la(parser->tokens, 2) == TOKEN_IDENTIFIER)) {
		/* Consume and discard the '.' token. */
        consume(parser);

		/* The consumed identifier is saved for later inspection. */
        identifier = matchAndYield(parser, TOKEN_IDENTIFIER);
		jtk_ArrayList_add(context->identifiers, identifier);
    }

	/* Optionally, the user may specify a wildcard; recognized when
	 * LA(1) is the '.' token and LA(2) is the '*' token.
	 */
    if (la(parser, 1) == TOKEN_DOT) {
		consume(parser);
		match(parser, TOKEN_ASTERISK);
        context->wildcard = true;
    }

    /* Pop the semicolon token from the follow set. */
    popFollowToken(parser);
	/* The import declaration is terminated with a semicolon.
	 * Therefore, we are expecting it here.
	 */
    match(parser, TOKEN_SEMICOLON);

    return context;
}

VariableType* parseTypeEx(Parser* parser, bool includeVoid) {
    static const TokenType tokens[] = {
        TOKEN_IDENTIFIER,
        TOKEN_KEYWORD_BOOLEAN,
        TOKEN_KEYWORD_I8,
        TOKEN_KEYWORD_I16,
        TOKEN_KEYWORD_I32,
        TOKEN_KEYWORD_I64,
        TOKEN_KEYWORD_UI8,
        TOKEN_KEYWORD_UI16,
        TOKEN_KEYWORD_UI32,
        TOKEN_KEYWORD_UI64,
        TOKEN_KEYWORD_F32,
        TOKEN_KEYWORD_F64,
        TOKEN_KEYWORD_STRING,
        TOKEN_KEYWORD_VOID
    };
    int32_t index;
    int32_t size = sizeof (tokens) / sizeof (TokenType);
    Token* token = matchAndYieldEx(parser, tokens, includeVoid? size : (size - 1),
        &index);
    int32_t dimensions = 0;
    while (la(parser, 1) == TOKEN_LEFT_SQUARE_BRACKET) {
        consume(parser);
        dimensions++;
        match(parser, TOKEN_RIGHT_SQUARE_BRACKET);
    }

    return newVariableType(token, dimensions);
}

/**
 * componentType
 * :    'i8'
 * |    'i16'
 * |    'i32'
 * |    'i64'
 * |    'f32'
 * |    'f64'
 * |    'boolean'
 * ;
 *
 * type
 * :    componentType ('[' ']')*
 * ;
 */
VariableType* parseType(Parser* parser) {
    return parseTypeEx(parser, false);
}

/**
 * returnType
 * :    type
 * |    'void'
 * ;
 */
VariableType* parseReturnType(Parser* parser) {
    return parseTypeEx(parser, true);
}

/*
 * functionDeclaration
 * :    returnType IDENTIFIER functionParameters (functionBody | SEMICOLON)
 * ;
 */
Function* parseFunctionDeclaration(Parser* parser,
    uint32_t modifiers) {
	/* If function parameters fails, skip tokens until ';', '{', or '}' is found. */
    pushFollowToken(parser, TOKEN_SEMICOLON);
    pushFollowToken(parser, TOKEN_LEFT_BRACE);
    pushFollowToken(parser, TOKEN_RIGHT_BRACE);

    VariableType* returnVariableType = parseReturnType(parser);
    Token* identifier = matchAndYield(parser, TOKEN_IDENTIFIER);
    jtk_ArrayList_t* parameters = jtk_ArrayList_new();
    Variable* variableParameter = NULL;
    parseFunctionParameters(parser, parameters, &variableParameter);

    /* Pop the ';', '{', and '}' tokens from the follow set. */
    popFollowToken(parser);
    popFollowToken(parser);
    popFollowToken(parser);

    Block* body = NULL;
    if (false /*k_Modifier_hasNative(modifiers) */) {
        match(parser, TOKEN_SEMICOLON);
    }
    else {
	    body = parseBlock(parser);
    }

    return newFunction(identifier->text, identifier->length, identifier,
        parameters, variableParameter, body, returnVariableType);
}

/*
 * functionParameters
 * :   '(' functionParameterList? ')'
 * ;
 *
 * functionParameterList
 * :   functionParameter (',' functionParameter)* (',' variableFunctionParameter)?
 * |   variableFunctionParameter
 * ;
 *
 * functionParameter
 * :   type IDENTIFIER
 * ;
 *
 * variableFunctionParameter
 * :   type '...' IDENTIFIER
 * ;
 *
 * The following function combines the above mentioned rules. This measure was
 * taken to avoid redundant nodes in the AST.
 */
void parseFunctionParameters(Parser* parser,
    jtk_ArrayList_t* fixedParameters, Variable** variableParameter) {
    match(parser, TOKEN_LEFT_PARENTHESIS);
    pushFollowToken(parser, TOKEN_RIGHT_PARENTHESIS);

    TokenType la1 = la(parser, 1);
    if (isType(la1)) {
        bool first = true;
        do {
            if (!first) {
                match(parser, TOKEN_COMMA);
            }

            bool variadic = false;
            Type* type = parseType(parser);
            if (la(parser, 1) == TOKEN_ELLIPSIS) {
                consume(parser);
                variadic = true;
            }
            Token* identifier = matchAndYield(parser, TOKEN_IDENTIFIER);
            first = false;

            Variable* parameter = newVariable(false, false, type, identifier->text,
                identifier->length, identifier, NULL, NULL);
            if (variadic) {
                *variableParameter = parameter;
                break;
            }
            else {
                jtk_ArrayList_add(fixedParameters, parameter);
            }
        }
        while (la(parser, 1) == TOKEN_COMMA);
    }

    popFollowToken(parser);
    match(parser, TOKEN_RIGHT_PARENTHESIS);
}

/*
 * block
 * |    '{' statement* '}'
 * ;
 *
 * statement
 * :    simpleStatement
 * |    compoundStatement
 * ;
 *
 * The following function combines the above mentioned rules. This measure was
 * taken to avoid redundant nodes in the AST.
 */
Block* parseBlock(Parser* parser) {
	Block* context = newBlock();

    /* Consume and discard the '{' token. */
    match(parser, TOKEN_LEFT_BRACE);
    /* If statement fails, discard tokens until the '}' token is
     * encountered.
     */
    pushFollowToken(parser, TOKEN_RIGHT_BRACE);

    while (true) {
        TokenType la1 = la(parser, 1);
        if (isSimpleStatementFollow(la1)) {
            Context* statement = parseSimpleStatement(parser);
            jtk_ArrayList_add(context->statements, statement);
        }
        else if (isCompoundStatementFollow(la1)) {
            Context* statement = parseCompoundStatement(parser);
            jtk_ArrayList_add(context->statements, statement);
        }
        else {
            break;
        }
    }

    /* Pop the '}' token from the follow set. */
    popFollowToken(parser);
    /* Consume and discard the '}' token. */
    match(parser, TOKEN_RIGHT_BRACE);

    return context;
}

#define isPrimitiveType(token) \
    (token == TOKEN_KEYWORD_BOOLEAN) || \
    (token == TOKEN_KEYWORD_I8) || \
    (token == TOKEN_KEYWORD_I16) || \
    (token == TOKEN_KEYWORD_I32) || \
    (token == TOKEN_KEYWORD_I64) || \
    (token == TOKEN_KEYWORD_UI8) || \
    (token == TOKEN_KEYWORD_UI16) || \
    (token == TOKEN_KEYWORD_UI32) || \
    (token == TOKEN_KEYWORD_UI64) || \
    (token == TOKEN_KEYWORD_F32) || \
    (token == TOKEN_KEYWORD_F64) || \
    (token == TOKEN_KEYWORD_STRING)

/* The parser needs to look ahead 3 tokens to differentiate between variable
 * declarations and expressions, recognizing an LL(3) grammar.
 *
 * followVariableDeclaration
 * :    'let'
 * |    'var'
 * |    IDENTIFIER (('[' ']') | IDENTIFIER)
 * ;
 */
bool followVariableDeclaration(Parser* parser) {
    TokenType la1 = la(parser, 1);
    return (la1 == TOKEN_KEYWORD_LET) ||
           (la1 == TOKEN_KEYWORD_VAR) ||
           isPrimitiveType(la1) ||
           ((la1 == TOKEN_IDENTIFIER) &&
            (((la(parser, 2) == TOKEN_LEFT_SQUARE_BRACKET) && (la(parser, 3) == TOKEN_RIGHT_SQUARE_BRACKET)) ||
            (la(parser, 2) == TOKEN_IDENTIFIER)));
}

/*
 * simpleStatement
 * :    unterminatedSimpleStatement SEMICOLON
 * ;
 *
 * unterminatedSimpleStatement
 * :   expressionStatement
 * |   emptyStatement
 * |   storageDeclaration
 * |   breakStatement
 * |   returnStatement
 * |   throwStatement
 * ;
 *
 * expressionStatement
 * :    expression
 * ;
 *
 * The following function combines the above mentioned rules. This measure was
 * taken to avoid redundant nodes in the AST.
 */
Context* parseSimpleStatement(Parser* parser) {
    Context* result = NULL;

    /* If expressionStatement, emptyStatement, variableDeclaration,
     * breakStatement, returnStatement, or throwStatement fails, discard tokens
     * until the semicolon token is encountered.
     */
    pushFollowToken(parser, TOKEN_SEMICOLON);

    TokenType la1 = la(parser, 1);
    if (followVariableDeclaration(parser)) {
        result = parseVariableDeclaration(parser, false, true);
    }
    else if (isExpressionFollow(la1)) {
        result = parseExpression(parser);
    }
    else {
        switch (la1) {
            case TOKEN_KEYWORD_BREAK: {
                result = parseBreakStatement(parser);
                break;
            }

            case TOKEN_KEYWORD_RETURN: {
                result = parseReturnStatement(parser);
                break;
            }

            case TOKEN_KEYWORD_THROW: {
                result = parseThrowStatement(parser);
                break;
            }

            default: {
                controlError();
                break;
            }
        }
    }

    /* Pop the ';' token from the follow set. */
    popFollowToken(parser);
    /* Match and discard the newline token. */
	match(parser, TOKEN_SEMICOLON);

    return result;
}

/*
 * variableDeclaration
 * :    ('var' | 'let' | type) variableDeclarator (',' variableDeclarator)*
 * ;
 */
VariableDeclaration* parseVariableDeclaration(Parser* parser, bool onlyVariable,
    bool allowInitializer) {
	VariableDeclaration* context = newVariableDeclaration();

    TokenType la1 = la(parser, 1);
    bool infer = (la1 == TOKEN_KEYWORD_VAR);
    bool constant = (la1 == TOKEN_KEYWORD_LET);
    VariableType* variableType = NULL;

    if (!infer && !constant) {
        variableType = parseType(parser);
    }
    else {
        consume(parser);
    }

	parseVariableDeclarator(parser, infer, constant, variableType, context->variables);

	while (la(parser, 1) == TOKEN_COMMA) {
        consume(parser);
		parseVariableDeclarator(parser, infer, constant, variableType, context->variables);
	}

    return context;
}

/*
 * variableDeclarator
 * :    IDENTIFIER ('=' expression)?
 * ;
 */
void parseVariableDeclarator(Parser* parser, bool infer, bool constant,
    VariableType* variableType, jtk_ArrayList_t* variables) {
    Token* identifier = matchAndYield(parser, TOKEN_IDENTIFIER);
    BinaryExpression* expression = NULL;

    TokenType la1 = la(parser, 1);
    if ((infer || constant) && (la1 != TOKEN_EQUAL)) {
        ErrorHandler* errorHandler = parser->compiler->errorHandler;
        handleSyntaxError(errorHandler, parser,
            ERROR_VARIABLE_INITIALIZER_EXPECTED, identifier, TOKEN_UNKNOWN);
    }

	if (la(parser, 1) == TOKEN_EQUAL) {
		consume(parser);
        expression = parseExpression(parser);
	}

    Variable* variable = newVariable(infer, constant, variableType,
        identifier->text, identifier->length, identifier, expression, NULL);
    jtk_ArrayList_add(variables, variable);
}

/*
 * breakStatement
 * :    'break' IDENTIFIER?
 * ;
 */
BreakStatement* parseBreakStatement(Parser* parser) {
    BreakStatement* context = newBreakStatement();
    match(parser, TOKEN_KEYWORD_BREAK);
    if (la(parser, 1) == TOKEN_IDENTIFIER) {
        context->identifier = consumeAndYield(parser);
    }
    return context;
}

/*
 * returnStatement
 * :    'return' expression
 * ;
 */
ReturnStatement* parseReturnStatement(Parser* parser) {
    ReturnStatement* context = newReturnStatement();
    context->keyword = matchAndYield(parser, TOKEN_KEYWORD_RETURN);
    context->expression = parseExpression(parser);
    return context;
}

/*
 * throwStatement
 * :    'throw' expression
 * ;
 */
ThrowStatement* parseThrowStatement(Parser* parser) {
    ThrowStatement* context = newThrowStatement();
    match(parser, TOKEN_KEYWORD_THROW);
    context->expression = parseExpression(parser);
    return context;
}

/*
 * compoundStatement
 * :    ifStatement
 * |    iterativeStatement
 * |    tryStatement
 * ;
 */
Context* parseCompoundStatement(Parser* parser) {
    Context* context = NULL;

	switch (la(parser, 1)) {
		case TOKEN_KEYWORD_IF: {
            context = parseIfStatement(parser);
			break;
		}

		case TOKEN_HASH:
		case TOKEN_KEYWORD_WHILE:
		case TOKEN_KEYWORD_FOR: {
            context = parseIterativeStatement(parser);
			break;
		}

		case TOKEN_KEYWORD_TRY: {
			context = parseTryStatement(parser);
			break;
		}

		default: {
			controlError();
			break;
		}
	}

    return context;
}

/*
 * ifStatement
 * :    ifClause elseIfClause* elseClause?
 * ;
 *
 * elseClause
 * :    'else' block
 * ;
 */
IfStatement* parseIfStatement(Parser* parser) {
    IfStatement* context = newIfStatement();

    context->ifClause = parseIfClause(parser);

    while ((la(parser, 1) == TOKEN_KEYWORD_ELSE) &&
	       (k_TokenStream_la(parser->tokens, 2) == TOKEN_KEYWORD_IF)) {
		IfClause* elseIfClause = parseElseIfClause(parser);
        jtk_ArrayList_add(context->elseIfClauses, elseIfClause);
	}

    if (la(parser, 1) == TOKEN_KEYWORD_ELSE) {
        consume(parser);
        context->elseClause = parseBlock(parser);
    }

    return context;
}

/*
 * ifClause
 * :    'if' expression block
 * ;
 */
IfClause* parseIfClause(Parser* parser) {
    IfClause* context = newIfClause();

	context->token = matchAndYield(parser, TOKEN_KEYWORD_IF);
    context->expression = parseExpression(parser);
    context->body = parseBlock(parser);

    return context;
}

/* elseIfClause
 * :    'else' 'if' expression block
 * ;
 */
IfClause* parseElseIfClause(Parser* parser) {
    IfClause* context = newIfClause();

	match(parser, TOKEN_KEYWORD_ELSE);
	context->token = matchAndYield(parser, TOKEN_KEYWORD_IF);
    context->expression = parseExpression(parser);
    context->body = parseBlock(parser);

    return context;
}

/*
 * iterativeStatement
 * :    labelClause? (whileStatement | forStatement)
 * ;
 *
 * labelClause
 * :    '#' IDENTIFIER
 * ;
 *
 * whileStatement
 * :    'while' expression block
 * ;
 *
 * forStatement
 * :    'for' forParameter ':' expression block
 * ;
 *
 * forParameter
 * :    'let' IDENTIFIER
 * ;
 */
IterativeStatement* parseIterativeStatement(Parser* parser) {
    IterativeStatement* context = newIterativeStatement();

    // TODO: Should context->name really be allocated? Can't we just reuse context->label->text?
	if (la(parser, 1) == TOKEN_HASH) {
        consume(parser);
	    context->label = matchAndYield(parser, TOKEN_IDENTIFIER);
        context->nameSize = context->label->length;
        context->name = jtk_CString_newEx(context->label->text, context->nameSize);
	}

    TokenType la1 = la(parser, 1);
    context->keyword = consumeAndYield(parser);
	switch (la1) {
		case TOKEN_KEYWORD_WHILE: {
            context->expression = parseExpression(parser);
            context->body = parseBlock(parser);
			break;
		}

		case TOKEN_KEYWORD_FOR: {
            match(parser, TOKEN_KEYWORD_LET);
            context->parameter = matchAndYield(parser, TOKEN_IDENTIFIER);
            match(parser, TOKEN_COLON);
            context->expression = parseExpression(parser);
            context->body = parseBlock(parser);

			break;
		}

        default: {
            // Error: Expected 'for' or 'while'
            reportAndRecover(parser, TOKEN_KEYWORD_WHILE);
            break;
        }
	}

    return context;
}

/*
 * tryStatement
 * :    tryClause catchClause* finallyClause?
 * ;
 *
 * tryClause
 * :    'try' block
 * ;
 *
 * finallyClause
 * :    'finally' block
 * ;
 */
TryStatement* parseTryStatement(Parser* parser) {
    TryStatement* context = newTryStatement();
	bool hasCatch = false;
	bool hasFinally = false;

    Token* tryKeyword = matchAndYield(parser, TOKEN_KEYWORD_TRY);
    context->tryClause = parseBlock(parser);

	while ((la(parser, 1) == TOKEN_KEYWORD_CATCH)) {
		hasCatch = true;
		CatchClause* catchClause = parseCatchClause(parser);
        jtk_ArrayList_add(context->catchClauses, catchClause);
	}

	if (la(parser, 1) == TOKEN_KEYWORD_FINALLY) {
		hasFinally = true;
        consume(parser);
        context->finallyClause = parseBlock(parser);
	}

	if (!hasCatch && !hasFinally) {
		/* Try clause without catch or finally. According to the grammar of KUSH,
         * this is not an error. However, the KUSH specification requires a try
         * clause to be followed by at least a catch or finally clause.
         */
        handleSyntaxError(parser->compiler->errorHandler,
            parser, ERROR_TRY_STATEMENT_EXPECTS_CATCH_OR_FINALLY,
            tryKeyword, TOKEN_UNKNOWN);
	}

    return context;
}

/*
 * catchClause
 * :	'catch' catchFilter? IDENTIFIER block
 * ;
 *
 * catchFilter
 * :	(STRING_LITERAL | IDENTIFIER) ('|' (STRING_LITERAL | IDENTIFIER))*
 * ;
 */
CatchClause* parseCatchClause(Parser* parser) {
    CatchClause* context = newCatchClause();

    match(parser, TOKEN_KEYWORD_CATCH);

    if ((la(parser, 1) == TOKEN_STRING_LITERAL) ||
        ((la(parser, 1) == TOKEN_IDENTIFIER) && (la(parser, 2) == TOKEN_IDENTIFIER))) {
        TokenType validTokens[] = {
            TOKEN_STRING_LITERAL,
            TOKEN_IDENTIFIER
        };
        int32_t index;
        Token* capture = matchAndYieldEx(parser, validTokens, 2, &index);
        jtk_ArrayList_add(context->captures, capture);

        while (la(parser, 1) == TOKEN_VERTICAL_BAR) {
            /* Consume and discard the '|' token. */
            consume(parser);

		    capture = matchAndYieldEx(parser, validTokens, 2, &index);
            jtk_ArrayList_add(context->captures, capture);
        }
	}

    Token* identifier = matchAndYield(parser, TOKEN_IDENTIFIER);
    context->parameter = newVariable(false, true, &primitives.string,
        identifier->text, identifier->length, identifier, NULL, NULL);
    context->body = parseBlock(parser);

    return context;
}

/*
 * structureDeclaration
 * :	'struct' IDENTIFIER structureBody
 * ;
 *
 * structureBody
 * :	'{' structureMember+ '}'
 * ;
 *
 * structureMember
 * :	variableDeclaration ';'
 * ;
 */
Structure* parseStructureDeclaration(Parser* parser) {
    match(parser, TOKEN_KEYWORD_STRUCT);

    Token* identifier = matchAndYield(parser, TOKEN_IDENTIFIER);

    match(parser, TOKEN_LEFT_BRACE);
    pushFollowToken(parser, TOKEN_RIGHT_BRACE);

    jtk_ArrayList_t* variables = jtk_ArrayList_new();
    while (isType(la(parser, 1))) {
        pushFollowToken(parser, TOKEN_SEMICOLON);

        VariableDeclaration* declaration = parseVariableDeclaration(parser, true, false); // typed, expression
        jtk_ArrayList_add(variables, declaration);
        match(parser, TOKEN_SEMICOLON);

        popFollowToken(parser);
    }

    popFollowToken(parser);
    match(parser, TOKEN_RIGHT_BRACE);

    return newStructure(identifier->text, identifier->length, identifier,
        variables);
}

/**
 * expressions
 * :    expression (',' expression)*
 * ;
 */
jtk_ArrayList_t* parseExpressions(Parser* parser) {
    jtk_ArrayList_t* expressions = jtk_ArrayList_new();

    BinaryExpression* expression = parseExpression(parser);
    jtk_ArrayList_add(expressions, expression);

    while (la(parser, 1) == TOKEN_COMMA) {
        consume(parser);
        expression = parseExpression(parser);
        jtk_ArrayList_add(expressions, expression);
    }

    return expressions;
}

/*
 * expression
 * :	assignmentExpression
 * ;
 */
BinaryExpression* parseExpression(Parser* parser) {
    /* Use the function stack to save the previous label. This is
     * important in the case of nested assignment expressions. Consider
     * the expression `x = (y = z)`, if the previous label is unsaved,
     * the nested assignment expression can alter the label of the outer
     * assignment expression.
     */
    bool previous = parser->placeholder;
    BinaryExpression* result = parseAssignmentExpression(parser);
    parser->placeholder = previous;

    return result;
}

/*
 * assignmentExpression
 * :	conditionalExpression (assignmentOperator conditionalExpression)*
 * ;
 */
BinaryExpression* parseAssignmentExpression(Parser* parser) {
    BinaryExpression* context = newBinaryExpression(CONTEXT_ASSIGNMENT_EXPRESSION);

    context->left = parseConditionalExpression(parser);

    while (isAssignmentOperator(la(parser, 1))) {
        jtk_Pair_t* pair = jtk_Pair_new();
        jtk_ArrayList_add(context->others, pair);
        pair->m_left = consumeAndYield(parser);

        if (!parser->placeholder) {
            ErrorHandler* handler = parser->compiler->errorHandler;
            handleSyntaxError(handler, parser, ERROR_INVALID_LVALUE,
                (Token*)pair->m_left, TOKEN_UNKNOWN);
        }

        pair->m_right = parseConditionalExpression(parser);
    }

    return context;
}

/*
 * conditionalExpression
 * :	condition ('then' expression 'else' conditionalExpression)?
 * ;
 */
ConditionalExpression* parseConditionalExpression(Parser* parser) {
    ConditionalExpression* context = newConditionalExpression();
    context->condition = parseLogicalOrExpression(parser);

    if (la(parser, 1) == TOKEN_HOOK) {
        context->hook = consumeAndYield(parser);
        context->then = parseExpression(parser);
        match(parser, TOKEN_COLON);
        context->otherwise = parseConditionalExpression(parser);
        parser->placeholder = false;
    }

    return context;
}

/*
 * condition
 * :	logicalAndExpression ('||' logicalAndExpression)*
 * ;
 */
BinaryExpression* parseLogicalOrExpression(Parser* parser) {
    BinaryExpression* context = newBinaryExpression(CONTEXT_LOGICAL_OR_EXPRESSION);

    context->left = parseLogicalAndExpression(parser);

    while (la(parser, 1) == TOKEN_VERTICAL_BAR_2) {
        jtk_Pair_t* pair = jtk_Pair_new();
        pair->m_left = consumeAndYield(parser);
        pair->m_right = parseLogicalAndExpression(parser);
        jtk_ArrayList_add(context->others, pair);
        parser->placeholder = false;
    }

    return context;
}

/*
 * logicalAndExpression
 * :	inclusiveOrExpression ('and' logicalAndExpression)?
 * ;
 */
BinaryExpression* parseLogicalAndExpression(Parser* parser) {
    BinaryExpression* context = newBinaryExpression(CONTEXT_LOGICAL_AND_EXPRESSION);

    context->left = parseInclusiveOrExpression(parser);

    while (la(parser, 1) == TOKEN_AMPERSAND_2) {
        jtk_Pair_t* pair = jtk_Pair_new();
        pair->m_left = consumeAndYield(parser);
        pair->m_right = parseInclusiveOrExpression(parser);
        jtk_ArrayList_add(context->others, pair);
        parser->placeholder = false;
    }

    return context;
}

/*
 * inclusiveOrExpression
 * :	exclusiveOrExpression ('|' exclusiveOrExpression)*
 * ;
 */
BinaryExpression* parseInclusiveOrExpression(Parser* parser) {
    BinaryExpression* context = newBinaryExpression(CONTEXT_INCLUSIVE_OR_EXPRESSION);

    context->left = parseExclusiveOrExpression(parser);

    while (la(parser, 1) == TOKEN_VERTICAL_BAR) {
        jtk_Pair_t* pair = jtk_Pair_new();
        pair->m_left = consumeAndYield(parser);
        pair->m_right = parseExclusiveOrExpression(parser);
        jtk_ArrayList_add(context->others, pair);
        parser->placeholder = false;
    }

    return context;
}

/*
 * exclusiveOrExpression
 * :	andExpression ('^' andExpression)*
 * ;
 */
BinaryExpression* parseExclusiveOrExpression(Parser* parser) {
    BinaryExpression* context = newBinaryExpression(CONTEXT_EXCLUSIVE_OR_EXPRESSION);

    context->left = parseAndExpression(parser);

    while (la(parser, 1) == TOKEN_CARET) {
        jtk_Pair_t* pair = jtk_Pair_new();
        pair->m_left = consumeAndYield(parser);
        pair->m_right = parseAndExpression(parser);
        jtk_ArrayList_add(context->others, pair);
        parser->placeholder = false;
    }

    return context;
}

/*
 * andExpression
 * :	equalityExpression ('&' equalityExpression)*
 * ;
 */
BinaryExpression* parseAndExpression(Parser* parser) {
    BinaryExpression* context = newBinaryExpression(CONTEXT_AND_EXPRESSION);

    context->left = parseEqualityExpression(parser);

    while (la(parser, 1) == TOKEN_AMPERSAND) {
        jtk_Pair_t* pair = jtk_Pair_new();
        pair->m_left = consumeAndYield(parser);
        pair->m_right = parseEqualityExpression(parser);
        jtk_ArrayList_add(context->others, pair);
        parser->placeholder = false;
    }

    return context;
}

/*
 * equalityExpression
 * :	relationalExpression (equalityOperator relationalExpression)*
 * ;
 */
BinaryExpression* parseEqualityExpression(Parser* parser) {
    BinaryExpression* context = newBinaryExpression(CONTEXT_EQUALITY_EXPRESSION);

    context->left = parseRelationalExpression(parser);

    while (isEqualityOperator(la(parser, 1))) {
        jtk_Pair_t* pair = jtk_Pair_new();
        pair->m_left = consumeAndYield(parser);
        pair->m_right = parseRelationalExpression(parser);
        jtk_ArrayList_add(context->others, pair);
        parser->placeholder = false;
    }

    return context;
}

/*
 * relationalExpression
 * :	shiftExpression (relationalOperator shiftExpression)*
 * ;
 */
BinaryExpression* parseRelationalExpression(Parser* parser) {
    BinaryExpression* context = newBinaryExpression(CONTEXT_RELATIONAL_EXPRESSION);

    context->left = parseShiftExpression(parser);

    while (isRelationalOperator(la(parser, 1))) {
        jtk_Pair_t* pair = jtk_Pair_new();
        pair->m_left = consumeAndYield(parser);
        pair->m_right = parseShiftExpression(parser);
        jtk_ArrayList_add(context->others, pair);
        parser->placeholder = false;
    }

    return context;
}

/*
 * shiftExpression
 * :	additiveExpression (shiftOperator additiveExpression)*
 * ;
 */
BinaryExpression* parseShiftExpression(Parser* parser) {
    BinaryExpression* context = newBinaryExpression(CONTEXT_SHIFT_EXPRESSION);

    context->left = parseAdditiveExpression(parser);

    while (isShiftOperator(la(parser, 1))) {
        jtk_Pair_t* pair = jtk_Pair_new();
        pair->m_left = consumeAndYield(parser);
        pair->m_right = parseAdditiveExpression(parser);
        jtk_ArrayList_add(context->others, pair);
        parser->placeholder = false;
    }

    return context;
}

/*
 * additiveExpression
 * :	multiplicativeExpression (multiplicativeOperator multiplicativeExpression)*
 * ;
 */
BinaryExpression* parseAdditiveExpression(Parser* parser) {
    BinaryExpression* context = newBinaryExpression(CONTEXT_ADDITIVE_EXPRESSION);

    context->left = parseMultiplicativeExpression(parser);

    while (isAdditiveOperator(la(parser, 1))) {
        jtk_Pair_t* pair = jtk_Pair_new();
        pair->m_left = consumeAndYield(parser);
        pair->m_right = parseMultiplicativeExpression(parser);
        jtk_ArrayList_add(context->others, pair);
        parser->placeholder = false;
    }

    return context;
}

/*
 * multiplicativeExpression
 * :	unaryExpression (multiplicativeOperator unaryExpression)*
 * ;
 */
BinaryExpression* parseMultiplicativeExpression(Parser* parser) {
    BinaryExpression* context = newBinaryExpression(CONTEXT_MULTIPLICATIVE_EXPRESSION);

    context->left = (Context*)parseUnaryExpression(parser);

    while (isMultiplicativeOperator(la(parser, 1))) {
        jtk_Pair_t* pair = jtk_Pair_new();
        pair->m_left = (void*)consumeAndYield(parser);
        pair->m_right = (void*)parseUnaryExpression(parser);
        jtk_ArrayList_add(context->others, pair);
        parser->placeholder = false;
    }

    return context;
}

/*
 * unaryExpression
 * :	unaryOperator unaryExpression
 * |	postfixExpression
 * ;
 */
UnaryExpression* parseUnaryExpression(Parser* parser) {
    UnaryExpression* context = newUnaryExpression();

    TokenType la1 = la(parser, 1);
    if (isUnaryOperator(la1)) {
        context->operator = consumeAndYield(parser);
        context->expression = (Context*)parseUnaryExpression(parser);
        parser->placeholder = false;
    }
    else if (isPostfixExpressionFollow(la1)) {
        context->expression = (Context*)parsePostfixExpression(parser);
    }
    else {
        // Error: Expected unary operator or postfix expression follow
        reportAndRecover(parser, TOKEN_INTEGER_LITERAL);
    }

    return context;
}

/*
 * postfixExpression
 * :	primaryExpression postfixPart*
 * ;
 *
 * postfixPart
 * :    subscript
 * |    functionArguments
 * |    memberAccess
 * |    postfixOperator         -- future
 * ;
 *
 * The following function combines both the rules. This measure was
 * taken to avoid redundant nodes in the AST.
 */
PostfixExpression* parsePostfixExpression(Parser* parser) {
    PostfixExpression* context = newPostfixExpression();

    context->primary = parsePrimaryExpression(parser,
        &context->token);

    TokenType la1 = la(parser, 1);
    while (isPostfixPartFollow(la1)) {
        switch (la1) {
            case TOKEN_LEFT_SQUARE_BRACKET: {
                void* subscript = parseSubscript(parser);
                jtk_ArrayList_add(context->postfixParts, subscript);
                break;
            }

            case TOKEN_LEFT_PARENTHESIS: {
                void* functionArguments = parseFunctionArguments(parser);
                jtk_ArrayList_add(context->postfixParts, functionArguments);
                break;
            }

            case TOKEN_DOT: {
                void* memberAccess = parseMemberAccess(parser);
                jtk_ArrayList_add(context->postfixParts, memberAccess);
                break;
            }
        }
        la1 = la(parser, 1);
    }

    return context;
}

/*
 * This function returns a pair of the form:
 * ( '[' token, assignment expression )
 *
 * subscript
 * :	'[' expression ']'
 * ;
 */
Subscript* parseSubscript(Parser* parser) {
    Subscript* context = newSubscript();

    context->bracket = matchAndYield(parser, TOKEN_LEFT_SQUARE_BRACKET);
    pushFollowToken(parser, TOKEN_RIGHT_SQUARE_BRACKET);
    context->expression = parseExpression(parser);
    popFollowToken(parser);
    match(parser, TOKEN_RIGHT_SQUARE_BRACKET);
    parser->placeholder = true;

    return context;
}

/*
 * This function returns a pair of the form:
 * [ '(' token, list of assignment expressions ].
 *
 * functionArguments
 * :	'(' expressions? ')'
 * ;
 */
FunctionArguments* parseFunctionArguments(Parser* parser) {
    FunctionArguments* context = newFunctionArguments();

    context->parenthesis = matchAndYield(parser, TOKEN_LEFT_PARENTHESIS);

    if (isExpressionFollow(la(parser, 1))) {
        pushFollowToken(parser, TOKEN_RIGHT_PARENTHESIS);
        context->expressions = parseExpressions(parser);
        popFollowToken(parser);
    }
    match(parser, TOKEN_RIGHT_PARENTHESIS);
    parser->placeholder = false;

    return context;
}

/*
 * This function returns a pair of the form:
 * ( '.' token, identifier token ).
 *
 * memberAccess
 * :	'.' IDENTIFIER
 * ;
 */
MemberAccess* parseMemberAccess(Parser* parser) {
    MemberAccess* context = newMemberAccess();
    match(parser, TOKEN_DOT);
    context->identifier = matchAndYield(parser, TOKEN_IDENTIFIER);
    parser->placeholder = false;
    return context;
}

/*
 * This function returns either a context or a token.
 *
 * primaryExpression
 * :	IDENTIFIER
 * |	literal
 * |	'(' expression ')'
 * |	newExpression
 * |	arrayExpression
 * ;
 *
 * literal
 * :	INTEGER_LITERAL
 * |	FLOATING_POINT_LITERAL
 * |	'true'
 * |	'false'
 * |	STRING_LITERAL
 * |	'null'
 * ;
 *
 * NOTE: The primaryExpression rule has no context. It simply forwards the
 * AST node it receives to the best matching child rule.
 */
void* parsePrimaryExpression(Parser* parser, bool* token) {
    void* result = NULL;
    *token = false;

    TokenType la1 = la(parser, 1);
    if (isLiteral(la1)) {
        result = consumeAndYield(parser);
        *token = true;
    }
    else {
        switch (la1) {
            case TOKEN_IDENTIFIER: {
                result = consumeAndYield(parser);
                *token = true;
                parser->placeholder = true;
                break;
            }

            case TOKEN_LEFT_PARENTHESIS: {
                consume(parser);
                pushFollowToken(parser, TOKEN_RIGHT_PARENTHESIS);
                result = parseExpression(parser);
                popFollowToken(parser);
                match(parser, TOKEN_RIGHT_PARENTHESIS);
                parser->placeholder = false;
                break;
            }

            case TOKEN_KEYWORD_NEW: {
                result = parseNewExpression(parser);
                break;
            }

            case TOKEN_LEFT_SQUARE_BRACKET: {
                result = parseArrayExpression(parser);
                break;
            }

            default: {
                controlError();
                break;
            }
        }
    }

    return result;
}

/*
 * newExpression
 * :	'new' type (arrayDimensions | initializer)
 * ;
 *
 * arrayDimensions
 * :    ('[' expression ']')+
 * ;
 *
 * initializer
 * :    '{' initializerEntries? '}'
 * ;
 *
 * initializerEntries
 * :	initializerEntry (',' initializerEntry)*
 * ;
 *
 * TODO: We can add an arbitary ',' in the end of a map, list, or an array.
 *		 Simply use the isExpressionFollow() function.
 */
NewExpression* parseNewExpression(Parser* parser) {
    match(parser, TOKEN_KEYWORD_NEW);

    static const TokenType tokens[] = {
        TOKEN_IDENTIFIER,
        TOKEN_KEYWORD_BOOLEAN,
        TOKEN_KEYWORD_I8,
        TOKEN_KEYWORD_I16,
        TOKEN_KEYWORD_I32,
        TOKEN_KEYWORD_I64,
        TOKEN_KEYWORD_UI8,
        TOKEN_KEYWORD_UI16,
        TOKEN_KEYWORD_UI32,
        TOKEN_KEYWORD_UI64,
        TOKEN_KEYWORD_F32,
        TOKEN_KEYWORD_F64,
        TOKEN_KEYWORD_STRING
    };
    int32_t index;
    int32_t size = sizeof (tokens) / sizeof (TokenType);
    Token* token = matchAndYieldEx(parser, tokens, size, &index);
    int32_t dimensions = 0;

    NewExpression* context = newNewExpression();
    if ((token->type == TOKEN_IDENTIFIER) && (la(parser, 1) == TOKEN_LEFT_BRACE)) {
        consume(parser);
        pushFollowToken(parser, TOKEN_RIGHT_BRACE);

        if (la(parser, 1) == TOKEN_IDENTIFIER) {
            jtk_Pair_t* entry = parseInitializerEntry(parser);
            jtk_ArrayList_add(context->entries, entry);

            while (la(parser, 1) == TOKEN_COMMA) {
                consume(parser);
                entry = parseInitializerEntry(parser);
                jtk_ArrayList_add(context->entries, entry);
            }
        }

        popFollowToken(parser);
        match(parser, TOKEN_RIGHT_BRACE);
    }
    else {
        do {
            match(parser, TOKEN_LEFT_SQUARE_BRACKET);
            pushFollowToken(parser, TOKEN_RIGHT_SQUARE_BRACKET);
            dimensions++;

            BinaryExpression* expression = parseExpression(parser);
            jtk_ArrayList_add(context->expressions, expression);

            popFollowToken(parser);
            match(parser, TOKEN_RIGHT_SQUARE_BRACKET);
        }
        while (la(parser, 1) == TOKEN_LEFT_SQUARE_BRACKET);
    }
    context->variableType = newVariableType(token, dimensions);
    parser->placeholder = false;

    return context;
}

/*
 * initializerEntry
 * :	IDENTIFIER ':' expression
 * ;
 */
jtk_Pair_t* parseInitializerEntry(Parser* parser) {
    jtk_Pair_t* pair = jtk_Pair_new();
    pair->m_left = matchAndYield(parser, TOKEN_IDENTIFIER);
    match(parser, TOKEN_COLON);
    pair->m_right = parseExpression(parser);

    return pair;
}

/*
 * arrayExpression
 * :    '[' expressions ']'
 * ;
 */
ArrayExpression* parseArrayExpression(Parser* parser) {
    ArrayExpression* context = newArrayExpression();

    context->token = matchAndYield(parser, TOKEN_LEFT_SQUARE_BRACKET);
    if (isExpressionFollow(la(parser, 1))) {
        pushFollowToken(parser, TOKEN_RIGHT_SQUARE_BRACKET);
        // TODO: parseExpressions should accept ArrayList not return it.
        context->expressions = parseExpressions(parser);
        popFollowToken(parser);
    }
    else {
        ErrorHandler* errorHandler = parser->compiler->errorHandler;
        handleSyntaxError(errorHandler, parser,
            ERROR_EMPTY_ARRAY_INITIALIZER, context->token, TOKEN_UNKNOWN);
    }
    match(parser, TOKEN_RIGHT_SQUARE_BRACKET);
    parser->placeholder = false;

    return context;
}

// Constructor

Parser* parserNew(Compiler* compiler, TokenStream* tokens) {
    jtk_Assert_assertObject(compiler, "The specified compiler is null.");

    Parser* parser = allocate(Parser, 1);
    parser->compiler = compiler;
    parser->tokens = tokens;
    parser->followSet = allocate(int32_t, 16);
    parser->followSetSize = 0;
    parser->followSetCapacity = 16;
    parser->recovery = false;
    parser->placeholder = false;

    return parser;
}

// Destructor

void parserDelete(Parser* parser) {
    jtk_Assert_assertObject(parser, "The specified parser is null.");

    deallocate(parser->followSet);
    deallocate(parser);
}

// Parse

Module* parse(Parser* parser) {
    return parseModule(parser);
}

// Reset

void resetParser(Parser* parser, TokenStream* tokens) {
    jtk_Assert_assertObject(parser, "The specified parser is null.");

    parser->tokens = tokens;
    parser->followSetSize = 0;
    parser->recovery = false;
}
