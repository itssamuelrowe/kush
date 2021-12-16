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

// Sunday, June 18 2020

#include <jtk/collection/Pair.h>
#include <jtk/core/CString.h>
#include <kush/generator.h>

#define invalidate(generator) generator->scope = generator->scope->parent

/******************************************************************************
 * Generator                                                                  *
 ******************************************************************************/

static void generateType(Generator* generator, Type* type);
static void generateForwardReferences(Generator* generator, Module* module);
static void generateStructures(Generator* generator, Module* module);
static void generateBinary(Generator* generator, BinaryExpression* expression);
static void generateConditional(Generator* generator, ConditionalExpression* expression);
static void generateUnary(Generator* generator, UnaryExpression* expression);
static void generateSubscript(Generator* generator, Subscript* subscript);
static void generateFunctionArguments(Generator* generator, FunctionArguments* arguments);
static void generateMemberAccess(Generator* generator, MemberAccess* access);
static void generatePostfix(Generator* generator, PostfixExpression* expression);
static void generateToken(Generator* generator, Token* token);
static void generateNewExpression(Generator* generator, NewExpression* expression);
static void generateArray(Generator* generator, ArrayExpression* expression);
static void generateExpression(Generator* generator, Context* context);
static void generateIndentation(Generator* generator, int32_t depth);
static void generateBlock(Generator* generator, Block* block, int32_t depth);
static void generateFunction(Generator* generator, Function* function);
static void generateFunctions(Generator* generator, Module* module);
static void generateConstructors(Generator* generator, Module* module);
static void generateHeader(Generator* generator, Module* module);

void generateType(Generator* generator, Type* type) {
    const char* output = NULL;
    if (type == &primitives.boolean) {
        output = "bool";
    }
    else if (type == &primitives.i8) {
        output = "int8_t";
    }
    else if (type == &primitives.i16) {
        output = "int16_t";
    }
    else if (type == &primitives.i32) {
        output = "int32_t";
    }
    else if (type == &primitives.i64) {
        output = "int64_t";
    }
    else if (type == &primitives.ui8) {
        output = "uint8_t";
    }
    else if (type == &primitives.ui16) {
        output = "uint16_t";
    }
    else if (type == &primitives.ui32) {
        output = "uint32_t";
    }
    else if (type == &primitives.ui64) {
        output = "uint64_t";
    }
    else if (type == &primitives.f32) {
        output = "float";
    }
    else if (type == &primitives.f64) {
        output = "double";
    }
    else if (type == &primitives.void_) {
        output = "void";
    }
    else if (type == &primitives.string) {
        output = "k_String_t*";
    }
    if (output != NULL) {
        fprintf(generator->output, "%s", output);
    }
    else {
        if (type->tag == TYPE_ARRAY) {
            if (type->array.base == &primitives.i32) {
                fprintf(generator->output, "k_IntegerArray_t*");
            }
            else {
                fprintf(generator->output, "k_Array_t*");
            }
        }
        else if (type->tag == TYPE_STRUCTURE) {
            fprintf(generator->output, "kush_%s*", type->structure->name);
        }
    }
}

void generateForwardReferences(Generator* generator, Module* module) {
    int32_t structureCount = jtk_ArrayList_getSize(module->structures);
    int32_t j;
    for (j = 0; j < structureCount; j++) {
        Structure* structure = (Structure*)jtk_ArrayList_getValue(
            module->structures, j);
        fprintf(generator->output, "typedef struct kush_%s kush_%s;\n", structure->name, structure->name);
    }
    fprintf(generator->output, "\n");

    int32_t functionCount = jtk_ArrayList_getSize(module->functions);
    int32_t i;
    for (i = 0; i < functionCount; i++) {
        Function* function = (Function*)jtk_ArrayList_getValue(
            module->functions, i);
        generateType(generator, function->returnType);
        fprintf(generator->output, " kush_%s(k_Runtime_t* runtime", function->name);
        int32_t parameterCount = jtk_ArrayList_getSize(function->parameters);
        int32_t i;
        for (i = 0; i < parameterCount; i++) {
            Variable* parameter = (Variable*)jtk_ArrayList_getValue(function->parameters, i);
            fprintf(generator->output, ", ");
            generateType(generator, parameter->type);
            fprintf(generator->output, " %s", parameter->name);
        }
        fprintf(generator->output, ");\n");
    }
    fprintf(generator->output, "\n");
}

void generateStructures(Generator* generator, Module* module) {
    int32_t structureCount = jtk_ArrayList_getSize(module->structures);
    int32_t j;
    for (j = 0; j < structureCount; j++) {
        Structure* structure = (Structure*)jtk_ArrayList_getValue(
            module->structures, j);
        fprintf(generator->output, "struct kush_%s {\n", structure->name);
        fprintf(generator->output, "    k_ObjectHeader_t header;\n");

        int32_t declarationCount = jtk_ArrayList_getSize(structure->declarations);
        int32_t i;
        for (i = 0; i < declarationCount; i++) {
            VariableDeclaration* declaration =
                (VariableDeclaration*)jtk_ArrayList_getValue(structure->declarations, i);

            int32_t limit = jtk_ArrayList_getSize(declaration->variables);
            int32_t j;
            for (j = 0; j < limit; j++) {
                Variable* variable = (Variable*)jtk_ArrayList_getValue(declaration->variables, j);
                fprintf(generator->output, "    ");
                generateType(generator, variable->type);
                fprintf(generator->output, " %s;\n", variable->name);
            }
        }

        fprintf(generator->output, "};\n");
    }
    fprintf(generator->output, "\n");

    for (j = 0; j < structureCount; j++) {
        Structure* structure = (Structure*)jtk_ArrayList_getValue(
            module->structures, j);
        fprintf(generator->output, "kush_%s* $%s_new(k_Runtime_t* runtime", structure->name, structure->name);

        int32_t declarationCount = jtk_ArrayList_getSize(structure->declarations);
        int32_t i;
        for (i = 0; i < declarationCount; i++) {
            VariableDeclaration* declaration =
                (VariableDeclaration*)jtk_ArrayList_getValue(structure->declarations, i);

            int32_t limit = jtk_ArrayList_getSize(declaration->variables);
            int32_t j;
            for (j = 0; j < limit; j++) {
                Variable* variable = (Variable*)jtk_ArrayList_getValue(declaration->variables, j);
                fprintf(generator->output, ", ");
                generateType(generator, variable->type);
                fprintf(generator->output, " %s", variable->name);
            }
        }

        fprintf(generator->output, ");\n");
    }
    fprintf(generator->output, "\n");
}

/* Return the type of the first expression, even if there are errors in the
 * right hand side.
 */
void generateAssignment(Generator* generator, BinaryExpression* expression) {
    int32_t count = jtk_ArrayList_getSize(expression->others);
    generateExpression(generator, (Context*)expression->left);

    if (count > 0) {
        int32_t i;
        for (i = 0; i < count; i++) {
            jtk_Pair_t* pair = (jtk_Pair_t*)jtk_ArrayList_getValue(expression->others, i);
            fprintf(generator->output, " %s ", ((Token*)pair->m_left)->text);
            generateExpression(generator, (Context*)pair->m_right);
        }
    }
}

/* Return the type of the first expression, even if there are errors in the
 * right hand side.
 */
void generateBinary(Generator* generator, BinaryExpression* expression) {
    generateExpression(generator, (Context*)expression->left);

    int32_t count = jtk_ArrayList_getSize(expression->others);
    if (count > 0) {
        int32_t i;
        for (i = 0; i < count; i++) {
            jtk_Pair_t* pair = (jtk_Pair_t*)jtk_ArrayList_getValue(expression->others, i);
            fprintf(generator->output, " %s ", ((Token*)pair->m_left)->text);
            generateExpression(generator, (Context*)pair->m_right);
        }
    }
}

void generateConditional(Generator* generator, ConditionalExpression* expression) {
    generateExpression(generator, (Context*)expression->condition);

    if (expression->hook != NULL) {
        fprintf(generator->output, "? ");
        generateExpression(generator, (Context*)expression->then);
        fprintf(generator->output, " : ");
        generateExpression(generator, (Context*)expression->otherwise);
    }
}

void generateUnary(Generator* generator, UnaryExpression* expression) {
    Token* operator = expression->operator;
    if (operator != NULL) {
        fprintf(generator->output, "%s", operator->text);
        generateExpression(generator, (Context*)expression->expression);
    }
    else {
        generateExpression(generator, (Context*)expression->expression);
    }
}

void generateSubscript(Generator* generator, Subscript* subscript) {
    fprintf(generator->output, "->value[");
    generateExpression(generator, (Context*)subscript->expression);
    fprintf(generator->output, "]");
}

void generateFunctionArguments(Generator* generator, FunctionArguments* arguments) {
    fprintf(generator->output, "(runtime");
    int32_t count = jtk_ArrayList_getSize(arguments->expressions);
    int32_t j;
    for (j = 0; j < count; j++) {
        Context* context = (Context*)jtk_ArrayList_getValue(arguments->expressions, j);
        fprintf(generator->output, ", ");
        generateExpression(generator, context);
    }
    fprintf(generator->output, ")");
}

void generateMemberAccess(Generator* generator, MemberAccess* access) {
    fprintf(generator->output, "->%s", access->identifier->text);
}

void generatePostfix(Generator* generator, PostfixExpression* expression) {
    if (expression->token) {
        generateToken(generator, (Token*)expression->primary);
    }
    else {
        fprintf(generator->output, "(");
        generateExpression(generator, (Context*)expression->primary);
        fprintf(generator->output, ")");
    }

    int32_t count = jtk_ArrayList_getSize(expression->postfixParts);
    int32_t i;
    for (i = 0; i < count; i++) {
        Context* postfix = (Context*)jtk_ArrayList_getValue(
            expression->postfixParts, i);

        if (postfix->tag == CONTEXT_SUBSCRIPT) {
            generateSubscript(generator, (Subscript*)postfix);
        }
        else if (postfix->tag == CONTEXT_FUNCTION_ARGUMENTS) {
            generateFunctionArguments(generator, (FunctionArguments*)postfix);
        }
        else if (postfix->tag == CONTEXT_MEMBER_ACCESS) {
            generateMemberAccess(generator, (MemberAccess*)postfix);
        }
        else {
            controlError();
            break;
        }
    }
}

void generateToken(Generator* generator, Token* token) {
    switch (token->type) {
        case TOKEN_KEYWORD_TRUE:
        case TOKEN_KEYWORD_FALSE: {
            fprintf(generator->output, "%s", token->text);
            break;
        }

        case TOKEN_IDENTIFIER: {
            bool done = false;
            Symbol* symbol = resolveSymbol(generator->scope, token->text);
            if (symbol->tag == CONTEXT_VARIABLE) {
                Variable* variable = (Variable*)symbol;
                if (variable->type->reference) {
                    fprintf(generator->output, "((");
                    generateType(generator, variable->type);
                    fprintf(generator->output, "*)$stackFrame->pointers)[%d]", variable->index);
                    done = true;
                }
            }

            if (!done) {
                fprintf(generator->output, "kush_%s", token->text);
            }

            break;
        }

        case TOKEN_INTEGER_LITERAL: {
            // TODO
            fprintf(generator->output, "%s", token->text);
            break;
        }

        case TOKEN_FLOATING_POINT_LITERAL: {
            fprintf(generator->output, "%s", token->text);
            break;
        }

        case TOKEN_STRING_LITERAL: {
            fprintf(generator->output, "makeString(runtime, \"%.*s\")", token->length - 2, token->text + 1);
            break;
        }

        case TOKEN_KEYWORD_NULL: {
            fprintf(generator->output, "NULL");
            break;
        }

        default: {
            fprintf(generator->output, "[internal error] Control should not reach here.\n");
        }
    }
}

void generateArraySuffix(Generator* generator, Type* base) {
    const char* output = NULL;
    if (base == &primitives.boolean) {
        output = "boolean";
    }
    else if (base == &primitives.i8) {
        output = "i8";
    }
    else if (base == &primitives.i16) {
        output = "i16";
    }
    else if (base == &primitives.i32) {
        output = "i32";
    }
    else if (base == &primitives.i64) {
        output = "i64";
    }
    else if (base == &primitives.ui8) {
        output = "ui8";
    }
    else if (base == &primitives.ui16) {
        output = "ui16";
    }
    else if (base == &primitives.ui32) {
        output = "ui32";
    }
    else if (base == &primitives.ui64) {
        output = "ui64";
    }
    else if (base == &primitives.f32) {
        output = "f32";
    }
    else if (base == &primitives.f64) {
        output = "f64";
    }
    else {
        output = "ref";
    }
    fprintf(generator->output, "%s", output);
}

// TODO: Remove declarations and keep only variables?
void generateObjectExpression(Generator* generator, NewExpression* expression) {
    Type* type = expression->type;
    Structure* structure = type->structure;

    int32_t count = 0;
    int32_t declarationCount = jtk_ArrayList_getSize(structure->declarations);
    int32_t i;
    for (i = 0; i < declarationCount; i++) {
        VariableDeclaration* declaration =
            (VariableDeclaration*)jtk_ArrayList_getValue(structure->declarations, i);
        count += jtk_ArrayList_getSize(declaration->variables);
    }

    Context** arguments = (Context**)malloc(sizeof (Context*) * count);
    for (i = 0; i < count; i++) {
        arguments[i] = NULL;
    }

    int32_t limit = jtk_ArrayList_getSize(expression->entries);
    for (i = 0; i < limit; i++) {
        jtk_Pair_t* pair = (jtk_Pair_t*)jtk_ArrayList_getValue(expression->entries, i);
        Token* identifier = (Token*)pair->m_left;
        Context* expression = (Context*)pair->m_right;
        int32_t index = -1;
        int32_t j;
        int32_t m = 0;
        for (j = 0; j < declarationCount; j++) {
            VariableDeclaration* declaration =
                (VariableDeclaration*)jtk_ArrayList_getValue(structure->declarations, j);
            int32_t variableCount = jtk_ArrayList_getSize(declaration->variables);
            int32_t k;
            for (k = 0; k < variableCount; k++) {
                Variable* variable = (Variable*)jtk_ArrayList_getValue(declaration->variables, k);
                if (jtk_CString_equals(variable->name, variable->nameSize,
                    identifier->text, identifier->length)) {
                    index = m;
                    break;
                }
                m++;
            }
        }

        if (index != -1) {
            arguments[index] = expression;
        }
    }

    fprintf(generator->output, "$%s_new(runtime", structure->name);
    for (i = 0; i < count; i++) {
        fprintf(generator->output, ", ");
        if (arguments[i] != NULL) {
            generateExpression(generator, arguments[i]);
        }
        else {
            fprintf(generator->output, "0");
        }
    }
    fprintf(generator->output, ")");
}

void generateNewExpression(Generator* generator, NewExpression* expression) {
    Type* type = expression->type;
    if (type->tag == TYPE_ARRAY) {
        fprintf(generator->output, "makeArray_");
        generateArraySuffix(generator, type->array.base);
        fprintf(generator->output, "(runtime, %d", type->array.dimensions);
        int32_t limit = jtk_ArrayList_getSize(expression->expressions);
        int32_t i;
        for (i = 0; i < limit; i++) {
            fprintf(generator->output, ", ");
            Context* context = (Context*)jtk_ArrayList_getValue(expression->expressions, i);
            generateExpression(generator, context);
        }
        fprintf(generator->output, ")");
    }
    else {
        generateObjectExpression(generator, expression);
    }
}

void generateArray(Generator* generator, ArrayExpression* expression) {
    int32_t limit = jtk_ArrayList_getSize(expression->expressions);
    fprintf(generator->output, "arrayLiteral_");
    generateArraySuffix(generator, expression->type->array.base);
    fprintf(generator->output, "(runtime, %d", limit);

    int32_t i;
    for (i = 0; i < limit; i++) {
        fprintf(generator->output, ", ");
        Context* argument = (Context*)jtk_ArrayList_getValue(expression->expressions, i);
        generateExpression(generator, argument);
    }

    fprintf(generator->output, ")");
}

void generateExpression(Generator* generator, Context* context) {
    switch (context->tag) {
        case CONTEXT_ASSIGNMENT_EXPRESSION: {
            generateAssignment(generator, (BinaryExpression*)context);
            break;
        }

        case CONTEXT_LOGICAL_OR_EXPRESSION:
        case CONTEXT_LOGICAL_AND_EXPRESSION:
        case CONTEXT_INCLUSIVE_OR_EXPRESSION:
        case CONTEXT_EXCLUSIVE_OR_EXPRESSION:
        case CONTEXT_AND_EXPRESSION:
        case CONTEXT_EQUALITY_EXPRESSION:
        case CONTEXT_RELATIONAL_EXPRESSION:
        case CONTEXT_SHIFT_EXPRESSION:
        case CONTEXT_ADDITIVE_EXPRESSION:
        case CONTEXT_MULTIPLICATIVE_EXPRESSION: {
            generateBinary(generator, (BinaryExpression*)context);
            break;
        }

        case CONTEXT_CONDITIONAL_EXPRESSION: {
            generateConditional(generator, (ConditionalExpression*)context);
            break;
        }

        case CONTEXT_UNARY_EXPRESSION: {
            generateUnary(generator, (UnaryExpression*)context);
            break;
        }

        case CONTEXT_POSTFIX_EXPRESSION: {
            generatePostfix(generator, (PostfixExpression*)context);
            break;
        }

        case CONTEXT_NEW_EXPRESSION: {
            generateNewExpression(generator, (NewExpression*)context);
           break;
        }

        case CONTEXT_ARRAY_EXPRESSION: {
            generateArray(generator, (ArrayExpression*)context);
           break;
        }

        default: {
            controlError();
            break;
        }
    }
}

void generateIndentation(Generator* generator, int32_t depth) {
    int32_t i;
    for (i = 0; i < depth; i++) {
        fprintf(generator->output, "    ");
    }
}

void generateBlock(Generator* generator, Block* block, int32_t depth) {
    fprintf(generator->output, "{\n");

    generator->scope = block->scope;
    int32_t limit = jtk_ArrayList_getSize(block->statements);
    if (limit > 0) {
        depth++;
        generateIndentation(generator, depth);

        int32_t i;
        for (i = 0; i < limit; i++) {
            Context* context = (Context*)jtk_ArrayList_getValue(
                block->statements, i);
            switch (context->tag) {
                case CONTEXT_ITERATIVE_STATEMENT: {
                    IterativeStatement* statement = (IterativeStatement*)context;

                    if (statement->name != NULL) {
                        fprintf(generator->output, "%s: ", statement->name);
                    }

                    if (statement->keyword->type == TOKEN_KEYWORD_WHILE) {
                        fprintf(generator->output, "while (");
                        generateExpression(generator, (Context*)statement->expression);
                        fprintf(generator->output, ") ");
                    }

                    generateBlock(generator, statement->body, depth);

                    if (statement->name != NULL) {
                        generateIndentation(generator, depth);
                        fprintf(generator->output, "__%sExit:\n", statement->name);
                    }

                    break;
                }

                case CONTEXT_IF_STATEMENT: {
                    IfStatement* statement = (IfStatement*)context;
                    fprintf(generator->output, "if (");
                    generateExpression(generator, (Context*)statement->ifClause->expression);
                    fprintf(generator->output, ") ");
                    generateBlock(generator, statement->ifClause->body, depth);

                    int32_t count = jtk_ArrayList_getSize(statement->elseIfClauses);
                    int32_t j;
                    for (j = 0; j < count; j++) {
                        generateIndentation(generator, depth);
                        IfClause* clause = (IfClause*)jtk_ArrayList_getValue(
                            statement->elseIfClauses, j);
                        fprintf(generator->output, "else if (");
                        generateExpression(generator, (Context*)clause->expression);
                        fprintf(generator->output, ") ");
                        generateBlock(generator, clause->body, depth);
                    }

                    if (statement->elseClause != NULL) {
                        generateIndentation(generator, depth);
                        fprintf(generator->output, "else ");
                        generateBlock(generator, statement->elseClause, depth);
                    }

                    break;
                }

                /*
                case CONTEXT_TRY_STATEMENT: {
                    TryStatement* statement = (TryStatement*)context;
                    defineLocals(generator, statement->tryClause);

                    int32_t count = jtk_ArrayList_getSize(statement->catchClauses);
                    int32_t j;
                    for (j = 0; j < count; j++) {
                        CatchClause* clause = (CatchClause*)jtk_ArrayList_getValue(
                            statement->catchClauses, j);
                        Variable* parameter = clause->parameter;
                        Scope* localScope = defineLocals(generator, clause->body);

                        if (isUndefined(localScope, parameter->identifier->text)) {
                            defineSymbol(localScope, (Symbol*)parameter);
                        }
                        else {
                            handleSemanticError(handler, generator, ERROR_REDECLARATION_AS_CATCH_PARAMETER,
                                parameter->identifier);
                        }
                    }

                    if (statement->finallyClause != NULL) {
                        defineLocals(generator, statement->finallyClause);
                    }

                break;
                }*/

                case CONTEXT_VARIABLE_DECLARATION: {
                    VariableDeclaration* statement = (VariableDeclaration*)context;
                    int32_t count = jtk_ArrayList_getSize(statement->variables);
                    int32_t j;
                    for (j = 0; j < count; j++) {
                        Variable* variable = (Variable*)jtk_ArrayList_getValue(
                            statement->variables, j);

                        // TODO: Capture parameters in pointers!
                        if (variable->type->reference) {
                            if (variable->expression != NULL) {
                                fprintf(generator->output, "$stackFrame->pointers[%d] = (void*)",
                                    variable->index);
                                generateExpression(generator, (Context*)variable->expression);
                            }
                        }
                        else {
                            generateType(generator, variable->type);
                            fprintf(generator->output, " kush_%s", variable->name);
                            if (variable->expression != NULL) {
                                fprintf(generator->output, " = ");
                                generateExpression(generator, (Context*)variable->expression);
                            }
                        }
                        fprintf(generator->output, ";\n");
                    }
                    break;
                }

                case CONTEXT_ASSIGNMENT_EXPRESSION: {
                    generateExpression(generator, context);
                    fprintf(generator->output, ";\n");
                    break;
                }

                case CONTEXT_BREAK_STATEMENT: {
                    BreakStatement* statement = (BreakStatement*)context;
                    if (statement->identifier != NULL) {
                        fprintf(generator->output, "goto __%sExit;\n", statement->identifier->text);
                    }
                    else {
                        fprintf(generator->output, "break;\n");
                    }
                    break;
                }


                case CONTEXT_RETURN_STATEMENT: {
                    ReturnStatement* statement = (ReturnStatement*)context;
                    fprintf(generator->output, "kush_return(");
                    generateExpression(generator, (Context*)statement->expression);
                    fprintf(generator->output, ");\n");

                    break;
                }

                /*
                case CONTEXT_THROW_STATEMENT: {
                    generateThrowStatement(generator, (ThrowStatement*)context);
                    break;
                }
                */

                default: {
                    controlError();
                    break;
                }
            }
            if (i + 1 < limit) {
                generateIndentation(generator, depth);
            }
        }
        generateIndentation(generator, depth - 1);
    }
    else {
        generateIndentation(generator, depth);
    }

    fprintf(generator->output, "}\n");
    invalidate(generator);
}

void generateFunction(Generator* generator, Function* function) {
    generator->scope = function->scope;

    generator->index = 0;
    generateType(generator, function->returnType);
    fprintf(generator->output, " kush_%s(k_Runtime_t* runtime", function->name);

    int32_t parameterCount = jtk_ArrayList_getSize(function->parameters);
    int32_t i;
    for (i = 0; i < parameterCount; i++) {
        fprintf(generator->output, ", ");
        Variable* parameter = (Variable*)jtk_ArrayList_getValue(function->parameters, i);
        generateType(generator, parameter->type);
        fprintf(generator->output, " kush_%s", parameter->name);
    }

    // TODO: Variable parameter

    fprintf(generator->output, ") {\n");

    fprintf(generator->output, "    k_StackFrame_t* $stackFrame = k_Runtime_pushStackFrame(runtime, \"%s\", %d, %d);\n    ",
        function->name, function->nameSize, function->totalReferences);

    for (i = 0; i < parameterCount; i++) {
        Variable* parameter = (Variable*)jtk_ArrayList_getValue(function->parameters, i);

        if (parameter->type->reference) {
            fprintf(generator->output, "    $stackFrame->pointers[%d] = kush_%s;\n",
                parameter->index, parameter->name);
        }
    }

    generateBlock(generator, function->body, 1);
    fprintf(generator->output, "    k_Runtime_popStackFrame(runtime);\n}\n\n");

    invalidate(generator);
}

void generateFunctions(Generator* generator, Module* module) {
    int32_t functionCount = jtk_ArrayList_getSize(module->functions);
    int32_t i;
    for (i = 0; i < functionCount; i++) {
        Function* function = (Function*)jtk_ArrayList_getValue(
            module->functions, i);
        generateFunction(generator, function);
    }
}

void generateConstructors(Generator* generator, Module* module) {
    int32_t structureCount = jtk_ArrayList_getSize(module->structures);
    int32_t j;
    for (j = 0; j < structureCount; j++) {
        Structure* structure = (Structure*)jtk_ArrayList_getValue(
            module->structures, j);

        fprintf(generator->output, "kush_%s* $%s_new(k_Runtime_t* runtime", structure->name, structure->name);

        int32_t declarationCount = jtk_ArrayList_getSize(structure->declarations);
        int32_t i;
        for (i = 0; i < declarationCount; i++) {
            VariableDeclaration* declaration =
                (VariableDeclaration*)jtk_ArrayList_getValue(structure->declarations, i);

            int32_t limit = jtk_ArrayList_getSize(declaration->variables);
            int32_t j;
            for (j = 0; j < limit; j++) {
                Variable* variable = (Variable*)jtk_ArrayList_getValue(declaration->variables, j);
                fprintf(generator->output, ", ");
                generateType(generator, variable->type);
                fprintf(generator->output, " %s", variable->name);
            }
        }

        fprintf(generator->output, ") {\n");

        int32_t references = 0;
        for (i = 0; i < declarationCount; i++) {
            VariableDeclaration* declaration =
                (VariableDeclaration*)jtk_ArrayList_getValue(structure->declarations, i);

            int32_t limit = jtk_ArrayList_getSize(declaration->variables);
            int32_t j;
            for (j = 0; j < limit; j++) {
                Variable* variable = (Variable*)jtk_ArrayList_getValue(declaration->variables, j);
                if (variable->type->reference) {
                    references++;
                }
            }
        }

        fprintf(generator->output, "    k_StackFrame_t* $stackFrame = k_Runtime_pushStackFrame(runtime, \"$%s_new\", %d, %d);\n\n",
            structure->name, structure->nameSize + 5, references + 1);
        fprintf(generator->output, "    kush_%s* self = (kush_%s*)k_Allocator_allocate(runtime->allocator, sizeof (kush_%s));\n",
            structure->name, structure->name, structure->name);
        fprintf(generator->output, "    self->header.type = K_OBJECT_STRUCTURE_INSTANCE;\n\n");
        fprintf(generator->output, "    $stackFrame->pointers[0] = self;\n");

        int32_t index = 1;
        for (i = 0; i < declarationCount; i++) {
            VariableDeclaration* declaration =
                (VariableDeclaration*)jtk_ArrayList_getValue(structure->declarations, i);

            int32_t limit = jtk_ArrayList_getSize(declaration->variables);
            int32_t j;
            for (j = 0; j < limit; j++) {
                Variable* variable = (Variable*)jtk_ArrayList_getValue(declaration->variables, j);
                if (variable->type->reference) {
                    fprintf(generator->output, "    $stackFrame->pointers[%d] = %s;\n",
                        index, variable->name);
                    index++;
                }
            }
        }

        fprintf(generator->output, "\n");
        for (i = 0; i < declarationCount; i++) {
            VariableDeclaration* declaration =
                (VariableDeclaration*)jtk_ArrayList_getValue(structure->declarations, i);

            int32_t limit = jtk_ArrayList_getSize(declaration->variables);
            int32_t j;
            for (j = 0; j < limit; j++) {
                Variable* variable = (Variable*)jtk_ArrayList_getValue(declaration->variables, j);
                fprintf(generator->output, "    self->%s = %s;\n", variable->name,
                    variable->name);
            }
        }

        fprintf(generator->output, "    kush_return(self);\n}");
    }

    fprintf(generator->output, "\n\n");
}

void generateHeader(Generator* generator, Module* module) {
    fprintf(generator->output, "// Do not edit this file.\n"
        "// It was automatically generated by kush v%d.%d.\n\n",
        KUSH_VERSION_MAJOR, KUSH_VERSION_MINOR);
    fprintf(generator->output, "#pragma once\n\n");
    fprintf(generator->output, "#include \"kush-runtime.h\"\n\n");

    generateForwardReferences(generator, module);
    generateStructures(generator, module);
}

void generateSource(Generator* generator, Module* module, const uint8_t* headerName) {
    fprintf(generator->output, "// Do not edit this file.\n"
        "// It was automatically generated by kush v%d.%d.\n\n",
        KUSH_VERSION_MAJOR, KUSH_VERSION_MINOR);
    fprintf(generator->output, "#include \"%s\"\n\n", headerName);

    generateConstructors(generator, module);
    generateFunctions(generator, module);
}

void generateC(Generator* generator, Module* module) {
    Compiler* compiler = generator->compiler;
    const uint8_t* path = (const uint8_t*)jtk_ArrayList_getValue(compiler->inputFiles,
        compiler->currentFileIndex);

    int pathSize = jtk_CString_getSize(path);
    uint8_t* sourceName = allocate(uint8_t, pathSize - 3);
    uint8_t* headerName = allocate(uint8_t, pathSize - 3);

    int32_t i;
    for (i = 0; i < pathSize - 4; i++) {
        sourceName[i] = path[i];
        headerName[i] = path[i];
    }
    sourceName[i] = 'c';
    sourceName[i + 1] = '\0';
    headerName[i] = 'h';
    headerName[i + 1] = '\0';

    generator->output = fopen(sourceName, "w+");
    generateSource(generator, module, headerName);
    fclose(generator->output);

    generator->output = fopen(headerName, "w+");
    generateHeader(generator, module);
    fclose(generator->output);

    deallocate(sourceName);
    deallocate(headerName);
}

Generator* newGenerator(Compiler* compiler) {
    Generator* generator = allocate(Generator, 1);
    generator->compiler = compiler;
    generator->scope = NULL;
    generator->index = 0;
    return generator;
}

void deleteGenerator(Generator* generator) {
    deallocate(generator);
}
