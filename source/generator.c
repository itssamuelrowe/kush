#include <kush/generator.h>
#include <jtk/collection/Pair.h>

LLVMValueRef generateArray(Generator* generator, ArrayExpression* context);
LLVMValueRef generateNew(Generator* generator, NewExpression* context);
int32_t getRadix(const uint8_t** text, int32_t* length);
LLVMValueRef generatePrimary(Generator* generator, void* context, bool token);
LLVMValueRef generateSubscript(Generator* generator, Subscript* context);
LLVMValueRef generateFunctionArguments(Generator* generator, FunctionArguments* context);
LLVMValueRef generateMemberAccess(Generator* generator, MemberAccess* context);
LLVMValueRef generatePostfix(Generator* generator, PostfixExpression* context);
LLVMValueRef generateUnary(Generator* generator, UnaryExpression* context);
LLVMValueRef generateMultiplicative(Generator* generator, BinaryExpression* context);
LLVMValueRef generateAdditive(Generator* generator, BinaryExpression* context);
LLVMValueRef generateShift(Generator* generator, BinaryExpression* context);
LLVMValueRef generateRelational(Generator* generator, BinaryExpression* context);
LLVMValueRef generateEquality(Generator* generator, BinaryExpression* context);
LLVMValueRef generateAnd(Generator* generator, BinaryExpression* context);
LLVMValueRef generateExclusiveOr(Generator* generator, BinaryExpression* context);
LLVMValueRef generateInclusiveOr(Generator* generator, BinaryExpression* context);
LLVMValueRef generateLogicalAnd(Generator* generator, BinaryExpression* context);
LLVMValueRef generateLogicalOr(Generator* generator, BinaryExpression* context);
LLVMValueRef generateConditional(Generator* generator, ConditionalExpression* context);
LLVMValueRef generateAssignment(Generator* generator, BinaryExpression* context);
LLVMValueRef generateExpression(Generator* generator, Context* context);
void generateBlock(Generator* generator, Block* block, int32_t depth);
void generateFunction(Generator* generator, Function* function);
void generateFunctions(Generator* generator, Module* module);
void generateStructures(Generator* generator, Module* module);
bool generateLLVM(Generator* generator, Module* module, const char* name);

#define invalidate(generator) generator->scope = generator->scope->parent

LLVMValueRef generateArray(Generator* generator, ArrayExpression* context) {
    LLVMValueRef result;
    return result;
}

LLVMValueRef generateNew(Generator* generator, NewExpression* context) {
    LLVMValueRef result;
    return result;
}

int32_t getRadix(const uint8_t** text, int32_t* length) {
    int32_t radix = 10;
    if (length > 2) {
        uint8_t c = (*text)[1];
        switch (c) {
            case 'b':
            case 'B': {
                radix = 2;
                break;
            }

            case 'c':
            case 'C': {
                radix = 8;
                break;
            }

            case 'x':
            case 'X': {
                radix = 16;
                break;
            }

            default: {
                if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) {
                    fprintf(stderr, "[error] getRadix(): invalid integer literal %s\n", *text);
                }
                break;
            }
        }
    }

    if (radix != 10) {
        *text += 2;
        *length -= 2;
    }

    return radix;
}

LLVMValueRef generatePrimary(Generator* generator, void* context, bool token) {
    LLVMValueRef result;
    if (token) {
        Token* token0 = (Token*)context;
        switch (token0->type) {
            case TOKEN_INTEGER_LITERAL: {
                const uint8_t* text = token0->text;
                int32_t length = token0->length; 
                int32_t radix = getRadix(&text, &length);
                result = LLVMConstIntOfStringAndSize(
                    LLVMInt32TypeInContext(generator->llvmContext), text, length, radix);
                break;
            }

            case TOKEN_FLOATING_POINT_LITERAL: {
                result = LLVMConstRealOfStringAndSize(
                    LLVMDoubleTypeInContext(generator->llvmContext), token0->text,
                        token0->length);
                break;
            }

            case TOKEN_KEYWORD_TRUE: {
                result = LLVMConstIntOfStringAndSize(
                    LLVMInt8TypeInContext(generator->llvmContext), "1", 1, 10);
                break;
            }

            case TOKEN_KEYWORD_FALSE: {
                result = LLVMConstIntOfStringAndSize(
                    LLVMInt8TypeInContext(generator->llvmContext), "0", 1, 10);
                break;
            }

            case TOKEN_KEYWORD_NULL: {
                result = LLVMConstNull(
                    LLVMPointerType(LLVMVoidTypeInContext(generator->llvmContext), 0)
                );
                break;
            }

            default: {
                controlError();
                break;
            }
        }
    }
    else {
        Context* context0 = (Context*)context;
        switch (context0->tag) {
            case CONTEXT_ASSIGNMENT_EXPRESSION: {
                result = generateExpression(generator, (BinaryExpression*)context0);
                break;
            }

            case CONTEXT_NEW_EXPRESSION: {
                result = generateNew(generator, (NewExpression*)context0);
                break;
            }

            case CONTEXT_ARRAY_EXPRESSION: {
                result = generateArray(generator, (ArrayExpression*)context0);
                break;
            }
        }
    }

    return result;
}

LLVMValueRef generateSubscript(Generator* generator, Subscript* context) {
    LLVMValueRef result;
    return result;
}

LLVMValueRef generateFunctionArguments(Generator* generator, FunctionArguments* context) {
    LLVMValueRef result;
    return result;
}

LLVMValueRef generateMemberAccess(Generator* generator, MemberAccess* context) {
    LLVMValueRef result;
    return result;
}

LLVMValueRef generatePostfix(Generator* generator, PostfixExpression* context) {
    LLVMValueRef result = generatePrimary(generator, context->primary, context->token);

    for (int32_t i = 0; i < context->postfixParts->m_size; i++) {
        Context* postfixPart = context->postfixParts->m_values[i];
        switch (postfixPart->tag) {
            case CONTEXT_SUBSCRIPT: {
                generateSubscript(generator, (Subscript*)postfixPart);
                break;
            }

            case CONTEXT_FUNCTION_ARGUMENTS: {
                generateFunctionArguments(generator, (FunctionArguments*)postfixPart);
                break;
            }

            case CONTEXT_MEMBER_ACCESS: {
                generateMemberAccess(generator, (MemberAccess*)postfixPart);
                break;
            }

            default: {
                fprintf(stderr, "[erorr] generatePostfix(): invalid postfixPart->tag");
                break;
            }
        }
    }

    return result;
}

LLVMValueRef generateUnary(Generator* generator, UnaryExpression* context) {
    LLVMValueRef lhs;
    if (context->operator != NULL) {
        lhs = generateUnary(generator, (UnaryExpression*)context->expression);
        switch (context->operator->type) {
            /* The `+` unary operator does nothing. */
            case TOKEN_PLUS: {
                break;
            }

            case TOKEN_DASH: {
                LLVMValueRef rhs = LLVMConstIntOfStringAndSize(
                    LLVMInt32TypeInContext(generator->llvmContext), "-1", 2, 10);
                LLVMBuildMul(generator->llvmBuilder, lhs, rhs, "negate");
                break;
            }

            /* Both `~` and `!` operators generate the same instruction. The semantic analyzer
             * is responsible for ensuring `!` always works with boolean values. Otherwise
             * the result will be "undefined".
             * 
             * TODO: Does not work for some reason.
             */
            case TOKEN_TILDE:
            case TOKEN_EXCLAMATION_MARK: {
                LLVMBuildNot(generator->llvmBuilder, lhs, "bitwise_not");
                break;
            }

            default: {
                fprintf(stderr, "[error] generateUnary(): invalid token type\n");
            }
        }
    }
    else {
        lhs = generatePostfix(generator, (PostfixExpression*)context->expression);
    }

    return lhs;
}

LLVMValueRef generateMultiplicative(Generator* generator, BinaryExpression* context) {
    LLVMValueRef lhs = generateUnary(generator, (BinaryExpression*)context->left);

    for (int32_t i = 0; i < context->others->m_size; i++) {
        jtk_Pair_t* pair = (jtk_Pair_t*)context->others->m_values[i];
        LLVMValueRef rhs = generateUnary(generator, (BinaryExpression*)pair->m_right);

        Token* token = (Token*)pair->m_left;
        switch (token->type) {
            case TOKEN_ASTERISK: {
                lhs = LLVMBuildMul(generator->llvmBuilder, lhs, rhs, "multiply");
                break;
            }

            case TOKEN_FORWARD_SLASH: {
                lhs = LLVMBuildSDiv(generator->llvmBuilder, lhs, rhs, "division_for_quotient");
                break;
            }

            case TOKEN_MODULUS: {
                lhs = LLVMBuildSRem(generator->llvmBuilder, lhs, rhs, "division_for_remainder");
                break;
            }

            default: {
                fprintf(stderr, "[error] generateMultiplicative(): invalid token type\n");
            }
        }
    }

    return lhs;
}

LLVMValueRef generateAdditive(Generator* generator, BinaryExpression* context) {
    LLVMValueRef lhs = generateMultiplicative(generator, context->left);

    for (int32_t i = 0; i < context->others->m_size; i++) {
        jtk_Pair_t* pair = (jtk_Pair_t*)context->others->m_values[i];
        LLVMValueRef rhs = generateMultiplicative(generator, (BinaryExpression*)pair->m_right);

        Token* token = (Token*)pair->m_left;
        switch (token->type) {
            case TOKEN_PLUS: {
                lhs = LLVMBuildAdd(generator->llvmBuilder, lhs, rhs, "add");
                break;
            }

            case TOKEN_DASH: {
                lhs = LLVMBuildSub(generator->llvmBuilder, lhs, rhs, "subtract");
                break;
            }

            default: {
                fprintf(stderr, "[error] generateAdditive(): invalid token type\n");
                break;
            }
        }
    }

    return lhs;
}

LLVMValueRef generateShift(Generator* generator, BinaryExpression* context) {
    LLVMValueRef lhs = generateAdditive(generator, (BinaryExpression*)context->left);

    for (int32_t i = 0; i < context->others->m_size; i++) {
        jtk_Pair_t* pair = (jtk_Pair_t*)context->others->m_values[i];
        LLVMValueRef rhs = generateAdditive(generator, (BinaryExpression*)pair->m_right);

        Token* token = (Token*)pair->m_left;
        switch (token->type) {
            case TOKEN_LEFT_ANGLE_BRACKET_2: {
                lhs = LLVMBuildShl(generator->llvmBuilder, lhs, rhs, "shift_left");
                break;
            }

            // NOTE: >> is arithmetic shift right and >>> is logical shift right
            case TOKEN_RIGHT_ANGLE_BRACKET_2: {
                lhs = LLVMBuildAShr(generator->llvmBuilder, lhs, rhs, "shift_right");
                break;
            }

            default: {
                fprintf(stderr, "[error] generateShift: invalid token type\n");
                break;
            }
        }
    }

    return lhs;
}

LLVMValueRef generateRelational(Generator* generator, BinaryExpression* context) {
    LLVMValueRef lhs = generateShift(generator, (BinaryExpression*)context->left);

    for (int32_t i = 0; i < context->others->m_size; i++) {
        jtk_Pair_t* pair = (jtk_Pair_t*)context->others->m_values[i];
        LLVMValueRef rhs = generateShift(generator, (BinaryExpression*)pair->m_right);

        TokenType tokenType = ((Token*)pair->m_left)->type;

        if (/*expression->type->tag == TYPE_DECIMAL*/false) {
            LLVMRealPredicate predicate;
            switch (tokenType) {
                case TOKEN_LEFT_ANGLE_BRACKET: {
                    predicate = LLVMRealOLT;
                    break;
                }

                case TOKEN_RIGHT_ANGLE_BRACKET: {
                    predicate = LLVMRealOGT;
                    break;
                }

                case TOKEN_LEFT_ANGLE_BRACKET_EQUAL: {
                    predicate = LLVMRealOLE;
                    break;
                }

                case TOKEN_RIGHT_ANGLE_BRACKET_EQUAL: {
                    predicate = LLVMRealOGE;
                    break;
                }

                default: {
                    controlError();
                    break;
                }
            }
            lhs = LLVMBuildFCmp(generator->llvmBuilder, predicate, lhs, rhs, "relational");
        }
        else {
            LLVMIntPredicate predicate;
            switch (tokenType) {
                case TOKEN_LEFT_ANGLE_BRACKET: {
                    predicate = LLVMIntSLT;
                    break;
                }

                case TOKEN_RIGHT_ANGLE_BRACKET: {
                    predicate = LLVMIntSGT;
                    break;
                }

                case TOKEN_LEFT_ANGLE_BRACKET_EQUAL: {
                    predicate = LLVMIntSLE;
                    break;
                }

                case TOKEN_RIGHT_ANGLE_BRACKET_EQUAL: {
                    predicate = LLVMIntSGE;
                    break;
                }

                default: {
                    controlError();
                    break;
                }
            }
            lhs = LLVMBuildICmp(generator->llvmBuilder, predicate, lhs, rhs, "relational");
        }
    }

    return lhs;
}

LLVMValueRef generateEquality(Generator* generator, BinaryExpression* context) {
    LLVMValueRef lhs = generateRelational(generator, (BinaryExpression*)context->left);

    for (int32_t i = 0; i < context->others->m_size; i++) {
        jtk_Pair_t* pair = (jtk_Pair_t*)context->others->m_values[i];
        LLVMValueRef rhs = generateRelational(generator, (BinaryExpression*)pair->m_right);
        TokenType tokenType = ((Token*)pair->m_left)->type;

        if (/*context->type->tag == TYPE_DECIMAL*/ false) {
            LLVMRealPredicate predicate;
            switch (tokenType) {
                case TOKEN_EQUAL_2: {
                    predicate = LLVMRealOEQ;
                    break;
                }

                case TOKEN_EXCLAMATION_MARK_EQUAL: {
                    predicate = LLVMRealONE;
                    break;
                }

                default: {
                    controlError();
                    break;
                }
            }
            lhs = LLVMBuildFCmp(generator->llvmBuilder, predicate, lhs, rhs, "equality");
        }
        else {
            LLVMIntPredicate predicate;
            switch (tokenType) {
                case TOKEN_EQUAL_2: {
                    predicate = LLVMIntEQ;
                    break;
                }

                case TOKEN_EXCLAMATION_MARK_EQUAL: {
                    predicate = LLVMIntNE;
                    break;
                }

                default: {
                    controlError();
                    break;
                }
            }
            lhs = LLVMBuildICmp(generator->llvmBuilder, predicate, lhs, rhs, "equality");
        }
    }

    return lhs;
}

LLVMValueRef generateAnd(Generator* generator, BinaryExpression* context) {
    LLVMValueRef lhs = generateEquality(generator, (BinaryExpression*)context->left);

    for (int32_t i = 0; i < context->others->m_size; i++) {
        jtk_Pair_t* pair = (jtk_Pair_t*)context->others->m_values[i];
        LLVMValueRef rhs = generateEquality(generator, (BinaryExpression*)pair->m_right);

        lhs = LLVMBuildAnd(generator->llvmBuilder, lhs, rhs, "and");
    }

    return lhs;
}

LLVMValueRef generateExclusiveOr(Generator* generator, BinaryExpression* context) {
    LLVMValueRef lhs = generateAnd(generator, (BinaryExpression*)context->left);

    for (int32_t i = 0; i < context->others->m_size; i++) {
        jtk_Pair_t* pair = (jtk_Pair_t*)context->others->m_values[i];
        LLVMValueRef rhs = generateAnd(generator, (BinaryExpression*)pair->m_right);
        
        lhs = LLVMBuildXor(generator->llvmBuilder, lhs, rhs, "exclusive_or");
    }

    return lhs;
}

LLVMValueRef generateInclusiveOr(Generator* generator, BinaryExpression* context) {
    LLVMValueRef lhs = generateExclusiveOr(generator, (BinaryExpression*)context->left);

    for (int32_t i = 0; i < context->others->m_size; i++) {
        jtk_Pair_t* pair = (jtk_Pair_t*)context->others->m_values[i];
        LLVMValueRef rhs = generateExclusiveOr(generator, (BinaryExpression*)pair->m_right);

        lhs = LLVMBuildOr(generator->llvmBuilder, lhs, rhs, "inclusive_or");
    }

    return lhs;
}

LLVMValueRef generateLogicalAnd(Generator* generator, BinaryExpression* context) {
    LLVMValueRef result = generateInclusiveOr(generator, (BinaryExpression*)context->left);

    for (int32_t i = 0; i < context->others->m_size; i++) {
        jtk_Pair_t* pair = (jtk_Pair_t*)context->others->m_values[i];
        /* TODO: Update result */
        generateInclusiveOr(generator, (BinaryExpression*)pair->m_right);
    }

    return result;
}

LLVMValueRef generateLogicalOr(Generator* generator, BinaryExpression* context) {
    LLVMValueRef result = generateLogicalAnd(generator, (BinaryExpression*)context->left);

    for (int32_t i = 0; i < context->others->m_size; i++) {
        jtk_Pair_t* pair = (jtk_Pair_t*)context->others->m_values[i];
        /* TODO: Update result */
        generateLogicalAnd(generator, (BinaryExpression*)pair->m_right);
    }

    return result;
}

LLVMValueRef generateConditional(Generator* generator, ConditionalExpression* context) {
    LLVMValueRef result = generateLogicalOr(generator, (BinaryExpression*)context->condition);

    if (context->hook != NULL) {
        /* TODO: Update result */
        generateExpression(generator, (Context*)context->then);
        generateConditional(generator, (ConditionalExpression*)context->otherwise);
    }

    return result;
}

LLVMValueRef generateAssignment(Generator* generator, BinaryExpression* context) {
    LLVMValueRef result;

    for (int32_t i = 0; i < context->others->m_size; i++) {
        ConditionalExpression* other = (ConditionalExpression*)context->others->m_values[i];
        result = generateConditional(generator, (ConditionalExpression*)other);
    }

    /* TODO */
    result = generateConditional(generator, (ConditionalExpression*)context->left);

    return result;
}

LLVMValueRef generateExpression(Generator* generator, Context* context) {
    return generateAssignment(generator, (BinaryExpression*)context);
}


void generateReturn(Generator* generator, ReturnStatement* context) {
    if (context->expression != NULL) {
        LLVMValueRef llvmValue = generateExpression(generator, (Context*)context->expression);
        LLVMBuildRet(generator->llvmBuilder, llvmValue);
    }
    else {
        LLVMBuildRetVoid(generator->llvmBuilder);
    }
}

void generateBlock(Generator* generator, Block* block, int32_t depth) {
    generator->scope = block->scope;
    
    int32_t statementCount = block->statements->m_size;
    for (int i = 0; i < statementCount; i++) {
        Context* context = (Context*)block->statements->m_values[i];
        switch (context->tag) {
            case CONTEXT_RETURN_STATEMENT: {
                generateReturn(generator, (ReturnStatement*)context);
                break;
            }

            default: {
                controlError();
                break;
            }
        }
    }

    invalidate(generator);
}

void generateFunction(Generator* generator, Function* function) {
    generator->scope = function->scope;
    generator->function = function;

    int32_t parameterCount = function->parameters->m_size;
    LLVMTypeRef* llvmParameterTypes = allocate(LLVMTypeRef, parameterCount);

    for (int32_t i = 0; i < parameterCount; i++) {
        Variable* parameter = (Variable*)function->parameters->m_values[i];
        llvmParameterTypes[i] = parameter->type->llvmType;
    }

    // TODO: Variable parameter

    LLVMTypeRef llvmFunctionType = LLVMFunctionType(function->returnType->llvmType, llvmParameterTypes, parameterCount, false);
    LLVMValueRef llvmFunction = LLVMAddFunction(generator->llvmModule, function->name, llvmFunctionType);
    function->llvmValue = llvmFunction;

    for (int32_t i = 0; i < parameterCount; i++) {
        Variable* parameter = (Variable*)function->parameters->m_values[i];
        parameter->llvmValue = LLVMGetParam(llvmFunction, i);
        parameter->parameter = true;
    }

    LLVMBasicBlockRef llvmBlock = LLVMAppendBasicBlockInContext(generator->llvmContext, llvmFunction, "");
    generator->llvmBuilder = LLVMCreateBuilder();
    LLVMPositionBuilderAtEnd(generator->llvmBuilder, llvmBlock);

    generateBlock(generator, function->body, 0);

    invalidate(generator);
    deallocate(llvmParameterTypes);
}

void generateFunctions(Generator* generator, Module* module) {
    int32_t functionCount = module->functions->m_size;
    int32_t i;
    for (i = 0; i < functionCount; i++) {
        Function* function = (Function*)module->functions->m_values[i];
        generateFunction(generator, function);
    }
}

void generateStructures(Generator* generator, Module* module) {
    int32_t structureCount = module->structures->m_size;
    int32_t j;
    for (j = 0; j < structureCount; j++) {
        Structure* structure = (Structure*)
            (module->structures->m_values[j]);
        structure->type->llvmType = LLVMStructCreateNamed(generator->llvmContext, structure->name);

        int32_t declarationCount = structure->declarations->m_size;
        int32_t totalVariables = 0;
        for (int i = 0; i < declarationCount; i++) {
            VariableDeclaration* declaration =
                (VariableDeclaration*)structure->declarations->m_values[i];
            totalVariables += declaration->variables->m_size;
        }

        LLVMTypeRef* llvmVariableTypes = allocate(LLVMTypeRef, totalVariables);
        for (int i = 0; i < declarationCount; i++) {
            VariableDeclaration* declaration =
                (VariableDeclaration*)structure->declarations->m_values[i];

            int32_t variableCount = declaration->variables->m_size;
            int32_t m = 0;
            for (int k = 0; k < variableCount; k++) {
                Variable* variable = (Variable*)declaration->variables->m_values[k];
                llvmVariableTypes[m++] = variable->type->llvmType;
            }
        }
        LLVMStructSetBody(structure->type->llvmType, llvmVariableTypes, totalVariables, false);
    }
}

bool generateLLVM(Generator* generator, Module* module, const char* name) {
    LLVMModuleRef llvmModule = LLVMModuleCreateWithNameInContext(name,
        generator->llvmContext);
    LLVMSetDataLayout(llvmModule, "");
    LLVMSetTarget(llvmModule, LLVMGetDefaultTargetTriple());

    generator->llvmModule = llvmModule;
    generator->llvmBuilder = LLVMCreateBuilder();

    generateStructures(generator, module);
    generateFunctions(generator, module);

    char* error = NULL;
    bool invalid = LLVMVerifyModule(generator->llvmModule, LLVMAbortProcessAction, &error);
    if (!invalid) {
        char* data = LLVMPrintModuleToString(generator->llvmModule);
        fprintf(generator->output, data);
        LLVMDisposeMessage(data);
    }
    else {
        fprintf(stderr, "[error] %s\n", error);
    }
    LLVMDisposeMessage(error);

    return !invalid;
}

void generateIR(Generator* generator, Module* module) {
    Compiler* compiler = generator->compiler;
    const uint8_t* path = (const uint8_t*)compiler->inputFiles
        ->m_values[compiler->currentFileIndex];

    int pathSize = jtk_CString_getSize(path);
    uint8_t* sourceName = allocate(uint8_t, pathSize - 1);

    int32_t i;
    for (i = 0; i < pathSize - 4; i++) {
        sourceName[i] = path[i];
    }
    sourceName[i] = 'l';
    sourceName[i + 1] = 'l';
    sourceName[i + 2] = '\0';

    generator->output = fopen(sourceName, "w+");
    generateLLVM(generator, module, sourceName);

    fprintf(generator->output, "\ndefine i32 @main() {\nret i32 0\n}");
    fclose(generator->output);

    deallocate(sourceName);
}

Generator* newGenerator(Compiler* compiler) {
    Generator* generator = allocate(Generator, 1);
    generator->compiler = compiler;
    generator->scope = NULL;
    generator->llvmContext = compiler->llvmContext;
    return generator;
}

void deleteGenerator(Generator* generator) {
    deallocate(generator);
}