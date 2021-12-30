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
    if (context->operator != NULL) {
        // TODO: Generate code
        return generateUnary(generator, (UnaryExpression*)context->expression);
    }

    return generatePostfix(generator, (PostfixExpression*)context->expression);
}

LLVMValueRef generateMultiplicative(Generator* generator, BinaryExpression* context) {
    LLVMValueRef result = generateUnary(generator, (BinaryExpression*)context->left);

    for (int32_t i = 0; i < context->others->m_size; i++) {
        jtk_Pair_t* pair = (jtk_Pair_t*)context->others->m_values[i];
        /* TODO: Update result */
        generateUnary(generator, (BinaryExpression*)pair->m_right);
    }

    return result;
}

LLVMValueRef generateAdditive(Generator* generator, BinaryExpression* context) {
    LLVMValueRef result = generateMultiplicative(generator, context->left);

    for (int32_t i = 0; i < context->others->m_size; i++) {
        jtk_Pair_t* pair = (jtk_Pair_t*)context->others->m_values[i];
        /* TODO: Update result */
        generateMultiplicative(generator, (BinaryExpression*)pair->m_right);
    }

    return result;
}

LLVMValueRef generateShift(Generator* generator, BinaryExpression* context) {
    LLVMValueRef result = generateAdditive(generator, (BinaryExpression*)context->left);

    for (int32_t i = 0; i < context->others->m_size; i++) {
        jtk_Pair_t* pair = (jtk_Pair_t*)context->others->m_values[i];
        /* TODO: Update result */
        generateAdditive(generator, (BinaryExpression*)pair->m_right);
    }

    return result;
}

LLVMValueRef generateRelational(Generator* generator, BinaryExpression* context) {
    LLVMValueRef result = generateShift(generator, (BinaryExpression*)context->left);

    for (int32_t i = 0; i < context->others->m_size; i++) {
        jtk_Pair_t* pair = (jtk_Pair_t*)context->others->m_values[i];
        /* TODO: Update result */
        generateShift(generator, (BinaryExpression*)pair->m_right);
    }

    return result;
}

LLVMValueRef generateEquality(Generator* generator, BinaryExpression* context) {
    LLVMValueRef result = generateRelational(generator, (BinaryExpression*)context->left);

    for (int32_t i = 0; i < context->others->m_size; i++) {
        jtk_Pair_t* pair = (jtk_Pair_t*)context->others->m_values[i];
        /* TODO: Update result */
        generateRelational(generator, (BinaryExpression*)pair->m_right);
    }

    return result;
}

LLVMValueRef generateAnd(Generator* generator, BinaryExpression* context) {
    LLVMValueRef result = generateEquality(generator, (BinaryExpression*)context->left);

    for (int32_t i = 0; i < context->others->m_size; i++) {
        jtk_Pair_t* pair = (jtk_Pair_t*)context->others->m_values[i];
        /* TODO: Update result */
        generateEquality(generator, (BinaryExpression*)pair->m_right);
    }

    return result;
}

LLVMValueRef generateExclusiveOr(Generator* generator, BinaryExpression* context) {
    LLVMValueRef result = generateAnd(generator, (BinaryExpression*)context->left);

    for (int32_t i = 0; i < context->others->m_size; i++) {
        jtk_Pair_t* pair = (jtk_Pair_t*)context->others->m_values[i];
        /* TODO: Update result */
        generateAnd(generator, (BinaryExpression*)pair->m_right);
    }

    return result;
}

LLVMValueRef generateInclusiveOr(Generator* generator, BinaryExpression* context) {
    LLVMValueRef result = generateExclusiveOr(generator, (BinaryExpression*)context->left);

    for (int32_t i = 0; i < context->others->m_size; i++) {
        jtk_Pair_t* pair = (jtk_Pair_t*)context->others->m_values[i];
        /* TODO: Update result */
        generateExclusiveOr(generator, (BinaryExpression*)pair->m_right);
    }

    return result;
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

void generateBlock(Generator* generator, Block* block, int32_t depth) {
    generator->scope = block->scope;
    
    int32_t statementCount = block->statements->m_size;
    for (int i = 0; i < statementCount; i++) {
        Context* context = (Context*)block->statements->m_values[i];
        switch (context->tag) {
            case CONTEXT_RETURN_STATEMENT: {
                ReturnStatement* statement = (ReturnStatement*)context;
                if (statement->expression != NULL) {
                    LLVMValueRef llvmValue = generateExpression(generator, (Context*)statement->expression);
                    LLVMBuildRet(generator->llvmBuilder, llvmValue);
                }
                else {
                    LLVMBuildRetVoid(generator->llvmBuilder);
                }
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