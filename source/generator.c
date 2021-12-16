#include <kush/generator.h>

void generateFunctions(Generator* generator);
void generateStructures(Generator* generator, Module* module);
bool generateLLVM(Generator* generator, Module* module, const char* name);

void generateStructures(Generator* generator, Module* module) {
    int32_t structureCount = module->structures->m_size;
    int32_t j;
    for (j = 0; j < structureCount; j++) {
        Structure* structure = (Structure*)
            (module->structures->m_values[j]);
        structure->type->llvmType = LLVMStructCreateNamed(generator->context, structure->name);

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

void generateFunctions(Generator* generator) {
}

bool generateLLVM(Generator* generator, Module* module, const char* name) {
    LLVMContextRef context = LLVMContextCreate();
    LLVMModuleRef llvmModule = LLVMModuleCreateWithNameInContext(name, context);
	LLVMSetDataLayout(llvmModule, "");
	LLVMSetTarget(llvmModule, LLVMGetDefaultTargetTriple());

    generator->context = context;
    generator->module = llvmModule;
    generator->builder = LLVMCreateBuilder();

    generateStructures(generator, module);
    generateFunctions(generator);

	char *error = NULL;
	bool invalid = LLVMVerifyModule(generator->module, LLVMAbortProcessAction, &error);
	if (!invalid) {
		char* data = LLVMPrintModuleToString(generator->module);
		fprintf(generator->output, data);
		LLVMDisposeMessage(data);
	}
    else {
        fprintf(stderr, "[error] %s\n", error);
    }
    LLVMDisposeMessage(error);
    LLVMContextDispose(context);
	
	return !invalid;
}

void generateIR(Generator* generator, Module* module) {
    Compiler* compiler = generator->compiler;
    const uint8_t* path = (const uint8_t*)jtk_ArrayList_getValue(compiler->inputFiles,
        compiler->currentFileIndex);

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
    return generator;
}

void deleteGenerator(Generator* generator) {
    deallocate(generator);
}