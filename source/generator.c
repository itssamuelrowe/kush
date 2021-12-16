#include <kush/generator.h>

void generateFunctions(Generator* generator);
void generateStructures(Generator* generator);
bool generateLLVM(Generator* generator, Module* module, const char* name);

void generateStructures(Generator* generator) {
}

void generateFunctions(Generator* generator) {
}

bool generateLLVM(Generator* generator, Module* module, const char* name) {
    LLVMModuleRef llvmModule = LLVMModuleCreateWithName(name);
	LLVMSetDataLayout(llvmModule, "");
	LLVMSetTarget(llvmModule, LLVMGetDefaultTargetTriple());

    generator->module = llvmModule;
    generator->builder = LLVMCreateBuilder();

    generateStructures(generator);
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