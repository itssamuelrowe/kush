/*
 * Copyright 2018-2020 Samuel Rowe
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

// Monday, March 16, 2020

#include <stdio.h>
#include <string.h>

/* The JTK_LOGGER_DISABLE constant is defined in Configuration.h. Therefore,
 * make sure it is included before any other file which may
 * include Logger.h!
 */
#include <kush/configuration.h>

#include <jtk/collection/list/ArrayList.h>
#include <jtk/collection/array/Arrays.h>
#include <jtk/fs/FileInputStream.h>
#include <jtk/fs/Path.h>
#include <jtk/io/BufferedInputStream.h>
#include <jtk/io/InputStream.h>
#include <jtk/log/ConsoleLogger.h>
#include <jtk/core/CStringObjectAdapter.h>
#include <jtk/core/CString.h>

#include <kush/compiler.h>
#include <kush/lexer.h>
#include <kush/parser.h>
#include <kush/error-handler.h>
#include <kush/analyzer.h>
#include <kush/generator.h>

// Error

void printErrors(Compiler* compiler);

// Phase

static void initialize(Compiler* compiler);
static void buildAST(Compiler* compiler);
static void analyze(Compiler* compiler);
static void generate(Compiler* compiler);
static void printToken(Token* token);
static void printTokens(Compiler* compiler, jtk_ArrayList_t* tokens);

// Token

void printToken(Token* token) {
    printf("[%d-%d:%d-%d:%s:%s]", token->startLine, token->stopLine,
        token->startColumn + 1, token->stopColumn + 1,
        token->channel == TOKEN_CHANNEL_DEFAULT? "default" : "hidden",
        tokenNames[(int32_t)token->type]);
    TokenType type = token->type;
    if ((type == TOKEN_IDENTIFIER) || (type == TOKEN_INTEGER_LITERAL) ||
        (type == TOKEN_STRING_LITERAL)) {
        printf(" %.*s", token->length, token->text);
    }
    puts("");
}

void printTokens(Compiler* compiler, jtk_ArrayList_t* tokens) {
    int32_t defaultChannel = 0;
    int32_t hiddenChannel = 0;
    int32_t otherChannel = 0;

    int32_t limit = jtk_ArrayList_getSize(tokens);
    int32_t i;
    for (i = 0; i < limit; i++) {
        Token* token = (Token* )jtk_ArrayList_getValue(tokens, i);
        TokenChannel channel = token->channel;
        if (channel == TOKEN_CHANNEL_DEFAULT) {
            defaultChannel++;
        }
        else if (channel == TOKEN_CHANNEL_HIDDEN) {
            hiddenChannel++;
        }
        else {
            otherChannel++;
        }
        printToken(token);
    }
    fflush(stdout);
    fprintf(stdout, "[info] %d tokens were recognized on the default channel.\n", defaultChannel);
    fprintf(stdout, "[info] %d tokens were recognized on the hidden channel.\n", hiddenChannel);
    fprintf(stdout, "[info] %d tokens were recognized on other channels.%s\n", otherChannel, (otherChannel > 0) ? " This is surprising to me." : "");
    fprintf(stdout, "[info] %d tokens were recognized in total.\n", limit);
}

/******************************************************************************
 * PathHelper                                                                 *
 ******************************************************************************/

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <jtk/core/CString.h>

bool jtk_PathHelper_exists(const uint8_t* path) {
    /*
    struct stat buffer;
    int32_t result = fstat(path, &buffer);
    return result == 0 && S_ISREG(buffer.st_mode);
    */
    return true;
}

#define JTK_FILE_OPEN_MODE_BUFFERED (1 << 0)

jtk_InputStream_t* jtk_PathHelper_read(const uint8_t* path);
jtk_InputStream_t* jtk_PathHelper_readEx(const uint8_t* path, uint32_t flags);

jtk_InputStream_t* jtk_PathHelper_read(const uint8_t* path) {
    return jtk_PathHelper_readEx(path, JTK_FILE_OPEN_MODE_BUFFERED);
}

jtk_InputStream_t* jtk_PathHelper_readEx(const uint8_t* path, uint32_t flags) {
    jtk_InputStream_t* result = NULL;

    jtk_FileInputStream_t* fileStream = jtk_FileInputStream_newFromString(path);
    result = fileStream->m_inputStream;

    if ((flags & JTK_FILE_OPEN_MODE_BUFFERED) != 0) {
        jtk_BufferedInputStream_t* bufferedStream = jtk_BufferedInputStream_new(fileStream->m_inputStream);
        result = bufferedStream->m_inputStream;
    }

    return result;
}

uint8_t* jtk_PathHelper_getParent(const uint8_t* path, int32_t size,
                                  int32_t* resultSize) {
    size = size < 0 ? jtk_CString_getSize(path) : size;

    int32_t index = jtk_CString_findLast_c(path, size, JTK_PATH_ELEMENT_SEPARATOR);
    if (resultSize != NULL) {
        *resultSize = index < 0 ? 0 : index;
    }
    return index < 0 ? NULL : jtk_CString_substringEx(path, size, 0, index);
}

/******************************************************************************
 * Compiler                                                                   *
 ******************************************************************************/

const char* errorMessages[] = {
    "None",

    // Lexical Errors
    "Unterminated string literal",
    "Unterminated multi-line comment",
    "Malformed unicode character sequence; expected four hexadecimal digits",
    "Invalid escape sequence",
    "Unknown character",
    "Invalid prefix in integer literal",
    "Expected digit after underscore in integer literal",
    "Expected letter after colon in identifier",

    // Syntactical Errors
    "Unexpected token",
    "Try statement expects at least one catch or finally clause",
    "Variable initializer expected",
    "Invalid lvalue",

    // Semantical Errors

    // Errors related to binary expressions
    "Cannot combine equality operators",
    "Type does not support invoking",
    "Type does not support indexing",
    "Type does not support accessing",

    // Errors related to unary expressions
    "Invalid operand",

    // Errors related to declaration
    "Unknown module",
    "Undeclared type",
    "Undeclared member",
    "Undeclared identifier",
    "Undeclared label",
    "Redeclaration of symbol as function",
    "Redeclaration of symbol as parameter",
    "Redeclaration of symbol as variable parameter",
    "Redeclaration of symbol as variable",
    "Redeclaration of symbol as constant",
    "Redeclaration of symbol as label",
    "Redeclaration of symbol as loop parameter",
    "Redeclaration of symbol as catch parameter",
    "Redeclaration of symbol as structure",
    "Redeclaration of symbol previously imported",
    "Invalid type",
    "Incompatible variable initializer",

    // Errors related to types
    "Expected boolean expression",
    "Expected boolean expression on left",
    "Expected boolean expression on right",
    "Expected integer expression on left",
    "Expected integer expression on right",
    "Invalid left operand",
    "Invalid right operand",
    "Incompatible operand types",
    "Expected variable",
    "Expected label",
    "Incompatible return value",
    "Invalid argument count",
    "Incompatible argument type",
    "Array members should have same type",
    "Empty array initializer",
    "Expected structure name",
    "Expected integer expression",

    // General errors
    "Corrupted module",
    "Invalid module version",

    // General Errors

    "Corrupted binary entity",
    "Invalid module version"
};

void printErrors(Compiler* compiler) {
    jtk_ArrayList_t* errors = compiler->errorHandler->errors;
    int32_t errorCount = jtk_ArrayList_getSize(errors);
    int32_t i;
    for (i = 0; i < errorCount; i++) {
        Error* error = (Error*)jtk_ArrayList_getValue(errors, i);
        Token* token = error->token;
        const char* message = errorMessages[(int32_t)error->code];
        char message0[128];

        char lineNumbers[100];
        if (token->startLine != token->stopLine) {
            sprintf(lineNumbers, "%d-%d", token->startLine, token->stopLine);
        }
        else {
            sprintf(lineNumbers, "%d", token->startLine);
        }

        if (error->expected != TOKEN_UNKNOWN) {
            sprintf(message0, "Expected token '%s', encountered token '%s'",
                tokenNames[(int32_t)error->expected], tokenNames[(int32_t)token->type]);
            message = message0;
        }
        fprintf(stderr, "\033[1;31m[error]\033[0m %s:%s:%d-%d: %s\n",
                token->file, lineNumbers, token->startColumn,
                token->stopColumn, message);
    }
}

void initialize(Compiler* compiler) {
    int32_t size = jtk_ArrayList_getSize(compiler->inputFiles);
    compiler->modules = allocate(Module*, size);
    compiler->packages = allocate(uint8_t* , size);
    compiler->packageSizes = allocate(int32_t, size);
}

void buildAST(Compiler* compiler) {
    Lexer* lexer = lexerNew(compiler);
    TokenStream* tokens = tokenStreamNew(compiler, lexer, TOKEN_CHANNEL_DEFAULT);
    Parser* parser = parserNew(compiler, tokens);

    int32_t size = jtk_ArrayList_getSize(compiler->inputFiles);
    int32_t i;
    for (i = 0; i < size; i++) {
        compiler->currentFileIndex = i;

        const uint8_t* path = (const uint8_t*)jtk_ArrayList_getValue(compiler->inputFiles, i);
        if (!jtk_PathHelper_exists(path)) {
            fprintf(stderr, "[error] Path '%s' does not exist.\n", path);
        }
        else {
            int32_t packageSize;
            uint8_t* package = jtk_PathHelper_getParent(path, -1, &packageSize);
            compiler->packages[i] = package;
            compiler->packageSizes[i] = packageSize;
            jtk_Arrays_replace_b((int8_t*)package, packageSize, '/', '.');

            jtk_InputStream_t* stream = jtk_PathHelper_read(path);
            resetLexer(lexer, stream);

            int32_t previousLexicalErrors = compiler->errorHandler->errors->m_size;
            resetTokenStream(tokens);
            fillTokenStream(tokens);

            int32_t currentLexicalErrors = compiler->errorHandler->errors->m_size;

            if (compiler->dumpTokens) {
                printTokens(compiler, tokens->tokens);
            }
            else {
                /* Perform syntax analysis for the current input source file only if
                 * there are no lexical errors.
                 */
                if (previousLexicalErrors == currentLexicalErrors) {
                    resetParser(parser, tokens);
                    Module* module = parse(parser);
                    compiler->modules[i] = module;

                    if (compiler->dumpNodes) {
                        // TODO
                    }
                }
            }

            jtk_InputStream_destroy(stream);
        }
    }

    compiler->trash = tokens->trash;

    parserDelete(parser);
    tokenStreamDelete(tokens);
    lexerDelete(lexer);

    printErrors(compiler);
}

void analyze(Compiler* compiler) {
    Analyzer* analyzer = newAnalyzer();

    int32_t size = jtk_ArrayList_getSize(compiler->inputFiles);
    int32_t i;
    for (i = 0; i < size; i++) {
        compiler->currentFileIndex = i;
        Module* module = compiler->modules[i];
        defineSymbols(analyzer, module);
    }

    if (jtk_ArrayList_isEmpty(compiler->errorHandler->errors)) {
        for (i = 0; i < size; i++) {
            compiler->currentFileIndex = i;
            Module* module = compiler->modules[i];
            resolveSymbols(analyzer, module);
        }
    }

    printErrors(compiler);
    deleteAnalyzer(analyzer);
}

void generate(Compiler* compiler) {
    Generator* generator = newGenerator(compiler);
    int32_t size = jtk_ArrayList_getSize(compiler->inputFiles);
    int32_t i;
    for (i = 0; i < size; i++) {
        compiler->currentFileIndex = i;
        Module* module = compiler->modules[i];
        generateC(generator, module);
    }
    deleteGenerator(generator);
}

void buildExecutable(Compiler* compiler) {
    jtk_StringBuilder_t* builder = jtk_StringBuilder_new();
    jtk_StringBuilder_appendEx_z(builder, "gcc ", 4);

    int32_t size = jtk_ArrayList_getSize(compiler->inputFiles);
    int32_t i;
    for (i = 0; i < size; i++) {
        const uint8_t* path = (const uint8_t*)jtk_ArrayList_getValue(compiler->inputFiles, i);
        int32_t size = jtk_CString_getSize(path) - 4;
        jtk_StringBuilder_appendCodePoint(builder, (int32_t)'"');
        jtk_StringBuilder_appendEx_z(builder, path, size);
        jtk_StringBuilder_appendEx_z(builder, "c\" ", 3);
    }

    uint8_t* output = "main";
    int32_t outputSize = 4;
    if (compiler->output != NULL) {
        output = compiler->output;
        outputSize = compiler->outputSize;
    }

    jtk_StringBuilder_appendEx_z(builder, "../runtime/kush-runtime.c -I../runtime -g -o ", 45);
    jtk_StringBuilder_appendEx_z(builder, output, outputSize);
    int32_t commandSize = -1;
    uint8_t* command = jtk_StringBuilder_toCString(builder, &commandSize);
    jtk_StringBuilder_delete(builder);

    printf("\033[1;33m[spawn]\033[1;37m %s\n\033[0m", command);
    system(command);
}

jtk_ArrayList_t* k_CString_split_c(const uint8_t* sequence, int32_t size,
    uint8_t value, bool inclusive) {
    jtk_ArrayList_t* result = jtk_ArrayList_new();
    int32_t i;
    for (i = 0; i < size; i++) {
        int32_t startIndex = i;
        while ((i < size) && (sequence[i] != value)) {
            i++;
        }
        int32_t stopIndex = ((i < size) && inclusive) ? i + 1 : i;
        uint8_t* substring = jtk_CString_substringEx(sequence, size, startIndex,
                                                     stopIndex);
        jtk_ArrayList_add(result, substring);
    }
    return result;
}

void printHelp() {
    printf(
        "[Usage]\n"
        "    kush [--tokens] [--nodes] [--footprint] [--instructions] [--core-api] [--log <level>] [--help] [--output|-o <path>] <inputFiles> [--run <arguments>]\n\n"
        "[Options]\n"
        "    --tokens            Print the tokens recognized by the lexer.\n"
        "    --nodes             Print the AST recognized by the parser.\n"
        "    --footprint         Print diagnostic information about the memory footprint of the compiler.\n"
        "    --instructions      Disassemble the binary entity generated.\n"
        "    --core-api          Disables the internal constant pool function index cache. This flag is valid only when compiling foreign function interfaces.\n"
        "    --run               Run the virtual machine after compiling the source files.\n"
        "    --log               Generate log messages. This flag is valid only if log messages were enabled at compile time.\n"
        "    --help              Print the help message.\n"
        "    --version           Print the current version of the compiler.\n"
        "    -o, --output path   Generate the executable to the specified file.\n"
        );
}

bool compileEx(Compiler* compiler, char** arguments, int32_t length) {
    jtk_Assert_assertObject(compiler, "The specified compiler is null.");

    // TODO: Add the --path flag
    // k_SymbolLoader_addDirectory(compiler->symbolLoader, ".");

    char** vmArguments = NULL;
    int32_t vmArgumentsSize = 0;

    bool invalidCommandLine = false;
    int32_t i;
    bool showVersion = false;
    bool showHelp = false;
    for (i = 1; i < length; i++) {
        if (arguments[i][0] == '-') {
            if (strcmp(arguments[i], "--tokens") == 0) {
                compiler->dumpTokens = true;
            }
            else if (strcmp(arguments[i], "--nodes") == 0) {
                compiler->dumpNodes = true;
            }
            else if (strcmp(arguments[i], "--footprint") == 0) {
                compiler->footprint = true;
            }
            else if (strcmp(arguments[i], "--instructions") == 0) {
                compiler->dumpInstructions = true;
            }
            else if (strcmp(arguments[i], "--core-api") == 0) {
                compiler->coreApi = true;
            }
            else if (strcmp(arguments[i], "--run") == 0) {
                vmArgumentsSize = length - i + 1;
                if (vmArgumentsSize > 0) {
                    vmArguments = arguments + (i + 1);
                    break;
                }
                else {
                    printf("[error] Please specify the main class.");
                    invalidCommandLine = true;
                }
            }
            else if (strcmp(arguments[i], "--version") == 0) {
                showVersion = true;
            }
            else if (strcmp(arguments[i], "--help") == 0) {
                showHelp = true;
            }
            else if ((strcmp(arguments[i], "--output") == 0) || (strcmp(arguments[i], "-o") == 0)) {
                if ((i + 1) < length) {
                    i++;
                    // Didn't make a copy of the output string. DO NOT DELETE IT.
                    compiler->output = arguments[i];
                    compiler->outputSize = strlen(arguments[i]);
                }
            }
            else if (strcmp(arguments[i], "--log") == 0) {
                if ((i + 1) < length) {
                    i++;
                    jtk_LogLevel_t level = JTK_LOG_LEVEL_NONE;
                    if (strcmp(arguments[i], "all") == 0) {
                        level = JTK_LOG_LEVEL_ALL;
                    }
                    else if (strcmp(arguments[i], "finest") == 0) {
                        level = JTK_LOG_LEVEL_FINEST;
                    }
                    else if (strcmp(arguments[i], "finer") == 0) {
                        level = JTK_LOG_LEVEL_FINER;
                    }
                    else if (strcmp(arguments[i], "fine") == 0) {
                        level = JTK_LOG_LEVEL_FINE;
                    }
                    else if (strcmp(arguments[i], "debug") == 0) {
                        level = JTK_LOG_LEVEL_DEBUG;
                    }
                    else if (strcmp(arguments[i], "configuration") == 0) {
                        level = JTK_LOG_LEVEL_CONFIGURATION;
                    }
                    else if (strcmp(arguments[i], "information") == 0) {
                        level = JTK_LOG_LEVEL_INFORMATION;
                    }
                    else if (strcmp(arguments[i], "warning") == 0) {
                        level = JTK_LOG_LEVEL_WARNING;
                    }
                    else if (strcmp(arguments[i], "severe") == 0) {
                        level = JTK_LOG_LEVEL_SEVERE;
                    }
                    else if (strcmp(arguments[i], "error") == 0) {
                        level = JTK_LOG_LEVEL_ERROR;
                    }
                    else if (strcmp(arguments[i], "none") == 0) {
                        level = JTK_LOG_LEVEL_NONE;
                    }
                    else {
                        printf("[error] Unknown log level '%s'\n", arguments[i]);
                        invalidCommandLine = true;
                    }

#ifdef JTK_LOGGER_DISABLE
    printf("[warning] The logger was disabled at compile time. Please consider building k without the `JTK_LOGGER_DISABLE` constant in 'Configuration.h'.\n");
#else
    jtk_Logger_setLevel(compiler->logger, level);
#endif
                }
                else {
                    printf("[error] The `--log` flag expects argument specifying log level.");
                    invalidCommandLine = true;
                }
            }
            else {
                printf("[error] Unknown flag `%s`\n", arguments[i]);
            }
        }
        else {
            // jtk_String_t* path = jtk_String_new(arguments[i]);
            // jtk_ArrayList_add(compiler->inputFiles, path);
            jtk_ArrayList_add(compiler->inputFiles, arguments[i]);
        }
    }

    if (showVersion) {
        printf("kush v%d.%d\n", KUSH_VERSION_MAJOR, KUSH_VERSION_MINOR);
    }
    else if (showHelp) {
        printHelp();
    }
    else {

        int32_t size = jtk_ArrayList_getSize(compiler->inputFiles);
        bool noErrors = false;
        if (size == 0) {
            fprintf(stderr, "\033[1;31m[error]\033[0m Please specify input files.\n");
        }
        else {
            initializePrimitives();
            initialize(compiler);
            buildAST(compiler);
            if (!compiler->dumpTokens && (noErrors = (compiler->errorHandler->errors->m_size == 0))) {
                analyze(compiler);

                if (jtk_ArrayList_isEmpty(compiler->errorHandler->errors)) {
                    generate(compiler);
                    buildExecutable(compiler);
                }
            }
            destroyPrimitives();
        }

        if (compiler->footprint) {
            /* int32_t footprint = k_Memory_getFootprint();
            printf("Memory Footprint = %.2f KB\n", footprint / 1024.0f);
            */
        }

        if ((vmArguments != NULL) && noErrors) {
            // TODO: Start the child process
        }
    }

    // TODO: Return true only if the compilation suceeded.
    return true;
}

bool compile(Compiler* compiler) {
    return compileEx(compiler, NULL, -1);
}

// Constructor

Compiler* newCompiler() {
    jtk_ObjectAdapter_t* stringObjectAdapter = jtk_CStringObjectAdapter_getInstance();

    Compiler* compiler = allocate(Compiler, 1);
    compiler->dumpTokens = false;
    compiler->dumpNodes = false;
    compiler->footprint = false;
    compiler->dumpInstructions = false;
    compiler->inputFiles = jtk_ArrayList_new();
    compiler->currentFileIndex = -1;
    compiler->errorHandler = newErrorHandler();
    compiler->modules = NULL;
    compiler->packages = NULL;
    compiler->packageSizes = NULL;
    compiler->symbolLoader = NULL; // k_SymbolLoader_new(compiler);
    compiler->repository = jtk_HashMap_new(stringObjectAdapter, NULL);
    compiler->trash = NULL;
    compiler->coreApi = false;
    compiler->output = NULL;
    compiler->outputSize = 0;
#ifdef JTK_LOGGER_DISABLE
    compiler->logger = NULL;
#else
    compiler->logger = jtk_Logger_new(jtk_ConsoleLogger_log);
    jtk_Logger_setLevel(compiler->logger, JTK_LOG_LEVEL_NONE);
#endif

    return compiler;
}

// Destructor

void deleteCompiler(Compiler* compiler) {
    jtk_Assert_assertObject(compiler, "The specified compiler is null.");

    if (compiler->modules != NULL) {
        deallocate(compiler->modules);
    }

    if (compiler->trash != NULL) {
        int32_t size = jtk_ArrayList_getSize(compiler->trash);
        int32_t i;
        for (i = 0; i < size; i++) {
            /* The ownership of the tokens produced by the lexer
            * is transfered to the token stream which demanded
            * their creation. Therefore, the token stream
            * has to destroy the buffered tokens.
            */
            Token* token = (Token*)jtk_ArrayList_getValue(compiler->trash, i);
            deleteToken(token);
        }
        jtk_ArrayList_delete(compiler->trash);
    }

    if (compiler->packages != NULL) {
        int32_t i;
        int32_t inputCount = jtk_ArrayList_getSize(compiler->inputFiles);
        for (i = 0; i < inputCount; i++) {
            jtk_CString_delete(compiler->packages[i]);
        }
        deallocate(compiler->packages);
        deallocate(compiler->packageSizes);
    }

    jtk_Iterator_t* iterator = jtk_HashMap_getKeyIterator(compiler->repository);
    while (jtk_Iterator_hasNext(iterator)) {
        uint8_t* key = (uint8_t* )jtk_Iterator_getNext(iterator);
        jtk_CString_delete(key);
    }
    jtk_Iterator_delete(iterator);
    jtk_HashMap_delete(compiler->repository);

    deleteErrorHandler(compiler->errorHandler);

#ifndef JTK_LOGGER_DISABLE
    jtk_Logger_delete(compiler->logger);
#endif

    jtk_ArrayList_delete(compiler->inputFiles);
    deallocate(compiler);
}