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

// Saturday, April 10, 2020

#include <jtk/collection/Iterator.h>
#include <jtk/collection/array/Arrays.h>
#include <jtk/collection/array/ByteArray.h>
#include <jtk/collection/list/DoublyLinkedList.h>
#include <jtk/fs/Path.h>
#include <jtk/fs/PathHandle.h>
#include <jtk/fs/FileInputStream.h>
#include <jtk/io/BufferedInputStream.h>
#include <jtk/io/InputStream.h>
#include <jtk/io/InputStreamHelper.h>
#include <jtk/core/CString.h>
#include <jtk/core/CStringObjectAdapter.h>

#include <kush/symbol-loader.h>
#include <kush/compiler.h>

/*******************************************************************************
 * SymbolLoader                                                                *
 *******************************************************************************/

SymbolLoader* k_SymbolLoader_new(Compiler* compiler) {
    jtk_ObjectAdapter_t* stringObjectAdapter = jtk_CStringObjectAdapter_getInstance();

    SymbolLoader* loader = allocate(SymbolLoader, 1);
    loader->directories = jtk_DoublyLinkedList_new();
    loader->flags = KUSH_ENTITY_LOADER_FLAG_PRIORITIZE_DIRECTORIES;
    loader->symbols = jtk_HashMap_newEx(stringObjectAdapter, NULL,
        KUSH_ENTITY_LOADER_DEFAULT_ENTITIES_MAP_CAPCITY, JTK_HASH_MAP_DEFAULT_LOAD_FACTOR);
    loader->compiler = compiler;
    loader->index = 0;
    loader->bytes = NULL;
    loader->size = -1;
    loader->constantPool.size = 0;
    loader->constantPool.entries = NULL;
    loader->symbol = NULL;

    return loader;
}

SymbolLoader* k_SymbolLoader_newWithEntityDirectories(Compiler* compiler,
    jtk_Iterator_t* entityDirectoryIterator) {
    jtk_ObjectAdapter_t* stringObjectAdapter = jtk_CStringObjectAdapter_getInstance();

    SymbolLoader* loader = k_SymbolLoader_new(compiler);
    while (jtk_Iterator_hasNext(entityDirectoryIterator)) {
        uint8_t* directory = (uint8_t*)jtk_Iterator_getNext(entityDirectoryIterator);
        k_SymbolLoader_addDirectory(loader, directory);
    }

    return loader;
}

void k_SymbolLoader_delete(SymbolLoader* loader) {
    jtk_Assert_assertObject(loader, "The specified entity loader is null.");

    int32_t size = jtk_DoublyLinkedList_getSize(loader->directories);
    jtk_Iterator_t* iterator = jtk_DoublyLinkedList_getIterator(loader->directories);
    while (jtk_Iterator_hasNext(iterator)) {
        jtk_Path_t* path = (jtk_Path_t*)jtk_Iterator_getNext(iterator);
        jtk_Path_delete(path);
    }
    jtk_Iterator_delete(iterator);
    jtk_DoublyLinkedList_delete(loader->directories);

    jtk_Iterator_t* entryIterator = jtk_HashMap_getEntryIterator(loader->symbols);
    while (jtk_Iterator_hasNext(entryIterator)) {
        jtk_HashMapEntry_t* entry = (jtk_HashMapEntry_t*)jtk_Iterator_getNext(entryIterator);

        uint8_t* descriptor = (uint8_t*)jtk_HashMapEntry_getKey(entry);
        jtk_CString_delete(descriptor);

        k_Symbol_t* symbol = (k_Symbol_t*)jtk_HashMapEntry_getValue(entry->value);
        k_Symbol_delete(symbol);
    }
    jtk_Iterator_delete(entryIterator);
    jtk_HashMap_delete(loader->symbols);

    if (loader->constantPool.entries != NULL) {
        deallocate(loader->constantPool.entries);
    }

    deallocate(loader);
}

/* The original algorithm ensured that a valid directory was added to the entity
 * directries. Since the directories are validated when loading entities the
 * algorithm was modified to include all the directories without checking.
 */
bool k_SymbolLoader_addDirectory(SymbolLoader* loader, const uint8_t* directory) {
    jtk_Assert_assertObject(loader, "The specified entity loader is null.");
    jtk_Assert_assertObject(directory, "The specified directory is null.");

    jtk_Path_t* path = jtk_Path_newFromString(directory);
    jtk_DoublyLinkedList_add(loader->directories, path);

    // bool result = jtk_Path_isDirectory(path);
    // if (result) {
    //    jtk_DoublyLinkedList_add(loader->directories, path);
    // }
    // else {
    //    jtk_Path_delete(path);
    // }

    return true;
}

// Find Symbol

k_Symbol_t* k_SymbolLoader_findSymbol(SymbolLoader* loader,
    const uint8_t* descriptor, int32_t descriptorSize) {
    jtk_Assert_assertObject(loader, "The specified entity loader is null.");
    jtk_Assert_assertObject(descriptor, "The specified descriptor is null.");

    k_Symbol_t* result = (k_Symbol_t*)jtk_HashMap_getValue(loader->symbols, descriptor);
    if (result == NULL) {
        result = k_SymbolLoader_loadSymbol(loader, descriptor,
            descriptorSize);
    }

    return result;
}

// Load Symbol

k_Symbol_t* k_SymbolLoader_loadSymbol(SymbolLoader* loader,
    const uint8_t* descriptor, int32_t descriptorSize) {
    jtk_Assert_assertObject(loader, "The specified entity loader is null.");
    jtk_Assert_assertObject(descriptor, "The specified descriptor is null.");

    k_Symbol_t* result = NULL;

    int32_t fileNameSize;
    uint8_t* fileName = jtk_CString_join(descriptor, ".feb", &fileNameSize);
    jtk_Arrays_replaceEx_b(fileName, fileNameSize, '.', '/', 0, fileNameSize - 4);
    jtk_Path_t* entityFile = jtk_Path_newFromStringEx(fileName, fileNameSize);
    jtk_CString_delete(fileName);

    /* Retrieve an iterator over the list of registered entity directories. */
    jtk_Iterator_t* iterator = jtk_DoublyLinkedList_getIterator(loader->directories);
    while (jtk_Iterator_hasNext(iterator)) {
        /* Retrieve the next directory for searching the entity file. */
        jtk_Path_t* directoryPath = (jtk_Path_t*)jtk_Iterator_getNext(iterator);
        jtk_PathHandle_t* directoryHandle = jtk_PathHandle_newFromPath(directoryPath);
        /* The specified path may point to a regular file. Therefore, make sure
         * the path is a directory to provide a warning.
         *
         * NOTE: The entity file path can be directly constructed without checking
         * if the entity directory exists. This makes the lookup process faster.
         * Right now, the original algorithm is maintained to print debugging
         * information. Probably should use conditional directives to enable and
         * disable the check at compile time.
         */
        if ((directoryHandle != NULL) && jtk_PathHandle_isDirectory(directoryHandle)) {
            /* Construct a hypothetical path to the entity file. */
            jtk_Path_t* entityPath = jtk_Path_newWithParentAndChild_oo(directoryPath, entityFile);
            jtk_PathHandle_t* entityPathHandle = jtk_PathHandle_newFromPath(entityPath);
            if (entityPathHandle != NULL) {
                if (jtk_PathHandle_isRegularFile(entityPathHandle)) {
                    // NOTE: The loader should not maintain any reference to entity path.
                    result = k_SymbolLoader_loadSymbolFromHandle(loader, entityPathHandle);
                    if (result != NULL) {
                        uint8_t* identifier = jtk_CString_newEx(descriptor, descriptorSize);
                        jtk_HashMap_put(loader->symbols, identifier, result);
                    }
                    else {
                        /* At this point, the entity loader found an entity file. Unfortunately, the
                         * entity file is corrupted. The entity loader may continue to look for entities
                         * in different files. It terminates here if the entity loader is not configured
                         * to ignore corrupt entity files.
                         */
                        if (!k_SymbolLoader_shouldIgnoreCorruptEntity(loader)) {
                            break;
                        }
                    }
                }
                /* Destroy the entity path handle created earlier. */
                jtk_PathHandle_delete(entityPathHandle);
            }
            else {
                // log("Could not find entity file here.");
            }
            jtk_Path_delete(entityPath);
        }
        else {
            fprintf(stderr, "[warning] Cannot find lookup directory '%s'\n",
                directoryPath->value);
        }
    }
    jtk_Path_delete(entityFile);

    return result;
}

// Load Entity From File

k_Symbol_t* k_SymbolLoader_loadSymbolFromHandle(SymbolLoader* loader,
    jtk_PathHandle_t* handle) {
    jtk_Assert_assertObject(loader, "The specified entity loader is null.");
    jtk_Assert_assertObject(handle, "The specified entity path handle is null.");

    k_Symbol_t* result = NULL;
    jtk_FileInputStream_t* fileInputStream = jtk_FileInputStreanewFromHandle(handle);
    if (fileInputStream != NULL) {
        jtk_BufferedInputStream_t* bufferedInputStream = jtk_BufferedInputStreanewEx(
            fileInputStream->inputStream, KUSH_ENTITY_LOADER_BUFFER_SIZE);
        jtk_InputStream_t* inputStream = bufferedInputStream->inputStream;

        jtk_ByteArray_t* input = jtk_InputStreamHelper_toByteArray(inputStream);

        result = k_SymbolLoader_parse(loader, input->values, input->size);

        jtk_ByteArray_delete(input);
        jtk_InputStream_destroy(inputStream);
    }
    else {
        // Warning: Failed to load entity from handle.
    }

    return result;
}

// Ignore Corrupt Entity

bool k_SymbolLoader_shouldIgnoreCorruptEntity(SymbolLoader* loader) {
    jtk_Assert_assertObject(loader, "The specified entity loader is null.");

    return (loader->flags & KUSH_ENTITY_LOADER_FLAG_IGNORE_CORRUPT_ENTITY) != 0;
}

void k_SymbolLoader_setIgnoreCorruptEntity(SymbolLoader* loader,
    bool ignoreCorruptEntity) {
    jtk_Assert_assertObject(loader, "The specified entity loader is null.");

    loader->flags = ignoreCorruptEntity?
        (loader->flags | KUSH_ENTITY_LOADER_FLAG_IGNORE_CORRUPT_ENTITY) :
        (loader->flags & ~KUSH_ENTITY_LOADER_FLAG_IGNORE_CORRUPT_ENTITY);
}

// Parse

#define KUSH_FEB_HEADER_SIZE 12

void k_SymbolLoader_parseConstantPool(SymbolLoader* loader) {
    jtk_Logger_t* logger = loader->compiler->logger;
    k_ConstantPool_t* constantPool = &loader->constantPool;
    constantPool->size = ((loader->bytes[loader->index++] & 0xFF) << 8) |
        (loader->bytes[loader->index++] & 0xFF);
    constantPool->entries = allocate(k_ConstantPoolEntry_t*, loader->size + 1);
    constantPool->entries[0] = NULL;

    int32_t i;
    for (i = 1; i <= constantPool->size; i++) {
        uint8_t tag = loader->bytes[loader->index++];

        switch (tag) {
            case KUSH_CONSTANT_POOL_TAG_INTEGER: {
                uint32_t value = ((loader->bytes[loader->index++] & 0xFF) << 24) |
                    ((loader->bytes[loader->index++] & 0xFF) << 16) |
                    ((loader->bytes[loader->index++] & 0xFF) << 8) |
                    (loader->bytes[loader->index++] & 0xFF);

                k_ConstantPoolInteger_t* constantPoolInteger =
                    allocate(k_ConstantPoolInteger_t, 1);
                constantPoolInteger->tag = KUSH_CONSTANT_POOL_TAG_INTEGER;
                constantPoolInteger->bytes = value;

                constantPool->entries[i] = constantPoolInteger;

                jtk_Logger_debug(logger, "Parsed constant pool entry `k_ConstantPoolInteger_t`, stored at index %d.", i);

                break;
            }

            case KUSH_CONSTANT_POOL_TAG_LONG: {
                uint32_t highBytes = ((loader->bytes[loader->index++] & 0xFF) << 24) |
                    ((loader->bytes[loader->index++] & 0xFF) << 16) |
                    ((loader->bytes[loader->index++] & 0xFF) << 8) |
                    (loader->bytes[loader->index++] & 0xFF);
                uint32_t lowBytes = ((loader->bytes[loader->index++] & 0xFF) << 24) |
                    ((loader->bytes[loader->index++] & 0xFF) << 16) |
                    ((loader->bytes[loader->index++] & 0xFF) << 8) |
                    (loader->bytes[loader->index++] & 0xFF);

                k_ConstantPoolLong_t* constantPoolLong = allocate(k_ConstantPoolLong_t, 1);
                constantPoolLong->tag = KUSH_CONSTANT_POOL_TAG_LONG;
                constantPoolLong->highBytes = highBytes;
                constantPoolLong->lowBytes = lowBytes;

                constantPool->entries[i] = constantPoolLong;

                // jtk_Logger_info(parser->logger, KUSH_BINARY_ENTITY_PARSER_TAG, "Parsed constant pool entry `k_ConstantPoolLong_t`, stored at index %d.", index);

                break;
            }

            case KUSH_CONSTANT_POOL_TAG_FLOAT: {
                uint32_t value = ((loader->bytes[loader->index++] & 0xFF) << 24) |
                    ((loader->bytes[loader->index++] & 0xFF) << 16) |
                    ((loader->bytes[loader->index++] & 0xFF) << 8) |
                    (loader->bytes[loader->index++] & 0xFF);

                k_ConstantPoolFloat_t* constantPoolFloat = allocate(k_ConstantPoolFloat_t, 1);
                constantPoolFloat->tag = KUSH_CONSTANT_POOL_TAG_FLOAT;
                constantPoolFloat->bytes = value;

                constantPool->entries[i] = constantPoolFloat;

                // jtk_Logger_info(parser->logger, KUSH_BINARY_ENTITY_PARSER_TAG, "Parsed constant pool entry `k_ConstantPoolFloat_t`, stored at index %d.", index);

                break;
            }

            case KUSH_CONSTANT_POOL_TAG_DOUBLE: {
                uint32_t highBytes = ((loader->bytes[loader->index++] & 0xFF) << 24) |
                    ((loader->bytes[loader->index++] & 0xFF) << 16) |
                    ((loader->bytes[loader->index++] & 0xFF) << 8) |
                    (loader->bytes[loader->index++] & 0xFF);
                uint32_t lowBytes = ((loader->bytes[loader->index++] & 0xFF) << 24) |
                    ((loader->bytes[loader->index++] & 0xFF) << 16) |
                    ((loader->bytes[loader->index++] & 0xFF) << 8) |
                    (loader->bytes[loader->index++] & 0xFF);

                k_ConstantPoolDouble_t* constantPoolDouble = allocate(k_ConstantPoolDouble_t, 1);
                constantPoolDouble->tag = KUSH_CONSTANT_POOL_TAG_DOUBLE;
                constantPoolDouble->highBytes = highBytes;
                constantPoolDouble->lowBytes = lowBytes;

                constantPool->entries[i] = constantPoolDouble;

                // jtk_Logger_info(parser->logger, KUSH_BINARY_ENTITY_PARSER_TAG, "Parsed constant pool entry `k_ConstantPoolDouble_t`, stored at index %d.", index);

                break;
            }

            case KUSH_CONSTANT_POOL_TAG_UTF8: {
                uint16_t length = ((loader->bytes[loader->index++] & 0xFF) << 8) |
                    (loader->bytes[loader->index++] & 0xFF);
                /* The specification guarantees that an empty string is never stored in a constant pool.
                 *
                 * Although the speciication does not specify the bytes to be null-terminated,
                 * the reference implementation of the compiler terminates UTF8 constant pool
                 * entries for performance.
                 */

                uint8_t* value = allocate(uint8_t, length + 1);
                value[length] = '\0';
                jtk_Arrays_copyEx_b(loader->bytes, loader->size, loader->index,
                    value, length, 0, length);
                loader->index += length;

                k_ConstantPoolUtf8_t* constantPoolUtf8 = allocate(k_ConstantPoolUtf8_t, 1);
                constantPoolUtf8->tag = KUSH_CONSTANT_POOL_TAG_UTF8;
                constantPoolUtf8->length = length;
                constantPoolUtf8->bytes = value;

                constantPool->entries[i] = constantPoolUtf8;

                // jtk_Logger_debug(logger, "Parsed constant pool entry `k_ConstantPoolUtf8_t`, stored at index %d.", index);

                break;
            }

            case KUSH_CONSTANT_POOL_TAG_STRING: {
                uint16_t stringIndex = ((loader->bytes[loader->index++] & 0xFF) << 8) |
                    (loader->bytes[loader->index++] & 0xFF);

                k_ConstantPoolString_t* constantPoolString = allocate(k_ConstantPoolString_t, 1);
                constantPoolString->tag = KUSH_CONSTANT_POOL_TAG_STRING;
                constantPoolString->stringIndex = stringIndex;

                constantPool->entries[i] = constantPoolString;

                // jtk_Logger_info(parser->logger, KUSH_BINARY_ENTITY_PARSER_TAG, "Parsed constant pool entry `k_ConstantPoolString_t`, stored at index %d.", index);

                break;
            }

            case KUSH_CONSTANT_POOL_TAG_FUNCTION: {
                uint16_t classIndex = ((loader->bytes[loader->index++] & 0xFF) << 8) |
                    (loader->bytes[loader->index++] & 0xFF);
                uint16_t descriptorIndex = ((loader->bytes[loader->index++] & 0xFF) << 8) |
                    (loader->bytes[loader->index++] & 0xFF);
                uint16_t nameIndex = ((loader->bytes[loader->index++] & 0xFF) << 8) |
                    (loader->bytes[loader->index++] & 0xFF);
                uint16_t tableIndex = ((loader->bytes[loader->index++] & 0xFF) << 8) |
                    (loader->bytes[loader->index++] & 0xFF);

                k_ConstantPoolFunction_t* constantPoolFunction = allocate(k_ConstantPoolFunction_t, 1);
                constantPoolFunction->tag = KUSH_CONSTANT_POOL_TAG_FUNCTION;
                constantPoolFunction->classIndex = classIndex;
                constantPoolFunction->descriptorIndex = descriptorIndex;
                constantPoolFunction->nameIndex = nameIndex;
                constantPoolFunction->tableIndex = tableIndex;

                constantPool->entries[i] = constantPoolFunction;

                // jtk_Logger_info(parser->logger, KUSH_BINARY_ENTITY_PARSER_TAG, "Parsed constant pool entry `k_ConstantPoolFunction_t`, stored at index %d.", index);

                break;
            }

            case KUSH_CONSTANT_POOL_TAG_FIELD: {
                uint16_t classIndex = ((loader->bytes[loader->index++] & 0xFF) << 8) |
                    (loader->bytes[loader->index++] & 0xFF);
                uint16_t descriptorIndex = ((loader->bytes[loader->index++] & 0xFF) << 8) |
                    (loader->bytes[loader->index++] & 0xFF);
                uint16_t nameIndex = ((loader->bytes[loader->index++] & 0xFF) << 8) |
                    (loader->bytes[loader->index++] & 0xFF);

                k_ConstantPoolField_t* constantPoolField = allocate(k_ConstantPoolField_t, 1);
                constantPoolField->tag = KUSH_CONSTANT_POOL_TAG_FIELD;
                constantPoolField->classIndex = classIndex;
                constantPoolField->descriptorIndex = descriptorIndex;
                constantPoolField->nameIndex = nameIndex;

                constantPool->entries[i] = constantPoolField;

                // jtk_Logger_info(parser->logger, KUSH_BINARY_ENTITY_PARSER_TAG, "Parsed constant pool entry `k_ConstantPoolField_t`, stored at index %d.", index);

                break;
            }

            case KUSH_CONSTANT_POOL_TAG_CLASS: {
                uint16_t nameIndex = ((loader->bytes[loader->index++] & 0xFF) << 8) |
                    (loader->bytes[loader->index++] & 0xFF);

                k_ConstantPoolClass_t* constantPoolClass = allocate(k_ConstantPoolClass_t, 1);
                constantPoolClass->tag = KUSH_CONSTANT_POOL_TAG_CLASS;
                constantPoolClass->nameIndex = nameIndex;

                constantPool->entries[i] = constantPoolClass;

                // jtk_Logger_info(parser->logger, KUSH_BINARY_ENTITY_PARSER_TAG, "Parsed constant pool entry `k_ConstantPoolClass_t`, stored at index %d.", index);

                break;
            }
        }
    }
}

void k_SymbolLoader_skipAttributeTable(SymbolLoader* loader) {
    uint16_t size = ((loader->bytes[loader->index++] & 0xFF) << 8) |
        (loader->bytes[loader->index++] & 0xFF);
    int32_t i;
    for (i = 0; i < size; i++) {
        uint16_t nameIndex = ((loader->bytes[loader->index++] & 0xFF) << 8) |
        (loader->bytes[loader->index++] & 0xFF);
        uint32_t length = ((loader->bytes[loader->index++] & 0xFF) << 24) |
            ((loader->bytes[loader->index++] & 0xFF) << 16) |
            ((loader->bytes[loader->index++] & 0xFF) << 8) |
            (loader->bytes[loader->index++] & 0xFF);

        /* Skip the bytes occupied by the unrecognized attribute. */
        loader->index += length;
    }
}

void k_SymbolLoader_parseField(SymbolLoader* loader) {
    // Flags
    uint16_t flags = ((loader->bytes[loader->index++] & 0xFF) << 8) |
        (loader->bytes[loader->index++] & 0xFF);

    // Name Index
    uint16_t nameIndex = ((loader->bytes[loader->index++] & 0xFF) << 8) |
        (loader->bytes[loader->index++] & 0xFF);

    // Descriptor Index
    uint16_t descriptorIndex = ((loader->bytes[loader->index++] & 0xFF) << 8) |
        (loader->bytes[loader->index++] & 0xFF);

    // Table Index
    uint16_t tableIndex = ((loader->bytes[loader->index++] & 0xFF) << 8) |
        (loader->bytes[loader->index++] & 0xFF);

    // Define field

    // Skip attribute table
    k_SymbolLoader_skipAttributeTable(loader);
}

void k_SymbolLoader_declareFunction(SymbolLoader* loader,
    k_Symbol_t* symbol, const uint8_t* descriptor, int32_t descriptorSize,
    uint16_t modifiers, uint16_t tableIndex) {
    k_FunctionSymbol_t* functionSymbol = &symbol->context.asFunction;
    k_FunctionSignature_t* signature = k_FunctionSignature_newEx(descriptor, descriptorSize,
        modifiers, tableIndex);
    k_FunctionSymbol_addSignature(functionSymbol, signature);
}

static const uint8_t* newName = "new";

void k_SymbolLoader_parseFunction(SymbolLoader* loader) {
    // Flags
    uint16_t flags = ((loader->bytes[loader->index++] & 0xFF) << 8) |
        (loader->bytes[loader->index++] & 0xFF);

    // Name Index
    uint16_t nameIndex = ((loader->bytes[loader->index++] & 0xFF) << 8) |
        (loader->bytes[loader->index++] & 0xFF);
    k_ConstantPoolUtf8_t* name = loader->constantPool.entries[nameIndex];

    // Descriptor Index
    uint16_t descriptorIndex = ((loader->bytes[loader->index++] & 0xFF) << 8) |
        (loader->bytes[loader->index++] & 0xFF);
    k_ConstantPoolUtf8_t* descriptor = loader->constantPool.entries[descriptorIndex];

    /* NOTE: Parameter threshold set by a statically typed language prevents
     * a dynamically typed language such as KUSH from declaring functions with
     * variable parameters.
     */
    // uint16_t parameterThreshold = ((loader->bytes[loader->index++] & 0xFF) << 8) |
    //     (loader->bytes[loader->index++] & 0xFF);

    uint16_t tableIndex = ((loader->bytes[loader->index++] & 0xFF) << 8) |
        (loader->bytes[loader->index++] & 0xFF);

    // Define function
    k_Symbol_t* classSymbol = loader->symbol;
    Scope* classScope = classSymbol->context.asClass.classScope;
    k_Symbol_t* functionSymbol = k_Scope_resolve(classScope, descriptor->bytes);
    if (functionSymbol == NULL) {
        functionSymbol = k_Symbol_forFunction(NULL, classScope);
        const uint8_t* name0 = name->bytes;
        int32_t nameSize0 = name->length;
        if (jtk_CString_equals(name->bytes, name->length, "<initialize>", 12)) {
            name0 = newName;
            nameSize0 = 3;
        }
        functionSymbol->name = name0;
        functionSymbol->nameSize = nameSize0;
        functionSymbol->modifiers = flags;
        k_Scope_defineEx(classScope, name0, nameSize0, functionSymbol);
    }
    k_SymbolLoader_declareFunction(loader, functionSymbol, descriptor->bytes,
        descriptor->length, flags, tableIndex);

    // Skip attribute table
    k_SymbolLoader_skipAttributeTable(loader);
}

void k_SymbolLoader_destroyConstantPool(SymbolLoader* loader) {
    k_ConstantPool_t* constantPool = &loader->constantPool;
    int32_t i;
    for (i = 1; i <= constantPool->size; i++) {
        k_ConstantPoolEntry_t* entry = (k_ConstantPoolEntry_t*)constantPool->entries[i];
        if (entry->tag == KUSH_CONSTANT_POOL_TAG_UTF8) {
            k_ConstantPoolUtf8_t* utf8Entry = (k_ConstantPoolUtf8_t*)entry;
            deallocate(utf8Entry->bytes);
        }
        deallocate(entry);
    }
    deallocate(loader->constantPool.entries);
}

k_Symbol_t* k_SymbolLoader_parse(SymbolLoader* loader, uint8_t* bytes,
    int32_t size) {
    Compiler* compiler = loader->compiler;
    ErrorHandler* errorHandler = compiler->errorHandler;
    jtk_Logger_t* logger = compiler->logger;

    loader->bytes = bytes;
    loader->size = size;

    if (loader->index + KUSH_FEB_HEADER_SIZE < size) {
        uint32_t magicNumber = ((loader->bytes[loader->index++] & 0xFF) << 24) |
                               ((loader->bytes[loader->index++] & 0xFF) << 16) |
                               ((loader->bytes[loader->index++] & 0xFF) << 8) |
                               (loader->bytes[loader->index++] & 0xFF);
        if (magicNumber == KUSH_BINARY_ENTITY_FORMAT_MAGIC_NUMBER) {
            uint16_t majorVersion = (uint16_t)(((uint32_t)(loader->bytes[loader->index++] & 0xFF) << 8) |
                (loader->bytes[loader->index++] & 0xFF));
            uint16_t minorVersion = (uint16_t)(((uint32_t)(loader->bytes[loader->index++] & 0xFF) << 8) |
                (loader->bytes[loader->index++] & 0xFF));

            if ((majorVersion < KUSH_BINARY_ENTITY_FORMAT_MAJOR_VERSION) ||
                ((majorVersion == KUSH_BINARY_ENTITY_FORMAT_MAJOR_VERSION) &&
                (minorVersion <= KUSH_BINARY_ENTITY_FORMAT_MINOR_VERSION))) {
                uint16_t entityFlags = (uint16_t)(((uint32_t)(loader->bytes[loader->index++] & 0xFF) << 8) |
                    (loader->bytes[loader->index++] & 0xFF));

                // Constant Pool
                k_SymbolLoader_parseConstantPool(loader);

                /* Parse the entity */
                uint8_t type = (loader->bytes[loader->index++] & 0xFF);
                uint16_t flags = ((loader->bytes[loader->index++] & 0xFF) << 8) |
                                (loader->bytes[loader->index++] & 0xFF);
                uint16_t reference = ((loader->bytes[loader->index++] & 0xFF) << 8) |
                                (loader->bytes[loader->index++] & 0xFF);
                uint16_t superclassCount = ((loader->bytes[loader->index++] & 0xFF) << 8) |
                    (loader->bytes[loader->index++] & 0xFF);
                uint16_t* superclasses = allocate(uint16_t, superclassCount);
                int32_t i;
                for (i = 0; i < superclassCount; i++) {
                    superclasses[i] = ((loader->bytes[loader->index++] & 0xFF) << 8) |
                        (loader->bytes[loader->index++] & 0xFF);
                }

                k_ConstantPoolUtf8_t* descriptor = loader->constantPool.entries[reference];

                Scope* classScope = scopeForStructure(NULL);
                loader->symbol = k_Symbol_forClassAlt( classScope, descriptor->bytes,
                    descriptor->length);
                classScope->symbol = loader->symbol;

                /* Skip attribute table */
                k_SymbolLoader_skipAttributeTable(loader);

                /* Parse fields
                 * fieldCount fieldEntity*
                 */
                uint16_t fieldCount = ((loader->bytes[loader->index++] & 0xFF) << 8) |
                    (loader->bytes[loader->index++] & 0xFF);
                uint16_t fieldTableSize = ((loader->bytes[loader->index++] & 0xFF) << 8) |
                    (loader->bytes[loader->index++] & 0xFF);
                int32_t j;
                for (j = 0; j < fieldCount; j++) {
                    k_SymbolLoader_parseField(loader);
                }

                /* Parse functions
                 * functionCount functionEntity*
                 */

                uint16_t functionCount = ((loader->bytes[loader->index++] & 0xFF) << 8) |
                    (loader->bytes[loader->index++] & 0xFF);
                uint16_t functionTableSize = ((loader->bytes[loader->index++] & 0xFF) << 8) |
                    (loader->bytes[loader->index++] & 0xFF);
                int32_t k;
                for (k = 0; k < functionCount; k++) {
                    k_SymbolLoader_parseFunction(loader);
                }
            }
            else {
                handleGeneralError(errorHandler, loader, ERROR_INVALID_FEB_VERSION);
            }
        }
        else {
            handleGeneralError(errorHandler, loader, ERROR_CORRUPTED_BINARY_ENTITY);
        }
    }

    k_Symbol_t* result = loader->symbol;


    /* Reset the symbol loader. */
    loader->index = 0;
    loader->bytes = NULL;
    loader->size = 0;
    // TODO: The constant pool needs to be freed.
    /*if (loader->constantPool.entries != NULL) {
        k_SymbolLoader_destroyConstantPool(loader);
    }*/
    loader->constantPool.size = 0;
    loader->constantPool.entries = NULL;
    loader->symbol = NULL;

    return result;
}


int SymbolLoader()
{
	FILE *f = fopen("object", "rb");
	fseek(f, 0, SEEK_END);
	long fsize = ftell(f);
	fseek(f, 0, SEEK_SET);

	uint8_t *bytes = malloc(fsize + 1);

	/*
	 *  READ BINARY FORMATS WITHOUT USING fread()
	 */

	fclose(f);

	bytes[fsize] = 0;

	int i = 0;
	uint16_t magicNumber = bytes[i++] << 24 | bytes[i++] << 16 | bytes[i++] << 8 | bytes[i++];
	uint16_t majorVersion = bytes[i++] << 8 | bytes[i++];
	uint16_t minorVersion = bytes[i++] << 8 | bytes[i++];

	uint16_t structureCount = bytes[i++] << 8 | bytes[i++];
	k_Structure_t** structures;
	for(int i = 0; i < structureCount; ++i) {
		structures[i] = parseStructure();
	}

	uint16_t functionCount = bytes[i++] << 8 | bytes[i++];
	k_Function_t** functions;
	for(int i = 0; i < functionCount; ++i) {
		functions[i] = parseFunction();
	}

	Module* k = malloc(sizeof(Module));
	k->magicNumber = magicNumber;
	k->majorVersion = majorVersion;
	k->minorVersion = minorVersion;
	k->structureCount = structureCount;
	k->functionCount = functionCount;

	// Serializing Structures
	for(int i = 0; i < k->structureCount; ++i) {
		k->structures = structures[i];
	}

	// Serializing Functions
	for(int i = 0; i < k->functionCount; ++i) {
		k->functions = functions[i];
	}

	k_Structure_t* parseStructure() {
		uint16_t flags = bytes[i++] << 8 | bytes[i++];

		uint16_t nameSize = bytes[i++] << 8 | bytes[i++];
		uint8_t* name = malloc(nameSize*sizeof(uint8_t);
		for(int j = 0; j < nameSize; ++j) {
			name[j] = bytes[i++];
		}

		uint16_t attributeCount = bytes[i++] << 8 | bytes[i++];
		uint16_t* attributeNameSizes = malloc(attributeCount*sizeof(uint16_t));
		for(int j = 0; j < attributeCount; ++j) {
			attributeNameSizes[j] = malloc(sizeof(uint16_t));
			attributeNameSizes[j] = bytes[i++] << 8 | bytes[i++];
		}

		uint8_t** attributeNames = malloc(attributeCount*sizeof(uint8_t*));
		for(int j = 0; j < attributeCount; ++j) {
			attributeNames[j] = malloc(attributeNameSizes[j]*sizeof(uint8_t));
			for(int k = 0; k < attributeNameSizes[j]; ++k) {
				attributeNames[j][k] = bytes[i++];
			}
		}

		k_Structure_t* k = malloc(sizeof(k_Structure_t));
		k->flags = flags;
		k->nameSize = nameSize;
		k->name = name;
		k->attributeCount = attributeCount;
		k->attributeNameSizes = attributeNameSizes;
		k->attributeNames = attributeNames;
		return k;
	}

	k_Function_t* parseFunction() {
		uint16_t flags = (bytes[i++] << 8) | (bytes[i++]);

		uint16_t nameSize = (bytes[i++] << 8) | (bytes[i++]);
		uint8_t* name = malloc(nameSize*sizeof(uint8_t);
		for(int j = 0; j < nameSize; ++j) {
			name[j] = bytes[i++];
		}

		uint16_t signatureSize = bytes[i++] << 8 | bytes[i++];
		uint8_t* signature = malloc(signatureSize*sizeof(uint8_t));
		for(int j = 0; j < signatureSize; ++j) {
			signature[j] = bytes[i++];
		}

		uint8_t captureCount = bytes[i++];
		uint8_t* captures = malloc(captureCount*sizeof(uint8_t));
		for(int j = 0; j < captureCount; ++j) {
			captures[j] = bytes[i++];
		}

		k_Function_t* k = malloc(sizeof(k_Function_t));
		k->flags = flags;
		k->nameSize = nameSize;
		k->name = name;
		k->signatureSize = signatureSize;
		k->signature = signature;
		k->captureCount = captureCount;
		k->captures = captures;
		return k;
	}
	return k;
}
