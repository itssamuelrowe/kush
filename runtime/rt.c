#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define ARRAY_HEADER_SIZE 4

#define STRING_HEADER_SIZE 4

#define ksArrayAfterHeader(pointer) ((uint8_t*)(pointer) + ARRAY_HEADER_SIZE)

uint8_t* ksAllocateArray(int32_t itemSize, int32_t itemCount) {
   uint8_t* result = malloc(ARRAY_HEADER_SIZE + (itemSize * itemCount));
   /* Set headers */
   ((int32_t*)result)[0] = itemCount;
   return result;
}

int32_t ksArrayGetI32(int8_t* array, int32_t index) {
   return ((int32_t*)ksArrayAfterHeader(array))[index];
}

int32_t* ksArrayGetPointerI32(int8_t* array, int32_t index) {
   return &((int32_t*)ksArrayAfterHeader(array))[index];
}

int32_t ksArrayGetSize(int8_t* array) {
   return ((int32_t*)array)[0];
}

const uint8_t* ksToCString(const uint8_t* value) {
   return (((uint8_t*)(value)) + STRING_HEADER_SIZE);
}

int32_t ksStringGetSize(const uint8_t* string) {
   return (string[0] << 6 |
      string[1] << 4 |
      string[2] << 2 |
      string[3] << 0);
}

int32_t ksStringGet(const uint8_t* value, int32_t index) {
   return ksToCString(value)[index];
}

const uint8_t types[12][15] = {
   { 0, 0, 0, 9, 's', 't', 'r', 'u', 'c', 't', 'u', 'r', 'e', '\0' },
   { 0, 0, 0, 7, 'i', 'n', 't', 'e', 'g', 'e', 'r', '\0' },
   { 0, 0, 0, 7, 'd', 'e', 'c', 'i', 'm', 'a', 'l', '\0' },
   { 0, 0, 0, 5, 'a', 'r', 'r', 'a', 'y', '\0' },
   { 0, 0, 0, 4, 'v', 'o', 'i', 'd', '\0' },
   { 0, 0, 0, 4, 'n', 'u', 'l', 'l', '\0' },
   { 0, 0, 0, 6, 's', 't', 'r', 'i', 'n', 'g', '\0' },
   { 0, 0, 0, 7, 'b', 'o', 'o', 'l', 'e', 'a', 'n', '\0' },
   { 0, 0, 0, 8, 'f', 'u', 'n', 'c', 't', 'i', 'o', 'n', '\0' },
   { 0, 0, 0, 3, 'a', 'n', 'y', '\0' },
   { 0, 0, 0, 7, 'u', 'n', 'k', 'n', 'o', 'w', 'n', '\0' }
};

const uint8_t* type(__int128_t parts) {
   int32_t part = (int32_t)(parts >> 64);
   return types[part];
}