#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define ARRAY_HEADER_SIZE 4
#define afterHeader(pointer) ((uint8_t*)(pointer) + ARRAY_HEADER_SIZE)

uint8_t* ksAllocateArray(int32_t itemSize, int32_t itemCount) {
   uint8_t* result = malloc(ARRAY_HEADER_SIZE + (itemSize * itemCount));
   /* Set headers */
   ((int32_t*)result)[0] = itemCount;
   return result;
}

int32_t ksArrayGetI32(int8_t* array, int32_t index) {
   return ((int32_t*)afterHeader(array))[index];
}

int32_t* ksArrayGetPointerI32(int8_t* array, int32_t index) {
   return &((int32_t*)afterHeader(array))[index];
}

int32_t ksArrayGetSize(int8_t* array) {
   return ((int32_t*)array)[0];
}