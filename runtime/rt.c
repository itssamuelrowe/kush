#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

uint8_t* ksAllocateArray(int32_t itemSize, int32_t itemCount) {
   return malloc(itemSize * itemCount);
}

int32_t ksArrayGetInteger32(int8_t* array, int32_t index) {
   return ((int32_t*)array)[index];
}

int32_t* ksArrayGetPointerInteger32(int8_t* array, int32_t index) {
   return &((int32_t*)array)[index];
}