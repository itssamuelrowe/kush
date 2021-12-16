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

// Wedensday, October 18, 2017

#ifndef KUSH_CONFIGURATION_H
#define KUSH_CONFIGURATION_H

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>

/* Uncomment/comment the following definition to enable/disable
 * the logger at compile-time.
 */
#ifndef JTK_LOGGER_DISABLE
    // #define JTK_LOGGER_DISABLE
#endif

#include <jtk/Configuration.h>

#ifndef NULL
    #define NULL 0
#endif

#define KUSH_VERSION_MAJOR 0
#define KUSH_VERSION_MINOR 1

#define allocate(type, units) (type*)allocate0(sizeof (type) * (units))
#define deallocate(object) free(object)
#define k_Assert_assertObject(object, message) assert((object) != NULL)
#define k_Assert_assertTrue(expression, message) assert(expression)
#define k_Assert_assertFalse(expression, message) assert(!(expression))
#define k_Assert_assertIndex(index, size, message) assert(((index) < (size)) && ((index) >= 0))
#define k_Assert_assertRange(startIndex, stopIndex, size, message) puts("TODO: k_Assert_assertRange");
#define k_Assert_assertAddIndex(index, size, message) assert(((index) <= (size)) && ((index) >= 0))

void* allocate0(int32_t size);

#define controlError() printf("[internal error] %s:%d: Control should not reach here.\n", __FILE__, __LINE__);

#endif /* KUSH_CONFIGURATION_H */
