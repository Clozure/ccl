/*
 * Copyright 1994-2009 Clozure Associates
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifndef __lisptypes__
#define __lisptypes__

#include <stdint.h>

#if WORD_SIZE == 64
typedef uint64_t LispObj;
typedef uint64_t natural;
typedef int64_t signed_natural;
#else
typedef uint32_t LispObj;
typedef uint32_t natural;
typedef int32_t signed_natural;
#endif

typedef int32_t lisp_char_code;

typedef int OSStatus, OSErr;
#define noErr ((OSErr) 0)
typedef int Boolean;
typedef void *LogicalAddress;
typedef char *Ptr, *BytePtr, *StringPtr;

#define true 1
#define false 0

#endif /*__lisptypes__ */
