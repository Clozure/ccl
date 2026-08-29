/*
 * Copyright 1994-2010 Clozure Associates
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

#define WORD_SIZE 32
#define PLATFORM_OS PLATFORM_OS_NETBSD
#define PLATFORM_CPU PLATFORM_CPU_ARM
#define PLATFORM_WORD_SIZE PLATFORM_WORD_SIZE_32

#include <ucontext.h>

typedef ucontext_t ExceptionInformation;

#define MAXIMUM_MAPPABLE_MEMORY (3<<29)
#define IMAGE_BASE_ADDRESS 0x10000000

#include "lisptypes.h"
#include "arm-constants.h"

/* NetBSD stores r0-r15 and the CPSR consecutively in __gregs. */
#define xpGPRvector(x) ((natural *)((x)->uc_mcontext.__gregs))
#define xpGPR(x,gprno) (xpGPRvector(x))[gprno]
#define xpPC(x) (*((pc *)(&(xpGPR(x,_REG_PC)))))
#define xpLR(x) (*((pc *)(&(xpGPR(x,_REG_LR)))))
#define xpPSR(x) xpGPR(x,_REG_CPSR)

#define DarwinSigReturn(context)
#define SIGRETURN(context)

#include "os-netbsd.h"

#define PROTECT_CSTACK 1
