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
#define PLATFORM_OS PLATFORM_OS_ANDROID
#define PLATFORM_CPU PLATFORM_CPU_ARM
#define PLATFORM_WORD_SIZE PLATFORM_WORD_SIZE_32

typedef struct ucontext ExceptionInformation;

#define MAXIMUM_MAPPABLE_MEMORY ((3<<28)-(1<<16))
#define IMAGE_BASE_ADDRESS 0x50000000

#include "lisptypes.h"
#include "arm-constants.h"

/* xp accessors */
#define xpGPRvector(x) ((natural *)&((x)->uc_mcontext.arm_r0))
#define xpGPR(x,gprno) (xpGPRvector(x))[gprno]
#define xpPC(x) (*((pc*)(&(xpGPR(x,15)))))
#define xpLR(x) (*((pc*)(&(xpGPR(x,14)))))
#define xpPSR(x) xpGPR(x,16)
#define xpFaultAddress(x) xpGPR(x,17)
#define xpTRAP(x) xpGPR(x,-3)
#define xpERROR(x) xpGPR(x,-2)
#define xpFaultStatus(x) xpERROR(x)

#define DarwinSigReturn(context)
#define SIGRETURN(context)

#include "os-linux.h"

#define PROTECT_CSTACK 1

/* Nonsense */
#define SYS_futex __NR_futex
#define PTHREAD_DESTRUCTOR_ITERATIONS 1
#define __fpurge(f)
