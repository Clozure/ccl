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
#define PLATFORM_OS PLATFORM_OS_DARWIN
#define PLATFORM_CPU PLATFORM_CPU_ARM
#define PLATFORM_WORD_SIZE PLATFORM_WORD_SIZE_32

#include <sys/signal.h>
#include <sys/ucontext.h>

typedef ucontext_t ExceptionInformation;

#define MAXIMUM_MAPPABLE_MEMORY (256<<20) /* uh, no */
#define IMAGE_BASE_ADDRESS 0x04001000

#include "lisptypes.h"
#include "arm-constants.h"

#define UC_MCONTEXT(UC) UC->uc_mcontext

/* xp accessors */
#define xpGPRvector(x) ((natural *)&((x)->uc_mcontext->__ss.__r[0]))
#define xpGPR(x,gprno) (xpGPRvector(x))[gprno]
#define xpPC(x) (*((pc*)(&(xpGPR(x,15)))))
#define xpLR(x) (*((pc*)(&(xpGPR(x,14)))))
#define xpPSR(x) xpGPR(x,16)
#define xpFaultAddress(x) ((x)->uc_mcontext->__es.__far)
#define xpFaultStatus(x)  ((x)->uc_mcontext->__es.__fsr)


#define DarwinSigReturn(context)
#define SIGRETURN(context)

#include "os-darwin.h"

