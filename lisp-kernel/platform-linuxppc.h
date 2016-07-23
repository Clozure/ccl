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
#define PLATFORM_OS PLATFORM_OS_LINUX
#define PLATFORM_CPU PLATFORM_CPU_PPC
#define PLATFORM_WORD_SIZE PLATFORM_WORD_SIZE_32

typedef struct ucontext ExceptionInformation;

#define MAXIMUM_MAPPABLE_MEMORY (1U<<30)
#define IMAGE_BASE_ADDRESS 0x31000000

#include "lisptypes.h"
#include "ppc-constants32.h"

/* xp accessors.  Logically identical on linuxppc32/64. */
#define XP_PTREGS(x) ((x)->uc_mcontext.regs)
#define xpGPRvector(x) ((natural *)(XP_PTREGS(x)))
#define xpGPR(x,gprno) (xpGPRvector(x)[gprno])
#define set_xpGPR(x,gpr,new) xpGPR((x),(gpr)) = (natural)(new)
#define xpPC(x) (*((pc*)(&(XP_PTREGS(x)->nip))))
#define set_xpPC(x,new) (xpPC(x) = (pc)(new))
#define xpLR(x) (*((pc*)(&(XP_PTREGS(x)->link))))
#define xpCTR(x) (*(pc*)(&(XP_PTREGS(x)->ctr)))
#define xpXER(x) (XP_PTREGS(x)->xer)
#define xpCCR(x) (XP_PTREGS(x)->ccr)
#define xpMSR(x) (XP_PTREGS(x)->msr)
#define xpDSISR(x) (XP_PTREGS(x)->dsisr)
#define xpDAR(x) (XP_PTREGS(x)->dar)
#define xpTRAP(x) (XP_PTREGS(x)->trap)
#define xpFPSCR(x) (XP_PTREGS(x)->gpr[PT_FPSCR])
#define xpFPRvector(x) ((double *)(&(XP_PTREGS(x)->gpr[PT_FPR0])))
#define xpFPR(x,fprno) (xpFPRvector(x)[fprno])

/* 
   Work around a Darwin G5 bug (present in OSX 10.2.7, 10.2.8, and later
   versions.  See platform-darwinppc*.h for details
*/
#define DarwinSigReturn(context)
#define SIGRETURN(context)

#include "os-linux.h"
