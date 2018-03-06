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

#define WORD_SIZE 64
#define PLATFORM_OS PLATFORM_OS_FREEBSD
#define PLATFORM_CPU PLATFORM_CPU_X86
#define PLATFORM_WORD_SIZE PLATFORM_WORD_SIZE_64

typedef struct __ucontext ExceptionInformation;

#define MAXIMUM_MAPPABLE_MEMORY (512L<<30L)
#define IMAGE_BASE_ADDRESS 0x300000000000L

#include "lisptypes.h"
#include "x86-constants64.h"
#define REG_RDI 1
#define REG_RSI 2
#define REG_RDX 3
#define REG_RCX 4
#define REG_R8 5
#define REG_R9 6
#define REG_RAX 7
#define REG_RBX 8
#define REG_RBP 9
#define REG_R10 10
#define REG_R11 11
#define REG_R12 12
#define REG_R13 13
#define REG_R14 14
#define REG_R15 15
#define REG_RIP 20
#define REG_RFL 22
#define REG_RSP 23

#include <machine/fpu.h>
#define xpGPRvector(x) ((natural *)(&((x)->uc_mcontext)))
#define xpGPR(x,gprno) (xpGPRvector(x)[gprno])
#define set_xpGPR(x,gpr,new) xpGPR((x),(gpr)) = (natural)(new)
#define eflags_register(xp) xpGPR(xp,Iflags)
#define xpPC(x) xpGPR(x,Iip)
#define xpMMXreg(x,n) *((natural *)(&(((struct savefpu *)(&(x)->uc_mcontext.mc_fpstate))->sv_fp[n])))
#define xpXMMregs(x)(&(((struct savefpu *)(&(x)->uc_mcontext.mc_fpstate))->sv_xmm[0]))
#define xpMXCSR(x) (((struct savefpu *)(&(x)->uc_mcontext.mc_fpstate))->sv_env.en_mxcsr)
extern void freebsd_sigreturn(ExceptionInformation *);
#define SIGNUM_FOR_INTN_TRAP SIGBUS
#define IS_MAYBE_INT_TRAP(info,xp) (((uint32_t)(xp->uc_mcontext.mc_trapno) == T_PROTFLT) && ((xp->uc_mcontext.mc_err & 7) == 2))
#define IS_PAGE_FAULT(info,xp) ((uint32_t)(xp->uc_mcontext.mc_trapno) == T_PAGEFLT)
#define SIGRETURN(context) do {freebsd_sigreturn(context); \
    Bug(context,"sigreturn returned"); \
  } while (0)

#include <sys/param.h>

#define AVX_CONTEXT_PRESENT(xp) ((xp)->uc_mcontext.mc_trapno & 4)

#if __FreeBSD_version < 901000
/* AVX stuff.  Funky, because some of this isn't defined until
   fbsd 9.1 headers; if we built on an older OS version, we still need
   to know about this if we run on 9.1+ */

#define AVX_CONTEXT_PTR(xp) (((xp)->uc_mcontext.mc_fpstate[66]))
#define AVX_CONTEXT_SIZE(xp) ((natural)((xp)->uc_mcontext.mc_fpstate[67]))
#else
#define AVX_CONTEXT_PTR(xp) (((xp)->uc_mcontext.mc_xfpustate))
#define AVX_CONTEXT_SIZE(xp) ((natural)((xp)->uc_mcontext.mc_xfpustate_len))
#endif

#include "os-freebsd.h"

