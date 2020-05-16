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
#define PLATFORM_CPU PLATFORM_CPU_X86
#define PLATFORM_WORD_SIZE PLATFORM_WORD_SIZE_32

/* ucontext/mcontext stuff; saner if OS >= 10.5 */
#include <sys/signal.h>
#include <sys/ucontext.h>

#define UC_MCONTEXT(UC) UC->uc_mcontext
typedef mcontext_t MCONTEXT_T;
typedef ucontext_t ExceptionInformation;

#define MAXIMUM_MAPPABLE_MEMORY ((1U<<31)-2*heap_segment_size)
#define IMAGE_BASE_ADDRESS 0x04000000

#include "lisptypes.h"
#include "x86-constants32.h"

/* offsets into uc_mcontext.ss */

#define REG_EAX 0
#define REG_EBX 1
#define REG_ECX 2
#define REG_EDX 3
#define REG_EDI 4
#define REG_ESI 5
#define REG_EBP 6
#define REG_ESP 7
#define REG_EFL 9
#define REG_EIP 10

/* xp accessors, sigreturn stuff */
#define DARWIN_USE_PSEUDO_SIGRETURN 1

extern void darwin_sigreturn(ExceptionInformation *, unsigned);
extern natural os_major_version;

#define DarwinSigReturn(context) do {		     \
    darwin_sigreturn(context, 0x1e);		     \
    Bug(context,"sigreturn returned");		     \
  } while (0)


#define xpGPRvector(x) ((natural *)(&(UC_MCONTEXT(x)->__ss)))
#define xpGPR(x,gprno) (xpGPRvector(x)[gprno])
#define set_xpGPR(x,gpr,new) xpGPR((x),(gpr)) = (natural)(new)
#define xpPC(x) (xpGPR(x,Iip))
#define eflags_register(xp) xpGPR(xp,Iflags)
#define xpFPRvector(x) ((natural *)(&(UC_MCONTEXT(x)->__fs.__fpu_xmm0)))
#define xpMMXvector(x) (&(UC_MCONTEXT(x)->__fs.__fpu_stmm0))
/* Note that this yields only the lower half of the MMX reg on x8632 */
#define xpMMXreg(x,n) *(natural *)&(xpMMXvector(x)[n])
#define xpMXCSR(x) (UC_MCONTEXT(x)->__fs.__fpu_mxcsr)
#define SIGNUM_FOR_INTN_TRAP SIGSEGV /* Not really, but our Mach handler fakes that */
#define IS_MAYBE_INT_TRAP(info,xp) ((UC_MCONTEXT(xp)->__es.__trapno == 0xd) && (((UC_MCONTEXT(xp)->__es.__err)&7)==2))
#define IS_PAGE_FAULT(info,xp) (UC_MCONTEXT(xp)->__es.__trapno == 0xe)
#define SIGRETURN(context) DarwinSigReturn(context)

#include <mach/mach.h>
#include <mach/mach_error.h>
#include <mach/machine/thread_state.h>
#include <mach/machine/thread_status.h>

#include "os-darwin.h"

