/* SPDX-License-Identifier: Apache-2.0 */

/*
 * Darwin/arm64 (Apple Silicon) platform header.
 *
 * Low-tag scheme matches linuxarm64 / compiler/ARM64.  Unlike
 * darwinx8664 we cannot reserve low memory with -pagezero_size (see
 * doc/porting/darwin.md); STATIC_BASE_ADDRESS in arm64-constants.h is
 * provisional and will need a register-relative (rnil) redesign.
 */

#define WORD_SIZE 64
#define PLATFORM_OS PLATFORM_OS_DARWIN
#define PLATFORM_CPU PLATFORM_CPU_ARM64
#define PLATFORM_WORD_SIZE PLATFORM_WORD_SIZE_64

#ifndef _DARWIN_C_SOURCE
#define _DARWIN_C_SOURCE
#endif

#include <sys/signal.h>
#include <sys/ucontext.h>

typedef mcontext_t MCONTEXT_T;
typedef ucontext_t ExceptionInformation;
#define UC_MCONTEXT(UC) UC->uc_mcontext

#define MAXIMUM_MAPPABLE_MEMORY (512L<<30L)
/* Preferred image base; ASLR may relocate. Same provisional value as linuxarm64. */
#define IMAGE_BASE_ADDRESS 0x300000000000L

#include "lisptypes.h"
#include "arm64-constants.h"

#ifndef TCR_BIAS
#define TCR_BIAS (0)
#endif

#ifndef unbound
#define unbound unbound_marker
#endif
#ifndef slot_unbound
#define slot_unbound slot_unbound_marker
#endif
#ifndef stack_alloc_marker
#define stack_alloc_marker SUBTAG(fulltag_imm_1, 6)
#endif

#ifndef ABI_VERSION_CURRENT
#define ABI_VERSION_MIN 1046
#define ABI_VERSION_CURRENT 1046
#define ABI_VERSION_MAX 1046
#endif

#ifndef lisp_frame_size
#define lisp_frame_size sizeof(lisp_frame)
#endif

#ifndef fixnum_bitmask
#define fixnum_bitmask(n)  (1LL<<((n)+fixnumshift))
#endif

#ifndef NSAVEREGS
#define NSAVEREGS 4
#endif

#ifndef subtag_single_float
#define subtag_single_float fulltag_single_float
#endif

/* is_node_fulltag: prefer gc.h's ARM64 definition when present. */

/* AArch64 instructions are 32-bit; also defined under DARWIN in arm64-exceptions.h. */
#ifndef __lisp_kernel_opcode_defined
#define __lisp_kernel_opcode_defined
typedef uint32_t opcode, *pc;
#endif

#define DARWIN_USE_PSEUDO_SIGRETURN 1

extern void darwin_sigreturn(ExceptionInformation *, unsigned);
extern natural os_major_version;

#define DarwinSigReturn(context) do {                \
    darwin_sigreturn(context, 0x1e);                 \
    Bug(context,"sigreturn returned");               \
  } while (0)
#define SIGRETURN(context) DarwinSigReturn(context)

/* arm_thread_state64_t: __x[0..28], __fp, __lr, __sp, __pc, __cpsr, __flags */
#define xpGPRvector(x) ((natural *)(&(UC_MCONTEXT(x)->__ss.__x)))
#define xpGPR(x,gprno) (xpGPRvector(x)[gprno])
#define set_xpGPR(x,gpr,new) xpGPR((x),(gpr)) = (natural)(new)
#define xpSP(x) (UC_MCONTEXT(x)->__ss.__sp)
#define xpLR(x) (UC_MCONTEXT(x)->__ss.__lr)
#define xpPC(x) (*(pc *)&(UC_MCONTEXT(x)->__ss.__pc))
#define set_xpPC(x, new) (xpPC(x) = (pc)(new))
#define xpFaultAddress(x) (UC_MCONTEXT(x)->__es.__far)
#define xpPSR(x) (UC_MCONTEXT(x)->__ss.__cpsr)

#include <mach/mach.h>
#include <mach/mach_error.h>
#include <mach/machine/thread_state.h>
#include <mach/machine/thread_status.h>

#include "os-darwin.h"
