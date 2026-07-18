/* SPDX-License-Identifier: Apache-2.0 */

#define WORD_SIZE 64
#define PLATFORM_OS PLATFORM_OS_DARWIN
#define PLATFORM_CPU PLATFORM_CPU_ARM64
#define PLATFORM_WORD_SIZE PLATFORM_WORD_SIZE_64

#define _DARWIN_C_SOURCE

#include <sys/signal.h>
#include <sys/ucontext.h>

typedef mcontext_t MCONTEXT_T;
typedef ucontext_t ExceptionInformation;
#define UC_MCONTEXT(UC) UC->uc_mcontext

#define MAXIMUM_MAPPABLE_MEMORY (512L<<30L)
// this will end up being some random address
#define IMAGE_BASE_ADDRESS 0x300000000000L

#include "lisptypes.h"
#include "arm64-constants.h"

extern void darwin_sigreturn(ExceptionInformation *, unsigned);
#define DarwinSigReturn(context) do {                \
    darwin_sigreturn(context, 0x1e);                 \
    Bug(context,"sigreturn returned");               \
  } while (0)
#define SIGRETURN(context) DarwinSigReturn(context)

#define xpGPRvector(x) ((natural *)(&(UC_MCONTEXT(x)->__ss.__x)))
#define xpGPR(x,gprno) (xpGPRvector(x)[gprno])
#define xpSP(x) (UC_MCONTEXT(x)->__ss.__sp)
#define xpPC(x) (*(pc *)&(UC_MCONTEXT(x)->__ss.__pc))

#include <mach/mach.h>
#include <mach/mach_error.h>
#include <mach/machine/thread_state.h>
#include <mach/machine/thread_status.h>

#include "os-darwin.h"
