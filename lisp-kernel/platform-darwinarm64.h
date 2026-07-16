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

#include "lisptypes.h"
#include "arm64-constants.h"

extern void darwin_sigreturn(ExceptionInformation *, unsigned);
#define DarwinSigReturn(context) do {                \
    darwin_sigreturn(context, 0x1e);                 \
    Bug(context,"sigreturn returned");               \
  } while (0)
#define SIGRETURN(context) DarwinSigReturn(context)

#include <mach/mach.h>
#include <mach/mach_error.h>
#include <mach/machine/thread_state.h>
#include <mach/machine/thread_status.h>

#include "os-darwin.h"
