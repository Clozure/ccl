/* SPDX-License-Identifier: Apache-2.0 */

/*
 * Darwin/arm64 Mach exception handling.
 *
 * Port of the darwinx8664 path in x86-exceptions.c to ARM64 thread
 * state / neon float state / EXC_ARM_* codes.  UUOs (udf) arrive as
 * EXC_BAD_INSTRUCTION → resumed into signal_handler via a synthetic
 * ucontext, then return through pseudo_sigreturn.
 */

#include "lisp.h"
#include "lisp-exceptions.h"
#include "threads.h"
#include "area.h"
#include "memprotect.h"

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <pthread.h>
#include <signal.h>
#include <sys/mman.h>
#include <mach/mach.h>
#include <mach/mach_error.h>
#include <mach/arm/exception.h>
#include <dlfcn.h>

#ifdef DARWIN

extern area *readonly_area;
extern int page_size, log2_page_size;

/* CCL_DEBUG_WX=1 enables verbose Mach-exception diagnostics (AltConsole). */
static int
darwin_arm64_debug_wx(void)
{
  static int flag = -1;
  if (flag < 0)
    flag = (getenv("CCL_DEBUG_WX") != NULL);
  return flag;
}

/* Resolve Darwin W^X PROTECTION_FAILURE without the unix signal path.
 * The signal_handler path depends on ESR in the synthetic mcontext; when
 * that is missing/zero we never UnProtect / mprotect(RX) and the same
 * page oscillates forever → beachball ("app not responding").
 * pc==far ⇒ instruction abort (need RX); else data abort into RX (need RW).
 *
 * Only the purified readonly_area is handled here: mprotect is
 * process-wide, so fixing it up from the Mach exception thread works.
 * MAP_JIT write-protection is per-thread (APRR); toggling
 * pthread_jit_write_protect_np on this (server) thread would not change
 * the faulting thread's state, so JIT faults are deliberately NOT fixed
 * up here — all JIT stores must go through the same-thread C helpers
 * (darwin_arm64_jit_install_code and friends), and a JIT WP fault falls
 * through to signal_handler as a real error instead of looping. */
static Boolean
darwin_arm64_try_wx_fixup(natural pcval, natural far, Boolean nx)
{
  LogicalAddress page;
  int rc;

  if (!far)
    return false;

  if (nx) {
    if (readonly_area &&
        pcval >= (natural)readonly_area->low &&
        pcval < (natural)readonly_area->active) {
      page = (LogicalAddress)truncate_to_power_of_2(pcval, log2_page_size);
      rc = mprotect(page, page_size, PROT_READ | PROT_EXEC);
      if (rc == 0)
        return true;
      fprintf(dbgout, "[darwinarm64] mprotect(RX) page 0x%lx failed errno=%d\n",
              (unsigned long)(natural)page, errno);
      fflush(dbgout);
      return false;
    }
    return false;
  }

  /* Data fault: write into RX purified code. */
  if (readonly_area &&
      far >= (natural)readonly_area->low &&
      far < (natural)readonly_area->active) {
    page = (LogicalAddress)truncate_to_power_of_2(far, log2_page_size);
    if (UnProtectMemory(page, page_size) == 0)
      return true;
    fprintf(dbgout, "[darwinarm64] UnProtect page 0x%lx failed errno=%d\n",
            (unsigned long)(natural)page, errno);
    fflush(dbgout);
    return false;
  }
  return false;
}

#define TCR_FROM_EXCEPTION_PORT(p) find_tcr_from_exception_port(p)
#define TCR_TO_EXCEPTION_PORT(t) \
  ((mach_port_name_t)((natural)(((TCR *)(t))->io_datum)))

#define LISP_EXCEPTIONS_HANDLED_MASK \
  (EXC_MASK_SOFTWARE | EXC_MASK_BAD_ACCESS | \
   EXC_MASK_BAD_INSTRUCTION | EXC_MASK_ARITHMETIC)

#define NUM_LISP_EXCEPTIONS_HANDLED 4

typedef struct {
  int foreign_exception_port_count;
  exception_mask_t masks[NUM_LISP_EXCEPTIONS_HANDLED];
  mach_port_t ports[NUM_LISP_EXCEPTIONS_HANDLED];
  exception_behavior_t behaviors[NUM_LISP_EXCEPTIONS_HANDLED];
  thread_state_flavor_t flavors[NUM_LISP_EXCEPTIONS_HANDLED];
  natural saved_last_lisp_frame;
} MACH_foreign_exception_state;

extern void pseudo_sigreturn(void);
extern Boolean create_system_thread(size_t, void *, void *(*)(void *), void *);
extern void signal_handler(int, siginfo_t *, ExceptionInformation *, TCR *, int);
extern Boolean use_mach_exception_handling;
void fatal_mach_error(char *format, ...);

#define MACH_CHECK_ERROR(context, x) \
  do { \
    if ((x) != KERN_SUCCESS) \
      fatal_mach_error("Mach error while %s : %d", (context), (x)); \
  } while (0)

#define C_REDZONE_LEN 128
#define C_STK_ALIGN 16
#define TRUNC_DOWN(a, b, c) (((((natural)(a)) - (b)) / (c)) * (c))

#define ts_pc(t) ((t)->__pc)

/* Emergency scratch for signal frames when the faulting SP is already
 * in/near the OS or CCL stack guard.  Without this, setup_signal_frame's
 * memmove into the guard kills the Mach exception thread and the process
 * beachballs / SIGSEGVs with a useless secondary crash.
 *
 * Lifecycle: the flag is set when a frame is placed in the scratch buffer
 * and cleared in do_pseudo_sigreturn when that frame's context is torn
 * down.  A second deep fault while the buffer is occupied is fatal —
 * aliasing the buffer would silently corrupt the first frame. */
static uint8_t darwin_arm64_exc_frame_scratch[8192]
  __attribute__((aligned(16)));
static int darwin_arm64_exc_frame_scratch_used = 0;

static Boolean
darwin_arm64_in_exc_scratch(natural addr)
{
  return (addr >= (natural)darwin_arm64_exc_frame_scratch &&
          addr < ((natural)darwin_arm64_exc_frame_scratch
                  + sizeof(darwin_arm64_exc_frame_scratch)));
}

static LispObj *
find_foreign_sp(LispObj sp, area *foreign_area, TCR *tcr)
{
  BytePtr bsp;
  natural need = sizeof(siginfo_t) + sizeof(ExceptionInformation)
    + 1024 /* mcontext + slop */ + C_REDZONE_LEN + 64;

  /* ARM64 TCR has no foreign_sp (x86); last_lisp_frame is the cstack
   * boundary recorded by ff-call spentries when SP is off the lisp stack. */
  if (((BytePtr)sp < foreign_area->low) ||
      ((BytePtr)sp > foreign_area->high)) {
    sp = (LispObj)(tcr->last_lisp_frame);
  }
  bsp = (BytePtr)((sp - C_REDZONE_LEN) & ~(LispObj)(C_STK_ALIGN - 1));

  /* If the frame would land at/below softlimit (or the area has no room),
   * use the process-wide scratch buffer. */
  if ((natural)bsp < (natural)foreign_area->softlimit + need
      || (natural)bsp < (natural)foreign_area->low + need) {
    if (darwin_arm64_exc_frame_scratch_used) {
      Fatal("Mach exception",
            "nested deep-stack exception frame: scratch buffer in use");
    }
    darwin_arm64_exc_frame_scratch_used = 1;
    if (darwin_arm64_debug_wx()) {
      fprintf(dbgout,
              "\n[darwinarm64] signal frame: SP 0x%lx near/below softlimit "
              "0x%lx — using scratch\n",
              (unsigned long)sp, (unsigned long)(natural)foreign_area->softlimit);
      fflush(dbgout);
    }
    bsp = (BytePtr)darwin_arm64_exc_frame_scratch
      + sizeof(darwin_arm64_exc_frame_scratch);
    bsp = (BytePtr)((natural)bsp & ~(natural)(C_STK_ALIGN - 1));
  }
  return (LispObj *)bsp;
}

TCR *
find_tcr_from_exception_port(mach_port_t port)
{
  mach_port_context_t context = 0;
  kern_return_t kret;

  kret = mach_port_get_context(mach_task_self(), port, &context);
  MACH_CHECK_ERROR("finding TCR from exception port", kret);
  return (TCR *)(natural)context;
}

void
associate_tcr_with_exception_port(mach_port_t port, TCR *tcr)
{
  kern_return_t kret;

  kret = mach_port_set_context(mach_task_self(), port,
                               (mach_port_context_t)(natural)tcr);
  MACH_CHECK_ERROR("associating TCR with exception port", kret);
}

void
disassociate_tcr_from_exception_port(mach_port_t port)
{
  kern_return_t kret;

  kret = mach_port_set_context(mach_task_self(), port, 0);
  MACH_CHECK_ERROR("disassociating TCR with exception port", kret);
}

static void
restore_mach_thread_state(mach_port_t thread,
                          ExceptionInformation *pseudosigcontext,
                          native_thread_state_t *ts)
{
  kern_return_t kret;
  MCONTEXT_T mc = UC_MCONTEXT(pseudosigcontext);

  /* Neon float state lives in __ns on Darwin arm64 mcontext64. */
  kret = thread_set_state(thread,
                          NATIVE_FLOAT_STATE_FLAVOR,
                          (thread_state_t)&(mc->__ns),
                          NATIVE_FLOAT_STATE_COUNT);
  MACH_CHECK_ERROR("setting thread FP state", kret);
  *ts = mc->__ss;
}

kern_return_t
do_pseudo_sigreturn(mach_port_t thread, TCR *tcr, native_thread_state_t *out)
{
  ExceptionInformation *xp;
  MACH_foreign_exception_state *fxs =
    (MACH_foreign_exception_state *)tcr->native_thread_info;

  xp = tcr->pending_exception_context;
  if (xp) {
    tcr->pending_exception_context = NULL;
    tcr->valence = TCR_STATE_LISP;
    if (fxs)
      tcr->last_lisp_frame = fxs->saved_last_lisp_frame;
    if (darwin_arm64_in_exc_scratch((natural)xp))
      darwin_arm64_exc_frame_scratch_used = 0;
    restore_mach_thread_state(thread, xp, out);
    if ((TCR_INTERRUPT_LEVEL(tcr) >= 0) && tcr->interrupt_pending)
      pthread_kill((pthread_t)(tcr->osid), SIGNAL_FOR_PROCESS_INTERRUPT);
  } else {
    Bug(NULL, "no xp here!\n");
  }
  return KERN_SUCCESS;
}

ExceptionInformation *
create_thread_context_frame(mach_port_t thread,
                            natural *new_stack_top,
                            siginfo_t **info_ptr,
                            TCR *tcr,
                            native_thread_state_t *ts)
{
  mach_msg_type_number_t thread_state_count;
  ExceptionInformation *pseudosigcontext;
  MCONTEXT_T mc;
  natural stackp;
  kern_return_t kret;

  stackp = (LispObj)find_foreign_sp(ts->__sp, tcr->cs_area, tcr);
  stackp = TRUNC_DOWN(stackp, sizeof(siginfo_t), C_STK_ALIGN);
  if (info_ptr)
    *info_ptr = (siginfo_t *)stackp;
  stackp = TRUNC_DOWN(stackp, sizeof(*pseudosigcontext), C_STK_ALIGN);
  pseudosigcontext = (ExceptionInformation *)ptr_from_lispobj(stackp);

  stackp = TRUNC_DOWN(stackp, sizeof(*mc), C_STK_ALIGN);
  mc = (MCONTEXT_T)ptr_from_lispobj(stackp);

  memmove(&(mc->__ss), ts, sizeof(*ts));

  thread_state_count = NATIVE_FLOAT_STATE_COUNT;
  kret = thread_get_state(thread,
                          NATIVE_FLOAT_STATE_FLAVOR,
                          (thread_state_t)&(mc->__ns),
                          &thread_state_count);
  MACH_CHECK_ERROR("getting thread FP state", kret);

  thread_state_count = NATIVE_EXCEPTION_STATE_COUNT;
  kret = thread_get_state(thread,
                          NATIVE_EXCEPTION_STATE_FLAVOR,
                          (thread_state_t)&(mc->__es),
                          &thread_state_count);
  MACH_CHECK_ERROR("getting thread exception state", kret);

  UC_MCONTEXT(pseudosigcontext) = mc;
  if (new_stack_top)
    *new_stack_top = stackp;
  return pseudosigcontext;
}

int
setup_signal_frame(mach_port_t thread,
                   void *handler_address,
                   int signum,
                   int code,
                   TCR *tcr,
                   native_thread_state_t *ts,
                   native_thread_state_t *new_ts)
{
  ExceptionInformation *pseudosigcontext;
  int old_valence = tcr->valence;
  natural stackp, *stackpp;
  siginfo_t *info;
  MACH_foreign_exception_state *fxs =
    (MACH_foreign_exception_state *)tcr->native_thread_info;

  if (fxs)
    fxs->saved_last_lisp_frame = tcr->last_lisp_frame;

  pseudosigcontext =
    create_thread_context_frame(thread, &stackp, &info, tcr, ts);
  bzero(info, sizeof(*info));
  info->si_code = code;
  info->si_addr = (void *)(UC_MCONTEXT(pseudosigcontext)->__es.__far);
  info->si_signo = signum;
  pseudosigcontext->uc_onstack = 0;
  pseudosigcontext->uc_sigmask = (sigset_t)0;
  pseudosigcontext->uc_stack.ss_sp = 0;
  pseudosigcontext->uc_stack.ss_size = 0;
  pseudosigcontext->uc_stack.ss_flags = 0;
  pseudosigcontext->uc_link = NULL;
  pseudosigcontext->uc_mcsize = sizeof(*UC_MCONTEXT(pseudosigcontext));
  tcr->pending_exception_context = pseudosigcontext;
  tcr->valence = TCR_STATE_EXCEPTION_WAIT;
  tcr->last_lisp_frame = (natural)ptr_to_lispobj(ts->__sp);

  /* AAPCS64: x0..x4 = handler args; lr = pseudo_sigreturn; sp 16-aligned. */
  bzero(new_ts, sizeof(*new_ts));
  *new_ts = *ts;
  new_ts->__pc = (natural)handler_address;
  new_ts->__lr = (natural)pseudo_sigreturn;
  new_ts->__x[0] = (natural)signum;
  new_ts->__x[1] = (natural)info;
  new_ts->__x[2] = (natural)pseudosigcontext;
  new_ts->__x[3] = (natural)tcr;
  new_ts->__x[4] = (natural)old_valence;
  stackpp = (natural *)stackp;
  /* Keep SP 16-byte aligned (Apple + hardware). */
  stackp = (natural)stackpp & ~(natural)15;
  new_ts->__sp = stackp;
  return 0;
}

kern_return_t
catch_mach_exception_raise(mach_port_t exception_port,
                           mach_port_t thread,
                           mach_port_t task,
                           exception_type_t exception,
                           mach_exception_data_t code,
                           mach_msg_type_number_t code_count)
{
  (void)exception_port;
  (void)thread;
  (void)task;
  (void)exception;
  (void)code;
  (void)code_count;
  abort();
  return KERN_FAILURE;
}

kern_return_t
catch_mach_exception_raise_state(mach_port_t exception_port,
                                 exception_type_t exception,
                                 mach_exception_data_t code,
                                 mach_msg_type_number_t code_count,
                                 int *flavor,
                                 thread_state_t in_state,
                                 mach_msg_type_number_t in_state_count,
                                 thread_state_t out_state,
                                 mach_msg_type_number_t *out_state_count)
{
  int64_t code0 = code[0];
  int signum = 0;
  TCR *tcr = TCR_FROM_EXCEPTION_PORT(exception_port);
  mach_port_t thread = (mach_port_t)((natural)tcr->native_thread_id);
  kern_return_t kret;
  native_thread_state_t *ts = (native_thread_state_t *)in_state;
  native_thread_state_t *out_ts = (native_thread_state_t *)out_state;

  (void)code_count;
  (void)in_state_count;

  if (tcr->flags & (1 << TCR_FLAG_BIT_PENDING_EXCEPTION))
    CLR_TCR_FLAG(tcr, TCR_FLAG_BIT_PENDING_EXCEPTION);

  if ((natural)(ts_pc(ts)) == (natural)pseudo_sigreturn) {
    kret = do_pseudo_sigreturn(thread, tcr, out_ts);
  } else if (tcr->flags & (1 << TCR_FLAG_BIT_PROPAGATE_EXCEPTION)) {
    CLR_TCR_FLAG(tcr, TCR_FLAG_BIT_PROPAGATE_EXCEPTION);
    kret = 17;
  } else {
    switch (exception) {
    case EXC_BAD_ACCESS:
      /* Fast W^X fixup (see darwin_arm64_try_wx_fixup). */
      if (code0 == KERN_PROTECTION_FAILURE && code_count > 1) {
        natural pcval = (natural)ts->__pc;
        natural far = (natural)code[1];
        Boolean nx = (pcval == far);
        static int wx_logs;
        if (darwin_arm64_try_wx_fixup(pcval, far, nx)) {
          /* Successful fixups are normal purify traffic.  Logging them to
             dbgout launches AltConsole and looks like a fault.  Set
             CCL_DEBUG_WX=1 to keep the first few traces. */
          if (wx_logs < 16 && darwin_arm64_debug_wx()) {
            wx_logs++;
            fprintf(dbgout,
                    "[darwinarm64] W^X fixup #%d %s far=0x%lx pc=0x%lx\n",
                    wx_logs, nx ? "RX" : "RW",
                    (unsigned long)far, (unsigned long)pcval);
            fflush(dbgout);
          }
          *out_ts = *ts;
          *out_state_count = NATIVE_THREAD_STATE_COUNT;
          return KERN_SUCCESS;
        }
      }
      /* Register/TCR summary for the first few faults; CCL_DEBUG_WX only.
         Reads only thread state and TCR fields — never chases pointers
         out of a possibly-corrupt image (that killed the exception server
         during bring-up). */
      if (darwin_arm64_debug_wx()) {
        static int bad_access_logs;
        if (bad_access_logs < 8) {
          Dl_info di;
          bad_access_logs++;
          if (dladdr((void *)(natural)ts->__pc, &di) && di.dli_fname) {
            fprintf(dbgout, "  pc_sym=%s+%lu in %s\n",
                    di.dli_sname ? di.dli_sname : "?",
                    (unsigned long)((natural)ts->__pc - (natural)di.dli_saddr),
                    di.dli_fname);
          }
          fprintf(dbgout,
                  "\n[darwinarm64] EXC_BAD_ACCESS #%d code0=%lld "
                  "pc=0x%llx lr=0x%llx sp=0x%llx far=0x%llx "
                  "tcr=%llx valence=%d\n",
                  bad_access_logs, (long long)code0,
                  (unsigned long long)ts->__pc,
                  (unsigned long long)ts->__lr,
                  (unsigned long long)ts->__sp,
                  (unsigned long long)(code_count > 1 ? code[1] : 0),
                  (unsigned long long)(natural)tcr,
                  (int)tcr->valence);
          fflush(dbgout);
        }
      }
      /* Alignment / debug faults still surface as BAD_ACCESS. */
      if (code0 == EXC_ARM_DA_ALIGN || code0 == EXC_ARM_SP_ALIGN)
        signum = SIGBUS;
      else
        signum = SIGSEGV;
      break;
    case EXC_BAD_INSTRUCTION:
      /* udf #n → EXC_ARM_UNDEFINED → SIGILL (UUO path). */
      signum = SIGILL;
      break;
    case EXC_SOFTWARE:
      if (code0 == EXC_ARM_BREAKPOINT)
        signum = SIGTRAP;
      else
        signum = SIGILL;
      break;
    case EXC_ARITHMETIC:
      signum = SIGFPE;
      break;
    default:
      break;
    }
    if (signum) {
      kret = setup_signal_frame(thread,
                                (void *)signal_handler,
                                signum,
                                (int)code0,
                                tcr,
                                ts,
                                out_ts);
    } else {
      kret = 17;
    }
  }

  if (kret) {
    *out_state_count = 0;
    *flavor = 0;
  } else {
    *out_state_count = NATIVE_THREAD_STATE_COUNT;
  }
  return kret;
}

kern_return_t
catch_mach_exception_raise_state_identity(mach_port_t exception_port,
                                          mach_port_t thread,
                                          mach_port_t task,
                                          exception_type_t exception,
                                          mach_exception_data_t code,
                                          mach_msg_type_number_t code_count,
                                          int *flavor,
                                          thread_state_t old_state,
                                          mach_msg_type_number_t old_count,
                                          thread_state_t new_state,
                                          mach_msg_type_number_t *new_count)
{
  (void)exception_port;
  (void)thread;
  (void)task;
  (void)exception;
  (void)code;
  (void)code_count;
  (void)flavor;
  (void)old_state;
  (void)old_count;
  (void)new_state;
  (void)new_count;
  abort();
  return KERN_FAILURE;
}

static mach_port_t mach_exception_thread = (mach_port_t)0;

/*
 * ARM64 THREAD_STATE64 alone is 272 bytes; EXCEPTION_STATE raise_state
 * messages are ~336B on the wire, and the MIG max request is >5KiB.
 * x86 Darwin's 256-byte mach_msg_server limit fits x86_THREAD_STATE64
 * but MACH_RCV_TOO_LARGE aborts the exception thread on arm64.
 */
#ifndef MACH_EXCEPTION_MSG_SIZE
#define MACH_EXCEPTION_MSG_SIZE 8192
#endif

void *
exception_handler_proc(void *arg)
{
  extern boolean_t mach_exc_server();
  mach_port_t p = (mach_port_t)((natural)arg);
  mach_msg_return_t mr;

  mach_exception_thread = pthread_mach_thread_np(pthread_self());
  mr = mach_msg_server(mach_exc_server, MACH_EXCEPTION_MSG_SIZE, p, 0);
  /* Should never return. */
  fprintf(dbgout, "mach_msg_server returned %d\n", (int)mr);
  abort();
  return NULL;
}

void
mach_exception_thread_shutdown(void)
{
  kern_return_t kret;

  fprintf(dbgout, "terminating Mach exception thread, 'cause exit can't\n");
  kret = thread_terminate(mach_exception_thread);
  if (kret != KERN_SUCCESS)
    fprintf(dbgout, "Couldn't terminate exception thread, kret = %d\n", kret);
}

mach_port_t
mach_exception_port_set(void)
{
  static mach_port_t __exception_port_set = MACH_PORT_NULL;
  kern_return_t kret;

  if (__exception_port_set == MACH_PORT_NULL) {
    kret = mach_port_allocate(mach_task_self(),
                              MACH_PORT_RIGHT_PORT_SET,
                              &__exception_port_set);
    MACH_CHECK_ERROR("allocating thread exception_ports", kret);
    create_system_thread(0, NULL, exception_handler_proc,
                         (void *)((natural)__exception_port_set));
  }
  return __exception_port_set;
}

kern_return_t
tcr_establish_exception_port(TCR *tcr, mach_port_t thread)
{
  kern_return_t kret;
  MACH_foreign_exception_state *fxs =
    (MACH_foreign_exception_state *)tcr->native_thread_info;
  int i;
  unsigned n = NUM_LISP_EXCEPTIONS_HANDLED;
  mach_port_t lisp_port = TCR_TO_EXCEPTION_PORT(tcr), foreign_port;
  exception_mask_t mask = 0;

  kret = thread_swap_exception_ports(thread,
                                     LISP_EXCEPTIONS_HANDLED_MASK,
                                     lisp_port,
                                     MACH_EXCEPTION_CODES | EXCEPTION_STATE,
                                     ARM_THREAD_STATE64,
                                     fxs->masks,
                                     &n,
                                     fxs->ports,
                                     fxs->behaviors,
                                     fxs->flavors);
  if (kret == KERN_SUCCESS) {
    fxs->foreign_exception_port_count = n;
    for (i = 0; i < (int)n; i++) {
      foreign_port = fxs->ports[i];
      if ((foreign_port != lisp_port) && (foreign_port != MACH_PORT_NULL))
        mask |= fxs->masks[i];
    }
    tcr->foreign_exception_status = (int)mask;
  }
  return kret;
}

kern_return_t
tcr_establish_lisp_exception_port(TCR *tcr)
{
  return tcr_establish_exception_port(tcr,
                                      (mach_port_t)((natural)tcr->native_thread_id));
}

kern_return_t
restore_foreign_exception_ports(TCR *tcr)
{
  exception_mask_t m = (exception_mask_t)tcr->foreign_exception_status;
  kern_return_t kret = KERN_SUCCESS;

  if (m) {
    MACH_foreign_exception_state *fxs =
      (MACH_foreign_exception_state *)tcr->native_thread_info;
    int i, n = fxs->foreign_exception_port_count;
    exception_mask_t tm;

    for (i = 0; i < n; i++) {
      if ((tm = fxs->masks[i]) & m) {
        kret = thread_set_exception_ports(
          (mach_port_t)((natural)tcr->native_thread_id),
          tm, fxs->ports[i], fxs->behaviors[i], fxs->flavors[i]);
        MACH_CHECK_ERROR("restoring thread exception ports", kret);
      }
    }
  }
  return kret;
}

kern_return_t
setup_mach_exception_handling(TCR *tcr)
{
  mach_port_t thread_exception_port = TCR_TO_EXCEPTION_PORT(tcr);
  mach_port_t task_self = mach_task_self();
  kern_return_t kret;

  kret = mach_port_insert_right(task_self,
                                thread_exception_port,
                                thread_exception_port,
                                MACH_MSG_TYPE_MAKE_SEND);
  MACH_CHECK_ERROR("adding send right to exception_port", kret);

  kret = tcr_establish_exception_port(tcr,
                                      (mach_port_t)((natural)tcr->native_thread_id));
  if (kret == KERN_SUCCESS) {
    mach_port_t exception_port_set = mach_exception_port_set();
    kret = mach_port_move_member(task_self,
                                 thread_exception_port,
                                 exception_port_set);
  }
  return kret;
}

void
darwin_exception_init(TCR *tcr)
{
  kern_return_t kret;
  MACH_foreign_exception_state *fxs;

  if (!use_mach_exception_handling) {
    /* Unix-signal bring-up: leave Mach ports unused (pre-port stub). */
    tcr->native_thread_info = NULL;
    return;
  }

  fxs = calloc(1, sizeof(MACH_foreign_exception_state));
  tcr->native_thread_info = (void *)fxs;
  if ((kret = setup_mach_exception_handling(tcr)) != KERN_SUCCESS) {
    fprintf(dbgout, "Couldn't setup exception handler - error = %d\n", kret);
    terminate_lisp();
  }
}

void
darwin_exception_cleanup(TCR *tcr)
{
  mach_port_t exception_port;
  void *fxs = tcr->native_thread_info;

  if (fxs) {
    tcr->native_thread_info = NULL;
    free(fxs);
  }
  exception_port = TCR_TO_EXCEPTION_PORT(tcr);
  disassociate_tcr_from_exception_port(exception_port);
  mach_port_deallocate(mach_task_self(), exception_port);
  mach_port_destroy(mach_task_self(), exception_port);
}

void
fatal_mach_error(char *format, ...)
{
  va_list args;
  char s[512];

  va_start(args, format);
  vsnprintf(s, sizeof(s), format, args);
  va_end(args);
  Fatal("Mach error", s);
}

#endif /* DARWIN */
