/* SPDX-License-Identifier: Apache-2.0 */

/*
 * Darwin/arm64 Mach exception scaffolding.
 *
 * Full Mach exception ports (as on darwinx8664 / x86-exceptions.c) are
 * not wired yet.  For early bring-up we keep the Darwin TCR lifecycle
 * hooks so the kernel links, and rely on Unix signal handlers from
 * arm64-exceptions.c (udf → SIGILL), same as linuxarm64.
 *
 * TODO: port associate_tcr_with_exception_port / mach_exc_server /
 * setup_mach_exception_handling from x86-exceptions.c, then flip
 * use_mach_exception_handling back to the Darwin default path.
 */

#include "lisp.h"
#include "lisp-exceptions.h"
#include "threads.h"

#include <stdlib.h>
#include <stdio.h>

#ifdef DARWIN

void
associate_tcr_with_exception_port(mach_port_t port, TCR *tcr)
{
  (void)port;
  (void)tcr;
}

void
disassociate_tcr_from_exception_port(mach_port_t port)
{
  (void)port;
}

void
darwin_exception_init(TCR *tcr)
{
  /*
   * Intentionally empty for now: Unix signal handlers installed by
   * exception_init() / install_pmcl_exception_handlers() cover UUO
   * delivery.  Mach ports allocated in allocate_tcr() are retained but
   * unused until the Mach server is ported.
   */
  (void)tcr;
}

void
darwin_exception_cleanup(TCR *tcr)
{
  mach_port_t exception_port;

  if (tcr == NULL)
    return;

  exception_port = (mach_port_t)((natural)TCR_AUX(tcr)->io_datum);
  if (exception_port != MACH_PORT_NULL) {
    disassociate_tcr_from_exception_port(exception_port);
    mach_port_deallocate(mach_task_self(), exception_port);
    TCR_AUX(tcr)->io_datum = NULL;
  }
}

/* Stubs until Mach exception server is ported from x86-exceptions.c. */

ExceptionInformation *
create_thread_context_frame(mach_port_t thread,
                            natural *stackp,
                            siginfo_t *info,
                            TCR *tcr,
                            native_thread_state_t *ts)
{
  (void)thread;
  (void)stackp;
  (void)info;
  (void)tcr;
  (void)ts;
  return NULL;
}

kern_return_t
tcr_establish_lisp_exception_port(TCR *tcr)
{
  (void)tcr;
  return KERN_SUCCESS;
}

#endif /* DARWIN */
