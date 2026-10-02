/* SPDX-License-Identifier: Apache-2.0 */

/*
 * Lisp backtrace for arm64.  (albt.c is the 32-bit ARM version.)
 */

#include "lispdcmd.h"
#include <dlfcn.h>

extern Boolean lisp_frame_p(lisp_frame *);

static void
print_lisp_frame(lisp_frame *frame)
{
  LispObj fun = frame->savefn, rpc = frame->savelr;
  int delta = 0;
  Dl_info info;
  char *spname;

  if ((fun == 0) || (fun == fulltag_misc)) {
    spname = "unknown ?";
    if (dladdr((void *)ptr_from_lispobj(rpc), &info) && info.dli_sname) {
      spname = (char *)(info.dli_sname);
#ifdef DARWIN
      if (spname[-1] != '_') {
        --spname;
      }
#endif
    }
    Dprintf("(#x%016lX) #x%016lX : (subprimitive %s)", (natural)frame, rpc,
            spname);
  } else {
    if ((fulltag_of(fun) != fulltag_misc) ||
        (header_subtag(header_of(fun)) != subtag_function)) {
      Dprintf("(#x%016lX) #x%016lX : (not a function!)", (natural)frame, rpc);
    } else {
      /* A function is a header followed by its code vector. */
      LispObj code_vector = deref(fun, 1);

      if ((rpc >= (code_vector+misc_data_offset)) &&
          (rpc < ((code_vector+misc_data_offset) +
                  (header_element_count(header_of(code_vector))<<2)))) {
        delta = (rpc - (code_vector+misc_data_offset));
      }
      Dprintf("(#x%016lX) #x%016lX : %s + %d", (natural)frame, rpc,
              print_lisp_object(fun), delta);
    }
  }
}

/*
 * Walk frames from "start" to "end".
 *
 * The lisp part of the control stack contains only lisp frames and
 * u64-vectors (nfp frames, C frames, and the vectors that start_lisp
 * and callbacks use to cover foreign frames).  Anything else means
 * we're not looking at a lisp stack region, so stop.
 */
static void
walk_stack_frames(lisp_frame *start, lisp_frame *end)
{
  lisp_frame *next;
  Dprintf("\n");
  while (start < end) {

    if (lisp_frame_p(start)) {
      print_lisp_frame(start);
      next = start + 1;
    } else {
      LispObj header = *(LispObj *)start;

      if (header_subtag(header) == subtag_u64_vector) {
        next = (lisp_frame *)skip_over_ivector((natural)start, header);
      } else {
        next = NULL;
      }
      if ((next <= start) || (next > end)) {
        fprintf(dbgout, "Bad frame at %p (word #x%016lX)\n", (void *)start,
                header);
        next = end;
      }
    }
    start = next;
  }
}

static char *
interrupt_level_description(TCR *tcr)
{
  signed_natural level = (signed_natural) TCR_INTERRUPT_LEVEL(tcr);
  if (level < 0) {
    if (tcr->interrupt_pending) {
      return "disabled(pending)";
    } else {
      return "disabled";
    }
  } else {
    return "enabled";
  }
}

static void
walk_other_areas(void)
{
  TCR *start = (TCR *)get_tcr(true), *tcr = start->next;
  area *a;
  char *ilevel;

  while (tcr != start) {
    a = tcr->cs_area;
    ilevel = interrupt_level_description(tcr);
    Dprintf("\n\n TCR = 0x%lx, cstack area #x%lx,  native thread ID = 0x%lx, interrupts %s", tcr, a,  tcr->native_thread_id, ilevel);
    walk_stack_frames((lisp_frame *) (a->active), (lisp_frame *) (a->high));
    tcr = tcr->next;
  }
}

void
plbt_sp(LispObj currentSP)
{
  area *cs_area;

  TCR *tcr = (TCR *)get_tcr(true);
  char *ilevel = interrupt_level_description(tcr);

  /*
   * If we're not running lisp code (e.g., we're in the kernel
   * debugger after an exception), currentSP may point into C frames.
   * The thread's lisp frames start at last_lisp_frame.
   */
  if (tcr->valence != TCR_STATE_LISP) {
    currentSP = (LispObj)tcr->last_lisp_frame;
  }
  cs_area = tcr->cs_area;
  if ((((LispObj) ptr_to_lispobj(cs_area->low)) > currentSP) ||
      (((LispObj) ptr_to_lispobj(cs_area->high)) < currentSP)) {
    Dprintf("\nStack pointer [#x%lX] in unknown area.", currentSP);
  } else {
    fprintf(dbgout, "current thread: tcr = 0x%lx, native thread ID = 0x%lx, interrupts %s\n", tcr, tcr->native_thread_id, ilevel);
    walk_stack_frames((lisp_frame *) ptr_from_lispobj(currentSP),
                      (lisp_frame *) (cs_area->high));
    walk_other_areas();
  }
}

void
plbt(ExceptionInformation *xp)
{
  plbt_sp(xpSP(xp));
}
