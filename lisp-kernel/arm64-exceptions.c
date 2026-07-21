/* SPDX-License-Identifier: Apache-2.0 */

#include "lisp.h"
#include "lisp-exceptions.h"
#include "arm64-constants.h"

#ifdef DARWIN
int page_size = 16384;
int log2_page_size = 14;
#else
/* On some systems, the page size is configurable */
int page_size = 4096;
int log2_page_size = 2;
#endif

void
update_area_active (area **aptr, char *value)
{
  area *a = *aptr;
  for (; a; a = a->older) {
    if ((a->low <= value) && (a->high >= value)) break;
  };
  if (a == NULL) Bug(NULL, "Can't find active area");
  a->active = value;
  *aptr = a;
  for (a = a->younger; a; a = a->younger) {
    a->active = a->high;
  }
}

/* Used when creating variable-length C frames on the control stack */
#define STP_IMM0_IMM1_SP    0xa90007e0U  /* stp x0, x1, [sp, #0] */

/* Used when creating lisp frames on the control stack. */
#define STP_FN_LR_SP        0xa9017be7U  /* stp fn, lr, [sp, #16] */

/*
 * If it looks like we're in the middle of some multi-instruction
 * operation that must be atomic, make it seem as if that operation is
 * either complete, or not yet begun, or otherwise in a safe state.
 */
void
pc_luser_xp(ExceptionInformation *xp, TCR *tcr, signed_natural *alloc_disp)
{
  pc program_counter = xpPC(xp);
  opcode instr = *program_counter;

  /*
   * Stack-allocating a variable-length u64-vector
   *
   * See, for example, the vinsn allocate-variable-c-frame.
   *
   * We are (probably) building a u64-vector on the control stack, and
   * we don't know what the size of the vector is until run-time.
   * Ideally, we'd able to write "stp header, savesp, [sp, sizereg]!"
   * to do this in one instruction.  Unfortunately, that addressing
   * mode is not available: pre-indexing only works for an immediate
   * offset.
   *
   * Thus, we have to write this
   *   sub sp, sp, sizereg            // make room on the stack
   *   stp imm0, imm1, [sp, #0]       // u64-header (plus prevsp in elem 0)
   *
   * We wire the header register to imm0 and the prevsp register to
   * imm1 so that we can read the values back from a known place here.
   *
   * The hazard is that if the gc runs between these two instructions
   * (i.e., if the pc is at the stp instruction), it will probably get
   * confused by whatever junk is now on top of the stack.  So, we
   * complete the store from imm0/imm1 and skip over the stp.
   *
   * We only look for the stp instruction, so it's possible that we
   * are not really in the middle of allocating a variable C frame: we
   * could be in unrelated code.  However, completing the stp here
   * should be harmless: the code was going to do it anyway.
   */
  if (instr == STP_IMM0_IMM1_SP) {
    LispObj *sp = (LispObj *)ptr_from_lispobj(xpSP(xp));
    sp[0] = xpGPR(xp, Rimm0);         /* the u64-vector header */
    sp[1] = xpGPR(xp, Rimm1);         /* saved previous SP (element 0) */
    xpPC(xp) = program_counter + 1;   /* resume just past the stp */
    return;
  }

  /* TODO(arm64): heap-allocation windows -- port classify_alloc_instruction /
     finish_allocating_{cons,uvector} / restart_allocation from
     arm-exceptions.c; this is what fills in *alloc_disp. */

  /*
   * Bulding a lisp frame
   *
   * We are (probably) building a 4-word lisp frame on the control stack.
   *
   *   stp  marker, vsp, [sp, #-32]!   // reserve space, write marker & vsp
   *   stp  fn, lr, [sp, #16]          // save fn, lr
   *
   * The hazard is that if the gc runs between these two instructions,
   * it will probably get confused because the fn and lr slots in the
   * lisp frame will be random junk instead of gc roots (well, a gc
   * root and a pc-locative).
   *
   * The easiest fix is to zero those slots, which is safe for the gc.
   *
   * We could check whether frame->marker == lisp_frame_marker before
   * zeroing the stack slots (to more narrowly match the "building a
   * lisp frame" case) but it seems harmless to zero unconditionally,
   * since the stack slots are about to get overwritten with real
   * values anyway.
   */
  if (instr == STP_FN_LR_SP) {
    lisp_frame *frame = (lisp_frame *)ptr_from_lispobj(xpSP(xp));
    frame->savefn = 0;
    frame->savelr = 0;
    return;
  }

  /* TODO(arm64): EGC write-barrier windows. */
}

void
update_bytes_allocated(TCR *tcr, void *cur_allocptr)
{
  char *last = tcr->last_allocptr;
  char *current = cur_allocptr;

  if (last && (cur_allocptr != (void *)VOID_ALLOCPTR)) {
    tcr->bytes_allocated += last - current;
  }
  tcr->last_allocptr = 0;
}

void
normalize_tcr(ExceptionInformation *xp, TCR *tcr, Boolean is_other_tcr)
{
  void *cur_allocptr = 0;
  LispObj freeptr = 0;

  if (xp) {
    if (is_other_tcr) {
      pc_luser_xp(xp, tcr, NULL);
      freeptr = xpGPR(xp, Rallocptr);
      if (fulltag_of(freeptr) == 0) {
          cur_allocptr = (void *)ptr_from_lispobj(freeptr);
      }
    }
    update_area_active(&tcr->cs_area, (char *)ptr_from_lispobj(xpSP(xp)));
    update_area_active(&tcr->vs_area, (char* )ptr_from_lispobj(xpGPR(xp,
                                                                     Rvsp)));
    update_area_active(&tcr->ts_area, (char *)ptr_from_lispobj(xpGPR(xp,
                                                                     Rtsp)));
  } else {
    /* In ff-call. */
    cur_allocptr = (void *)tcr->save_allocptr;
    update_area_active(&tcr->cs_area, (char *)tcr->last_lisp_frame);
    update_area_active(&tcr->vs_area, (char *)tcr->save_vsp);
    update_area_active(&tcr->ts_area, (char *)tcr->save_tsp);
  }

  /* retire current memory segment */
  tcr->save_allocptr = tcr->save_allocbase = (void *)VOID_ALLOCPTR;
  if (cur_allocptr) {
    update_bytes_allocated(tcr, cur_allocptr);
    if (freeptr) {
      xpGPR(xp, Rallocptr) = VOID_ALLOCPTR;
      xpGPR(xp, Rallocbase) = VOID_ALLOCPTR;
    }
  }                      
}

LispObj
code_vector_from_pc(LispObj pcloc)
{
  int32_t *pc = (int32_t *)pcloc;
  /*
   * There is a udf #0 (which encodes as 0) sentinel instruction at
   * the beginning of every code-vector.  Scan backwards until we find
   * it.
   */
  while(*pc != 0) {
    pc--;
  }
  /*
   * The header directly precedes the sentinel instruction.
   */
  char *code_vector = (char *)(pc - 1) + fulltag_misc;
  return ptr_to_lispobj(code_vector);
}

#ifdef DARWIN
void
fatal_mach_error(char *format, ...);

#define MACH_CHECK_ERROR(context, x) if (x != KERN_SUCCESS) {fatal_mach_error("Mach error while %s : %d", context, x);}

void
associate_tcr_with_exception_port(mach_port_t port, TCR *tcr)
{
    kern_return_t kret;
    
    kret = mach_port_set_context(mach_task_self(),
                                 port, (mach_vm_address_t)tcr);
    MACH_CHECK_ERROR("associating TCR with exception port", kret);
}

void
disassociate_tcr_from_exception_port(mach_port_t port)
{
  kern_return_t kret;

  kret = mach_port_set_context(mach_task_self(), port, 0);
  MACH_CHECK_ERROR("disassociating TCR with exception port", kret);
}

void
fatal_mach_error(char *format, ...)
{
  va_list args;
  char s[512];

  va_start(args, format);
  vsnprintf(s, sizeof(s),format, args);
  va_end(args);

  Fatal("Mach error", s);
}
#endif /* DARWIN */
