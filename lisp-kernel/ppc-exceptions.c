/*
 * Copyright 1994-2009 Clozure Associates
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

#include "lisp.h"
#include "lisp-exceptions.h"
#include "lisp_globals.h"
#include <ctype.h>
#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>
#include <stdio.h>
#ifdef LINUX
#include <strings.h>
#include <sys/mman.h>
#include <linux/prctl.h>
#endif

#ifdef DARWIN
#include <sys/mman.h>
#ifndef SA_NODEFER
#define SA_NODEFER 0
#endif
#include <sysexits.h>

/* a distinguished UUO at a distinguished address */
extern void pseudo_sigreturn(ExceptionInformation *);
#endif

#ifndef _FPU_RESERVED
#define _FPU_RESERVED 0xffffff00
#endif


#include "threads.h"

#define MSR_FE0_MASK (((unsigned)0x80000000)>>20)
#define MSR_FE1_MASK (((unsigned)0x80000000)>>23)
#define MSR_FE0_FE1_MASK (MSR_FE0_MASK|MSR_FE1_MASK)
extern void enable_fp_exceptions(void);
extern void disable_fp_exceptions(void);

#ifdef LINUX
/* Some relatively recent kernels support this interface.
   If this prctl isn't supported, assume that we're always
   running with excptions enabled and "precise". 
*/
#ifndef PR_SET_FPEXC
#define PR_SET_FPEXC 12
#endif
#ifndef PR_FP_EXC_DISABLED
#define PR_FP_EXC_DISABLED 0
#endif
#ifndef PR_FP_EXC_PRECISE
#define PR_FP_EXC_PRECISE 3
#endif

void
enable_fp_exceptions()
{
  prctl(PR_SET_FPEXC, PR_FP_EXC_PRECISE);
}

void
disable_fp_exceptions()
{
  prctl(PR_SET_FPEXC, PR_FP_EXC_DISABLED);
}

#endif

/*
  Handle exceptions.

*/

extern LispObj lisp_nil;

extern natural lisp_heap_gc_threshold;
extern Boolean grow_dynamic_area(natural);






int
page_size = 4096;

int
log2_page_size = 12;





/*
  If the PC is pointing to an allocation trap, the previous instruction
  must have decremented allocptr.  Return the non-zero amount by which
  allocptr was decremented.
*/
signed_natural
allocptr_displacement(ExceptionInformation *xp)
{
  pc program_counter = xpPC(xp);
  opcode instr = *program_counter, prev_instr = *(program_counter-1);

  if (instr == ALLOC_TRAP_INSTRUCTION) {
    if (match_instr(prev_instr, 
                    XO_MASK | RT_MASK | RB_MASK,
                    XO(major_opcode_X31,minor_opcode_SUBF, 0, 0) |
                    RT(allocptr) |
                    RB(allocptr))) {
      return ((signed_natural) xpGPR(xp, RA_field(prev_instr)));
    }
    if (match_instr(prev_instr,
                    OP_MASK | RT_MASK | RA_MASK,
                    OP(major_opcode_ADDI) | 
                    RT(allocptr) |
                    RA(allocptr))) {
      return (signed_natural) -((short) prev_instr);
    }
    Bug(xp, "Can't determine allocation displacement");
  }
  return 0;
}


/*
  A cons cell's been successfully allocated, but the allocptr's
  still tagged (as fulltag_cons, of course.)  Emulate any instructions
  that might follow the allocation (stores to the car or cdr, an
  assignment to the "result" gpr) that take place while the allocptr's
  tag is non-zero, advancing over each such instruction.  When we're
  done, the cons cell will be allocated and initialized, the result
  register will point to it, the allocptr will be untagged, and
  the PC will point past the instruction that clears the allocptr's
  tag.
*/
void
finish_allocating_cons(ExceptionInformation *xp)
{
  pc program_counter = xpPC(xp);
  opcode instr;
  LispObj cur_allocptr = xpGPR(xp, allocptr);
  cons *c = (cons *)ptr_from_lispobj(untag(cur_allocptr));
  int target_reg;

  while (1) {
    instr = *program_counter++;

    if (instr == UNTAG_ALLOCPTR_INSTRUCTION) {
      xpGPR(xp, allocptr) = untag(cur_allocptr);
      xpPC(xp) = program_counter;
      return;
    }
    
    switch (instr & STORE_CXR_ALLOCPTR_MASK) {
    case STORE_CAR_ALLOCPTR_INSTRUCTION:
      c->car = xpGPR(xp,RT_field(instr));
      break;
    case STORE_CDR_ALLOCPTR_INSTRUCTION:
      c->cdr = xpGPR(xp,RT_field(instr));
      break;
    default:
      /* Assume that this is an assignment: {rt/ra} <- allocptr.
         There are several equivalent instruction forms
         that might have that effect; just assign to target here.
      */
      if (major_opcode_p(instr,major_opcode_X31)) {
	target_reg = RA_field(instr);
      } else {
	target_reg = RT_field(instr);
      }
      xpGPR(xp,target_reg) = cur_allocptr;
      break;
    }
  }
}

/*
  We were interrupted in the process of allocating a uvector; we
  survived the allocation trap, and allocptr is tagged as fulltag_misc.
  Emulate any instructions which store a header into the uvector,
  assign the value of allocptr to some other register, and clear
  allocptr's tag.  Don't expect/allow any other instructions in
  this environment.
*/
void
finish_allocating_uvector(ExceptionInformation *xp)
{
  pc program_counter = xpPC(xp);
  opcode instr;
  LispObj cur_allocptr = xpGPR(xp, allocptr);
  int target_reg;

  while (1) {
    instr = *program_counter++;
    if (instr == UNTAG_ALLOCPTR_INSTRUCTION) {
      xpGPR(xp, allocptr) = untag(cur_allocptr);
      xpPC(xp) = program_counter;
      return;
    }
    if ((instr &  STORE_HEADER_ALLOCPTR_MASK) == 
        STORE_HEADER_ALLOCPTR_INSTRUCTION) {
      header_of(cur_allocptr) = xpGPR(xp, RT_field(instr));
    } else {
      /* assume that this is an assignment */

      if (major_opcode_p(instr,major_opcode_X31)) {
	target_reg = RA_field(instr);
      } else {
	target_reg = RT_field(instr);
      }
      xpGPR(xp,target_reg) = cur_allocptr;
    }
  }
}


Boolean
allocate_object(ExceptionInformation *xp,
                natural bytes_needed, 
                signed_natural disp_from_allocptr,
		TCR *tcr)
{
  area *a = active_dynamic_area;

  /* Maybe do an EGC */
  if (a->older && lisp_global(OLDEST_EPHEMERAL)) {
    if (((a->active)-(a->low)) >= a->threshold) {
      gc_from_xp(xp, 0L);
    }
  }

  /* Life is pretty simple if we can simply grab a segment
     without extending the heap.
  */
  if (new_heap_segment(xp, bytes_needed, false, tcr, NULL)) {
    xpGPR(xp, allocptr) += disp_from_allocptr;
#ifdef DEBUG
    fprintf(dbgout, "New heap segment for #x%x, no GC: #x%x/#x%x, vsp = #x%x\n",
            tcr,xpGPR(xp,allocbase),tcr->last_allocptr, xpGPR(xp,vsp));
#endif
    return true;
  }
  
  /* It doesn't make sense to try a full GC if the object
     we're trying to allocate is larger than everything
     allocated so far.
  */
  if ((lisp_global(HEAP_END)-lisp_global(HEAP_START)) > bytes_needed) {
    untenure_from_area(tenured_area); /* force a full GC */
    gc_from_xp(xp, 0L);
  }
  
  /* Try again, growing the heap if necessary */
  if (new_heap_segment(xp, bytes_needed, true, tcr, NULL)) {
    xpGPR(xp, allocptr) += disp_from_allocptr;
#ifdef DEBUG
    fprintf(dbgout, "New heap segment for #x%x after GC: #x%x/#x%x\n",
            tcr,xpGPR(xp,allocbase),tcr->last_allocptr);
#endif
    return true;
  }
  
  return false;
}

#ifndef XNOMEM
#define XNOMEM 10
#endif

void
update_bytes_allocated(TCR* tcr, void *cur_allocptr)
{
  BytePtr 
    last = (BytePtr) tcr->last_allocptr, 
    current = (BytePtr) cur_allocptr;
  if (last && (cur_allocptr != ((void *)VOID_ALLOCPTR))) {
    tcr->bytes_allocated += last-current;
  }
  tcr->last_allocptr = 0;
}

void
lisp_allocation_failure(ExceptionInformation *xp, TCR *tcr, natural bytes_needed)
{
  /* Couldn't allocate the object.  If it's smaller than some arbitrary
     size (say 128K bytes), signal a "chronically out-of-memory" condition;
     else signal a "allocation request failed" condition.
  */
  xpGPR(xp,allocptr) = xpGPR(xp,allocbase) = VOID_ALLOCPTR;
  handle_error(xp, bytes_needed < (128<<10) ? XNOMEM : error_alloc_failed, 0, 0,  xpPC(xp));
}

/*
  Allocate a large list, where "large" means "large enough to
  possibly trigger the EGC several times if this was done
  by individually allocating each CONS."  The number of 
  ocnses in question is in arg_z; on successful return,
  the list will be in arg_z 
*/

Boolean
allocate_list(ExceptionInformation *xp, TCR *tcr)
{
  natural 
    nconses = (unbox_fixnum(xpGPR(xp,arg_z))),
    bytes_needed = (nconses << dnode_shift);
  LispObj
    prev = lisp_nil,
    current,
    initial = xpGPR(xp,arg_y);

  if (nconses == 0) {
    /* Silly case */
    xpGPR(xp,arg_z) = lisp_nil;
    xpGPR(xp,allocptr) = lisp_nil;
    return true;
  }
  update_bytes_allocated(tcr, (void *)(void *) tcr->save_allocptr);
  if (allocate_object(xp,bytes_needed,(-bytes_needed)+fulltag_cons,tcr)) {
    for (current = xpGPR(xp,allocptr);
         nconses;
         prev = current, current+= dnode_size, nconses--) {
      deref(current,0) = prev;
      deref(current,1) = initial;
    }
    xpGPR(xp,arg_z) = prev;
    xpGPR(xp,arg_y) = xpGPR(xp,allocptr);
    xpGPR(xp,allocptr)-=fulltag_cons;
  } else {
    lisp_allocation_failure(xp,tcr,bytes_needed);
  }
  return true;
}

OSStatus
handle_alloc_trap(ExceptionInformation *xp, TCR *tcr)
{
  pc program_counter;
  natural cur_allocptr, bytes_needed = 0;
  opcode prev_instr;
  signed_natural disp = 0;
  unsigned allocptr_tag;

  cur_allocptr = xpGPR(xp,allocptr);
  program_counter = xpPC(xp);
  prev_instr = *(program_counter-1);
  allocptr_tag = fulltag_of(cur_allocptr);

  switch (allocptr_tag) {
  case fulltag_cons:
    bytes_needed = sizeof(cons);
    disp = -sizeof(cons) + fulltag_cons;
    break;

  case fulltag_even_fixnum:
  case fulltag_odd_fixnum:
    break;

  case fulltag_misc:
    if (match_instr(prev_instr, 
                    XO_MASK | RT_MASK | RB_MASK,
                    XO(major_opcode_X31,minor_opcode_SUBF, 0, 0) |
                    RT(allocptr) |
                    RB(allocptr))) {
      disp = -((signed_natural) xpGPR(xp, RA_field(prev_instr)));
    } else if (match_instr(prev_instr,
                           OP_MASK | RT_MASK | RA_MASK,
                           OP(major_opcode_ADDI) | 
                           RT(allocptr) |
                           RA(allocptr))) {
      disp = (signed_natural) ((short) prev_instr);
    }
    if (disp) {
      bytes_needed = (-disp) + fulltag_misc;
      break;
    }
    /* else fall thru */
  default:
    return -1;
  }

  if (bytes_needed) {
    update_bytes_allocated(tcr,((BytePtr)(cur_allocptr-disp)));
    if (allocate_object(xp, bytes_needed, disp, tcr)) {
#if 0
      fprintf(dbgout, "alloc_trap in 0x%lx, new allocptr = 0x%lx\n",
              tcr, xpGPR(xp, allocptr));
#endif
      adjust_exception_pc(xp,4);
      return 0;
    }
    lisp_allocation_failure(xp,tcr,bytes_needed);
    return -1;
  }
  return -1;
}

natural gc_deferred = 0, full_gc_deferred = 0;

signed_natural
flash_freeze(TCR *tcr, signed_natural param)
{
  return 0;
}

OSStatus
handle_gc_trap(ExceptionInformation *xp, TCR *tcr)
{
  LispObj 
    selector = xpGPR(xp,imm0), 
    arg = xpGPR(xp,imm1);
  area *a = active_dynamic_area;
  Boolean egc_was_enabled = (a->older != NULL);
  natural gc_previously_deferred = gc_deferred;


  switch (selector) {
  case GC_TRAP_FUNCTION_EGC_CONTROL:
    egc_control(arg != 0, a->active);
    xpGPR(xp,arg_z) = lisp_nil + (egc_was_enabled ? t_offset : 0);
    break;

  case GC_TRAP_FUNCTION_CONFIGURE_EGC:
    a->threshold = unbox_fixnum(xpGPR(xp, arg_x));
    g1_area->threshold = unbox_fixnum(xpGPR(xp, arg_y));
    g2_area->threshold = unbox_fixnum(xpGPR(xp, arg_z));
    xpGPR(xp,arg_z) = lisp_nil+t_offset;
    break;

  case GC_TRAP_FUNCTION_SET_LISP_HEAP_THRESHOLD:
    if (((signed_natural) arg) > 0) {
      lisp_heap_gc_threshold = 
        align_to_power_of_2((arg-1) +
                            (heap_segment_size - 1),
                            log2_heap_segment_size);
    }
    /* fall through */
  case GC_TRAP_FUNCTION_GET_LISP_HEAP_THRESHOLD:
    xpGPR(xp, imm0) = lisp_heap_gc_threshold;
    break;

  case GC_TRAP_FUNCTION_USE_LISP_HEAP_THRESHOLD:
    /*  Try to put the current threshold in effect.  This may
        need to disable/reenable the EGC. */
    untenure_from_area(tenured_area);
    resize_dynamic_heap(a->active,lisp_heap_gc_threshold);
    if (egc_was_enabled) {
      if ((a->high - a->active) >= a->threshold) {
        tenure_to_area(tenured_area);
      }
    }
    xpGPR(xp, imm0) = lisp_heap_gc_threshold;
    break;

  case GC_TRAP_FUNCTION_ENSURE_STATIC_CONSES:
    ensure_static_conses(xp,tcr,32768);
    break;

  case GC_TRAP_FUNCTION_FLASH_FREEZE:
    untenure_from_area(tenured_area);
    gc_like_from_xp(xp,flash_freeze,0);
    a->active = (BytePtr) align_to_power_of_2(a->active, log2_page_size);
    tenured_area->static_dnodes = area_dnode(a->active, a->low);
    if (egc_was_enabled) {
      tenure_to_area(tenured_area);
    }
    xpGPR(xp, imm0) = tenured_area->static_dnodes << dnode_shift;
    break;

  default:
    update_bytes_allocated(tcr, (void *) ptr_from_lispobj(xpGPR(xp, allocptr)));

    if (selector == GC_TRAP_FUNCTION_IMMEDIATE_GC) {
      if (!full_gc_deferred) {
        gc_from_xp(xp, 0L);
        break;
      }
      /* Tried to do a full GC when gc was disabled.  That failed,
         so try full GC now */
      selector = GC_TRAP_FUNCTION_GC;
    }
    
    if (egc_was_enabled) {
      egc_control(false, (BytePtr) a->active);
    }
    gc_from_xp(xp, 0L);
    if (gc_deferred > gc_previously_deferred) {
      full_gc_deferred = 1;
    } else {
      full_gc_deferred = 0;
    }
    if (selector > GC_TRAP_FUNCTION_GC) {
      if (selector & GC_TRAP_FUNCTION_IMPURIFY) {
        impurify_from_xp(xp, 0L);
        /*        nrs_GC_EVENT_STATUS_BITS.vcell |= gc_integrity_check_bit; */
        lisp_global(OLDSPACE_DNODE_COUNT) = 0;
        gc_from_xp(xp, 0L);
      }
      if (selector & GC_TRAP_FUNCTION_PURIFY) {
        purify_from_xp(xp, 0L);
        lisp_global(OLDSPACE_DNODE_COUNT) = 0;
        gc_from_xp(xp, 0L);
      }
      if (selector & GC_TRAP_FUNCTION_SAVE_APPLICATION) {
        OSErr err;
        extern OSErr save_application(unsigned, Boolean);
        TCR *tcr = TCR_FROM_TSD(xpGPR(xp, rcontext));
        area *vsarea = tcr->vs_area;
	
        nrs_TOPLFUNC.vcell = *((LispObj *)(vsarea->high)-1);
        err = save_application(arg, egc_was_enabled);
        if (err == noErr) {
          _exit(0);
        }
        fatal_oserr(": save_application", err);
      }
      switch (selector) {


      case GC_TRAP_FUNCTION_FREEZE:
        a->active = (BytePtr) align_to_power_of_2(a->active, log2_page_size);
        tenured_area->static_dnodes = area_dnode(a->active, a->low);
        xpGPR(xp, imm0) = tenured_area->static_dnodes << dnode_shift;
        break;
      default:
        break;
      }
    }
    
    if (egc_was_enabled) {
      egc_control(true, NULL);
    }
    break;
    
  }

  adjust_exception_pc(xp,4);
  return 0;
}



void
signal_stack_soft_overflow(ExceptionInformation *xp, unsigned reg)
{
  /* The cstack just overflowed.  Force the current thread's
     control stack to do so until all stacks are well under their overflow
     limits. 
  */

#if 0
  lisp_global(CS_OVERFLOW_LIMIT) = CS_OVERFLOW_FORCE_LIMIT; /* force unsigned traps to fail */
#endif
  handle_error(xp, error_stack_overflow, reg, 0,  xpPC(xp));
}

/*
  Lower (move toward 0) the "end" of the soft protected area associated
  with a by a page, if we can.
*/

void
adjust_soft_protection_limit(area *a)
{
  char *proposed_new_soft_limit = a->softlimit - 4096;
  protected_area_ptr p = a->softprot;
  
  if (proposed_new_soft_limit >= (p->start+16384)) {
    p->end = proposed_new_soft_limit;
    p->protsize = p->end-p->start;
    a->softlimit = proposed_new_soft_limit;
  }
  protect_area(p);
}

void
restore_soft_stack_limit(unsigned stkreg)
{
  area *a;
  TCR *tcr = get_tcr(true);

  switch (stkreg) {
  case sp:
    a = tcr->cs_area;
    if ((a->softlimit - 4096) > (a->hardlimit + 16384)) {
      a->softlimit -= 4096;
    }
    tcr->cs_limit = (LispObj)ptr_to_lispobj(a->softlimit);
    break;
  case vsp:
    a = tcr->vs_area;
    adjust_soft_protection_limit(a);
    break;
  case tsp:
    a = tcr->ts_area;
    adjust_soft_protection_limit(a);
  }
}

/* Maybe this'll work someday.  We may have to do something to
   make the thread look like it's not handling an exception */
void
reset_lisp_process(ExceptionInformation *xp)
{
  TCR *tcr = TCR_FROM_TSD(xpGPR(xp,rcontext));
  catch_frame *last_catch = (catch_frame *) ptr_from_lispobj(untag(tcr->catch_top));

  tcr->save_allocptr = (void *) ptr_from_lispobj(xpGPR(xp, allocptr));
  tcr->save_allocbase = (void *) ptr_from_lispobj(xpGPR(xp, allocbase));

  tcr->save_vsp = (LispObj *) ptr_from_lispobj(((lisp_frame *)ptr_from_lispobj(last_catch->csp))->savevsp);
  tcr->save_tsp = (LispObj *) ptr_from_lispobj((LispObj) ptr_to_lispobj(last_catch)) - (2*node_size); /* account for TSP header */

  start_lisp(tcr, 1);
}


void
platform_new_heap_segment(ExceptionInformation *xp, TCR *tcr, BytePtr low, BytePtr high)
{
  tcr->last_allocptr = (void *)high;
  xpGPR(xp,allocptr) = (LispObj) high;
  xpGPR(xp,allocbase) = (LispObj) low;
}

 
void
update_area_active (area **aptr, BytePtr value)
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

LispObj *
tcr_frame_ptr(TCR *tcr)
{
  ExceptionInformation *xp;
  LispObj *bp = NULL;

  if (tcr->pending_exception_context)
    xp = tcr->pending_exception_context;
  else {
    xp = tcr->suspend_context;
  }
  if (xp) {
    bp = (LispObj *) xpGPR(xp, sp);
  }
  return bp;
}

void
normalize_tcr(ExceptionInformation *xp, TCR *tcr, Boolean is_other_tcr)
{
  void *cur_allocptr = NULL;
  LispObj freeptr = 0;

  if (xp) {
    if (is_other_tcr) {
      pc_luser_xp(xp, tcr, NULL);
      freeptr = xpGPR(xp, allocptr);
      if (fulltag_of(freeptr) == 0){
	cur_allocptr = (void *) ptr_from_lispobj(freeptr);
      }
    }
    update_area_active((area **)&tcr->cs_area, (BytePtr) ptr_from_lispobj(xpGPR(xp, sp)));
    update_area_active((area **)&tcr->vs_area, (BytePtr) ptr_from_lispobj(xpGPR(xp, vsp)));
    update_area_active((area **)&tcr->ts_area, (BytePtr) ptr_from_lispobj(xpGPR(xp, tsp)));
#ifdef DEBUG
    fprintf(dbgout, "TCR 0x%x in lisp code, vsp = 0x%lx, tsp = 0x%lx\n",
            tcr, xpGPR(xp, vsp), xpGPR(xp, tsp));
    fprintf(dbgout, "TCR 0x%x, allocbase/allocptr were 0x%x/0x%x at #x%x\n",
            tcr,
            xpGPR(xp, allocbase),
            xpGPR(xp, allocptr),
            xpPC(xp));
    fprintf(dbgout, "TCR 0x%x, exception context = 0x%x\n",
            tcr,
            tcr->pending_exception_context);
#endif
  } else {
    /* In ff-call.  No need to update cs_area */
    cur_allocptr = (void *) (tcr->save_allocptr);
#ifdef DEBUG
    fprintf(dbgout, "TCR 0x%x in foreign code, vsp = 0x%lx, tsp = 0x%lx\n",
            tcr, tcr->save_vsp, tcr->save_tsp);
    fprintf(dbgout, "TCR 0x%x, save_allocbase/save_allocptr were 0x%x/0x%x at #x%x\n",
            tcr,
            tcr->save_allocbase,
            tcr->save_allocptr,
            xpPC(xp));

#endif
    update_area_active((area **)&tcr->vs_area, (BytePtr) tcr->save_vsp);
    update_area_active((area **)&tcr->ts_area, (BytePtr) tcr->save_tsp);
  }


  tcr->save_allocptr = tcr->save_allocbase = (void *)VOID_ALLOCPTR;
  if (cur_allocptr) {
    update_bytes_allocated(tcr, cur_allocptr);
    if (freeptr) {
      xpGPR(xp, allocptr) = VOID_ALLOCPTR;
      xpGPR(xp, allocbase) = VOID_ALLOCPTR;
    }
  }
}

TCR *gc_tcr = NULL;

/* Suspend and "normalize" other tcrs, then call a gc-like function
   in that context.  Resume the other tcrs, then return what the
   function returned */

signed_natural
gc_like_from_xp(ExceptionInformation *xp, 
                signed_natural(*fun)(TCR *, signed_natural), 
                signed_natural param)
{
  TCR *tcr = TCR_FROM_TSD(xpGPR(xp, rcontext)), *other_tcr;
  int result;
  signed_natural inhibit;

  suspend_other_threads(true);
  inhibit = (signed_natural)(lisp_global(GC_INHIBIT_COUNT));
  if (inhibit != 0) {
    if (inhibit > 0) {
      lisp_global(GC_INHIBIT_COUNT) = (LispObj)(-inhibit);
    }
    resume_other_threads(true);
    gc_deferred++;
    return 0;
  }
  gc_deferred = 0;

  gc_tcr = tcr;

  xpGPR(xp, allocptr) = VOID_ALLOCPTR;
  xpGPR(xp, allocbase) = VOID_ALLOCPTR;

  normalize_tcr(xp, tcr, false);


  for (other_tcr = tcr->next; other_tcr != tcr; other_tcr = other_tcr->next) {
    if (other_tcr->pending_exception_context) {
      other_tcr->gc_context = other_tcr->pending_exception_context;
    } else if (other_tcr->valence == TCR_STATE_LISP) {
      other_tcr->gc_context = other_tcr->suspend_context;
    } else {
      /* no pending exception, didn't suspend in lisp state:
	 must have executed a synchronous ff-call. 
      */
      other_tcr->gc_context = NULL;
    }
    normalize_tcr(other_tcr->gc_context, other_tcr, true);
  }
    


  result = fun(tcr, param);

  other_tcr = tcr;
  do {
    other_tcr->gc_context = NULL;
    other_tcr = other_tcr->next;
  } while (other_tcr != tcr);

  gc_tcr = NULL;

  resume_other_threads(true);

  return result;

}



/* Returns #bytes freed by invoking GC */

signed_natural
gc_from_tcr(TCR *tcr, signed_natural param)
{
  area *a;
  BytePtr oldfree, newfree;
  BytePtr oldend, newend;

#ifdef DEBUG
  fprintf(dbgout, "Start GC  in 0x%lx\n", tcr);
#endif
  a = active_dynamic_area;
  oldend = a->high;
  oldfree = a->active;
  gc(tcr, param);
  newfree = a->active;
  newend = a->high;
#if 0
  fprintf(dbgout, "End GC  in 0x%lx\n", tcr);
#endif
  return ((oldfree-newfree)+(newend-oldend));
}

signed_natural
gc_from_xp(ExceptionInformation *xp, signed_natural param)
{
  signed_natural status = gc_like_from_xp(xp, gc_from_tcr, param);

  freeGCptrs();
  return status;
}

signed_natural
purify_from_xp(ExceptionInformation *xp, signed_natural param)
{
  return gc_like_from_xp(xp, purify, param);
}

signed_natural
impurify_from_xp(ExceptionInformation *xp, signed_natural param)
{
  return gc_like_from_xp(xp, impurify, param);
}






protection_handler
 * protection_handlers[] = {
   do_spurious_wp_fault,
   do_soft_stack_overflow,
   do_soft_stack_overflow,
   do_soft_stack_overflow,
   do_hard_stack_overflow,    
   do_hard_stack_overflow,
   do_hard_stack_overflow
   };


Boolean
is_write_fault(ExceptionInformation *xp, siginfo_t *info)
{
  /* use the siginfo if it's available.  Some versions of Linux
     don't propagate the DSISR and TRAP fields correctly from
     64- to 32-bit handlers.
  */
  if (info) {
    /* 
       To confuse matters still further, the value of SEGV_ACCERR
       varies quite a bit among LinuxPPC variants (the value defined
       in the header files varies, and the value actually set by
       the kernel also varies.  So far, we're only looking at the
       siginfo under Linux and Linux always seems to generate
       SIGSEGV, so check for SIGSEGV and check the low 16 bits
       of the si_code.
    */
    return ((info->si_signo == SIGSEGV) &&
	    ((info->si_code & 0xff) == (SEGV_ACCERR & 0xff)));
  }
  return(((xpDSISR(xp) & (1 << 25)) != 0) &&
	 (xpTRAP(xp) == 
#ifdef LINUX
0x0300
#endif
#ifdef DARWIN
0x0300/0x100
#endif
)
	 );
#if 0 
  /* Maybe worth keeping around; not sure if it's an exhaustive
     list of PPC instructions that could cause a WP fault */
  /* Some OSes lose track of the DSISR and DSR SPRs, or don't provide
     valid values of those SPRs in the context they provide to
     exception handlers.  Look at the opcode of the offending
     instruction & recognize 32-bit store operations */
  opcode instr = *(xpPC(xp));

  if (xp->regs->trap != 0x300) {
    return 0;
  }
  switch (instr >> 26) {
  case 47:			/* STMW */
  case 36:			/* STW */
  case 37:			/* STWU */
    return 1;
  case 31:
    switch ((instr >> 1) & 1023) {
    case 151:			/* STWX */
    case 183:			/* STWUX */
      return 1;
    default:
      return 0;
    }
  default:
    return 0;
  }
#endif
}

OSStatus
handle_protection_violation(ExceptionInformation *xp, siginfo_t *info, TCR *tcr, int old_valence)
{
  BytePtr addr;
  protected_area_ptr area;
  protection_handler *handler;
  extern Boolean touch_page(void *);
  extern void touch_page_end(void);

  if (info) {
    addr = (BytePtr)(info->si_addr);
  } else {
    addr = (BytePtr) ((natural) (xpDAR(xp)));
  }

  if (addr && (addr == tcr->safe_ref_address)) {
    adjust_exception_pc(xp,4);

    xpGPR(xp,imm0) = 0;
    return 0;
  }

  if (xpPC(xp) == (pc)touch_page) {
    xpGPR(xp,imm0) = 0;
    xpPC(xp) = (pc)touch_page_end;
    return 0;
  }


  if (is_write_fault(xp,info)) {
    area = find_protected_area(addr);
    if (area != NULL) {
      handler = protection_handlers[area->why];
      return handler(xp, area, addr);
    } else {
      if ((addr >= readonly_area->low) &&
	  (addr < readonly_area->active)) {
        UnProtectMemory((LogicalAddress)(truncate_to_power_of_2(addr,log2_page_size)),
                        page_size);
	return 0;
      }
    }
  }
  if (old_valence == TCR_STATE_LISP) {
    callback_for_trap(nrs_CMAIN.vcell, xp, (pc)xpPC(xp), SIGBUS, (natural)addr, is_write_fault(xp,info));
  }
  return -1;
}





OSStatus
do_hard_stack_overflow(ExceptionInformation *xp, protected_area_ptr area, BytePtr addr)
{
#ifdef SUPPORT_PRAGMA_UNUSED
#pragma unused(area,addr)
#endif
  reset_lisp_process(xp);
  return -1;
}

extern area*
allocate_vstack(natural useable);       /* This is in "pmcl-kernel.c" */

extern area*
allocate_tstack(natural useable);       /* This is in "pmcl-kernel.c" */

#ifdef EXTEND_VSTACK
Boolean
catch_frame_p(lisp_frame *spPtr)
{
  catch_frame* catch = (catch_frame *) untag(lisp_global(CATCH_TOP));

  for (; catch; catch = (catch_frame *) untag(catch->link)) {
    if (spPtr == ((lisp_frame *) catch->csp)) {
      return true;
    }
  }
  return false;
}
#endif

Boolean
unwind_protect_cleanup_frame_p(lisp_frame *spPtr)
{
  if ((spPtr->savevsp == (LispObj)NULL) ||  /* The frame to where the unwind-protect will return */
      (((spPtr->backlink)->savevsp) == (LispObj)NULL)) {  /* The frame that returns to the kernel  from the cleanup form */
    return true;
  } else {
    return false;
  }
}

Boolean
lexpr_entry_frame_p(lisp_frame *spPtr)
{
  LispObj savelr = spPtr->savelr;
  LispObj lexpr_return = (LispObj) lisp_global(LEXPR_RETURN);
  LispObj lexpr_return1v = (LispObj) lisp_global(LEXPR_RETURN1V);
  LispObj ret1valn = (LispObj) lisp_global(RET1VALN);

  return
    (savelr == lexpr_return1v) ||
    (savelr == lexpr_return) ||
    ((savelr == ret1valn) &&
     (((spPtr->backlink)->savelr) == lexpr_return));
}

Boolean
lisp_frame_p(lisp_frame *spPtr)
{
  LispObj savefn;
  /* We can't just look at the size of the stack frame under the EABI
     calling sequence, but that's the first thing to check. */
  if (((lisp_frame *) spPtr->backlink) != (spPtr+1)) {
    return false;
  }
  savefn = spPtr->savefn;
  return (savefn == 0) || (fulltag_of(savefn) == fulltag_misc);
  
}


int ffcall_overflow_count = 0;

/* Find a frame that is neither a catch frame nor one of the
   lexpr_entry frames We don't check for non-lisp frames here because
   we'll always stop before we get there due to a dummy lisp frame
   pushed by .SPcallback that masks out the foreign frames.  The one
   exception is that there is a non-lisp frame without a valid VSP
   while in the process of ppc-ff-call. We recognize that because its
   savelr is NIL.  If the saved VSP itself is 0 or the savevsp in the
   next frame is 0, then we're executing an unwind-protect cleanup
   form, and the top stack frame belongs to its (no longer extant)
   catch frame.  */

#ifdef EXTEND_VSTACK
lisp_frame *
find_non_catch_frame_from_xp (ExceptionInformation *xp)
{
  lisp_frame *spPtr = (lisp_frame *) xpGPR(xp, sp);
  if ((((natural) spPtr) + sizeof(lisp_frame)) != ((natural) (spPtr->backlink))) {
    ffcall_overflow_count++;          /* This is mostly so I can breakpoint here */
  }
  for (; !lisp_frame_p(spPtr)  || /* In the process of ppc-ff-call */
         unwind_protect_cleanup_frame_p(spPtr) ||
         catch_frame_p(spPtr) ||
         lexpr_entry_frame_p(spPtr) ; ) {
     spPtr = spPtr->backlink;
     };
  return spPtr;
}
#endif

#ifdef EXTEND_VSTACK
Boolean
db_link_chain_in_area_p (area *a)
{
  LispObj *db = (LispObj *) lisp_global(DB_LINK),
          *high = (LispObj *) a->high,
          *low = (LispObj *) a->low;
  for (; db; db = (LispObj *) *db) {
    if ((db >= low) && (db < high)) return true;
  };
  return false;
}
#endif




/* Note: CURRENT_VS (CURRENT_TS) is always either the area containing
  the current value of VSP (TSP) or an older area.  */

OSStatus
do_vsp_overflow (ExceptionInformation *xp, BytePtr addr)
{
  TCR* tcr = TCR_FROM_TSD(xpGPR(xp, rcontext));
  area *a = tcr->vs_area;
  protected_area_ptr vsp_soft = a->softprot;
  unprotect_area(vsp_soft);
  signal_stack_soft_overflow(xp,vsp);
  return 0;
}


OSStatus
do_tsp_overflow (ExceptionInformation *xp, BytePtr addr)
{
  TCR* tcr = TCR_FROM_TSD(xpGPR(xp, rcontext));
  area *a = tcr->ts_area;
  protected_area_ptr tsp_soft = a->softprot;
  unprotect_area(tsp_soft);
  signal_stack_soft_overflow(xp,tsp);
  return 0;
}

OSStatus
do_soft_stack_overflow(ExceptionInformation *xp, protected_area_ptr prot_area, BytePtr addr)
{
  /* Trying to write into a guard page on the vstack or tstack.
     Allocate a new stack segment, emulate stwu and stwux for the TSP, and
     signal an error_stack_overflow condition.
      */
  lisp_protection_kind which = prot_area->why;
  Boolean on_TSP = (which == kTSPsoftguard);

  if (on_TSP) {
    return do_tsp_overflow(xp, addr);
   } else {
    return do_vsp_overflow(xp, addr);
   }
}

OSStatus
do_spurious_wp_fault(ExceptionInformation *xp, protected_area_ptr area, BytePtr addr)
{
#ifdef SUPPORT_PRAGMA_UNUSED
#pragma unused(xp,area,addr)
#endif
  return -1;
}


/*
  We have a couple of choices here.  We can simply unprotect the page
  and let the store happen on return, or we can try to emulate writes
  that we know will involve an intergenerational reference.  Both are
  correct as far as EGC constraints go, but the latter approach is
  probably more efficient.  (This only matters in the case where the
  GC runs after this exception handler returns but before the write
  actually happens.  If we didn't emulate node stores here, the EGC
  would scan the newly-writen page, find nothing interesting, and
  run to completion.  This thread will try the write again afer it
  resumes, the page'll be re-protected, and we'll have taken this
  fault twice.  The whole scenario shouldn't happen very often, but
  (having already taken a fault and committed to an mprotect syscall)
  we might as well emulate stores involving intergenerational references,
  since they're pretty easy to identify.

  Note that cases involving two or more threads writing to the same
  page (before either of them can run this handler) is benign: one
  invocation of the handler will just unprotect an unprotected page in
  that case.

  If there are GCs (or any other suspensions of the thread between
  the time that the write fault was detected and the time that the
  exception lock is obtained) none of this stuff happens.
*/

/*
  Return true (and emulate the instruction) iff:
  a) the fault was caused by an "stw rs,d(ra)" or "stwx rs,ra.rb"
     instruction.
  b) RS is a node register (>= fn)
  c) RS is tagged as a cons or vector
  d) RS is in some ephemeral generation.
  This is slightly conservative, since RS may be no younger than the
  EA being written to.
*/
Boolean
is_ephemeral_node_store(ExceptionInformation *xp, BytePtr ea)
{
  if (((ptr_to_lispobj(ea)) & 3) == 0) {
    opcode instr = *xpPC(xp);
    
    if (X_opcode_p(instr,major_opcode_X31,minor_opcode_STWX) ||
        major_opcode_p(instr, major_opcode_STW)) {
      LispObj 
        rs = RS_field(instr), 
        rsval = xpGPR(xp,rs),
        tag = fulltag_of(rsval);
      
      if (rs >= fn) {
        if ((tag == fulltag_misc) || (tag == fulltag_cons)) {
          if (((BytePtr)ptr_from_lispobj(rsval) > tenured_area->high) &&
              ((BytePtr)ptr_from_lispobj(rsval) < active_dynamic_area->high)) {
            *(LispObj *)ea = rsval;
            return true;
          }
        }
      }
    }
  }
  return false;
}

      





OSStatus
handle_sigfpe(ExceptionInformation *xp, TCR *tcr)
{
  (void) zero_fpscr(tcr);
  enable_fp_exceptions();


  tcr->lisp_fpscr.words.l =  xpFPSCR(xp) & ~_FPU_RESERVED;

  /* 'handle_fpux_binop' scans back from the specified PC until it finds an FPU
     operation; there's an FPU operation right at the PC, so tell it to start
     looking one word beyond */
  return handle_fpux_binop(xp, (pc)((natural)(xpPC(xp))+4));
}

    
int
altivec_present = 1;


/* This only tries to implement the "optional" fsqrt and fsqrts
   instructions, which were generally implemented on IBM hardware
   but generally not available on Motorola/Freescale systems.
*/		  
OSStatus
handle_unimplemented_instruction(ExceptionInformation *xp,
                                 opcode instruction,
                                 TCR *tcr)
{
  (void) zero_fpscr(tcr);
  enable_fp_exceptions();
  /* the rc bit (bit 0 in the instruction) is supposed to cause
     some FPSCR bits to be copied to CR1.  Clozure CL doesn't generate
     fsqrt. or fsqrts.
  */
  if (((major_opcode_p(instruction,major_opcode_FPU_DOUBLE)) || 
       (major_opcode_p(instruction,major_opcode_FPU_SINGLE))) &&
      ((instruction & ((1 << 6) -2)) == (22<<1))) {
    double b, d, sqrt(double);

    b = xpFPR(xp,RB_field(instruction));
    d = sqrt(b);
    xpFPSCR(xp) = ((xpFPSCR(xp) & ~_FPU_RESERVED) |
                   (get_fpscr() & _FPU_RESERVED));
    xpFPR(xp,RT_field(instruction)) = d;
    adjust_exception_pc(xp,4);
    return 0;
  }

  return -1;
}

OSStatus
PMCL_exception_handler(int xnum, 
                       ExceptionInformation *xp, 
                       TCR *tcr, 
                       siginfo_t *info,
                       int old_valence)
{
  OSStatus status = -1;
  pc program_counter;
  opcode instruction = 0;


  program_counter = xpPC(xp);
  
  if ((xnum == SIGILL) | (xnum == SIGTRAP)) {
    instruction = *program_counter;
  }

  if (instruction == ALLOC_TRAP_INSTRUCTION) {
    status = handle_alloc_trap(xp, tcr);
  } else if ((xnum == SIGSEGV) ||
	     (xnum == SIGBUS)) {
    status = handle_protection_violation(xp, info, tcr, old_valence);
  } else if (xnum == SIGFPE) {
    status = handle_sigfpe(xp, tcr);
  } else if ((xnum == SIGILL) || (xnum == SIGTRAP)) {
    if (instruction == GC_TRAP_INSTRUCTION) {
      status = handle_gc_trap(xp, tcr);
    } else if (IS_UUO(instruction)) {
      status = handle_uuo(xp, instruction, program_counter);
    } else if (is_conditional_trap(instruction)) {
      status = handle_trap(xp, instruction, program_counter, info);
    } else {
      status = handle_unimplemented_instruction(xp,instruction,tcr);
    }
  } else if (xnum == SIGNAL_FOR_PROCESS_INTERRUPT) {
    tcr->interrupt_pending = 0;
    callback_for_trap(nrs_CMAIN.vcell, xp, 0, TRI_instruction(TO_GT,nargs,0),0, 0);
    status = 0;
  }

  return status;
}

void
adjust_exception_pc(ExceptionInformation *xp, int delta)
{
  xpPC(xp) += (delta >> 2);
}


/* 
  This wants to scan backwards until "where" points to an instruction
   whose major opcode is either 63 (double-float) or 59 (single-float)
*/

OSStatus
handle_fpux_binop(ExceptionInformation *xp, pc where)
{
  OSStatus err;
  opcode *there = (opcode *) where, instr, errnum = 0;
  int i = TRAP_LOOKUP_TRIES, delta = 0;
  
  while (i--) {
    instr = *--there;
    delta -= 4;
    if (codevec_hdr_p(instr)) {
      return -1;
    }
    if (major_opcode_p(instr, major_opcode_FPU_DOUBLE)) {
      errnum = error_FPU_exception_double;
      break;
    }

    if (major_opcode_p(instr, major_opcode_FPU_SINGLE)) {
      errnum = error_FPU_exception_short;
      break;
    }
  }
  
  err = handle_error(xp, errnum, rcontext, 0,  there);
  /* Yeah, we said "non-continuable".  In case we ever change that ... */
  
  adjust_exception_pc(xp, delta);
  xpFPSCR(xp)  &=  0x03fff;
  
  return err;

}

OSStatus
handle_uuo(ExceptionInformation *xp, opcode the_uuo, pc where) 
{
#ifdef SUPPORT_PRAGMA_UNUSED
#pragma unused(where)
#endif
  unsigned 
    minor = UUO_MINOR(the_uuo),
    rb = 0x1f & (the_uuo >> 11),
    errnum = 0x3ff & (the_uuo >> 16);

  OSStatus status = -1;

  int bump = 4;

  switch (minor) {

  case UUO_ZERO_FPSCR:
    status = 0;
    xpFPSCR(xp) = 0;
    break;


  case UUO_INTERR:
    {
      TCR * target = (TCR *)xpGPR(xp,arg_z);
      status = 0;
      switch (errnum) {
      case error_propagate_suspend:
	break;
      case error_interrupt:
	xpGPR(xp,imm0) = (LispObj) raise_thread_interrupt(target);
	break;
      case error_suspend:
	xpGPR(xp,imm0) = (LispObj) lisp_suspend_tcr(target);
	break;
      case error_suspend_all:
	lisp_suspend_other_threads();
	break;
      case error_resume:
	xpGPR(xp,imm0) = (LispObj) lisp_resume_tcr(target);
	break;
      case error_resume_all:
	lisp_resume_other_threads();
	break;
      case error_kill:
	xpGPR(xp,imm0) = (LispObj)kill_tcr(target);
	break;
      case error_allocate_list:
        allocate_list(xp,get_tcr(true));
        break;
      default:
	status = handle_error(xp, errnum, rb, 0,  where);
	break;
      }
    }
    break;

  case UUO_INTCERR:
    status = handle_error(xp, errnum, rb, 1,  where);
    if (errnum == error_udf_call) {
      /* If lisp's returned from a continuable undefined-function call,
	 it's put a code vector in the xp's PC.  Don't advance the
	 PC ... */
      bump = 0;
    }
    break;

  case UUO_FPUX_BINOP:
    status = handle_fpux_binop(xp, where);
    bump = 0;
    break;

  default:
    status = -1;
    bump = 0;
  }
  
  if ((!status) && bump) {
    adjust_exception_pc(xp, bump);
  }
  return status;
}

natural
register_codevector_contains_pc (natural lisp_function, pc where)
{
  natural code_vector, size;

  if ((fulltag_of(lisp_function) == fulltag_misc) &&
      (header_subtag(header_of(lisp_function)) == subtag_function)) {
    code_vector = deref(lisp_function, 1);
    size = header_element_count(header_of(code_vector)) << 2;
    if ((untag(code_vector) < (natural)where) && 
	((natural)where < (code_vector + size)))
      return(code_vector);
  }

  return(0);
}

/* Callback to lisp to handle a trap. Need to translate the
   PC (where) into one of two forms of pairs:

   1. If PC is in fn or nfn's code vector, use the register number
      of fn or nfn and the index into that function's code vector.
   2. Otherwise use 0 and the pc itself
*/
void
callback_for_trap (LispObj callback_macptr, ExceptionInformation *xp, pc where,
                   natural arg1, natural arg2, natural arg3)
{
  natural code_vector = register_codevector_contains_pc(xpGPR(xp, fn), where);
  unsigned register_number = fn;
  natural index = (natural)where;

  if (code_vector == 0) {
    register_number = nfn;
    code_vector = register_codevector_contains_pc(xpGPR(xp, nfn), where);
  }
  if (code_vector == 0)
    register_number = 0;
  else
    index = ((natural)where - (code_vector + misc_data_offset)) >> 2;
  callback_to_lisp(callback_macptr, xp, register_number, index, arg1, arg2, arg3);
}

void
callback_to_lisp (LispObj callback_macptr, ExceptionInformation *xp,
                  natural arg1, natural arg2, natural arg3, natural arg4, natural arg5)
{
  natural  callback_ptr;
  area *a;

  TCR *tcr = TCR_FROM_TSD(xpGPR(xp, rcontext));

  /* Put the active stack pointer where .SPcallback expects it */
  a = tcr->cs_area;
  a->active = (BytePtr) ptr_from_lispobj(xpGPR(xp, sp));

  /* Copy globals from the exception frame to tcr */
  tcr->save_allocptr = (void *)ptr_from_lispobj(xpGPR(xp, allocptr));
  tcr->save_allocbase = (void *)ptr_from_lispobj(xpGPR(xp, allocbase));
  tcr->save_vsp = (LispObj*) ptr_from_lispobj(xpGPR(xp, vsp));
  tcr->save_tsp = (LispObj*) ptr_from_lispobj(xpGPR(xp, tsp));

#ifdef DARWIN
  enable_fp_exceptions();
#endif


  /* Call back.
     Lisp will handle trampolining through some code that
     will push lr/fn & pc/nfn stack frames for backtrace.
  */
  callback_ptr = ((macptr *)ptr_from_lispobj(untag(callback_macptr)))->address;
#ifdef DEBUG
  fprintf(dbgout, "0x%x releasing exception lock for callback\n", tcr);
#endif
  UNLOCK(lisp_global(EXCEPTION_LOCK), tcr);
  ((void (*)())callback_ptr) (xp, arg1, arg2, arg3, arg4, arg5);
  LOCK(lisp_global(EXCEPTION_LOCK), tcr);
#ifdef DEBUG
  fprintf(dbgout, "0x%x acquired exception lock after callback\n", tcr);
#endif



  /* Copy GC registers back into exception frame */
  xpGPR(xp, allocbase) = (LispObj) ptr_to_lispobj(tcr->save_allocbase);
  xpGPR(xp, allocptr) = (LispObj) ptr_to_lispobj(tcr->save_allocptr);
}

area *
allocate_no_stack (natural size)
{
#ifdef SUPPORT_PRAGMA_UNUSED
#pragma unused(size)
#endif

  return (area *) NULL;
}






/* callback to (symbol-value cmain) if it is a macptr, 
   otherwise report cause and function name to console.
   Returns noErr if exception handled OK */
OSStatus
handle_trap(ExceptionInformation *xp, opcode the_trap, pc where, siginfo_t *info)
{
  LispObj   cmain = nrs_CMAIN.vcell;
  TCR *tcr = TCR_FROM_TSD(xpGPR(xp, rcontext));

  /* If we got here, "the_trap" is either a TRI or a TR instruction.
     It's a TRI instruction iff its major opcode is major_opcode_TRI. */

  /* If it's a "trllt" instruction where RA == sp, it's a failed 
     control stack overflow check.  In that case:
     
     a) We're in "yellow zone" mode if the value of the
     lisp_global(CS_OVERFLOW_LIMIT) is CS_OVERFLOW_FORCE_LIMIT.  If
     we're not already in yellow zone mode, attempt to create a new
     thread and continue execution on its stack. If that fails, call
     signal_stack_soft_overflow to enter yellow zone mode and signal
     the condition to lisp.
     
     b) If we're already in "yellow zone" mode, then:
     
     1) if the SP is past the current control-stack area's hard
     overflow limit, signal a "hard" stack overflow error (e.g., throw
     to toplevel as quickly as possible. If we aren't in "yellow zone"
     mode, attempt to continue on another thread first.
     
     2) if SP is "well" (> 4K) below its soft overflow limit, set
     lisp_global(CS_OVERFLOW_LIMIT) to its "real" value.  We're out of
     "yellow zone mode" in this case.
     
     3) Otherwise, do nothing.  We'll continue to trap every time
     something gets pushed on the control stack, so we should try to
     detect and handle all of these cases fairly quickly.  Of course,
     the trap overhead is going to slow things down quite a bit.
     */

  if (X_opcode_p(the_trap,major_opcode_X31,minor_opcode_TR) &&
      (RA_field(the_trap) == sp) &&
      (TO_field(the_trap) == TO_LO)) {
    area 
      *CS_area = tcr->cs_area,
      *VS_area = tcr->vs_area;
      
    natural 
      current_SP = xpGPR(xp,sp),
      current_VSP = xpGPR(xp,vsp);

    if (current_SP  < (natural) (CS_area->hardlimit)) {
      /* If we're not in soft overflow mode yet, assume that the
         user has set the soft overflow size very small and try to
         continue on another thread before throwing to toplevel */
      if ((tcr->cs_limit == CS_OVERFLOW_FORCE_LIMIT)) {
        reset_lisp_process(xp);
      }
    } else {
      if (tcr->cs_limit == CS_OVERFLOW_FORCE_LIMIT) {
        /* If the control stack pointer is at least 4K away from its soft limit
	   and the value stack pointer is at least 4K away from its soft limit,
           stop trapping.  Else keep trapping. */
        if ((current_SP > (natural) ((CS_area->softlimit)+4096)) &&
	    (current_VSP > (natural) ((VS_area->softlimit)+4096))) {
	  protected_area_ptr vs_soft = VS_area->softprot;
	  if (vs_soft->nprot == 0) {
	    protect_area(vs_soft);
	  }
          tcr->cs_limit = ptr_to_lispobj(CS_area->softlimit);
        }
      } else {
	tcr->cs_limit = ptr_to_lispobj(CS_area->hardlimit);	  
	signal_stack_soft_overflow(xp, sp);
      }
    }
    
    adjust_exception_pc(xp, 4);
    return noErr;
  } else {
    if (the_trap == LISP_BREAK_INSTRUCTION) {
      char *message =  (char *) ptr_from_lispobj(xpGPR(xp,3));
      set_xpPC(xp, xpLR(xp));
      if (message == NULL) {
	message = "Lisp Breakpoint";
      }
      lisp_Debugger(xp, info, debug_entry_dbg, false, message);
      return noErr;
    }
    if (the_trap == QUIET_LISP_BREAK_INSTRUCTION) {
      adjust_exception_pc(xp,4);
      lisp_Debugger(xp, info, debug_entry_dbg, false, "Lisp Breakpoint");
      return noErr;
    }
    /*
      twlle ra,rb is used to detect tlb overflow, where RA = current
      limit and RB = index to use.
    */
    if ((X_opcode_p(the_trap, 31, minor_opcode_TR)) && 
        (TO_field(the_trap) == (TO_LO|TO_EQ))) {
      if (extend_tcr_tlb(tcr, xp, RA_field(the_trap), RB_field(the_trap))) {
        return noErr;
      }
      return -1;
    }

    if ((fulltag_of(cmain) == fulltag_misc) &&
        (header_subtag(header_of(cmain)) == subtag_macptr)) {
      if (the_trap == TRI_instruction(TO_GT,nargs,0)) {
        /* reset interrup_level, interrupt_pending */
        TCR_INTERRUPT_LEVEL(tcr) = 0;
        tcr->interrupt_pending = 0;
      }
#if 0
      fprintf(dbgout, "About to do trap callback in 0x%x\n",tcr);
#endif
      callback_for_trap(cmain, xp,  where, (natural) the_trap,  0, 0);
      adjust_exception_pc(xp, 4);
      return(noErr);
    }
    return -1;
  }
}


/* Look at up to TRAP_LOOKUP_TRIES instrs before trap instr for a pattern.
   Stop if subtag_code_vector is encountered. */
unsigned
scan_for_instr( unsigned target, unsigned mask, pc where )
{
  int i = TRAP_LOOKUP_TRIES;

  while( i-- ) {
    unsigned instr = *(--where);
    if ( codevec_hdr_p(instr) ) {
      return 0;
    } else if ( match_instr(instr, mask, target) ) {
      return instr;
    }
  }
  return 0;
}


void non_fatal_error( char *msg )
{
  fprintf( dbgout, "Non-fatal error: %s.\n", msg );
  fflush( dbgout );
}

/* The main opcode.  */

int 
is_conditional_trap(opcode instr)
{
  unsigned to = TO_field(instr);
  int is_tr = X_opcode_p(instr,major_opcode_X31,minor_opcode_TR);

#ifndef MACOS
  if ((instr == LISP_BREAK_INSTRUCTION) ||
      (instr == QUIET_LISP_BREAK_INSTRUCTION)) {
    return 1;
  }
#endif
  if (is_tr || major_opcode_p(instr,major_opcode_TRI)) {
    /* A "tw/td" or "twi/tdi" instruction.  To be unconditional, the
       EQ bit must be set in the TO mask and either the register
       operands (if "tw") are the same or either both of the signed or
       both of the unsigned inequality bits must be set. */
    if (! (to & TO_EQ)) {
      return 1;			/* Won't trap on EQ: conditional */
    }
    if (is_tr && (RA_field(instr) == RB_field(instr))) {
      return 0;			/* Will trap on EQ, same regs: unconditional */
    }
    if (((to & (TO_LO|TO_HI)) == (TO_LO|TO_HI)) || 
	((to & (TO_LT|TO_GT)) == (TO_LT|TO_GT))) {
      return 0;			/* Will trap on EQ and either (LT|GT) or (LO|HI) : unconditional */
    }
    return 1;			/* must be conditional */
  }
  return 0;			/* Not "tw/td" or "twi/tdi".  Let
                                   debugger have it */
}

OSStatus
handle_error(ExceptionInformation *xp, unsigned errnum, unsigned rb, unsigned continuable, pc where)
{
  LispObj   errdisp = nrs_ERRDISP.vcell;

  if ((fulltag_of(errdisp) == fulltag_misc) &&
      (header_subtag(header_of(errdisp)) == subtag_macptr)) {
    /* errdisp is a macptr, we can call back to lisp */
    callback_for_trap(errdisp, xp, where, errnum, rb, continuable);
    return(0);
    }

  return(-1);
}
	       

/* 
   Current thread has all signals masked.  Before unmasking them,
   make it appear that the current thread has been suspended.
   (This is to handle the case where another thread is trying
   to GC before this thread is able to sieze the exception lock.)
*/
int
prepare_to_wait_for_exception_lock(TCR *tcr, ExceptionInformation *context)
{
  int old_valence = tcr->valence;

  tcr->pending_exception_context = context;
  tcr->valence = TCR_STATE_EXCEPTION_WAIT;

  ALLOW_EXCEPTIONS(context);
  return old_valence;
}  

void
wait_for_exception_lock_in_handler(TCR *tcr, 
				   ExceptionInformation *context,
				   xframe_list *xf)
{

  LOCK(lisp_global(EXCEPTION_LOCK), tcr);
#ifdef DEBUG
  fprintf(dbgout, "0x%x has exception lock\n", tcr);
#endif
  xf->curr = context;
  xf->prev = tcr->xframe;
  tcr->xframe =  xf;
  tcr->pending_exception_context = NULL;
  tcr->valence = TCR_STATE_FOREIGN; 
}

void
unlock_exception_lock_in_handler(TCR *tcr)
{
  tcr->pending_exception_context = tcr->xframe->curr;
  tcr->xframe = tcr->xframe->prev;
  tcr->valence = TCR_STATE_EXCEPTION_RETURN;
#ifdef DEBUG
  fprintf(dbgout, "0x%x releasing exception lock\n", tcr);
#endif
  UNLOCK(lisp_global(EXCEPTION_LOCK),tcr);
}

/* 
   If an interrupt is pending on exception exit, try to ensure
   that the thread sees it as soon as it's able to run.
*/
void
raise_pending_interrupt(TCR *tcr)
{
  if (TCR_INTERRUPT_LEVEL(tcr) > 0) {
    pthread_kill((pthread_t)ptr_from_lispobj(tcr->osid), SIGNAL_FOR_PROCESS_INTERRUPT);
  }
}

void
exit_signal_handler(TCR *tcr, int old_valence)
{
  sigset_t mask;
  sigfillset(&mask);
  
  pthread_sigmask(SIG_SETMASK,&mask, NULL);
  tcr->valence = old_valence;
  tcr->pending_exception_context = NULL;
}


void
signal_handler(int signum, siginfo_t *info, ExceptionInformation  *context)
{
  TCR *tcr;
  int old_valence;
  xframe_list xframe_link;

  tcr = (TCR *) get_interrupt_tcr(false);
  
  /* The signal handler's entered with all signals (notably the
     thread_suspend signal) blocked.  Don't allow any other signals
     (notably the thread_suspend signal) to preempt us until we've
     set the TCR's xframe slot to include the current exception
     context.
  */
    
    old_valence = prepare_to_wait_for_exception_lock(tcr, context);

  if (tcr->flags & (1<<TCR_FLAG_BIT_PENDING_SUSPEND)) {
    CLR_TCR_FLAG(tcr, TCR_FLAG_BIT_PENDING_SUSPEND);
    pthread_kill(pthread_self(), thread_suspend_signal);
  }

  
  wait_for_exception_lock_in_handler(tcr, context, &xframe_link);
  if ((noErr != PMCL_exception_handler(signum, context, tcr, info, old_valence))) {
    char msg[512];
    snprintf(msg, sizeof(msg), "Unhandled exception %d at 0x%lx, context->regs at #x%lx", signum, xpPC(context), (natural)xpGPRvector(context));
    if (lisp_Debugger(context, info, signum, false, msg)) {
      SET_TCR_FLAG(tcr,TCR_FLAG_BIT_PROPAGATE_EXCEPTION);
    }
  }

  unlock_exception_lock_in_handler(tcr);

  /* This thread now looks like a thread that was suspended while
     executing lisp code.  If some other thread gets the exception
     lock and GCs, the context (this thread's suspend_context) will
     be updated.  (That's only of concern if it happens before we
     can return to the kernel/to the Mach exception handler).
  */
  exit_signal_handler(tcr, old_valence);
  raise_pending_interrupt(tcr);
}

/*
  If it looks like we're in the middle of an atomic operation, make
  it seem as if that operation is either complete or hasn't started
  yet.

  The cases handled include:

  a) storing into a newly-allocated lisp frame on the stack.
  b) marking a newly-allocated TSP frame as containing "raw" data.
  c) consing: the GC has its own ideas about how this should be
     handled, but other callers would be best advised to back
     up or move forward, according to whether we're in the middle
     of allocating a cons cell or allocating a uvector.
  d) a STMW to the vsp
  e) EGC write-barrier subprims.
*/

extern opcode
  egc_write_barrier_start,
  egc_write_barrier_end, 
  egc_store_node_conditional, 
  egc_store_node_conditional_test,
  egc_set_hash_key, egc_set_hash_key_did_store,
  egc_gvset, egc_gvset_did_store,
  egc_rplaca, egc_rplaca_did_store,
  egc_rplacd, egc_rplacd_did_store,
  egc_set_hash_key_conditional,
  egc_set_hash_key_conditional_test;


extern opcode ffcall_return_window, ffcall_return_window_end;

void
pc_luser_xp(ExceptionInformation *xp, TCR *tcr, signed_natural *alloc_disp)
{
  pc program_counter = xpPC(xp);
  opcode instr = *program_counter;
  lisp_frame *frame = (lisp_frame *)ptr_from_lispobj(xpGPR(xp,sp));
  LispObj cur_allocptr = xpGPR(xp, allocptr);
  int allocptr_tag = fulltag_of(cur_allocptr);
  


  if ((program_counter < &egc_write_barrier_end) && 
      (program_counter >= &egc_write_barrier_start)) {
    LispObj *ea = 0, val = 0, root = 0;
    bitvector refbits = (bitvector)(lisp_global(REFBITS));
    Boolean need_check_memo = true, need_memoize_root = false;

    if (program_counter >= &egc_set_hash_key_conditional) {
      if ((program_counter < &egc_set_hash_key_conditional_test) ||
	  ((program_counter == &egc_set_hash_key_conditional_test) &&
	   (! (xpCCR(xp) & 0x20000000)))) {
	return;
      }
      root = xpGPR(xp,arg_x);
      ea = (LispObj*)(root + unbox_fixnum(xpGPR(xp,temp0)));
      need_memoize_root = true;
    } else if (program_counter >= &egc_store_node_conditional) {
      if ((program_counter < &egc_store_node_conditional_test) ||
	  ((program_counter == &egc_store_node_conditional_test) &&
	   (! (xpCCR(xp) & 0x20000000)))) {
	/* The conditional store either hasn't been attempted yet, or
	   has failed.  No need to adjust the PC, or do memoization. */
	return;
      }
      ea = (LispObj*)(xpGPR(xp,arg_x) + unbox_fixnum(xpGPR(xp,temp0)));
      xpGPR(xp,arg_z) = t_value;
    } else if (program_counter >= &egc_set_hash_key) {
      if (program_counter < &egc_set_hash_key_did_store) {
        return;
      }
      root = xpGPR(xp,arg_x);
      val = xpGPR(xp,arg_z);
      ea = (LispObj *) (root+xpGPR(xp,arg_y)+misc_data_offset);
      need_memoize_root = true;
    } else if (program_counter >= &egc_gvset) {
      if (program_counter < &egc_gvset_did_store) {
        return;
      }
      ea = (LispObj *) (xpGPR(xp,arg_x)+xpGPR(xp,arg_y)+misc_data_offset);
      val = xpGPR(xp,arg_z);
    } else if (program_counter >= &egc_rplacd) {
      if (program_counter < &egc_rplacd_did_store) {
        return;
      }
      ea = (LispObj *) untag(xpGPR(xp,arg_y));
      val = xpGPR(xp,arg_z);
    } else {                      /* egc_rplaca */
      if (program_counter < &egc_rplaca_did_store) {
        return;
      }
      ea =  ((LispObj *) untag(xpGPR(xp,arg_y)))+1;
      val = xpGPR(xp,arg_z);
    }
    if (need_check_memo) {
      natural  bitnumber = area_dnode(ea, lisp_global(REF_BASE));
      if ((bitnumber < lisp_global(OLDSPACE_DNODE_COUNT)) &&
          ((LispObj)ea < val)) {
        atomic_set_bit(refbits, bitnumber);
        atomic_set_bit(global_refidx, bitnumber>>8);
        if (need_memoize_root) {
          bitnumber = area_dnode(root, lisp_global(REF_BASE));
          atomic_set_bit(refbits, bitnumber);
          atomic_set_bit(global_refidx,bitnumber>>8);
        }
      }
    }
    set_xpPC(xp, xpLR(xp));
    return;
  }


  if (instr == MARK_TSP_FRAME_INSTRUCTION) {
    LispObj tsp_val = xpGPR(xp,tsp);
    
    ((LispObj *)ptr_from_lispobj(tsp_val))[1] = tsp_val;
    adjust_exception_pc(xp, 4);
    return;
  }
  
  if (frame->backlink == (frame+1)) {
    if (
#ifdef PPC64
        (major_opcode_p(instr, major_opcode_DS_STORE64)) &&
        (DS_VARIANT_FIELD(instr) == DS_STORE64_VARIANT_STD) &&
#else
        (major_opcode_p(instr, major_opcode_STW)) && 
#endif
	(RA_field(instr) == sp) &&
	/* There are a few places in the runtime that store into
	   a previously-allocated frame atop the stack when
	   throwing values around.  We only care about the case
	   where the frame was newly allocated, in which case
	   there must have been a CREATE_LISP_FRAME_INSTRUCTION
	   a few instructions before the current program counter.
	   (The whole point here is that a newly allocated frame
	   might contain random values that we don't want the
	   GC to see; a previously allocated frame should already
	   be completely initialized.)
	*/
	((program_counter[-1] == CREATE_LISP_FRAME_INSTRUCTION) ||
	 (program_counter[-2] == CREATE_LISP_FRAME_INSTRUCTION) ||
	 (program_counter[-3] == CREATE_LISP_FRAME_INSTRUCTION)))  {
#ifdef PPC64
      int disp = DS_field(instr);
#else      
      int disp = D_field(instr);
#endif


      if (disp < (4*node_size)) {
#if 0
        fprintf(dbgout, "pc-luser: finish SP frame in 0x%x, disp = %d\n",tcr, disp);
#endif
	frame->savevsp = 0;
	if (disp < (3*node_size)) {
	  frame->savelr = 0;
	  if (disp == node_size) {
	    frame->savefn = 0;
	  }
	}
      }
      return;
    }
  }

  if (allocptr_tag != tag_fixnum) {
    signed_natural disp = allocptr_displacement(xp);

    if (disp) {
      /* Being architecturally "at" the alloc trap doesn't tell
         us much (in particular, it doesn't tell us whether
         or not the thread has committed to taking the trap
         and is waiting for the exception lock (or waiting
         for the Mach exception thread to tell it how bad
         things are) or is about to execute a conditional
         trap.
         Regardless of which case applies, we want the
         other thread to take (or finish taking) the
         trap, and we don't want it to consider its
         current allocptr to be valid.
         The difference between this case (suspend other
         thread for GC) and the previous case (suspend
         current thread for interrupt) is solely a
         matter of what happens after we leave this
         function: some non-current thread will stay
         suspended until the GC finishes, then take
         (or start processing) the alloc trap.   The
         current thread will go off and do PROCESS-INTERRUPT
         or something, and may return from the interrupt
         and need to finish the allocation that got interrupted.
      */

      if (alloc_disp) {
        *alloc_disp = disp;
        xpGPR(xp,allocptr) += disp;
        /* Leave the PC at the alloc trap.  When the interrupt
           handler returns, it'll decrement allocptr by disp
           and the trap may or may not be taken.
        */
      } else {
        update_bytes_allocated(tcr, (void *) ptr_from_lispobj(cur_allocptr + disp));
        xpGPR(xp, allocbase) = VOID_ALLOCPTR;
        xpGPR(xp, allocptr) = VOID_ALLOCPTR - disp;
      }
    } else {
#ifdef DEBUG
      fprintf(dbgout, "tcr 0x%x is past alloc trap, finishing alloc at 0x%x\n", tcr, xpGPR(xp,allocptr));
#endif
      /* If we're already past the alloc_trap, finish allocating
         the object. */
      if (allocptr_tag == fulltag_cons) {
        finish_allocating_cons(xp);
#ifdef DEBUG
          fprintf(dbgout, "finish allocating cons in TCR = #x%x\n",
                  tcr);
#endif
      } else {
        if (allocptr_tag == fulltag_misc) {
#ifdef DEBUG
          fprintf(dbgout, "finish allocating uvector in TCR = #x%x\n",
                  tcr);
#endif
          finish_allocating_uvector(xp);
        } else {
          Bug(xp, "what's being allocated here ?");
        }
      }
      /* Whatever we finished allocating, reset allocptr/allocbase to
         VOID_ALLOCPTR */
      xpGPR(xp,allocptr) = xpGPR(xp,allocbase) = VOID_ALLOCPTR;
    }
    return;
  }

  if ((instr & INIT_CATCH_FRAME_MASK) == INIT_CATCH_FRAME_INSTRUCTION) {
    LispObj *frame = ptr_from_lispobj(untag(xpGPR(xp, nargs)));
    int idx = ((int)((short)(D_field(instr))+fulltag_misc))>>fixnumshift;
#if 0
        fprintf(dbgout, "pc-luser: CATCH frame in 0x%x, idx = %d\n",tcr, idx);
#endif

    for (;idx < sizeof(catch_frame)/sizeof(LispObj); idx++) {
      deref(frame,idx) = 0;
    }
    ((LispObj *)(xpGPR(xp, tsp)))[1] = 0;
    return;
  }

#ifndef PC64
  if ((major_opcode_p(instr, 47)) && /* 47 = stmw */
      (RA_field(instr) == vsp)) {
    int r;
    LispObj *vspptr = ptr_from_lispobj(xpGPR(xp,vsp));
    
    for (r = RS_field(instr); r <= 31; r++) {
      *vspptr++ = xpGPR(xp,r);
    }
    adjust_exception_pc(xp, 4);
  }
#endif
}

void
interrupt_handler (int signum, siginfo_t *info, ExceptionInformation *context)
{
  TCR *tcr = get_interrupt_tcr(false);
  if (tcr) {
    if (TCR_INTERRUPT_LEVEL(tcr) < 0) {
      tcr->interrupt_pending = 1 << fixnumshift;
    } else {
      LispObj cmain = nrs_CMAIN.vcell;

      if ((fulltag_of(cmain) == fulltag_misc) &&
	  (header_subtag(header_of(cmain)) == subtag_macptr)) {
	/* 
	   This thread can (allegedly) take an interrupt now.
	   It's tricky to do that if we're executing
	   foreign code (especially Linuxthreads code, much
	   of which isn't reentrant.)
           If we're unwinding the stack, we also want to defer
           the interrupt.
	*/
	if ((tcr->valence != TCR_STATE_LISP) ||
            (tcr->unwinding != 0)) {
	  TCR_INTERRUPT_LEVEL(tcr) = (1 << fixnumshift);
	} else {
	  xframe_list xframe_link;
	  int old_valence;
          signed_natural disp=0;
	  
	  pc_luser_xp(context, tcr, &disp);
	  old_valence = prepare_to_wait_for_exception_lock(tcr, context);
	  wait_for_exception_lock_in_handler(tcr, context, &xframe_link);
#ifdef DEBUG
          fprintf(dbgout, "[0x%x acquired exception lock for interrupt]\n",tcr);
#endif
	  PMCL_exception_handler(signum, context, tcr, info, old_valence);
          if (disp) {
            xpGPR(context,allocptr) -= disp;
          }
	  unlock_exception_lock_in_handler(tcr);
#ifdef DEBUG
          fprintf(dbgout, "[0x%x released exception lock for interrupt]\n",tcr);
#endif
	  exit_signal_handler(tcr, old_valence);
	}
      }
    }
  }
#ifdef DARWIN
    DarwinSigReturn(context);
#endif
}



void
install_signal_handler(int signo, void *handler, unsigned flags)
{
  struct sigaction sa;
  int err;
  
  sa.sa_sigaction = (void *)handler;
  sigfillset(&sa.sa_mask);
  sa.sa_flags = SA_SIGINFO;

  if (flags & RESTART_SYSCALLS)
    sa.sa_flags |= SA_RESTART;
  if (flags & RESERVE_FOR_LISP) {
    extern sigset_t user_signals_reserved;
    sigaddset(&user_signals_reserved, signo);
  }

  err = sigaction(signo, &sa, NULL);
  if (err) {
    perror("sigaction");
    exit(1);
  }
}

void
install_pmcl_exception_handlers()
{

  extern int no_sigtrap;
  install_signal_handler(SIGILL, (void *)signal_handler, RESERVE_FOR_LISP);
  if (no_sigtrap != 1) {
    install_signal_handler(SIGTRAP, (void *)signal_handler, RESERVE_FOR_LISP);
  }
  install_signal_handler(SIGBUS,  (void *)signal_handler, RESERVE_FOR_LISP);
  install_signal_handler(SIGSEGV, (void *)signal_handler, RESERVE_FOR_LISP);
  install_signal_handler(SIGFPE, (void *)signal_handler, RESERVE_FOR_LISP);

  
  install_signal_handler(SIGNAL_FOR_PROCESS_INTERRUPT,
			 (void *)interrupt_handler, RESERVE_FOR_LISP);
  signal(SIGPIPE, SIG_IGN);
}

void
thread_kill_handler(int signum, siginfo_t info, ExceptionInformation *xp)
{
  TCR *tcr = get_tcr(false);
  area *a;
  sigset_t mask;
  
  sigemptyset(&mask);

  if (tcr) {
    tcr->valence = TCR_STATE_FOREIGN;
    a = tcr->vs_area;
    if (a) {
      a->active = a->high;
    }
    a = tcr->ts_area;
    if (a) {
      a->active = a->high;
    }
    a = tcr->cs_area;
    if (a) {
      a->active = a->high;
    }
  }
  
  pthread_sigmask(SIG_SETMASK,&mask,NULL);
  pthread_exit(NULL);
}

void
thread_signal_setup()
{
  thread_suspend_signal = SIG_SUSPEND_THREAD;
  thread_kill_signal = SIG_KILL_THREAD;

  install_signal_handler(thread_suspend_signal, (void *)suspend_resume_handler,
			 RESERVE_FOR_LISP|RESTART_SYSCALLS);
  install_signal_handler(thread_kill_signal, (void *)thread_kill_handler,
			 RESERVE_FOR_LISP);
}



void
unprotect_all_areas()
{
  protected_area_ptr p;

  for(p = AllProtectedAreas, AllProtectedAreas = NULL; p; p = p->next) {
    unprotect_area(p);
  }
}

/*
  A binding subprim has just done "twlle limit_regno,idx_regno" and
  the trap's been taken.  Extend the tcr's tlb so that the index will
  be in bounds and the new limit will be on a page boundary, filling
  in the new page(s) with 'no_thread_local_binding_marker'.  Update
  the tcr fields and the registers in the xp and return true if this
  all works, false otherwise.

  Note that the tlb was allocated via malloc, so realloc can do some
  of the hard work.
*/
Boolean
extend_tcr_tlb(TCR *tcr, 
               ExceptionInformation *xp, 
               unsigned limit_regno,
               unsigned idx_regno)
{
  unsigned
    index = (unsigned) (xpGPR(xp,idx_regno)),
    old_limit = tcr->tlb_limit,
    new_limit = align_to_power_of_2(index+1,12),
    new_bytes = new_limit-old_limit;
  LispObj 
    *old_tlb = tcr->tlb_pointer,
    *new_tlb = realloc(old_tlb, new_limit),
    *work;

  if (new_tlb == NULL) {
    return false;
  }
  
  work = (LispObj *) ((BytePtr)new_tlb+old_limit);

  while (new_bytes) {
    *work++ = no_thread_local_binding_marker;
    new_bytes -= sizeof(LispObj);
  }
  tcr->tlb_pointer = new_tlb;
  tcr->tlb_limit = new_limit;
  xpGPR(xp, limit_regno) = new_limit;
  return true;
}



void
exception_init()
{
  install_pmcl_exception_handlers();
}


