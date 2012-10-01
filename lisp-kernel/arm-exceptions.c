

/*
   Copyright (C) 2010 Clozure Associates
   This file is part of Clozure CL.  

   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public
   License , known as the LLGPL and distributed with Clozure CL as the
   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
   which is distributed with Clozure CL as the file "LGPL".  Where these
   conflict, the preamble takes precedence.  

   Clozure CL is referenced in the preamble as the "LIBRARY."

   The LLGPL is also available online at
   http://opensource.franz.com/preamble.html
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
#ifndef ANDROID
#include <fpu_control.h>
#include <linux/prctl.h>
#endif
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


#include "threads.h"

#ifdef ANDROID
#define pthread_sigmask(how,in,out) rt_sigprocmask(how,in,out,8)
#endif

#ifdef LINUX

void
enable_fp_exceptions()
{
}

void
disable_fp_exceptions()
{
}
#endif

/*
  Handle exceptions.

*/

extern LispObj lisp_nil;

extern natural lisp_heap_gc_threshold;
extern Boolean grow_dynamic_area(natural);

Boolean allocation_enabled = true;




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
  opcode instr = *program_counter, prev_instr;
  int delta = -3;

  if (IS_ALLOC_TRAP(instr)) {
    /* The alloc trap must have been preceded by a cmp and a
       load from tcr.allocbase. */
    if (IS_BRANCH_AROUND_ALLOC_TRAP(program_counter[-1])) {
      delta = -4;
    }
    prev_instr = program_counter[delta];

    if (IS_SUB_RM_FROM_ALLOCPTR(prev_instr)) {
      return -((signed_natural)xpGPR(xp,RM_field(prev_instr)));
    }
    
    if (IS_SUB_LO_FROM_ALLOCPTR(prev_instr)) {
      return -((signed_natural)(prev_instr & 0xff));
    }

    if (IS_SUB_FROM_ALLOCPTR(prev_instr)) {
      natural disp = ror(prev_instr&0xff,(prev_instr&0xf00)>>7);

      instr = program_counter[delta-1];
      if (IS_SUB_LO_FROM_ALLOCPTR(instr)) {
        return -((signed_natural)(disp | (instr & 0xff)));
      }
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

    if (IS_CLR_ALLOCPTR_TAG(instr)) {
      xpGPR(xp, allocptr) = untag(cur_allocptr);
      xpPC(xp) = program_counter;
      return;
    } else if (IS_SET_ALLOCPTR_CAR_RD(instr)) {
      c->car = xpGPR(xp,RD_field(instr));
    } else if (IS_SET_ALLOCPTR_CDR_RD(instr)) {
      c->cdr = xpGPR(xp,RD_field(instr));
    } else {
      /* assert(IS_SET_ALLOCPTR_RESULT_RD(instr)) */
      xpGPR(xp,RD_field(instr)) = cur_allocptr;
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
    if (IS_CLR_ALLOCPTR_TAG(instr)) {
      xpGPR(xp, allocptr) = untag(cur_allocptr);
      xpPC(xp) = program_counter;
      return;
    }
    if (IS_SET_ALLOCPTR_HEADER_RD(instr)) {
      header_of(cur_allocptr) = xpGPR(xp,RD_field(instr));
    } else if (IS_SET_ALLOCPTR_RESULT_RD(instr)) {
      xpGPR(xp,RD_field(instr)) = cur_allocptr;
    } else {
      Bug(xp, "Unexpected instruction following alloc trap at " LISP ":",program_counter);
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
  if (new_heap_segment(xp, bytes_needed, false, tcr)) {
    xpGPR(xp, allocptr) += disp_from_allocptr;
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
  if (new_heap_segment(xp, bytes_needed, true, tcr)) {
    xpGPR(xp, allocptr) += disp_from_allocptr;
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
  handle_error(xp, bytes_needed < (128<<10) ? XNOMEM : error_alloc_failed,0, NULL);
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

Boolean
handle_alloc_trap(ExceptionInformation *xp, TCR *tcr)
{
  pc program_counter;
  natural cur_allocptr, bytes_needed = 0;
  opcode prev_instr;
  signed_natural disp = 0;
  unsigned allocptr_tag;

  if (!allocation_enabled) {
    /* Back up before the alloc_trap, then let pc_luser_xp() back
       up some more. */
    xpPC(xp)-=1;
    pc_luser_xp(xp,tcr, NULL);
    allocation_enabled = true;
    tcr->save_allocbase = (void *)VOID_ALLOCPTR;
    handle_error(xp, error_allocation_disabled,0,NULL);
    return true;
  }

  cur_allocptr = xpGPR(xp,allocptr);

  allocptr_tag = fulltag_of(cur_allocptr);

  switch (allocptr_tag) {
  case fulltag_cons:
    bytes_needed = sizeof(cons);
    disp = -sizeof(cons) + fulltag_cons;
    break;

  case fulltag_misc:
    disp = allocptr_displacement(xp);
    bytes_needed = (-disp) + fulltag_misc;
    break;

    /* else fall thru */
  default:
    return false;
  }

  update_bytes_allocated(tcr,((BytePtr)(cur_allocptr-disp)));
  if (allocate_object(xp, bytes_needed, disp, tcr)) {
    adjust_exception_pc(xp,4);
    return true;
  }
  lisp_allocation_failure(xp,tcr,bytes_needed);
  return true;
}

natural gc_deferred = 0, full_gc_deferred = 0;

signed_natural
flash_freeze(TCR *tcr, signed_natural param)
{
  return 0;
}

Boolean
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

  case GC_TRAP_FUNCTION_ALLOCATION_CONTROL:
    switch(arg) {
    case 0: /* disable if allocation enabled */
      xpGPR(xp, arg_z) = lisp_nil;
      if (allocation_enabled) {
        TCR *other_tcr;
        ExceptionInformation *other_context;
        suspend_other_threads(true);
        normalize_tcr(xp,tcr,false);
        for (other_tcr=tcr->next; other_tcr != tcr; other_tcr = other_tcr->next) {
          other_context = other_tcr->pending_exception_context;
          if (other_context == NULL) {
            other_context = other_tcr->suspend_context;
          }
          normalize_tcr(other_context, other_tcr, true);
        }
        allocation_enabled = false;
        xpGPR(xp, arg_z) = t_value;
        resume_other_threads(true);
      }
      break;

    case 1:                     /* enable if disabled */
      xpGPR(xp, arg_z) = lisp_nil;
      if (!allocation_enabled) {
        allocation_enabled = true;
        xpGPR(xp, arg_z) = t_value;
      }
      break;

    default:
      xpGPR(xp, arg_z) = lisp_nil;
      if (allocation_enabled) {
        xpGPR(xp, arg_z) = t_value;
      }
      break;
    }
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
        TCR *tcr = get_tcr(true);
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
  return true;
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
  handle_error(xp, error_stack_overflow, reg, NULL);
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
  case Rsp:
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
  }
}

/* Maybe this'll work someday.  We may have to do something to
   make the thread look like it's not handling an exception */
void
reset_lisp_process(ExceptionInformation *xp)
{
#if 0
  TCR *tcr = get_tcr(true);
  catch_frame *last_catch = (catch_frame *) ptr_from_lispobj(untag(tcr->catch_top));

  tcr->save_allocptr = (void *) ptr_from_lispobj(xpGPR(xp, allocptr));

  tcr->save_vsp = (LispObj *) ptr_from_lispobj(((lisp_frame *)ptr_from_lispobj(last_catch->csp))->savevsp);

  start_lisp(tcr, 1);
#endif
}

/*
  This doesn't GC; it returns true if it made enough room, false
  otherwise.
  If "extend" is true, it can try to extend the dynamic area to
  satisfy the request.
*/

Boolean
new_heap_segment(ExceptionInformation *xp, natural need, Boolean extend, TCR *tcr)
{
  area *a;
  natural newlimit, oldlimit;
  natural log2_allocation_quantum = tcr->log2_allocation_quantum;

  a  = active_dynamic_area;
  oldlimit = (natural) a->active;
  newlimit = (align_to_power_of_2(oldlimit, log2_allocation_quantum) +
	      align_to_power_of_2(need, log2_allocation_quantum));
  if (newlimit > (natural) (a->high)) {
    if (extend) {
      signed_natural inhibit = (signed_natural)(lisp_global(GC_INHIBIT_COUNT));
      natural extend_by = inhibit ? 0 : lisp_heap_gc_threshold;
      do {
        if (resize_dynamic_heap(a->active, (newlimit-oldlimit)+extend_by)) {
          break;
        }
        extend_by = align_to_power_of_2(extend_by>>1, log2_allocation_quantum);
        if (extend_by < 4<<20) {
          return false;
        }
      } while (1);
    } else {
      return false;
    }
  }
  a->active = (BytePtr) newlimit;
  tcr->last_allocptr = (void *)newlimit;
  xpGPR(xp,allocptr) = (LispObj) newlimit;
  tcr->save_allocbase = (void*) oldlimit;

  return true;
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
    bp = (LispObj *) xpGPR(xp, Rsp);
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
    update_area_active((area **)&tcr->cs_area, (BytePtr) ptr_from_lispobj(xpGPR(xp, Rsp)));
    update_area_active((area **)&tcr->vs_area, (BytePtr) ptr_from_lispobj(xpGPR(xp, vsp)));
  } else {
    /* In ff-call. */
    cur_allocptr = (void *) (tcr->save_allocptr);
    update_area_active((area **)&tcr->vs_area, (BytePtr) tcr->save_vsp);
    update_area_active((area **)&tcr->cs_area, (BytePtr) tcr->last_lisp_frame);
  }


  tcr->save_allocptr = tcr->save_allocbase = (void *)VOID_ALLOCPTR;
  if (cur_allocptr) {
    update_bytes_allocated(tcr, cur_allocptr);
    if (freeptr) {
      xpGPR(xp, allocptr) = VOID_ALLOCPTR;
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
  TCR *tcr = get_tcr(true), *other_tcr;
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
  return ((xpFaultStatus(xp) & 0x800) != 0);
}

Boolean
handle_protection_violation(ExceptionInformation *xp, siginfo_t *info, TCR *tcr, int old_valence)
{
  BytePtr addr;
  protected_area_ptr area;
  protection_handler *handler;
  extern Boolean touch_page(void *);
  extern void touch_page_end(void);

#ifdef LINUX
  addr = (BytePtr) ((natural) (xpFaultAddress(xp)));
#else
  if (info) {
    addr = (BytePtr)(info->si_addr);
  } else {
    addr = (BytePtr) ((natural) (xpFaultAddress(xp)));
  }
#endif

  if (addr && (addr == tcr->safe_ref_address)) {
    adjust_exception_pc(xp,4);

    xpGPR(xp,imm0) = 0;
    return true;
  }

  if (xpPC(xp) == (pc)touch_page) {
    xpGPR(xp,imm0) = 0;
    xpPC(xp) = (pc)touch_page_end;
    return true;
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
	return true;
      }
    }
  }
  if (old_valence == TCR_STATE_LISP) {
    LispObj cmain = nrs_CMAIN.vcell;
    
    if ((fulltag_of(cmain) == fulltag_misc) &&
      (header_subtag(header_of(cmain)) == subtag_macptr)) {
      
      callback_for_trap(nrs_CMAIN.vcell, xp, is_write_fault(xp,info)?SIGBUS:SIGSEGV, (natural)addr, NULL);
    }
  }
  return false;
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






Boolean
lisp_frame_p(lisp_frame *spPtr)
{
  return (spPtr->marker == lisp_frame_marker);
}


int ffcall_overflow_count = 0;






/* Note: CURRENT_VS (CURRENT_TS) is always either the area containing
  the current value of VSP (TSP) or an older area.  */

OSStatus
do_vsp_overflow (ExceptionInformation *xp, BytePtr addr)
{
  TCR* tcr = get_tcr(true);
  area *a = tcr->vs_area;
  protected_area_ptr vsp_soft = a->softprot;
  unprotect_area(vsp_soft);
  signal_stack_soft_overflow(xp,vsp);
  return 0;
}



OSStatus
do_soft_stack_overflow(ExceptionInformation *xp, protected_area_ptr prot_area, BytePtr addr)
{
  /* Trying to write into a guard page on the vstack or tstack.
     Allocate a new stack segment, emulate stwu and stwux for the TSP, and
     signal an error_stack_overflow condition.
      */
  if (prot_area->why == kVSPsoftguard) {
    return do_vsp_overflow(xp,addr);
  }
  unprotect_area(prot_area);
  signal_stack_soft_overflow(xp,Rsp);
  return 0;
}

OSStatus
do_spurious_wp_fault(ExceptionInformation *xp, protected_area_ptr area, BytePtr addr)
{
#ifdef SUPPORT_PRAGMA_UNUSED
#pragma unused(xp,area,addr)
#endif
  return -1;
}




      





Boolean
handle_sigfpe(ExceptionInformation *xp, TCR *tcr)
{
  return false;
}


Boolean
handle_unimplemented_instruction(ExceptionInformation *xp,
                                 opcode instruction,
                                 TCR *tcr)
{

  return false;
}

Boolean
handle_exception(int xnum, 
                 ExceptionInformation *xp, 
                 TCR *tcr, 
                 siginfo_t *info,
                 int old_valence)
{
  pc program_counter;
  opcode instruction = 0;

  if (old_valence != TCR_STATE_LISP) {
    return false;
  }

  program_counter = xpPC(xp);
  
  if ((xnum == SIGILL) | (xnum == SIGTRAP)) {
    instruction = *program_counter;
  }

  if (IS_ALLOC_TRAP(instruction)) {
    return handle_alloc_trap(xp, tcr);
  } else if ((xnum == SIGSEGV) ||
	     (xnum == SIGBUS)) {
    return handle_protection_violation(xp, info, tcr, old_valence);
  } else if (xnum == SIGFPE) {
    return handle_sigfpe(xp, tcr);
  } else if ((xnum == SIGILL)) {
    if (IS_GC_TRAP(instruction)) {
      return handle_gc_trap(xp, tcr);
    } else if (IS_UUO(instruction)) {
      return handle_uuo(xp, info, instruction);
    } else {
      return handle_unimplemented_instruction(xp,instruction,tcr);
    }
  } else if (xnum == SIGNAL_FOR_PROCESS_INTERRUPT) {
    tcr->interrupt_pending = 0;
    callback_for_trap(nrs_CMAIN.vcell, xp, 0, 0, NULL);
    return true;
  }

  return false;
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
  OSStatus err = -1;
  opcode *there = (opcode *) where, instr, errnum = 0;
  return err;
}

Boolean
handle_uuo(ExceptionInformation *xp, siginfo_t *info, opcode the_uuo) 
{
  unsigned 
    format = UUO_FORMAT(the_uuo);
  Boolean handled = false;
  int bump = 4;
  TCR *tcr = get_tcr(true);

  switch (format) {
  case uuo_format_kernel_service:
    {
      TCR * target = (TCR *)xpGPR(xp,arg_z);
      int service = UUO_UNARY_field(the_uuo);

      switch (service) {
      case error_propagate_suspend:
        handled = true;
        break;
      case error_interrupt:
        xpGPR(xp,imm0) = (LispObj) raise_thread_interrupt(target);
        handled = true;
        break;
      case error_suspend:
        xpGPR(xp,imm0) = (LispObj) lisp_suspend_tcr(target);
        handled = true;
        break;
      case error_suspend_all:
        lisp_suspend_other_threads();
        handled = true;
        break;
      case error_resume:
        xpGPR(xp,imm0) = (LispObj) lisp_resume_tcr(target);
        handled = true;
        break;
      case error_resume_all:
        lisp_resume_other_threads();
        handled = true;
        break;
      case error_kill:
        xpGPR(xp,imm0) = (LispObj)kill_tcr(target);
        handled = true;
        break;
      case error_allocate_list:
        allocate_list(xp,tcr);
        handled = true;
        break;
      default:
        handled = false;
        break;
      }
      break;
    }

  case uuo_format_unary:
    switch(UUO_UNARY_field(the_uuo)) {
    case 3:
      if (extend_tcr_tlb(tcr,xp,UUOA_field(the_uuo))) {
        handled = true;
        bump = 4;
        break;
      }
      /* fall in */
    default:
      handled = false;
      break;

    }
    break;

  case uuo_format_nullary:
    switch (UUOA_field(the_uuo)) {
    case 3:
      adjust_exception_pc(xp, bump);
      bump = 0;
      lisp_Debugger(xp, info, debug_entry_dbg, false, "Lisp Breakpoint");
      handled = true;
      break;

    case 4:
      tcr->interrupt_pending = 0;
      callback_for_trap(nrs_CMAIN.vcell, xp, 0, 0, NULL);
      handled = true;
      break;
    default:
      handled = false;
      break;
    }
    break;


  case uuo_format_error_lisptag:
  case uuo_format_error_fulltag:
  case uuo_format_error_xtype:
  case uuo_format_cerror_lisptag:
  case uuo_format_cerror_fulltag:
  case uuo_format_cerror_xtype:
  case uuo_format_nullary_error:
  case uuo_format_unary_error:
  case uuo_format_binary_error:
  case uuo_format_ternary:
  case uuo_format_ternary2:
    handled = handle_error(xp,0,the_uuo, &bump);
    break;

  default:
    handled = false;
    bump = 0;
  }
  
  if (handled && bump) {
    adjust_exception_pc(xp, bump);
  }
  return handled;
}

natural
register_codevector_contains_pc (natural lisp_function, pc where)
{
  natural code_vector, size;

  if ((fulltag_of(lisp_function) == fulltag_misc) &&
      (header_subtag(header_of(lisp_function)) == subtag_function)) {
    code_vector = deref(lisp_function, 2);
    size = header_element_count(header_of(code_vector)) << 2;
    if ((untag(code_vector) < (natural)where) && 
	((natural)where < (code_vector + size)))
      return(code_vector);
  }

  return(0);
}

Boolean
callback_for_trap (LispObj callback_macptr, ExceptionInformation *xp, natural info,natural arg, int *bumpP)
{
  return callback_to_lisp(callback_macptr, xp, info,arg, bumpP);
}

Boolean
callback_to_lisp (LispObj callback_macptr, ExceptionInformation *xp,
                  natural arg1, natural arg2, int *bumpP)
{
  natural  callback_ptr;
  area *a;
  natural fnreg = Rfn,  codevector, offset;
  pc where = xpPC(xp);
  int delta;

  codevector = register_codevector_contains_pc(xpGPR(xp,fnreg), where);
  if (codevector == 0) {
    fnreg = nfn;
    codevector = register_codevector_contains_pc(xpGPR(xp,fnreg), where);
    if (codevector == 0) {
      fnreg = 0;
    }
  }
  if (codevector) {
    offset = (natural)where - (codevector - (fulltag_misc-node_size));
  } else {
    offset = (natural)where;
  }
                                                 
                                               

  TCR *tcr = get_tcr(true);

  /* Put the active stack pointer where .SPcallback expects it */
  a = tcr->cs_area;
  a->active = (BytePtr) ptr_from_lispobj(xpGPR(xp, Rsp));

  /* Copy globals from the exception frame to tcr */
  tcr->save_allocptr = (void *)ptr_from_lispobj(xpGPR(xp, allocptr));
  tcr->save_vsp = (LispObj*) ptr_from_lispobj(xpGPR(xp, vsp));



  /* Call back.
     Lisp will handle trampolining through some code that
     will push lr/fn & pc/nfn stack frames for backtrace.
  */
  callback_ptr = ((macptr *)ptr_from_lispobj(untag(callback_macptr)))->address;
  UNLOCK(lisp_global(EXCEPTION_LOCK), tcr);
  delta = ((int (*)())callback_ptr) (xp, arg1, arg2, fnreg, offset);
  LOCK(lisp_global(EXCEPTION_LOCK), tcr);

  if (bumpP) {
    *bumpP = delta;
  }

  /* Copy GC registers back into exception frame */
  xpGPR(xp, allocptr) = (LispObj) ptr_to_lispobj(tcr->save_allocptr);
  return true;
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

}




void non_fatal_error( char *msg )
{
  fprintf( dbgout, "Non-fatal error: %s.\n", msg );
  fflush( dbgout );
}



Boolean
handle_error(ExceptionInformation *xp, unsigned arg1, unsigned arg2, int *bumpP)
{
  LispObj   errdisp = nrs_ERRDISP.vcell;

  if ((fulltag_of(errdisp) == fulltag_misc) &&
      (header_subtag(header_of(errdisp)) == subtag_macptr)) {
    /* errdisp is a macptr, we can call back to lisp */
    return callback_for_trap(errdisp, xp, arg1, arg2, bumpP);
    }

  return false;
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
exit_signal_handler(TCR *tcr, int old_valence, natural old_last_lisp_frame)
{
#ifndef ANDROID
  sigset_t mask;
  sigfillset(&mask);
#else
  int mask [] = {-1,-1};
#endif
  
  pthread_sigmask(SIG_SETMASK,(sigset_t *)&mask, NULL);
  tcr->valence = old_valence;
  tcr->pending_exception_context = NULL;
  tcr->last_lisp_frame = old_last_lisp_frame;
}


void
signal_handler(int signum, siginfo_t *info, ExceptionInformation  *context
#ifdef DARWIN
, TCR *tcr, int old_valence, natural old_last_lisp_frame
#endif
)
{
  xframe_list xframe_link;
#ifndef DARWIN
    
    TCR *tcr = (TCR *) get_interrupt_tcr(false);
  
    /* The signal handler's entered with all signals (notably the
       thread_suspend signal) blocked.  Don't allow any other signals
       (notably the thread_suspend signal) to preempt us until we've
       set the TCR's xframe slot to include the current exception
       context.
    */
    
    natural  old_last_lisp_frame = tcr->last_lisp_frame;
    int old_valence;

    tcr->last_lisp_frame = xpGPR(context,Rsp);
    old_valence = prepare_to_wait_for_exception_lock(tcr, context);
#endif

  if (tcr->flags & (1<<TCR_FLAG_BIT_PENDING_SUSPEND)) {
    CLR_TCR_FLAG(tcr, TCR_FLAG_BIT_PENDING_SUSPEND);
    pthread_kill(pthread_self(), thread_suspend_signal);
  }

  
  wait_for_exception_lock_in_handler(tcr, context, &xframe_link);
  if ((!handle_exception(signum, context, tcr, info, old_valence))) {
    char msg[512];
    snprintf(msg, sizeof(msg), "Unhandled exception %d at 0x%lx, context->regs at #x%lx", signum, xpPC(context), (natural)xpGPRvector(context));
    if (lisp_Debugger(context, info, signum, (old_valence != TCR_STATE_LISP), msg)) {
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
  exit_signal_handler(tcr, old_valence, old_last_lisp_frame);
  raise_pending_interrupt(tcr);
}


void
sigill_handler(int signum, siginfo_t *info, ExceptionInformation  *xp)
{
  pc program_counter = xpPC(xp);
  opcode instr = *program_counter;

  if (IS_UUO(instr)) {
    natural psr = xpPSR(xp);
    Boolean opcode_matched_condition = false,
      flip = ((instr & (1<<28)) != 0);
   

    switch (instr >> 29) {
    case 0: 
      opcode_matched_condition = ((psr & PSR_Z_MASK) != 0);
      break;
    case 1:
      opcode_matched_condition = ((psr & PSR_C_MASK) != 0);
      break;
    case 2:
      opcode_matched_condition = ((psr & PSR_N_MASK) != 0);
      break;
    case 3:
      opcode_matched_condition = ((psr & PSR_V_MASK) != 0);
      break;
    case 4:
      opcode_matched_condition = (((psr & PSR_C_MASK) != 0) &&
                                  ((psr & PSR_Z_MASK) == 0));
      break;
    case 5:
      opcode_matched_condition = (((psr & PSR_N_MASK) != 0) ==
                                  ((psr & PSR_V_MASK) != 0));
      break;
    case 6:
      opcode_matched_condition = ((((psr & PSR_N_MASK) != 0) ==
                                   ((psr & PSR_V_MASK) != 0)) &&
                                  ((psr & PSR_Z_MASK) == 0));
      break;
    case 7:
      opcode_matched_condition = true;
      flip = false;
      break;
    }
    if (flip) {
      opcode_matched_condition = !opcode_matched_condition;
    }
    if (!opcode_matched_condition) {
      adjust_exception_pc(xp,4);
      return;
    }
  }
  signal_handler(signum,info,xp);
}


#ifdef USE_SIGALTSTACK
void
invoke_handler_on_main_stack(int signo, siginfo_t *info, ExceptionInformation *xp, void *return_address, void *handler)
{
  ExceptionInformation *xp_copy;
  siginfo_t *info_copy;
  extern void call_handler_on_main_stack(int, siginfo_t *, ExceptionInformation *,void *, void *);
  
  BytePtr target_sp= (BytePtr)xpGPR(xp,Rsp);
  target_sp -= sizeof(ucontext_t);
  xp_copy = (ExceptionInformation *)target_sp;
  memmove(target_sp,xp,sizeof(*xp));
  xp_copy->uc_stack.ss_sp = 0;
  xp_copy->uc_stack.ss_size = 0;
  xp_copy->uc_stack.ss_flags = 0;
  xp_copy->uc_link = NULL;
  target_sp -= sizeof(siginfo_t);
  info_copy = (siginfo_t *)target_sp;
  memmove(target_sp,info,sizeof(*info));
  call_handler_on_main_stack(signo, info_copy, xp_copy, return_address, handler);
}
  
void
altstack_signal_handler(int signo, siginfo_t *info, ExceptionInformation *xp)
{
  TCR *tcr=get_tcr(true);
  
  if (signo == SIGBUS) {
    BytePtr addr = (BytePtr)(xp->uc_mcontext.fault_address); 
    area *a = tcr->cs_area;
    if (((BytePtr)truncate_to_power_of_2(addr,log2_page_size))== a->softlimit) 
{
      if (mmap(a->softlimit,
               page_size,
               PROT_READ|PROT_WRITE|PROT_EXEC,
               MAP_PRIVATE|MAP_ANON|MAP_FIXED,
               -1,
               0) == a->softlimit) {
        return;
      }
    }
  } else if (signo == SIGSEGV) {
    BytePtr addr = (BytePtr)(xp->uc_mcontext.fault_address);
    area *a = tcr->cs_area;
    
    if ((addr >= a->low) &&
        (addr < a->softlimit)) {
      if (addr < a->hardlimit) {
        Bug(xp, "hard stack overflow");
      } else {
        UnProtectMemory(a->hardlimit,a->softlimit-a->hardlimit);
      }
    }
  }
  invoke_handler_on_main_stack(signo, info, xp, __builtin_return_address(0), signal_handler);
}
#endif

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
  egc_set_hash_key,
  egc_gvset,
  egc_rplaca,
  egc_rplacd,
  egc_set_hash_key_conditional,
  egc_set_hash_key_conditional_test,
  swap_lr_lisp_frame_temp0,
  swap_lr_lisp_frame_arg_z;


extern opcode ffcall_return_window, ffcall_return_window_end;

void
pc_luser_xp(ExceptionInformation *xp, TCR *tcr, signed_natural *alloc_disp)
{
  pc program_counter = xpPC(xp);
  opcode instr = *program_counter;
  lisp_frame *frame = (lisp_frame *)ptr_from_lispobj(xpGPR(xp,Rsp));
  LispObj cur_allocptr = xpGPR(xp, allocptr);
  int allocptr_tag = fulltag_of(cur_allocptr);
  


  if ((program_counter < &egc_write_barrier_end) && 
      (program_counter >= &egc_write_barrier_start)) {
    LispObj *ea = 0, val = 0, root = 0;
    bitvector refbits = (bitvector)(lisp_global(REFBITS));
    Boolean need_store = true, need_check_memo = true, need_memoize_root = false;

    if (program_counter >= &egc_set_hash_key_conditional) {
      if ((program_counter < &egc_set_hash_key_conditional_test) ||
	  ((program_counter == &egc_set_hash_key_conditional_test) &&
	   (! (xpPSR(xp) & PSR_Z_MASK)))) {
	return;
      }
      need_store = false;
      root = xpGPR(xp,arg_x);
      ea = (LispObj *) (root+xpGPR(xp,arg_y)+misc_data_offset);
      need_memoize_root = true;
    } else if (program_counter >= &egc_store_node_conditional) {
      if ((program_counter < &egc_store_node_conditional_test) ||
	  ((program_counter == &egc_store_node_conditional_test) &&
	   (! (xpPSR(xp) & PSR_Z_MASK)))) {
	/* The conditional store either hasn't been attempted yet, or
	   has failed.  No need to adjust the PC, or do memoization. */
	return;
      }
      ea = (LispObj*)(xpGPR(xp,arg_x) + xpGPR(xp,imm0));
      xpGPR(xp,arg_z) = t_value;
      need_store = false;
    } else if (program_counter >= &egc_set_hash_key) {
      root = xpGPR(xp,arg_x);
      val = xpGPR(xp,arg_z);
      ea = (LispObj *) (root+xpGPR(xp,arg_y)+misc_data_offset);
      need_memoize_root = true;
    } else if (program_counter >= &egc_gvset) {
      ea = (LispObj *) (xpGPR(xp,arg_x)+xpGPR(xp,arg_y)+misc_data_offset);
      val = xpGPR(xp,arg_z);
    } else if (program_counter >= &egc_rplacd) {
      ea = (LispObj *) untag(xpGPR(xp,arg_y));
      val = xpGPR(xp,arg_z);
    } else {                      /* egc_rplaca */
      ea =  ((LispObj *) untag(xpGPR(xp,arg_y)))+1;
      val = xpGPR(xp,arg_z);
    }
    if (need_store) {
      *ea = val;
    }
    if (need_check_memo) {
      natural  bitnumber = area_dnode(ea, lisp_global(REF_BASE));
      if ((bitnumber < lisp_global(OLDSPACE_DNODE_COUNT)) &&
          ((LispObj)ea < val)) {
        atomic_set_bit(refbits, bitnumber);
        if (need_memoize_root) {
          bitnumber = area_dnode(root, lisp_global(REF_BASE));
          atomic_set_bit(refbits, bitnumber);
        }
      }
    }
    xpPC(xp) = xpLR(xp);
    return;
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
        xpGPR(xp,allocptr) -= disp;
        /* Leave the PC at the alloc trap.  When the interrupt
           handler returns, it'll decrement allocptr by disp
           and the trap may or may not be taken.
        */
      } else {
        Boolean ok = false;
        update_bytes_allocated(tcr, (void *) ptr_from_lispobj(cur_allocptr - disp));
        xpGPR(xp, allocptr) = VOID_ALLOCPTR + disp;
        instr = program_counter[-1];
        if (IS_BRANCH_AROUND_ALLOC_TRAP(instr)) {
          instr = program_counter[-2];
          if (IS_COMPARE_ALLOCPTR_TO_RM(instr)){
            xpGPR(xp,RM_field(instr)) = VOID_ALLOCPTR;
            ok = true;
          }
        }
        if (ok) {
          /* Clear the carry bit, so that the trap will be taken. */
          xpPSR(xp) &= ~PSR_C_MASK;
        } else {
          Bug(NULL, "unexpected instruction preceding alloc trap.");
        }
      }
    } else {
      /* we may be before or after the alloc trap.  If before, set
         allocptr to VOID_ALLOCPTR and back up to the start of the
         instruction sequence; if after, finish the allocation. */
      Boolean before_alloc_trap = false;

      if (IS_BRANCH_AROUND_ALLOC_TRAP(instr)) {
        before_alloc_trap = true;
        --program_counter;
        instr = *program_counter;
      }
      if (IS_COMPARE_ALLOCPTR_TO_RM(instr)) {
        before_alloc_trap = true;
        --program_counter;
        instr = *program_counter;
      }
      if (IS_LOAD_RD_FROM_ALLOCBASE(instr)) {
        before_alloc_trap = true;
        --program_counter;
        instr = *program_counter;
      }
      if (IS_SUB_HI_FROM_ALLOCPTR(instr)) {
        before_alloc_trap = true;
        --program_counter;
      }
      if (before_alloc_trap) {
        xpPC(xp) = program_counter;
        xpGPR(xp,allocptr) = VOID_ALLOCPTR;
      } else {
        /* If we're already past the alloc_trap, finish allocating
           the object. */
        if (allocptr_tag == fulltag_cons) {
          finish_allocating_cons(xp);
        } else {
          if (allocptr_tag == fulltag_misc) {
            finish_allocating_uvector(xp);
          } else {
            Bug(xp, "what's being allocated here ?");
          }
        }
        /* Whatever we finished allocating, reset allocptr/allocbase to
           VOID_ALLOCPTR */
        xpGPR(xp,allocptr) = VOID_ALLOCPTR;
      }
      return;
    }
    return;
  }
  {
    lisp_frame *swap_frame = NULL;
    pc base = &swap_lr_lisp_frame_temp0;
    
    if ((program_counter >base)             /* sic */
        && (program_counter < (base+3))) {
      swap_frame = (lisp_frame *)xpGPR(xp,temp0);
    } else {
      base = &swap_lr_lisp_frame_arg_z;
      if ((program_counter > base) && (program_counter < (base+3))) { 
        swap_frame = (lisp_frame *)xpGPR(xp,arg_z);
      }
    }
    if (swap_frame) {
      if (program_counter == (base+1)) {
        swap_frame->savelr = xpGPR(xp,Rlr);
      }
      xpGPR(xp,Rlr) = xpGPR(xp,imm0);
      xpPC(xp) = base+3;
      return;
    }
  }
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
          tcr->interrupt_pending = 1 << fixnumshift;
	} else {
	  xframe_list xframe_link;
	  int old_valence;
          signed_natural disp=0;
          natural old_last_lisp_frame = tcr->last_lisp_frame;
	  
          tcr->last_lisp_frame = xpGPR(context,Rsp);
	  pc_luser_xp(context, tcr, &disp);
	  old_valence = prepare_to_wait_for_exception_lock(tcr, context);
	  wait_for_exception_lock_in_handler(tcr, context, &xframe_link);
	  handle_exception(signum, context, tcr, info, old_valence);
          if (disp) {
            xpGPR(context,allocptr) -= disp;
          }
	  unlock_exception_lock_in_handler(tcr);
	  exit_signal_handler(tcr, old_valence, old_last_lisp_frame);
	}
      }
    }
  }
#ifdef DARWIN
    DarwinSigReturn(context);
#endif
}

#ifdef USE_SIGALTSTACK
void
altstack_interrupt_handler(int signum, siginfo_t *info, ExceptionInformation *context)
{
  invoke_handler_on_main_stack(signum, info, context, __builtin_return_address(0),interrupt_handler);
}
#endif


void
install_signal_handler(int signo, void *handler, unsigned flags)
{
  struct sigaction sa;
  int err;

  sigfillset(&sa.sa_mask);
  
  sa.sa_sigaction = (void *)handler;
  sigfillset(&sa.sa_mask);
  sa.sa_flags = SA_SIGINFO;

#ifdef ANDROID
  sa.sa_flags |= SA_NODEFER;
#endif
#ifdef USE_SIGALTSTACK
  if (flags & ON_ALTSTACK)
    sa.sa_flags |= SA_ONSTACK;
#endif
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
  install_signal_handler(SIGILL, (void *)sigill_handler, RESERVE_FOR_LISP);
  install_signal_handler(SIGSEGV, (void *)ALTSTACK(signal_handler),
                         RESERVE_FOR_LISP|ON_ALTSTACK);
  install_signal_handler(SIGBUS, (void *)ALTSTACK(signal_handler),
			   RESERVE_FOR_LISP|ON_ALTSTACK);
  install_signal_handler(SIGNAL_FOR_PROCESS_INTERRUPT,
			 (void *)interrupt_handler, RESERVE_FOR_LISP);
  signal(SIGPIPE, SIG_IGN);
}


#ifdef USE_SIGALTSTACK
void
setup_sigaltstack(area *a)
{
  stack_t stack;
#if 0
  stack.ss_sp = a->low;
  a->low += SIGSTKSZ*8;
#endif
  stack.ss_size = SIGSTKSZ*8;
  stack.ss_flags = 0;
  stack.ss_sp = mmap(NULL,stack.ss_size, PROT_READ|PROT_WRITE|PROT_EXEC,MAP_ANON|MAP_PRIVATE,-1,0);
  if (sigaltstack(&stack, NULL) != 0) {
    perror("sigaltstack");
    exit(-1);
  }
}
#endif

void
thread_kill_handler(int signum, siginfo_t info, ExceptionInformation *xp)
{
  TCR *tcr = get_tcr(false);
  area *a;
#ifndef ANDROID
  sigset_t mask;
  
  sigemptyset(&mask);
#else
  int mask[] = {0,0};
#endif

  if (tcr) {
    tcr->valence = TCR_STATE_FOREIGN;
    a = tcr->vs_area;
    if (a) {
      a->active = a->high;
    }
    a = tcr->cs_area;
    if (a) {
      a->active = a->high;
    }
  }
  
  pthread_sigmask(SIG_SETMASK,(sigset_t *)&mask,NULL);
  pthread_exit(NULL);
}

#ifdef USE_SIGALTSTACK
void
altstack_thread_kill_handler(int signo, siginfo_t *info, ExceptionInformation *xp)
{
  invoke_handler_on_main_stack(signo, info, xp, __builtin_return_address(0), thread_kill_handler);
}
#endif

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
  return true;
}



void
exception_init()
{
  install_pmcl_exception_handlers();
}





