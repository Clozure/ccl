

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
        lisp_global(OLDSPACE_DNODE_COUNT) = area_dnode(managed_static_area->active, managed_static_area->low);
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
  if (!use_mach_exception_handling) {
    exit_signal_handler(tcr, old_valence, old_last_lisp_frame);
    raise_pending_interrupt(tcr);
  }
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
#ifdef DARWIN
  extern Boolean use_mach_exception_handling;
#endif

  Boolean install_signal_handlers_for_exceptions =
#ifdef DARWIN
    !use_mach_exception_handling
#else
    true
#endif
    ;
  if (install_signal_handlers_for_exceptions) {
    install_signal_handler(SIGILL, (void *)sigill_handler, RESERVE_FOR_LISP);
    install_signal_handler(SIGSEGV, (void *)ALTSTACK(signal_handler),
			   RESERVE_FOR_LISP|ON_ALTSTACK);
    install_signal_handler(SIGBUS, (void *)ALTSTACK(signal_handler),
			   RESERVE_FOR_LISP|ON_ALTSTACK);

  }
  
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





#ifdef DARWIN


#define TCR_FROM_EXCEPTION_PORT(p) ((TCR *)((natural)p))
#define TCR_TO_EXCEPTION_PORT(tcr) ((mach_port_t)((natural)(tcr)))



#define LISP_EXCEPTIONS_HANDLED_MASK \
 (EXC_MASK_SOFTWARE | EXC_MASK_BAD_ACCESS | EXC_MASK_BAD_INSTRUCTION | EXC_MASK_ARITHMETIC)

/* (logcount LISP_EXCEPTIONS_HANDLED_MASK) */
#define NUM_LISP_EXCEPTIONS_HANDLED 4 

typedef struct {
  int foreign_exception_port_count;
  exception_mask_t         masks[NUM_LISP_EXCEPTIONS_HANDLED];
  mach_port_t              ports[NUM_LISP_EXCEPTIONS_HANDLED];
  exception_behavior_t behaviors[NUM_LISP_EXCEPTIONS_HANDLED];
  thread_state_flavor_t  flavors[NUM_LISP_EXCEPTIONS_HANDLED];
} MACH_foreign_exception_state;




/*
  Mach's exception mechanism works a little better than its signal
  mechanism (and, not incidentally, it gets along with GDB a lot
  better.

  Initially, we install an exception handler to handle each native
  thread's exceptions.  This process involves creating a distinguished
  thread which listens for kernel exception messages on a set of
  0 or more thread exception ports.  As threads are created, they're
  added to that port set; a thread's exception port is destroyed
  (and therefore removed from the port set) when the thread exits.

  A few exceptions can be handled directly in the handler thread;
  others require that we resume the user thread (and that the
  exception thread resumes listening for exceptions.)  The user
  thread might eventually want to return to the original context
  (possibly modified somewhat.)

  As it turns out, the simplest way to force the faulting user
  thread to handle its own exceptions is to do pretty much what
  signal() does: the exception handlng thread sets up a sigcontext
  on the user thread's stack and forces the user thread to resume
  execution as if a signal handler had been called with that
  context as an argument.  We can use a distinguished UUO at a
  distinguished address to do something like sigreturn(); that'll
  have the effect of resuming the user thread's execution in
  the (pseudo-) signal context.

  Since:
    a) we have miles of code in C and in Lisp that knows how to
    deal with Linux sigcontexts
    b) Linux sigcontexts contain a little more useful information
    (the DAR, DSISR, etc.) than their Darwin counterparts
    c) we have to create a sigcontext ourselves when calling out
    to the user thread: we aren't really generating a signal, just
    leveraging existing signal-handling code.

  we create a Linux sigcontext struct.

  Simple ?  Hopefully from the outside it is ...

  We want the process of passing a thread's own context to it to
  appear to be atomic: in particular, we don't want the GC to suspend
  a thread that's had an exception but has not yet had its user-level
  exception handler called, and we don't want the thread's exception
  context to be modified by a GC while the Mach handler thread is
  copying it around.  On Linux (and on Jaguar), we avoid this issue
  because (a) the kernel sets up the user-level signal handler and
  (b) the signal handler blocks signals (including the signal used
  by the GC to suspend threads) until tcr->xframe is set up.

  The GC and the Mach server thread therefore contend for the lock
  "mach_exception_lock".  The Mach server thread holds the lock
  when copying exception information between the kernel and the
  user thread; the GC holds this lock during most of its execution
  (delaying exception processing until it can be done without
  GC interference.)

*/


#define TRUNC_DOWN(a,b,c)  (((((natural)a)-(b))/(c)) * (c))

void
fatal_mach_error(char *format, ...);

#define MACH_CHECK_ERROR(context,x) if (x != KERN_SUCCESS) {fatal_mach_error("Mach error while %s : %d", context, x);}


void
restore_mach_thread_state(mach_port_t thread, ExceptionInformation *pseudosigcontext)
{
  kern_return_t kret;
  _STRUCT_MCONTEXT *mc = UC_MCONTEXT(pseudosigcontext);

  /* Set the thread's FP state from the pseudosigcontext */
  kret = thread_set_state(thread,
                          ARM_VFP_STATE,
                          (thread_state_t)&(mc->__fs),
                          ARM_VFP_STATE_COUNT);

  MACH_CHECK_ERROR("setting thread FP state", kret);

  /* The thread'll be as good as new ... */
  kret = thread_set_state(thread, 
                          MACHINE_THREAD_STATE,
                          (thread_state_t)&(mc->__ss),
                          MACHINE_THREAD_STATE_COUNT);
  MACH_CHECK_ERROR("setting thread state", kret);
}  

/* This code runs in the exception handling thread, in response
   to an attempt to execute the UU0 at "pseudo_sigreturn" (e.g.,
   in response to a call to pseudo_sigreturn() from the specified
   user thread.
   Find that context (the user thread's R3 points to it), then
   use that context to set the user thread's state.  When this
   function's caller returns, the Mach kernel will resume the
   user thread.
*/

kern_return_t
do_pseudo_sigreturn(mach_port_t thread, TCR *tcr)
{
  ExceptionInformation *xp;

#ifdef DEBUG_MACH_EXCEPTIONS
  fprintf(dbgout, "doing pseudo_sigreturn for 0x%x\n",tcr);
#endif
  tcr->last_lisp_frame = *((natural *)(tcr->last_lisp_frame));
  xp = tcr->pending_exception_context;
  if (xp) {
    tcr->pending_exception_context = NULL;
    tcr->valence = TCR_STATE_LISP;
    restore_mach_thread_state(thread, xp);
    raise_pending_interrupt(tcr);
  } else {
    Bug(NULL, "no xp here!\n");
  }
#ifdef DEBUG_MACH_EXCEPTIONS
  fprintf(dbgout, "did pseudo_sigreturn for 0x%x\n",tcr);
#endif
  return KERN_SUCCESS;
}  

ExceptionInformation *
create_thread_context_frame(mach_port_t thread, 
			    natural *new_stack_top)
{
  arm_thread_state_t ts;
  mach_msg_type_number_t thread_state_count;
  kern_return_t result;
  ExceptionInformation *pseudosigcontext;
  _STRUCT_MCONTEXT *mc;
  natural stackp, backlink;

  thread_state_count = MACHINE_THREAD_STATE_COUNT;
  result = thread_get_state(thread, 
                            ARM_THREAD_STATE,	/* GPRs, some SPRs  */
                            (thread_state_t)&ts,
                            &thread_state_count);
  
  if (result != KERN_SUCCESS) {
    get_tcr(true);
    Bug(NULL, "Exception thread can't obtain thread state, Mach result = %d", result);
  }
  stackp = ts.__sp;
  backlink = stackp;

  stackp -= sizeof(*pseudosigcontext);
  pseudosigcontext = (ExceptionInformation *) ptr_from_lispobj(stackp);

  stackp -= sizeof(*mc);
  mc = (_STRUCT_MCONTEXT *) ptr_from_lispobj(stackp);
  memmove(&(mc->__ss),&ts,sizeof(ts));

  thread_state_count = ARM_VFP_STATE_COUNT;
  thread_get_state(thread,
		   ARM_VFP_STATE,
		   (thread_state_t)&(mc->__fs),
		   &thread_state_count);


  thread_state_count = ARM_EXCEPTION_STATE_COUNT;
  thread_get_state(thread,
		   ARM_EXCEPTION_STATE,
		   (thread_state_t)&(mc->__es),
		   &thread_state_count);


  UC_MCONTEXT(pseudosigcontext) = mc;
  if (new_stack_top) {
    *new_stack_top = stackp;
  }
  return pseudosigcontext;
}

/*
  This code sets up the user thread so that it executes a "pseudo-signal
  handler" function when it resumes.  Create a linux sigcontext struct
  on the thread's stack and pass it as an argument to the pseudo-signal
  handler.

  Things are set up so that the handler "returns to" pseudo_sigreturn(),
  which will restore the thread's context.

  If the handler invokes code that throws (or otherwise never sigreturn()'s
  to the context), that's fine.

  Actually, check that: throw (and variants) may need to be careful and
  pop the tcr's xframe list until it's younger than any frame being
  entered.
*/

int
setup_signal_frame(mach_port_t thread,
		   void *handler_address,
		   int signum,
                   int code,
		   TCR *tcr)
{
  arm_thread_state_t ts;
  ExceptionInformation *pseudosigcontext;
  int old_valence = tcr->valence;
  natural stackp, *pstackp;

#ifdef DEBUG_MACH_EXCEPTIONS
  fprintf(dbgout,"Setting up exception handling for 0x%x\n", tcr);
#endif
  pseudosigcontext = create_thread_context_frame(thread, &stackp);
  pstackp = (natural *)stackp;
  *--pstackp = tcr->last_lisp_frame;
  stackp = (natural)pstackp;
  tcr->last_lisp_frame = stackp;
  pseudosigcontext->uc_onstack = 0;
  pseudosigcontext->uc_sigmask = (sigset_t) 0;
  pseudosigcontext->uc_mcsize = ARM_MCONTEXT_SIZE;
  tcr->pending_exception_context = pseudosigcontext;
  tcr->valence = TCR_STATE_EXCEPTION_WAIT;
  

  /* 
     It seems like we've created a  sigcontext on the thread's
     stack.  Set things up so that we call the handler (with appropriate
     args) when the thread's resumed.
  */

  ts.__pc = (natural) handler_address;
  ts.__sp = stackp;
  ts.__r[0] = signum;
  ts.__r[1] = (natural)pseudosigcontext;
  ts.__r[2] = (natural)tcr;
  ts.__r[3] = (natural)old_valence;
  ts.__lr = (natural)pseudo_sigreturn;
  ts.__cpsr = xpPSR(pseudosigcontext);


  thread_set_state(thread, 
		   MACHINE_THREAD_STATE,
		   (thread_state_t)&ts,
		   MACHINE_THREAD_STATE_COUNT);
#ifdef DEBUG_MACH_EXCEPTIONS
  fprintf(dbgout,"Set up exception context for 0x%x at 0x%x\n", tcr, tcr->pending_exception_context);
#endif
  return 0;
}


void
pseudo_signal_handler(int signum, ExceptionInformation *context, TCR *tcr, int old_valence)
{
  signal_handler(signum, NULL, context, tcr, old_valence, 0);
} 


int
thread_set_fp_exceptions_enabled(mach_port_t thread, Boolean enabled)
{
  /* Likely hopeless. */
  return 0;
}

/*
  This function runs in the exception handling thread.  It's
  called (by this precise name) from the library function "exc_server()"
  when the thread's exception ports are set up.  (exc_server() is called
  via mach_msg_server(), which is a function that waits for and dispatches
  on exception messages from the Mach kernel.)

  This checks to see if the exception was caused by a pseudo_sigreturn()
  UUO; if so, it arranges for the thread to have its state restored
  from the specified context.

  Otherwise, it tries to map the exception to a signal number and
  arranges that the thread run a "pseudo signal handler" to handle
  the exception.

  Some exceptions could and should be handled here directly.
*/

kern_return_t
catch_exception_raise(mach_port_t exception_port,
		      mach_port_t thread,
		      mach_port_t task, 
		      exception_type_t exception,
		      exception_data_t code_vector,
		      mach_msg_type_number_t code_count)
{
  int signum = 0, code = *code_vector;
  TCR *tcr = TCR_FROM_EXCEPTION_PORT(exception_port);
  kern_return_t kret;

#ifdef DEBUG_MACH_EXCEPTIONS
  fprintf(dbgout, "obtaining Mach exception lock in exception thread\n");
#endif

  if (tcr->flags & (1<<TCR_FLAG_BIT_PENDING_EXCEPTION)) {
    CLR_TCR_FLAG(tcr,TCR_FLAG_BIT_PENDING_EXCEPTION);
  } 
  /* On the ARM, code_vector[1] contains the undefined instruction
     in this case, not its address.  */
  if ((exception == EXC_BAD_INSTRUCTION) &&
      (code_vector[0] == EXC_ARM_UNDEFINED) &&
      (code_vector[1] == PSEUDO_SIGRETURN_UUO)) {
    kret = do_pseudo_sigreturn(thread, tcr);
  } else if (tcr->flags & (1<<TCR_FLAG_BIT_PROPAGATE_EXCEPTION)) {
    CLR_TCR_FLAG(tcr,TCR_FLAG_BIT_PROPAGATE_EXCEPTION);
    kret = 17;
  } else {
    switch (exception) {
    case EXC_BAD_ACCESS:
      signum = SIGSEGV;
      break;
        
    case EXC_BAD_INSTRUCTION:
      signum = SIGILL;
      break;
      
      break;
      
    case EXC_ARITHMETIC:
      signum = SIGFPE;
      break;

    default:
      break;
    }
    if (signum) {
      kret = setup_signal_frame(thread,
                                (void *)pseudo_signal_handler,
                                signum,
                                code,
                                tcr);
#if 0
      fprintf(dbgout, "Setup pseudosignal handling in 0x%x\n",tcr);
#endif

    } else {
      kret = 17;
    }
  }

  return kret;
}



typedef struct {
  mach_msg_header_t Head;
  /* start of the kernel processed data */
  mach_msg_body_t msgh_body;
  mach_msg_port_descriptor_t thread;
  mach_msg_port_descriptor_t task;
  /* end of the kernel processed data */
  NDR_record_t NDR;
  exception_type_t exception;
  mach_msg_type_number_t codeCnt;
  integer_t code[2];
  mach_msg_trailer_t trailer;
} exceptionRequest;


boolean_t
openmcl_exc_server(mach_msg_header_t *in, mach_msg_header_t *out)
{
  static NDR_record_t _NDR = {0};
  kern_return_t handled;
  mig_reply_error_t *reply = (mig_reply_error_t *) out;
  exceptionRequest *req = (exceptionRequest *) in;

  reply->NDR = _NDR;

  out->msgh_bits = in->msgh_bits & MACH_MSGH_BITS_REMOTE_MASK;
  out->msgh_remote_port = in->msgh_remote_port;
  out->msgh_size = sizeof(mach_msg_header_t)+(3 * sizeof(unsigned));
  out->msgh_local_port = MACH_PORT_NULL;
  out->msgh_id = in->msgh_id+100;

  /* Could handle other exception flavors in the range 2401-2403 */


  if (in->msgh_id != 2401) {
    reply->RetCode = MIG_BAD_ID;
    return FALSE;
  }
  handled = catch_exception_raise(req->Head.msgh_local_port,
                                  req->thread.name,
                                  req->task.name,
                                  req->exception,
                                  req->code,
                                  req->codeCnt);
  reply->RetCode = handled;
  return TRUE;
}

/*
  The initial function for an exception-handling thread.
*/

void *
exception_handler_proc(void *arg)
{
  extern boolean_t exc_server();
  mach_port_t p = TCR_TO_EXCEPTION_PORT(arg);

  mach_msg_server(openmcl_exc_server, 2048, p, 0);
  /* Should never return. */
  abort();
}



mach_port_t
mach_exception_port_set()
{
  static mach_port_t __exception_port_set = MACH_PORT_NULL;
  kern_return_t kret;  
  if (__exception_port_set == MACH_PORT_NULL) {
    kret = mach_port_allocate(mach_task_self(),
			      MACH_PORT_RIGHT_PORT_SET,
			      &__exception_port_set);
    MACH_CHECK_ERROR("allocating thread exception_ports",kret);
    create_system_thread(0,
                         NULL,
                         exception_handler_proc, 
                         (void *)((natural)__exception_port_set));
  }
  return __exception_port_set;
}

/*
  Setup a new thread to handle those exceptions specified by
  the mask "which".  This involves creating a special Mach
  message port, telling the Mach kernel to send exception
  messages for the calling thread to that port, and setting
  up a handler thread which listens for and responds to
  those messages.

*/

/*
  Establish the lisp thread's TCR as its exception port, and determine
  whether any other ports have been established by foreign code for
  exceptions that lisp cares about.

  If this happens at all, it should happen on return from foreign
  code and on entry to lisp code via a callback.

  This is a lot of trouble (and overhead) to support Java, or other
  embeddable systems that clobber their caller's thread exception ports.
  
*/
kern_return_t
tcr_establish_exception_port(TCR *tcr, mach_port_t thread)
{
  kern_return_t kret;
  MACH_foreign_exception_state *fxs = (MACH_foreign_exception_state *)tcr->native_thread_info;
  int i;
  unsigned n = NUM_LISP_EXCEPTIONS_HANDLED;
  mach_port_t lisp_port = TCR_TO_EXCEPTION_PORT(tcr), foreign_port;
  exception_mask_t mask = 0;

  kret = thread_swap_exception_ports(thread,
				     LISP_EXCEPTIONS_HANDLED_MASK,
				     lisp_port,
				     EXCEPTION_DEFAULT,
				     THREAD_STATE_NONE,
				     fxs->masks,
				     &n,
				     fxs->ports,
				     fxs->behaviors,
				     fxs->flavors);
  if (kret == KERN_SUCCESS) {
    fxs->foreign_exception_port_count = n;
    for (i = 0; i < n; i ++) {
      foreign_port = fxs->ports[i];

      if ((foreign_port != lisp_port) &&
	  (foreign_port != MACH_PORT_NULL)) {
	mask |= fxs->masks[i];
      }
    }
    tcr->foreign_exception_status = (int) mask;
  }
  return kret;
}

kern_return_t
tcr_establish_lisp_exception_port(TCR *tcr)
{
  return tcr_establish_exception_port(tcr, (mach_port_t)((natural)tcr->native_thread_id));
}

/*
  Do this when calling out to or returning from foreign code, if
  any conflicting foreign exception ports were established when we
  last entered lisp code.
*/
kern_return_t
restore_foreign_exception_ports(TCR *tcr)
{
  exception_mask_t m = (exception_mask_t) tcr->foreign_exception_status;
  
  if (m) {
    MACH_foreign_exception_state *fxs  = 
      (MACH_foreign_exception_state *) tcr->native_thread_info;
    int i, n = fxs->foreign_exception_port_count;
    exception_mask_t tm;

    for (i = 0; i < n; i++) {
      if ((tm = fxs->masks[i]) & m) {
	thread_set_exception_ports((mach_port_t)((natural)tcr->native_thread_id),
				   tm,
				   fxs->ports[i],
				   fxs->behaviors[i],
				   fxs->flavors[i]);
      }
    }
  }
}
				   

/*
  This assumes that a Mach port (to be used as the thread's exception port) whose
  "name" matches the TCR's 32-bit address has already been allocated.
*/

kern_return_t
setup_mach_exception_handling(TCR *tcr)
{
  mach_port_t 
    thread_exception_port = TCR_TO_EXCEPTION_PORT(tcr),
    task_self = mach_task_self();
  kern_return_t kret;

  kret = mach_port_insert_right(task_self,
				thread_exception_port,
				thread_exception_port,
				MACH_MSG_TYPE_MAKE_SEND);
  MACH_CHECK_ERROR("adding send right to exception_port",kret);

  kret = tcr_establish_exception_port(tcr, (mach_port_t)((natural) tcr->native_thread_id));
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
  void tcr_monitor_exception_handling(TCR*, Boolean);
  kern_return_t kret;
  MACH_foreign_exception_state *fxs = 
    calloc(1, sizeof(MACH_foreign_exception_state));
  
  tcr->native_thread_info = (void *) fxs;

  if ((kret = setup_mach_exception_handling(tcr))
      != KERN_SUCCESS) {
    fprintf(dbgout, "Couldn't setup exception handler - error = %d\n", kret);
    terminate_lisp();
  }
}

/*
  The tcr is the "name" of the corresponding thread's exception port.
  Destroying the port should remove it from all port sets of which it's
  a member (notably, the exception port set.)
*/
void
darwin_exception_cleanup(TCR *tcr)
{
  void *fxs = tcr->native_thread_info;
  extern Boolean use_mach_exception_handling;

  if (fxs) {
    tcr->native_thread_info = NULL;
    free(fxs);
  }
  if (use_mach_exception_handling) {
    mach_port_deallocate(mach_task_self(),TCR_TO_EXCEPTION_PORT(tcr));
    mach_port_destroy(mach_task_self(),TCR_TO_EXCEPTION_PORT(tcr));
  }
}


Boolean
suspend_mach_thread(mach_port_t mach_thread)
{
  kern_return_t status;
  Boolean aborted = false;
  
  do {
    aborted = false;
    status = thread_suspend(mach_thread);
    if (status == KERN_SUCCESS) {
      status = thread_abort_safely(mach_thread);
      if (status == KERN_SUCCESS) {
        aborted = true;
      } else {
        fprintf(dbgout, "abort failed on thread = 0x%x\n",mach_thread);
        thread_resume(mach_thread);
      }
    } else {
      return false;
    }
  } while (! aborted);
  return true;
}

/*
  Only do this if pthread_kill indicated that the pthread isn't
  listening to signals anymore, as can happen as soon as pthread_exit()
  is called on Darwin.  The thread could still call out to lisp as it
  is exiting, so we need another way to suspend it in this case.
*/
Boolean
mach_suspend_tcr(TCR *tcr)
{
  mach_port_t mach_thread = (mach_port_t)((natural)( tcr->native_thread_id));
  ExceptionInformation *pseudosigcontext;
  Boolean result = false;
  
  result = suspend_mach_thread(mach_thread);
  if (result) {
    pseudosigcontext = create_thread_context_frame(mach_thread, NULL);
    pseudosigcontext->uc_onstack = 0;
    pseudosigcontext->uc_sigmask = (sigset_t) 0;
    tcr->suspend_context = pseudosigcontext;
  }
  return result;
}

void
mach_resume_tcr(TCR *tcr)
{
  ExceptionInformation *xp;
  mach_port_t mach_thread = (mach_port_t)((natural)(tcr->native_thread_id));
  
  xp = tcr->suspend_context;
#ifdef DEBUG_MACH_EXCEPTIONS
  fprintf(dbgout, "resuming TCR 0x%x, pending_exception_context = 0x%x\n",
          tcr, tcr->pending_exception_context);
#endif
  tcr->suspend_context = NULL;
  restore_mach_thread_state(mach_thread, xp);
#ifdef DEBUG_MACH_EXCEPTIONS
  fprintf(dbgout, "restored state in TCR 0x%x, pending_exception_context = 0x%x\n",
          tcr, tcr->pending_exception_context);
#endif
  thread_resume(mach_thread);
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

void
pseudo_interrupt_handler(int signum, ExceptionInformation *context)
{
  interrupt_handler(signum, NULL, context);
}

int
mach_raise_thread_interrupt(TCR *target)
{
  mach_port_t mach_thread = (mach_port_t)((natural)(target->native_thread_id));
  kern_return_t kret;
  Boolean result = false;
  TCR *current = get_tcr(false);
  thread_basic_info_data_t info; 
  mach_msg_type_number_t info_count = THREAD_BASIC_INFO_COUNT;

  LOCK(lisp_global(TCR_AREA_LOCK), current);

  if (suspend_mach_thread(mach_thread)) {
    if (thread_info(mach_thread,
                    THREAD_BASIC_INFO,
                    (thread_info_t)&info,
                    &info_count) == KERN_SUCCESS) {
      if (info.suspend_count == 1) {
        if ((target->valence == TCR_STATE_LISP) &&
            (!target->unwinding) &&
            (TCR_INTERRUPT_LEVEL(target) >= 0)) {
          kret = setup_signal_frame(mach_thread,
                                    (void *)pseudo_interrupt_handler,
                                    SIGNAL_FOR_PROCESS_INTERRUPT,
                                    0,
                                    target);
          if (kret == KERN_SUCCESS) {
            result = true;
          }
        }
      }
    }
    if (! result) {
      target->interrupt_pending = 1 << fixnumshift;
    }
    thread_resume(mach_thread);
    
  }
  UNLOCK(lisp_global(TCR_AREA_LOCK), current);
  return 0;
}

#endif
