/*
   Copyright (C) 2005-2009 Clozure Associates
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
#include "x86-utils.h"
#include "threads.h"
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
#include <fpu_control.h>
#include <linux/prctl.h>
#endif
#ifdef DARWIN
#include <sysexits.h>
#include <dlfcn.h>
#include <mach/machine/vm_types.h>
#endif
#ifndef WINDOWS
#include <sys/syslog.h>
#endif
#ifdef WINDOWS
#include <windows.h>
#ifdef WIN_64
#include <winternl.h>
#include <ntstatus.h>
#endif
#ifndef EXCEPTION_WRITE_FAULT
#define EXCEPTION_WRITE_FAULT 1
#endif
#endif

/*
  We do all kinds of funky things to avoid handling a signal on the lisp
  stack.  One of those funky things involves using the  __builtin_return_address()
  intrinsic so that the real handler returns to the right place when it exits,
  even if it returns on a different stack.  Code at "the right place" is presumed
  to just do a sigrereturn, however the OS does that.

  Sadly, some JVMs (at least) do what they call "signal chaining": they install
  their own handlers for signals that we expect to handle, and call our handler
  when they realize that they don't know how to handle what we raise.  They
  don't observe sigaltstack, and they don't necessarily do call our handler
  tail-recursively (so our stack-switching code would cause our handler to
  return to the JVM's, running on the wrong stack.

  Try to work around this by setting up an "early" signal handler (before any
  of this JVM nonsense can take effect) and noting the address it'd return to.
*/

pc
real_sigreturn = (pc)0;

#define SIGRETURN_ADDRESS() (real_sigreturn ? real_sigreturn : __builtin_return_address(0))

#ifndef WINDOWS
#ifndef DARWIN
void
early_intn_handler(int signum, siginfo_t *info, ExceptionInformation *xp)
{
  real_sigreturn = (pc) __builtin_return_address(0);
  xpPC(xp) += 2;
}

#endif
#endif

void
do_intn()
{
  __asm volatile("int $0xcd");
}

#ifdef DARWIN
 typedef kern_return_t (*like_mach_port_get_context)(mach_port_t,mach_port_t,mach_vm_address_t *);

typedef kern_return_t (*like_mach_port_set_context)(mach_port_t,mach_port_t,mach_vm_address_t);

like_mach_port_get_context Pmach_port_get_context = NULL;
like_mach_port_set_context Pmach_port_set_context = NULL;

Boolean use_mach_port_context_functions = false;

void
darwin_early_exception_init()
{
  Pmach_port_get_context = dlsym(RTLD_DEFAULT, "mach_port_get_context");
  Pmach_port_set_context = dlsym(RTLD_DEFAULT, "mach_port_set_context");
  if ((Pmach_port_set_context != NULL) &&
      (Pmach_port_get_context != NULL)) {
    use_mach_port_context_functions = true;
  }
}
#endif

void
x86_early_exception_init()
{
#ifndef WINDOWS
#ifndef DARWIN
  struct sigaction action, oaction;

  action.sa_sigaction = (void *) early_intn_handler;
  sigfillset(&action.sa_mask);
  action.sa_flags = SA_SIGINFO;
  sigaction(SIGNUM_FOR_INTN_TRAP,&action,&oaction);
  
  do_intn();
  sigaction(SIGNUM_FOR_INTN_TRAP,&oaction, NULL);
#endif
#endif
}

int
page_size = 4096;

int
log2_page_size = 12;

Boolean
did_gc_notification_since_last_full_gc = false;


void
update_bytes_allocated(TCR* tcr, void *cur_allocptr)
{
  char *last = tcr->last_allocptr;
  char *current = cur_allocptr;
  u64_t *bytes_allocated = (u64_t *)&TCR_AUX(tcr)->bytes_allocated;

  if (last && (tcr->save_allocbase != ((void *)VOID_ALLOCPTR))) {
    *bytes_allocated += last - current;
  }
  tcr->last_allocptr = 0;
}



//  This doesn't GC; it returns true if it made enough room, false
//  otherwise.
//  If "extend" is true, it can try to extend the dynamic area to
//  satisfy the request.


Boolean
new_heap_segment(ExceptionInformation *xp, natural need, Boolean extend, TCR *tcr, Boolean *crossed_threshold)
{
  area *a;
  natural newlimit, oldlimit;
  natural log2_allocation_quantum = TCR_AUX(tcr)->log2_allocation_quantum;

  if (crossed_threshold) {
    *crossed_threshold = false;
  }

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
        extend_by = align_to_power_of_2(extend_by>>1,log2_allocation_quantum);
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
  tcr->save_allocptr = (void *)newlimit;
  xpGPR(xp,Iallocptr) = (LispObj) newlimit;
  tcr->save_allocbase = (void *) oldlimit;

  if (crossed_threshold && (!extend)) {
    if (((a->high - (BytePtr)newlimit) < lisp_heap_notify_threshold)&&
        ((a->high - (BytePtr)oldlimit) >= lisp_heap_notify_threshold)) {
      *crossed_threshold = true;
    }
  }
    

  return true;
}

Boolean
allocate_object(ExceptionInformation *xp,
                natural bytes_needed, 
                signed_natural disp_from_allocptr,
		TCR *tcr,
                Boolean *crossed_threshold)
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
  if (new_heap_segment(xp, bytes_needed, false, tcr, crossed_threshold)) {
    xpGPR(xp, Iallocptr) -= disp_from_allocptr;
    tcr->save_allocptr = (void *) (xpGPR(xp, Iallocptr));
    return true;
  }
  
  /* It doesn't make sense to try a full GC if the object
     we're trying to allocate is larger than everything
     allocated so far.
  */
  if ((lisp_global(HEAP_END)-lisp_global(HEAP_START)) > bytes_needed) {
    untenure_from_area(tenured_area); /* force a full GC */
    gc_from_xp(xp, 0L);
    did_gc_notification_since_last_full_gc = false;
  }
  
  /* Try again, growing the heap if necessary */
  if (new_heap_segment(xp, bytes_needed, true, tcr, NULL)) {
    xpGPR(xp, Iallocptr) -= disp_from_allocptr;
    tcr->save_allocptr = (void *) (xpGPR(xp, Iallocptr));
    return true;
  }
  
  return false;
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
  LispObj selector = xpGPR(xp,Iimm0);
#ifdef X8664
  LispObj arg = xpGPR(xp,Iimm1);
#else
  LispObj arg = xpMMXreg(xp,Imm0);
#endif
  area *a = active_dynamic_area;
  Boolean egc_was_enabled = (a->older != NULL);
  
  natural gc_previously_deferred = gc_deferred;

  switch (selector) {
  case GC_TRAP_FUNCTION_EGC_CONTROL:
    egc_control(arg != 0, a->active);
    xpGPR(xp,Iarg_z) = lisp_nil + (egc_was_enabled ? t_offset : 0);
    break;

  case GC_TRAP_FUNCTION_CONFIGURE_EGC:
#ifdef X8664
    a->threshold = unbox_fixnum(xpGPR(xp, Iarg_x));
#else
    a->threshold = unbox_fixnum(xpGPR(xp, Itemp0));
#endif
    g1_area->threshold = unbox_fixnum(xpGPR(xp, Iarg_y));
    g2_area->threshold = unbox_fixnum(xpGPR(xp, Iarg_z));
    xpGPR(xp,Iarg_z) = lisp_nil+t_offset;
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
    xpGPR(xp, Iimm0) = lisp_heap_gc_threshold;
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
    xpGPR(xp, Iimm0) = lisp_heap_gc_threshold;
    break;

  case GC_TRAP_FUNCTION_SET_GC_NOTIFICATION_THRESHOLD:
    if ((signed_natural)arg >= 0) {
      lisp_heap_notify_threshold = arg;
      did_gc_notification_since_last_full_gc = false;
    }
    /* fall through */

  case GC_TRAP_FUNCTION_GET_GC_NOTIFICATION_THRESHOLD:
    xpGPR(xp, Iimm0) = lisp_heap_notify_threshold;
    break;

  case GC_TRAP_FUNCTION_ENSURE_STATIC_CONSES:
    ensure_static_conses(xp, tcr, 32768);
    break;

  case GC_TRAP_FUNCTION_FLASH_FREEZE: /* Like freeze below, but no GC */
    untenure_from_area(tenured_area);
    gc_like_from_xp(xp,flash_freeze,0);
    a->active = (BytePtr) align_to_power_of_2(a->active, log2_page_size);
    tenured_area->static_dnodes = area_dnode(a->active, a->low);
    if (egc_was_enabled) {
      tenure_to_area(tenured_area);
    }
    xpGPR(xp, Iimm0) = tenured_area->static_dnodes << dnode_shift;
    break;

  default:
    update_bytes_allocated(tcr, (void *) tcr->save_allocptr);

    if (selector == GC_TRAP_FUNCTION_IMMEDIATE_GC) {
      if (!full_gc_deferred) {
        gc_from_xp(xp, 0L);
        did_gc_notification_since_last_full_gc = false;
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
    did_gc_notification_since_last_full_gc = false;
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
        purify_from_xp(xp, 1);
        lisp_global(OLDSPACE_DNODE_COUNT) = area_dnode(managed_static_area->active, managed_static_area->low);
        gc_from_xp(xp, 0L);
      }
      if (selector & GC_TRAP_FUNCTION_SAVE_APPLICATION) {
        OSErr err;
        extern OSErr save_application(int, Boolean);
        area *vsarea = tcr->vs_area;

#ifdef WINDOWS	
        arg = _open_osfhandle(arg,0);
#endif
        nrs_TOPLFUNC.vcell = *((LispObj *)(vsarea->high)-1);
        err = save_application((int)arg, egc_was_enabled);
        if (err == noErr) {
          _exit(0);
        }
        fatal_oserr(": save_application", err);
      }
      switch (selector) {
      case GC_TRAP_FUNCTION_FREEZE:
        a->active = (BytePtr) align_to_power_of_2(a->active, log2_page_size);
        tenured_area->static_dnodes = area_dnode(a->active, a->low);
        xpGPR(xp, Iimm0) = tenured_area->static_dnodes << dnode_shift;
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
  return true;
}

  



void
push_on_lisp_stack(ExceptionInformation *xp, LispObj value)
{
  LispObj *vsp = (LispObj *)xpGPR(xp,Isp);
  *--vsp = value;
  xpGPR(xp,Isp) = (LispObj)vsp;
}


/* Hard to know if or whether this is necessary in general.  For now,
   do it when we get a "wrong number of arguments" trap.
*/
void
finish_function_entry(ExceptionInformation *xp)
{
  natural nargs = xpGPR(xp,Inargs)>>fixnumshift;
  signed_natural disp = nargs - nargregs;
  LispObj *vsp =  (LispObj *) xpGPR(xp,Isp), ra = *vsp++;
   
  xpGPR(xp,Isp) = (LispObj) vsp;

  if (disp > 0) {               /* implies that nargs > nargregs */
    vsp[disp] = xpGPR(xp,Ifp);
    vsp[disp+1] = ra;
    xpGPR(xp,Ifp) = (LispObj)(vsp+disp);
#ifdef X8664
    push_on_lisp_stack(xp,xpGPR(xp,Iarg_x));
#endif
    push_on_lisp_stack(xp,xpGPR(xp,Iarg_y));
    push_on_lisp_stack(xp,xpGPR(xp,Iarg_z));
  } else {
    push_on_lisp_stack(xp,ra);
    push_on_lisp_stack(xp,xpGPR(xp,Ifp));
    xpGPR(xp,Ifp) = xpGPR(xp,Isp);
#ifdef X8664
    if (nargs == 3) {
      push_on_lisp_stack(xp,xpGPR(xp,Iarg_x));
    }
#endif
    if (nargs >= 2) {
      push_on_lisp_stack(xp,xpGPR(xp,Iarg_y));
    }
    if (nargs >= 1) {
      push_on_lisp_stack(xp,xpGPR(xp,Iarg_z));
    }
  }
}

Boolean
object_contains_pc(LispObj container, LispObj addr)
{
  if (fulltag_of(container) >= fulltag_misc) {
    natural elements = header_element_count(header_of(container));
    if ((addr >= container) &&
        (addr < ((LispObj)&(deref(container,1+elements))))) {
      return true;
    }
  }
  return false;
}

LispObj
create_exception_callback_frame(ExceptionInformation *xp, TCR *tcr)
{
  LispObj containing_uvector = 0, 
    relative_pc = lisp_nil,
    nominal_function = lisp_nil, 
    f, tra, tra_f = 0, abs_pc;
  LispObj pc_low, pc_high;

  f = xpGPR(xp,Ifn);
  tra = *(LispObj*)(xpGPR(xp,Isp));
  if (tra_p(tra)) {
    char *p = (char *)tra;
    extern char *spentry_start, *spentry_end;

    if (ptr_in_area(p, tcr->ts_area) ||
	(p > spentry_start && p < spentry_end) ||
	in_any_consing_area(tra))
      tra_f = tra_function(tra);
    else
      Bug(xp, "martian tra %p\n", tra);
  }
  abs_pc = (LispObj)xpPC(xp);
#if WORD_SIZE == 64
  pc_high = ((abs_pc >> 32) & 0xffffffff) << fixnumshift;
  pc_low = (abs_pc & 0xffffffff) << fixnumshift;
#else
  pc_high = ((abs_pc >> 16) & 0xffff) << fixnumshift;
  pc_low = (abs_pc & 0xffff) << fixnumshift;
#endif


  if (functionp(f))
    nominal_function = f;
  else if (tra_f)
    nominal_function = tra_f;
  
  f = xpGPR(xp,Ifn);
  if (object_contains_pc(f, abs_pc)) {
    containing_uvector = untag(f)+fulltag_misc;
  } else {
    f = xpGPR(xp,Ixfn);
    if (object_contains_pc(f, abs_pc)) {
      containing_uvector = untag(f)+fulltag_misc;
    } else {
      if (tra_f) {
        f = tra_f;
        if (object_contains_pc(f, abs_pc)) {
          containing_uvector = untag(f)+fulltag_misc;
          relative_pc = (abs_pc - f) << fixnumshift;
        }
      }
    }
  }
  if (containing_uvector) {
    relative_pc = (abs_pc - (LispObj)&(deref(containing_uvector,1))) << fixnumshift;
  } else {
    containing_uvector = lisp_nil;
  }
  push_on_lisp_stack(xp, pc_high);
  push_on_lisp_stack(xp, pc_low);
  push_on_lisp_stack(xp,(LispObj)(tcr->xframe->prev));
  push_on_lisp_stack(xp,(LispObj)(tcr->foreign_sp));
  push_on_lisp_stack(xp,tra);
  push_on_lisp_stack(xp,(LispObj)xp);
  push_on_lisp_stack(xp,containing_uvector); 
  push_on_lisp_stack(xp,relative_pc);
  push_on_lisp_stack(xp,nominal_function);
  push_on_lisp_stack(xp,0);
  push_on_lisp_stack(xp,xpGPR(xp,Ifp));
  xpGPR(xp,Ifp) = xpGPR(xp,Isp);
  return xpGPR(xp,Isp);
}

#ifndef XMEMFULL
#define XMEMFULL (76)
#endif

void
lisp_allocation_failure(ExceptionInformation *xp, TCR *tcr, natural bytes_needed )
{
  LispObj xcf = create_exception_callback_frame(xp, tcr),
    cmain = nrs_CMAIN.vcell;
  int skip;
    
  tcr->save_allocptr = tcr->save_allocbase = (void *)VOID_ALLOCPTR;
  xpGPR(xp,Iallocptr) = VOID_ALLOCPTR;

  skip = callback_to_lisp(tcr, cmain, xp, xcf, -1, XMEMFULL, 0, 0);
  xpPC(xp) += skip;
}

#ifndef SIGTRAP
#define SIGTRAP 5
#endif

void
callback_for_gc_notification(ExceptionInformation *xp, TCR *tcr)
{
  LispObj cmain = nrs_CMAIN.vcell;
  if ((fulltag_of(cmain) == fulltag_misc) &&
      (header_subtag(header_of(cmain)) == subtag_macptr)) {
    LispObj *save_vsp = (LispObj *)xpGPR(xp,Isp),
      word_beyond_vsp = save_vsp[-1],
      save_fp = xpGPR(xp,Ifp),
      xcf = create_exception_callback_frame(xp, tcr);

    callback_to_lisp(tcr, cmain, xp, xcf, SIGTRAP, 0, 0, 0);
    did_gc_notification_since_last_full_gc = true;
    xpGPR(xp,Ifp) = save_fp;
    xpGPR(xp,Isp) = (LispObj)save_vsp;
    save_vsp[-1] = word_beyond_vsp;
  }
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
    nconses = (unbox_fixnum(xpGPR(xp,Iarg_z))),
    bytes_needed = (nconses << dnode_shift);
  LispObj
    prev = lisp_nil,
    current,
    initial = xpGPR(xp,Iarg_y);
  Boolean notify_pending_gc = false;

  if (nconses == 0) {
    /* Silly case */
    xpGPR(xp,Iarg_z) = lisp_nil;
    xpGPR(xp,Iallocptr) = lisp_nil;
    return true;
  }
  update_bytes_allocated(tcr, (void *)tcr->save_allocptr);
  if (allocate_object(xp,bytes_needed,bytes_needed-fulltag_cons,tcr, &notify_pending_gc)) {
    tcr->save_allocptr -= fulltag_cons;
    for (current = xpGPR(xp,Iallocptr);
         nconses;
         prev = current, current+= dnode_size, nconses--) {
      deref(current,0) = prev;
      deref(current,1) = initial;
    }
    xpGPR(xp,Iarg_z) = prev;
    if (notify_pending_gc && !did_gc_notification_since_last_full_gc) {
      callback_for_gc_notification(xp,tcr);
    }
  } else {
    lisp_allocation_failure(xp,tcr,bytes_needed);
  }
  return true;
}

Boolean
handle_alloc_trap(ExceptionInformation *xp, TCR *tcr, Boolean *notify)
{
  natural cur_allocptr, bytes_needed;
  unsigned allocptr_tag;
  signed_natural disp;
  
  cur_allocptr = xpGPR(xp,Iallocptr);
  allocptr_tag = fulltag_of(cur_allocptr);
  if (allocptr_tag == fulltag_misc) {
#ifdef X8664
    disp = xpGPR(xp,Iimm1);
#else
    disp = xpGPR(xp,Iimm0);
#endif
  } else {
    disp = dnode_size-fulltag_cons;
  }
  bytes_needed = disp+allocptr_tag;

  update_bytes_allocated(tcr,((BytePtr)(cur_allocptr+disp)));
  if (allocate_object(xp, bytes_needed, disp, tcr, notify)) {
    if (notify && *notify) {
      xpPC(xp)+=2;
      /* Finish the allocation: add a header if necessary,
         clear the tag bits in tcr.save_allocptr. */
      pc_luser_xp(xp,tcr,NULL);
      callback_for_gc_notification(xp,tcr);
    }
    return true;
  }
  
  lisp_allocation_failure(xp,tcr,bytes_needed);

  return true;
}

  
int
callback_to_lisp (TCR * tcr, LispObj callback_macptr, ExceptionInformation *xp,
                  natural arg1, natural arg2, natural arg3, natural arg4, natural arg5)
{
  natural  callback_ptr;
  int delta;
  unsigned old_mxcsr = get_mxcsr();
#ifdef X8632
  natural saved_node_regs_mask = tcr->node_regs_mask;
  natural saved_unboxed0 = tcr->unboxed0;
  natural saved_unboxed1 = tcr->unboxed1;
  LispObj *vsp = (LispObj *)xpGPR(xp, Isp);
#endif

  set_mxcsr(0x1f80);

  /* Put the active stack pointers where .SPcallback expects them */
#ifdef X8632
  tcr->node_regs_mask = X8632_DEFAULT_NODE_REGS_MASK;

  *--vsp = tcr->save0;
  *--vsp = tcr->save1;
  *--vsp = tcr->save2;
  *--vsp = tcr->save3;
  *--vsp = tcr->next_method_context;
  xpGPR(xp, Isp) = (LispObj)vsp;
#endif
  tcr->save_vsp = (LispObj *)xpGPR(xp, Isp);
  tcr->save_fp = (LispObj *)xpGPR(xp, Ifp);

  /* Call back.  The caller of this function may have modified stack/frame
     pointers (and at least should have called prepare_for_callback()).
  */
  callback_ptr = ((macptr *)ptr_from_lispobj(untag(callback_macptr)))->address;
  UNLOCK(lisp_global(EXCEPTION_LOCK), tcr);
  delta = ((int (*)())callback_ptr) (xp, arg1, arg2, arg3, arg4, arg5);
  LOCK(lisp_global(EXCEPTION_LOCK), tcr);

#ifdef X8632
  tcr->next_method_context = *vsp++;
  tcr->save3 = *vsp++;
  tcr->save2 = *vsp++;
  tcr->save1 = *vsp++;
  tcr->save0 = *vsp++;
  xpGPR(xp, Isp) = (LispObj)vsp;

  tcr->node_regs_mask = saved_node_regs_mask;
  tcr->unboxed0 = saved_unboxed0;
  tcr->unboxed1 = saved_unboxed1;
#endif
  set_mxcsr(old_mxcsr);
  return delta;
}

void
callback_for_interrupt(TCR *tcr, ExceptionInformation *xp)
{
  LispObj *save_vsp = (LispObj *)xpGPR(xp,Isp),
    word_beyond_vsp = save_vsp[-1],
    save_fp = xpGPR(xp,Ifp),
    xcf = create_exception_callback_frame(xp, tcr);
  int save_errno = errno;

  callback_to_lisp(tcr, nrs_CMAIN.vcell,xp, xcf, 0, 0, 0, 0);
  xpGPR(xp,Ifp) = save_fp;
  xpGPR(xp,Isp) = (LispObj)save_vsp;
  save_vsp[-1] = word_beyond_vsp;
  errno = save_errno;
}

Boolean
handle_error(TCR *tcr, ExceptionInformation *xp)
{
  pc program_counter = (pc)xpPC(xp);
  unsigned char op0 = program_counter[0], op1 = program_counter[1];
  LispObj rpc, errdisp = nrs_ERRDISP.vcell,
    save_vsp = xpGPR(xp,Isp), xcf0,
    save_fp = xpGPR(xp,Ifp);
  int skip;

  if ((fulltag_of(errdisp) == fulltag_misc) &&
      (header_subtag(header_of(errdisp)) == subtag_macptr)) {

    if ((op0 == 0xcd) && (op1 >= 0xc0) && (op1 <= 0xc2)) {
      finish_function_entry(xp);
    }
    xcf0 = create_exception_callback_frame(xp, tcr);
    skip = callback_to_lisp(tcr, errdisp, xp, xcf0, 0, 0, 0, 0);
    if (skip == -1) {
      xcf *xcf1 = (xcf *)xcf0;
      LispObj container = xcf1->containing_uvector;
      
      rpc = xcf1->relative_pc >> fixnumshift;
      if (container == lisp_nil) {
        xpPC(xp) = rpc;
      } else {
        xpPC(xp) = (LispObj)(&(deref(container,
#ifdef X8664
                                     1
#else
                                     0
#endif
)))+rpc;
      }
        
      skip = 0;
    }
    xpGPR(xp,Ifp) = save_fp;
    xpGPR(xp,Isp) = save_vsp;
    if ((op0 == 0xcd) && (op1 == 0xc7)) {
      /* Continue after an undefined function call. The function
         that had been undefined has already been called (in the
         break loop), and a list of the values that it returned
         in in the xp's %arg_z.  A function that returns those
         values in in the xp's %fn; we just have to adjust the
         stack (keeping the return address in the right place
         and discarding any stack args/reserved stack frame),
         then set nargs and the PC so that that function's
         called when we resume.
      */
      LispObj *vsp =(LispObj *)save_vsp, ra = *vsp;
      int nargs = xpGPR(xp, Inargs)>>fixnumshift;

#ifdef X8664
      if (nargs > 3) {
        xpGPR(xp,Isp)=(LispObj) (vsp + (1 + 2 + (nargs - 3)));
        push_on_lisp_stack(xp,ra);
      }
#else
      if (nargs > 2) {
        xpGPR(xp,Isp)=(LispObj) (vsp + (1 + 2 + (nargs - 2)));
        push_on_lisp_stack(xp,ra);
      }
#endif
      xpPC(xp) = xpGPR(xp,Ifn);
      xpGPR(xp,Inargs) = 1<<fixnumshift;
    } else {
      xpPC(xp) += skip;
    }
    return true;
  } else {
    return false;
  }
}


protection_handler
* protection_handlers[] = {
  do_spurious_wp_fault,
  do_soft_stack_overflow,
  do_soft_stack_overflow,
  do_soft_stack_overflow,
  do_hard_stack_overflow,    
  do_hard_stack_overflow,
  do_hard_stack_overflow,
};


/* Maybe this'll work someday.  We may have to do something to
   make the thread look like it's not handling an exception */
void
reset_lisp_process(ExceptionInformation *xp)
{
}

Boolean
do_hard_stack_overflow(ExceptionInformation *xp, protected_area_ptr area, BytePtr addr)
{
  /*  reset_lisp_process(xp); */
  Bug(xp, "Unrecoverable stack overflow.");
  return false;
}


Boolean
do_spurious_wp_fault(ExceptionInformation *xp, protected_area_ptr area, BytePtr addr)
{

  return false;
}

Boolean
do_soft_stack_overflow(ExceptionInformation *xp, protected_area_ptr prot_area, BytePtr addr)
{
  /* Trying to write into a guard page on the vstack or tstack.
     Allocate a new stack segment, emulate stwu and stwux for the TSP, and
     signal an error_stack_overflow condition.
      */
  lisp_protection_kind which = prot_area->why;
  Boolean on_TSP = (which == kTSPsoftguard);
  LispObj save_fp = xpGPR(xp,Ifp);
  LispObj save_vsp = xpGPR(xp,Isp), 
    xcf,
    cmain = nrs_CMAIN.vcell;
  area *a;
  protected_area_ptr soft;
  TCR *tcr = get_tcr(false);
  int skip;

  if ((fulltag_of(cmain) == fulltag_misc) &&
      (header_subtag(header_of(cmain)) == subtag_macptr)) {
    if (on_TSP) {
      a = tcr->ts_area;
    } else {
      a = tcr->vs_area;
    }
    soft = a->softprot;
    unprotect_area(soft);
    xcf = create_exception_callback_frame(xp, tcr);
    skip = callback_to_lisp(tcr, cmain, xp, xcf, SIGSEGV, on_TSP, 0, 0);
    xpGPR(xp,Ifp) = save_fp;
    xpGPR(xp,Isp) = save_vsp;
    xpPC(xp) += skip;
    return true;
  }
  return false;
}

Boolean
is_write_fault(ExceptionInformation *xp, siginfo_t *info)
{
#ifdef DARWIN
  return (UC_MCONTEXT(xp)->__es.__err & 0x2) != 0;
#endif
#if defined(LINUX) || defined(SOLARIS)
  return (xpGPR(xp,REG_ERR) & 0x2) != 0;
#endif
#ifdef FREEBSD
  return (xp->uc_mcontext.mc_err & 0x2) != 0;
#endif
#ifdef WINDOWS
  return (info->ExceptionFlags == EXCEPTION_WRITE_FAULT);
#endif
}

Boolean
handle_fault(TCR *tcr, ExceptionInformation *xp, siginfo_t *info, int old_valence)
{
#ifdef FREEBSD
#ifdef X8664
  BytePtr addr = (BytePtr) xp->uc_mcontext.mc_addr;
#else
  BytePtr addr = (BytePtr) info->si_addr;
#endif
#else
#ifdef WINDOWS
  BytePtr addr = (BytePtr) info->ExceptionInformation[1];
#else
  BytePtr addr = (BytePtr) info->si_addr;
#endif
#endif
  Boolean valid = IS_PAGE_FAULT(info,xp);

  if (tcr->safe_ref_address != NULL) {
    xpGPR(xp,Iimm0) = 0;
    xpPC(xp) = xpGPR(xp,Ira0);
    tcr->safe_ref_address = NULL;
    return true;
  }

  if (valid) {
    {
      protected_area *a = find_protected_area(addr);
      protection_handler *handler;
      
      if (a) {
        handler = protection_handlers[a->why];
        return handler(xp, a, addr);
      }
    }

    if ((addr >= readonly_area->low) &&
	(addr < readonly_area->active)) {
      UnProtectMemory((LogicalAddress)(truncate_to_power_of_2(addr,log2_page_size)),
		      page_size);
      return true;
    }

    {
      area *a = area_containing(addr);

      if (a && a->code == AREA_WATCHED && addr < a->high) {
	/* caught a write to a watched object */
	LispObj *p = (LispObj *)a->low;
	LispObj node = *p;
	unsigned tag_n = fulltag_of(node);
	LispObj cmain = nrs_CMAIN.vcell;
	LispObj obj;

	if (immheader_tag_p(tag_n) || nodeheader_tag_p(tag_n))
	  obj = (LispObj)p + fulltag_misc;
	else
	  obj = (LispObj)p + fulltag_cons;

	if ((fulltag_of(cmain) == fulltag_misc) &&
	    (header_subtag(header_of(cmain)) == subtag_macptr)) {
	  LispObj save_vsp = xpGPR(xp, Isp);
	  LispObj save_fp = xpGPR(xp, Ifp);
	  LispObj xcf;
	  natural offset = (LispObj)addr - obj;
	  int skip;

	  push_on_lisp_stack(xp, obj);
	  xcf = create_exception_callback_frame(xp, tcr);

	  /* The magic 2 means this was a write to a watchd object */
	  skip = callback_to_lisp(tcr, cmain, xp, xcf, SIGSEGV, 2,
				  (natural)addr, offset);
	  xpPC(xp) += skip;
	  xpGPR(xp, Ifp) = save_fp;
	  xpGPR(xp, Isp) = save_vsp;
	  return true;
	}
      }
    }
  }

  if (old_valence == TCR_STATE_LISP) {
    LispObj cmain = nrs_CMAIN.vcell,
      xcf;
    if ((fulltag_of(cmain) == fulltag_misc) &&
      (header_subtag(header_of(cmain)) == subtag_macptr)) {
      xcf = create_exception_callback_frame(xp, tcr);
      callback_to_lisp(tcr, cmain, xp, xcf, SIGBUS, valid ? is_write_fault(xp,info) : (natural)-1, valid ? (natural)addr : 0, 0);
    }
  }
  return false;
}

Boolean
handle_foreign_fpe(TCR *tcr, ExceptionInformation *xp, siginfo_t *info)
{
#ifdef X8632
  return false;
#else
  int code;

#ifdef WINDOWS
  if (info->ExceptionCode == EXCEPTION_INT_DIVIDE_BY_ZERO)
    return false;
#else
  if (info->si_code == FPE_INTDIV)
    return false;
#endif

  /*
   * Cooperate with .SPffcall to avoid saving and restoring the MXCSR
   * around every foreign call.
   */
    if (! (tcr->flags & (1<<TCR_FLAG_BIT_FOREIGN_FPE))) {
      tcr->flags |= (1<<TCR_FLAG_BIT_FOREIGN_FPE);
      tcr->lisp_mxcsr = xpMXCSR(xp) & ~MXCSR_STATUS_MASK;
    }
    xpMXCSR(xp) &= ~MXCSR_STATUS_MASK;
    xpMXCSR(xp) |= MXCSR_CONTROL_MASK;
    return true;
#endif
}

Boolean
handle_floating_point_exception(TCR *tcr, ExceptionInformation *xp, siginfo_t *info)
{
  int code,skip;
  LispObj  xcf, cmain = nrs_CMAIN.vcell,
    save_vsp = xpGPR(xp,Isp),
    save_fp = xpGPR(xp,Ifp);
#ifdef WINDOWS
  code = info->ExceptionCode;
#else
  code = info->si_code;
#endif  

  if ((fulltag_of(cmain) == fulltag_misc) &&
      (header_subtag(header_of(cmain)) == subtag_macptr)) {
    xcf = create_exception_callback_frame(xp, tcr);
    skip = callback_to_lisp(tcr, cmain, xp, xcf, SIGFPE, code, 0, 0);
    xpPC(xp) += skip;
    xpGPR(xp,Ifp) = save_fp;
    xpGPR(xp,Isp) = save_vsp;
    return true;
  } else {
    return false;
  }
}


Boolean
extend_tcr_tlb(TCR *tcr, ExceptionInformation *xp)
{
  LispObj index, old_limit = tcr->tlb_limit, new_limit, new_bytes;
  LispObj *old_tlb = tcr->tlb_pointer, *new_tlb, *work, *tos;

  tos = (LispObj*)(xpGPR(xp,Isp));
  index = *tos++;
  (xpGPR(xp,Isp))=(LispObj)tos;
  
  new_limit = align_to_power_of_2(index+1,12);
  new_bytes = new_limit-old_limit;
  new_tlb = realloc(old_tlb, new_limit);

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


#if defined(FREEBSD) || defined(DARWIN)
static
char mxcsr_bit_to_fpe_code[] = {
  FPE_FLTINV,                   /* ie */
  0,                            /* de */
  FPE_FLTDIV,                   /* ze */
  FPE_FLTOVF,                   /* oe */
  FPE_FLTUND,                   /* ue */
  FPE_FLTRES                    /* pe */
};

void
decode_vector_fp_exception(siginfo_t *info, uint32_t mxcsr)
{
  /* If the exception appears to be an XMM FP exception, try to
     determine what it was by looking at bits in the mxcsr.
  */
  int xbit, maskbit;
  
  for (xbit = 0, maskbit = MXCSR_IM_BIT; xbit < 6; xbit++, maskbit++) {
    if ((mxcsr & (1 << xbit)) &&
        !(mxcsr & (1 << maskbit))) {
      info->si_code = mxcsr_bit_to_fpe_code[xbit];
      return;
    }
  }
}

#ifdef FREEBSD
void
freebsd_decode_vector_fp_exception(siginfo_t *info, ExceptionInformation *xp)
{
  if (info->si_code == 0) {
#ifdef X8664
    struct savefpu *fpu = (struct savefpu *) &(xp->uc_mcontext.mc_fpstate);
#else
    struct ccl_savexmm *fpu = (struct ccl_savexmm *) &(xp->uc_mcontext.mc_fpstate);
#endif
    uint32_t mxcsr = fpu->sv_env.en_mxcsr;

    decode_vector_fp_exception(info, mxcsr);
  }
}
#endif

#ifdef DARWIN
void
darwin_decode_vector_fp_exception(siginfo_t *info, ExceptionInformation *xp)
{
  if (info->si_code == EXC_I386_SSEEXTERR) {
    uint32_t mxcsr = UC_MCONTEXT(xp)->__fs.__fpu_mxcsr;

    decode_vector_fp_exception(info, mxcsr);
  }
}

#endif

#endif

void
get_lisp_string(LispObj lisp_string, char *c_string, natural max)
{
  lisp_char_code *src = (lisp_char_code *)  (ptr_from_lispobj(lisp_string + misc_data_offset));
  natural i, n = header_element_count(header_of(lisp_string));

  if (n > max) {
    n = max;
  }

  for (i = 0; i < n; i++) {
    c_string[i] = 0xff & (src[i]);
  }
  c_string[n] = 0;
}

Boolean handle_watch_trap(ExceptionInformation *xp, TCR *tcr);

Boolean
handle_exception(int signum, siginfo_t *info, ExceptionInformation  *context, TCR *tcr, int old_valence)
{
  pc program_counter = (pc)xpPC(context);

  if (old_valence != TCR_STATE_LISP) {
    if (old_valence == TCR_STATE_FOREIGN && signum == SIGFPE) {
      return handle_foreign_fpe(tcr, context, info);
    } else {
      return false;
    }
  }

  switch (signum) {
  case SIGNUM_FOR_INTN_TRAP:
    if (IS_MAYBE_INT_TRAP(info,context)) {
      /* Something mapped to SIGSEGV/SIGBUS that has nothing to do with
	 a memory fault.  On x86, an "int n" instruction that's
         not otherwise implemented causes a "protecton fault".  Of
         course that has nothing to do with accessing protected
         memory; of course, most Unices act as if it did.*/
      if ((program_counter != NULL) &&
          (*program_counter == INTN_OPCODE)) {
        program_counter++;
        switch (*program_counter) {
        case UUO_ALLOC_TRAP:
          {
            Boolean did_notify = false,
              *notify_ptr = &did_notify;
            if (did_gc_notification_since_last_full_gc) {
              notify_ptr = NULL;
            }
            if (handle_alloc_trap(context, tcr, notify_ptr)) {
              if (! did_notify) {
                xpPC(context) += 2;	/* we might have GCed. */
              }
              return true;
            }
          }
          break;
        case UUO_GC_TRAP:
          if (handle_gc_trap(context, tcr)) {
            xpPC(context) += 2;
            return true;
          }
          break;
	case UUO_WATCH_TRAP:
	  /* add or remove watched object */
	  if (handle_watch_trap(context, tcr)) {
	    xpPC(context) += 2;
	    return true;
	  }
	  break;
        case UUO_DEBUG_TRAP:
          xpPC(context) = (natural) (program_counter+1);
          lisp_Debugger(context, info, debug_entry_dbg, false, "Lisp Breakpoint");
          return true;
            
        case UUO_DEBUG_TRAP_WITH_STRING:
          xpPC(context) = (natural) (program_counter+1);
          {
            char msg[512];

            get_lisp_string(xpGPR(context,Iarg_z),msg, sizeof(msg)-1);
            lisp_Debugger(context, info, debug_entry_dbg, false, msg);
          }
	  return true;
          
        default:
          return handle_error(tcr, context);
	}
      } else {
	return false;
      }

    } else {
      return handle_fault(tcr, context, info, old_valence);
    }
    break;

  case SIGNAL_FOR_PROCESS_INTERRUPT:
    tcr->interrupt_pending = 0;
    callback_for_interrupt(tcr, context);
    return true;
    break;


  case SIGILL:
    if ((program_counter[0] == XUUO_OPCODE_0) &&
	(program_counter[1] == XUUO_OPCODE_1)) {
      TCR *target = (TCR *)xpGPR(context, Iarg_z);

      switch (program_counter[2]) {
      case XUUO_TLB_TOO_SMALL:
        if (extend_tcr_tlb(tcr,context)) {
          xpPC(context)+=3;
          return true;
        }
	break;
	
      case XUUO_INTERRUPT_NOW:
	callback_for_interrupt(tcr,context);
	xpPC(context)+=3;
	return true;

      case XUUO_SUSPEND_NOW:
	xpPC(context)+=3;
	return true;

      case XUUO_INTERRUPT:
        raise_thread_interrupt(target);
	xpPC(context)+=3;
	return true;

      case XUUO_SUSPEND:
        xpGPR(context,Iimm0) = (LispObj) lisp_suspend_tcr(target);
	xpPC(context)+=3;
	return true;

      case XUUO_SUSPEND_ALL:
        lisp_suspend_other_threads();
	xpPC(context)+=3;
	return true;


      case XUUO_RESUME:
        xpGPR(context,Iimm0) = (LispObj) lisp_resume_tcr(target);
	xpPC(context)+=3;
	return true;
        
      case XUUO_RESUME_ALL:
        lisp_resume_other_threads();
	xpPC(context)+=3;
	return true;
	
      case XUUO_KILL:
        xpGPR(context,Iimm0) = (LispObj)kill_tcr(target);
        xpPC(context)+=3;
        return true;

      case XUUO_ALLOCATE_LIST:
        allocate_list(context,tcr);
        xpPC(context)+=3;
        return true;

      default:
	return false;
      }
    } else {
      return false;
    }
    break;
    
  case SIGFPE:
#ifdef FREEBSD
    /* As of 6.1, FreeBSD/AMD64 doesn't seem real comfortable
       with this newfangled XMM business (and therefore info->si_code
       is often 0 on an XMM FP exception.
       Try to figure out what really happened by decoding mxcsr
       bits.
    */
    freebsd_decode_vector_fp_exception(info,context);
#endif
#ifdef DARWIN
    /* Same general problem with Darwin as of 8.7.2 */
    darwin_decode_vector_fp_exception(info,context);
#endif

    return handle_floating_point_exception(tcr, context, info);

#if SIGBUS != SIGNUM_FOR_INTN_TRAP
  case SIGBUS:
    return handle_fault(tcr, context, info, old_valence);
#endif
    
#if SIGSEGV != SIGNUM_FOR_INTN_TRAP
  case SIGSEGV:
    return handle_fault(tcr, context, info, old_valence);
#endif    
    
  default:
    return false;
  }
  return false;
}


/* 
   Current thread has all signals masked.  Before unmasking them,
   make it appear that the current thread has been suspended.
   (This is to handle the case where another thread is trying
   to GC before this thread is able to seize the exception lock.)
*/
int
prepare_to_wait_for_exception_lock(TCR *tcr, ExceptionInformation *context)
{
  int old_valence = tcr->valence;

  tcr->pending_exception_context = context;
  tcr->valence = TCR_STATE_EXCEPTION_WAIT;

#ifdef WINDOWS
  if (tcr->flags & (1<<TCR_FLAG_BIT_PENDING_SUSPEND)) {
    CLR_TCR_FLAG(tcr, TCR_FLAG_BIT_PENDING_SUSPEND);
    SEM_RAISE(TCR_AUX(tcr)->suspend);
    SEM_WAIT_FOREVER(TCR_AUX(tcr)->resume);
  }
#else
  ALLOW_EXCEPTIONS(context);
#endif
  return old_valence;
}  

void
wait_for_exception_lock_in_handler(TCR *tcr, 
				   ExceptionInformation *context,
				   xframe_list *xf)
{

  LOCK(lisp_global(EXCEPTION_LOCK), tcr);
#if 0
  fprintf(dbgout, "0x" LISP " has exception lock\n", tcr);
#endif
  xf->curr = context;
#ifdef X8632
  xf->node_regs_mask = tcr->node_regs_mask;
#endif
  xf->prev = tcr->xframe;
  tcr->xframe =  xf;
  tcr->pending_exception_context = NULL;
  tcr->valence = TCR_STATE_FOREIGN; 
}

void
unlock_exception_lock_in_handler(TCR *tcr)
{
  tcr->pending_exception_context = tcr->xframe->curr;
#ifdef X8632
  tcr->node_regs_mask = tcr->xframe->node_regs_mask;
#endif
  tcr->xframe = tcr->xframe->prev;
  tcr->valence = TCR_STATE_EXCEPTION_RETURN;
  UNLOCK(lisp_global(EXCEPTION_LOCK),tcr);
#if 0
  fprintf(dbgout, "0x" LISP " released exception lock\n", tcr);
#endif
}

/* 
   If an interrupt is pending on exception exit, try to ensure
   that the thread sees it as soon as it's able to run.
*/
#ifdef WINDOWS
void
raise_pending_interrupt(TCR *tcr)
{
}
void
exit_signal_handler(TCR *tcr, int old_valence)
{
}
void
signal_handler(int signum, siginfo_t *info, ExceptionInformation  *context, TCR *tcr, int old_valence)
{
}
#else
void
raise_pending_interrupt(TCR *tcr)
{
  if ((TCR_INTERRUPT_LEVEL(tcr) >= 0) &&
      (tcr->interrupt_pending)) {
    pthread_kill((pthread_t)(tcr->osid), SIGNAL_FOR_PROCESS_INTERRUPT);
  }
}

void
exit_signal_handler(TCR *tcr, int old_valence)
{
  sigset_t mask;
  sigfillset(&mask);
#ifdef FREEBSD
  sigdelset(&mask,SIGTRAP);
#endif
  
  pthread_sigmask(SIG_SETMASK,&mask, NULL);
  tcr->valence = old_valence;
  tcr->pending_exception_context = NULL;
}

void
signal_handler(int signum, siginfo_t *info, ExceptionInformation  *context
#ifdef DARWIN
               , TCR *tcr, int old_valence
#endif
)
{
  xframe_list xframe_link;
#ifndef DARWIN
  TCR *tcr = get_tcr(false);

  int old_valence = prepare_to_wait_for_exception_lock(tcr, context);
#endif
  if (tcr->flags & (1<<TCR_FLAG_BIT_PENDING_SUSPEND)) {
    CLR_TCR_FLAG(tcr, TCR_FLAG_BIT_PENDING_SUSPEND);
    pthread_kill(pthread_self(), thread_suspend_signal);
  }
  wait_for_exception_lock_in_handler(tcr,context, &xframe_link);


  if (! handle_exception(signum, info, context, tcr, old_valence)) {
    char msg[512];
    Boolean foreign = (old_valence != TCR_STATE_LISP);

    snprintf(msg, sizeof(msg), "Unhandled exception %d at 0x" LISP ", context->regs at #x" LISP "", signum, xpPC(context), (natural)xpGPRvector(context));
    
    if (lisp_Debugger(context, info, signum,  foreign, msg)) {
      SET_TCR_FLAG(tcr,TCR_FLAG_BIT_PROPAGATE_EXCEPTION);
    }
  }
  unlock_exception_lock_in_handler(tcr);
#ifndef DARWIN_USE_PSEUDO_SIGRETURN
  exit_signal_handler(tcr, old_valence);
#endif
  /* raise_pending_interrupt(tcr); */
#ifndef DARWIN_USE_PSEUDO_SIGRETURN
  SIGRETURN(context);
#endif
}
#endif




#ifdef LINUX
/* type of pointer to saved fp state */
#ifdef X8664
typedef fpregset_t FPREGS;
#else
typedef struct _fpstate *FPREGS;
#endif
LispObj *
copy_fpregs(ExceptionInformation *xp, LispObj *current, FPREGS *destptr)
{
  FPREGS src = (FPREGS)(xp->uc_mcontext.fpregs), dest;
  
  if (src) {
    dest = ((FPREGS)current)-1;
    *dest = *src;
    *destptr = dest;
    current = (LispObj *) dest;
  }
  return current;
}
#endif

#ifdef DARWIN
LispObj *
copy_darwin_mcontext(MCONTEXT_T context, 
                     LispObj *current, 
                     MCONTEXT_T *out)
{
  MCONTEXT_T dest = ((MCONTEXT_T)current)-1;
  dest = (MCONTEXT_T) (((LispObj)dest) & ~15);

  *dest = *context;
  *out = dest;
  return (LispObj *)dest;
}
#endif

LispObj *
copy_siginfo(siginfo_t *info, LispObj *current)
{
  siginfo_t *dest = ((siginfo_t *)current) - 1;
#if !defined(LINUX) || !defined(X8632)
  dest = (siginfo_t *) (((LispObj)dest)&~15);
#endif
  *dest = *info;
  return (LispObj *)dest;
}

#ifdef LINUX
typedef FPREGS copy_ucontext_last_arg_t;
#else
typedef void * copy_ucontext_last_arg_t;
#endif

#ifndef WINDOWS
LispObj *
copy_ucontext(ExceptionInformation *context, LispObj *current, copy_ucontext_last_arg_t fp)
{
  ExceptionInformation *dest = ((ExceptionInformation *)current)-1;
#if !defined(LINUX) || !defined(X8632)
  dest = (ExceptionInformation *) (((LispObj)dest) & ~15);
#endif

  *dest = *context;
  /* Fix it up a little; where's the signal mask allocated, if indeed
     it is "allocated" ? */
#ifdef LINUX
  dest->uc_mcontext.fpregs = (fpregset_t)fp;
#endif
  dest->uc_stack.ss_sp = 0;
  dest->uc_stack.ss_size = 0;
  dest->uc_stack.ss_flags = 0;
  dest->uc_link = NULL;
  return (LispObj *)dest;
}
#endif


LispObj *
tcr_frame_ptr(TCR *tcr)
{
  ExceptionInformation *xp;
  LispObj *fp;

  if (tcr->pending_exception_context)
    xp = tcr->pending_exception_context;
  else if (tcr->valence == TCR_STATE_LISP) {
    xp = TCR_AUX(tcr)->suspend_context;
  } else {
    xp = NULL;
  }
  if (xp) {
    fp = (LispObj *)xpGPR(xp, Ifp);
  } else {
    fp = tcr->save_fp;
  }
  return fp;
}


LispObj *
find_foreign_rsp(LispObj rsp, area *foreign_area, TCR *tcr)
{

  if (((BytePtr)rsp < foreign_area->low) ||
      ((BytePtr)rsp > foreign_area->high)) {
    rsp = (LispObj)(tcr->foreign_sp);
  }
  return (LispObj *) (((rsp-128) & ~15));
}

#ifdef X8632
#ifdef LINUX
/* This is here for debugging.  On entry to a signal handler that
   receives info and context arguments, the stack should look exactly
   like this.  The "pretcode field" of the structure is the address
   of code that does an rt_sigreturn syscall, and rt_sigreturn expects
   %esp at the time of that syscall to be pointing just past the
   pretcode field.
   handle_signal_on_foreign_stack() and helpers have to be very
   careful to duplicate this "structure" exactly.
   Note that on x8664 Linux, rt_sigreturn expects a ucontext to
   be on top of the stack (with a siginfo_t underneath it.)
   It sort of half-works to do sigreturn via setcontext() on 
   x8632 Linux, but (a) it may not be available on some distributions
   and (b) even a relatively modern version of it uses "fldenv" to
   restore FP context, and "fldenv" isn't nearly good enough.
*/

struct rt_sigframe {
	char *pretcode;
	int sig;
	siginfo_t  *pinfo;
	void  *puc;
	siginfo_t info;
	struct ucontext uc;
	struct _fpstate fpstate;
	char retcode[8];
};
struct rt_sigframe *rtsf = 0;

#endif
#endif


#ifndef WINDOWS
/* x8632 Linux requires that the stack-allocated siginfo is nearer
   the top of stack than the stack-allocated ucontext.  If other
   platforms care, they expect the ucontext to be nearer the top
   of stack.
*/

#if defined(LINUX) && defined(X8632)
#define UCONTEXT_ON_TOP_OF_STACK 0
#else
#define UCONTEXT_ON_TOP_OF_STACK 1
#endif
void
handle_signal_on_foreign_stack(TCR *tcr,
                               void *handler, 
                               int signum, 
                               siginfo_t *info, 
                               ExceptionInformation *context,
                               LispObj return_address
                               )
{
#ifdef LINUX
  FPREGS fpregs = NULL;
#else
  void *fpregs = NULL;
#endif
#ifdef DARWIN
  MCONTEXT_T mcontextp = NULL;
#endif
  siginfo_t *info_copy = NULL;
  ExceptionInformation *xp = NULL;
  LispObj *foreign_rsp = find_foreign_rsp(xpGPR(context,Isp), tcr->cs_area, tcr);

#ifdef LINUX
  foreign_rsp = copy_fpregs(context, foreign_rsp, &fpregs);
#endif
#ifdef DARWIN
  foreign_rsp = copy_darwin_mcontext(UC_MCONTEXT(context), foreign_rsp, &mcontextp);
#endif
#if UCONTEXT_ON_TOP_OF_STACK
  /* copy info first */
  foreign_rsp = copy_siginfo(info, foreign_rsp);
  info_copy = (siginfo_t *)foreign_rsp;
  foreign_rsp = copy_ucontext(context, foreign_rsp, fpregs);
  xp = (ExceptionInformation *)foreign_rsp;
#else
  foreign_rsp = copy_ucontext(context, foreign_rsp, fpregs);
  xp = (ExceptionInformation *)foreign_rsp;
  foreign_rsp = copy_siginfo(info, foreign_rsp);
  info_copy = (siginfo_t *)foreign_rsp;
#endif
#ifdef DARWIN
  UC_MCONTEXT(xp) = mcontextp;
#endif
  *--foreign_rsp = return_address;
  switch_to_foreign_stack(foreign_rsp,handler,signum,info_copy,xp);
}
#endif


#ifndef WINDOWS
#ifndef USE_SIGALTSTACK
void
arbstack_signal_handler(int signum, siginfo_t *info, ExceptionInformation *context)
{
  TCR *tcr = get_interrupt_tcr(false);
#if 1
  if (tcr->valence != TCR_STATE_LISP) {
    lisp_Debugger(context, info, signum, true, "exception in foreign context");
  }
#endif
  {
    area *vs = tcr->vs_area;
    BytePtr current_sp = (BytePtr) current_stack_pointer();


    if ((current_sp >= vs->low) &&
        (current_sp < vs->high)) {
      handle_signal_on_foreign_stack(tcr,
                                     signal_handler,
                                     signum,
                                     info,
                                     context,
                                     (LispObj)__builtin_return_address(0)

                                     );
    } else {
      signal_handler(signum, info, context, tcr, 0);
    }
  }
}

#else
void
altstack_signal_handler(int signum, siginfo_t *info, ExceptionInformation  *context)
{
  TCR* tcr = get_tcr(true);
  Boolean do_stack_switch = false;
  stack_t ss;

#if WORD_SIZE==64
  if ((signum == SIGFPE) && (tcr->valence != TCR_STATE_LISP)) {
    if (handle_foreign_fpe(tcr,context,info)) {
      return;
    }
  }
#endif
     
  /* Because of signal chaining - and the possibility that libaries
     that use it ignore sigaltstack-related issues - we have to check
     to see if we're actually on the altstack.

     When OpenJDK VMs overwrite preinstalled signal handlers (that're
     there for a reason ...), they're also casual about SA_RESTART.
     We care about SA_RESTART (mostly) in the PROCESS-INTERRUPT case,
     and whether a JVM steals the signal used for PROCESS-INTERRUPT
     is platform-dependent.  On those platforms where the same signal
     is used, we should strongly consider trying to use another one.
  */
  sigaltstack(NULL, &ss);
  if (ss.ss_flags == SS_ONSTACK) {
    do_stack_switch = true;
  } else {
    area *vs = tcr->vs_area;
    BytePtr current_sp = (BytePtr) current_stack_pointer();

    if ((current_sp >= vs->low) &&
        (current_sp < vs->high)) {
      do_stack_switch = true;
    }
  }
  if (do_stack_switch) {
    handle_signal_on_foreign_stack(tcr,signal_handler,signum,info,context,(LispObj)SIGRETURN_ADDRESS());
  } else {
    signal_handler(signum,info,context);
  }
}
#endif
#endif

Boolean
stack_pointer_on_vstack_p(LispObj stack_pointer, TCR *tcr)
{
  area *a = tcr->vs_area;
 
  return (((BytePtr)stack_pointer <= a->high) &&
          ((BytePtr)stack_pointer > a->low));
}


#ifdef WINDOWS
extern DWORD restore_windows_context(ExceptionInformation *, TCR *, int);
#endif

void
interrupt_handler (int signum, siginfo_t *info, ExceptionInformation *context)
{
  TCR *tcr = get_interrupt_tcr(false);
  int old_valence = tcr->valence;

  if (tcr) {
    if ((TCR_INTERRUPT_LEVEL(tcr) < 0) ||
        (tcr->valence != TCR_STATE_LISP) ||
        (tcr->unwinding != 0) ||
        ! stack_pointer_on_vstack_p(xpGPR(context,Isp), tcr) ||
        ! stack_pointer_on_vstack_p(xpGPR(context,Ifp), tcr)) {
      tcr->interrupt_pending = (((natural) 1)<< (nbits_in_word - ((natural)1)));
    } else {
      LispObj cmain = nrs_CMAIN.vcell;

      if ((fulltag_of(cmain) == fulltag_misc) &&
	  (header_subtag(header_of(cmain)) == subtag_macptr)) {
	/* 
	   This thread can (allegedly) take an interrupt now. 
        */

        xframe_list xframe_link;
        signed_natural alloc_displacement = 0;
        LispObj 
          *next_tsp = tcr->next_tsp,
          *save_tsp = tcr->save_tsp,
          *p,
          q;
        natural old_foreign_exception = tcr->flags & (1 << TCR_FLAG_BIT_FOREIGN_EXCEPTION);

        tcr->flags &= ~(1 << TCR_FLAG_BIT_FOREIGN_EXCEPTION);
            
        if (next_tsp != save_tsp) {
          tcr->next_tsp = save_tsp;
        } else {
          next_tsp = NULL;
        }
        /* have to do this before allowing interrupts */
        pc_luser_xp(context, tcr, &alloc_displacement);
        old_valence = prepare_to_wait_for_exception_lock(tcr, context);
        wait_for_exception_lock_in_handler(tcr, context, &xframe_link);
        handle_exception(signum, info, context, tcr, old_valence);
        if (alloc_displacement) {
          tcr->save_allocptr -= alloc_displacement;
        }
        if (next_tsp) {
          tcr->next_tsp = next_tsp;
          p = next_tsp;
          while (p != save_tsp) {
            *p++ = 0;
          }
          q = (LispObj)save_tsp;
          *next_tsp = q;
        }
        tcr->flags |= old_foreign_exception;
        unlock_exception_lock_in_handler(tcr);
#ifndef WINDOWS
        exit_signal_handler(tcr, old_valence);
#endif
      }
    }
  }
#ifdef WINDOWS
  restore_windows_context(context,tcr,old_valence);
#else
  SIGRETURN(context);
#endif
}


#ifndef WINDOWS
#ifndef USE_SIGALTSTACK
void
arbstack_interrupt_handler (int signum, siginfo_t *info, ExceptionInformation *context)
{
  TCR *tcr = get_interrupt_tcr(false);
  area *vs = tcr->vs_area;
  BytePtr current_sp = (BytePtr) current_stack_pointer();

  if ((current_sp >= vs->low) &&
      (current_sp < vs->high)) {
    handle_signal_on_foreign_stack(tcr,
                                   interrupt_handler,
                                   signum,
                                   info,
                                   context,
                                   (LispObj)__builtin_return_address(0)
                                   );
  } else {
    /* If we're not on the value stack, we pretty much have to be on
       the C stack.  Just run the handler. */
    interrupt_handler(signum, info, context);
  }
}

#else /* altstack works */

/* 
   There aren't likely any JVM-related signal-chaining issues here, since
   on platforms where that could be an issue we use either an RT signal
   or an unused synchronous hardware signal to raise interrupts. 
*/
void
altstack_interrupt_handler (int signum, siginfo_t *info, ExceptionInformation *context)
{
  TCR *tcr = get_interrupt_tcr(false);
  handle_signal_on_foreign_stack(tcr,interrupt_handler,signum,info,context,(LispObj)__builtin_return_address(0)
                                 );
}

#endif
#endif

#ifndef WINDOWS
void
install_signal_handler(int signo, void *handler, unsigned flags)
{
  struct sigaction sa;
  int err;
  
  sa.sa_sigaction = (void *)handler;
  sigfillset(&sa.sa_mask);
#ifdef FREEBSD
  /* Strange FreeBSD behavior wrt synchronous signals */
  sigdelset(&sa.sa_mask,SIGTRAP);  /* let GDB work */
#endif
  sa.sa_flags = SA_SIGINFO;

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
#endif

#ifdef WINDOWS
BOOL 
CALLBACK ControlEventHandler(DWORD event)
{
  switch(event) {
  case CTRL_C_EVENT:
    lisp_global(INTFLAG) = (1 << fixnumshift);
    return TRUE;
    break;
  case CTRL_BREAK_EVENT:
    lisp_global(INTFLAG) = (2 << fixnumshift);
    return TRUE;
    break;
  default:
    return FALSE;
  }
}

static
DWORD mxcsr_bit_to_fpe_code[] = {
  EXCEPTION_FLT_INVALID_OPERATION, /* ie */
  0,                            /* de */
  EXCEPTION_FLT_DIVIDE_BY_ZERO, /* ze */
  EXCEPTION_FLT_OVERFLOW,       /* oe */
  EXCEPTION_FLT_UNDERFLOW,      /* ue */
  EXCEPTION_FLT_INEXACT_RESULT  /* pe */
};

#ifndef STATUS_FLOAT_MULTIPLE_FAULTS
#define STATUS_FLOAT_MULTIPLE_FAULTS 0xc00002b4
#endif

#ifndef STATUS_FLOAT_MULTIPLE_TRAPS
#define  STATUS_FLOAT_MULTIPLE_TRAPS 0xc00002b5
#endif

int
map_windows_exception_code_to_posix_signal(DWORD code, siginfo_t *info, ExceptionInformation *context)
{
  switch (code) {
#ifdef WIN_32
  case STATUS_FLOAT_MULTIPLE_FAULTS:
  case STATUS_FLOAT_MULTIPLE_TRAPS:
    {
      int xbit, maskbit;
      DWORD mxcsr = *(xpMXCSRptr(context));

      for (xbit = 0, maskbit = MXCSR_IM_BIT; xbit < 6; xbit++, maskbit++) {
        if ((mxcsr & (1 << xbit)) &&
            !(mxcsr & (1 << maskbit))) {
          info->ExceptionCode = mxcsr_bit_to_fpe_code[xbit];
          break;
        }
      }
    }
    return SIGFPE;
#endif
      
  case EXCEPTION_ACCESS_VIOLATION:
    return SIGSEGV;
  case EXCEPTION_FLT_DENORMAL_OPERAND:
  case EXCEPTION_FLT_DIVIDE_BY_ZERO:
  case EXCEPTION_FLT_INEXACT_RESULT:
  case EXCEPTION_FLT_INVALID_OPERATION:
  case EXCEPTION_FLT_OVERFLOW:
  case EXCEPTION_FLT_STACK_CHECK:
  case EXCEPTION_FLT_UNDERFLOW:
  case EXCEPTION_INT_DIVIDE_BY_ZERO:
  case EXCEPTION_INT_OVERFLOW:
    return SIGFPE;
  case EXCEPTION_PRIV_INSTRUCTION:
  case EXCEPTION_ILLEGAL_INSTRUCTION:
    return SIGILL;
  case EXCEPTION_IN_PAGE_ERROR:
  case STATUS_GUARD_PAGE_VIOLATION:
    return SIGBUS;
  default:
    return -1;
  }
}


LONG
windows_exception_handler(EXCEPTION_POINTERS *exception_pointers, TCR *tcr)
{
  DWORD code = exception_pointers->ExceptionRecord->ExceptionCode;
  int old_valence, signal_number;
  ExceptionInformation *context = exception_pointers->ContextRecord;
  siginfo_t *info = exception_pointers->ExceptionRecord;
  xframe_list xframes;

  old_valence = prepare_to_wait_for_exception_lock(tcr, context);
  wait_for_exception_lock_in_handler(tcr, context, &xframes);

  signal_number = map_windows_exception_code_to_posix_signal(code, info, context);
  
  if (!handle_exception(signal_number, info, context, tcr, old_valence)) {
    char msg[512];
    Boolean foreign = (old_valence != TCR_STATE_LISP);

    snprintf(msg, sizeof(msg), "Unhandled exception %d (windows code 0x%x) at 0x%Ix, context->regs at 0x%Ix", signal_number, code, xpPC(context), (natural)xpGPRvector(context));
    
    if (lisp_Debugger(context, info, signal_number,  foreign, msg)) {
      SET_TCR_FLAG(tcr,TCR_FLAG_BIT_PROPAGATE_EXCEPTION);
    }
  }
  unlock_exception_lock_in_handler(tcr);
  return restore_windows_context(context, tcr, old_valence);
}

void
setup_exception_handler_call(CONTEXT *context,
                             LispObj new_sp,
                             void *handler,
                             EXCEPTION_POINTERS *new_ep,
                             TCR *tcr)
{
  extern void windows_halt(void);
  LispObj *p = (LispObj *)new_sp;
#ifdef WIN_64
  p-=4;                         /* win64 abi argsave nonsense */
  *(--p) = (LispObj)windows_halt;
  context->Rsp = (DWORD64)p;
  context->Rip = (DWORD64)handler;
  context->Rcx = (DWORD64)new_ep;
  context->Rdx = (DWORD64)tcr;
#else
  p-=4;                          /* args on stack, stack aligned */
  p[0] = (LispObj)new_ep;
  p[1] = (LispObj)tcr;
  *(--p) = (LispObj)windows_halt;
  context->Esp = (DWORD)p;
  context->Eip = (DWORD)handler;
#endif
  context->EFlags &= ~0x400;  /* clear direction flag */
}

void
prepare_to_handle_windows_exception_on_foreign_stack(TCR *tcr,
                                                     CONTEXT *context,
                                                     void *handler,
                                                     EXCEPTION_POINTERS *original_ep)
{
  LispObj foreign_rsp = 
    (LispObj) (tcr->foreign_sp - 128) & ~15;
  CONTEXT *new_context;
  siginfo_t *new_info;
  EXCEPTION_POINTERS *new_ep;

  new_context = ((CONTEXT *)(foreign_rsp&~15))-1;
  *new_context = *context;
  foreign_rsp = (LispObj)new_context;
  new_info = ((siginfo_t *)(foreign_rsp&~15))-1;
  *new_info = *original_ep->ExceptionRecord;
  foreign_rsp = (LispObj)new_info;
  new_ep = ((EXCEPTION_POINTERS *)(foreign_rsp&~15))-1;
  foreign_rsp = (LispObj)new_ep & ~15;
  new_ep->ContextRecord = new_context;
  new_ep->ExceptionRecord = new_info;
  setup_exception_handler_call(context,foreign_rsp,handler,new_ep, tcr);
}

LONG CALLBACK
windows_arbstack_exception_handler(EXCEPTION_POINTERS *exception_pointers)
{
  extern void ensure_safe_for_string_operations(void);
  DWORD code = exception_pointers->ExceptionRecord->ExceptionCode;


  
  if ((code & 0x80000000L) == 0) {
    return EXCEPTION_CONTINUE_SEARCH;
  } else {
    TCR *tcr = get_interrupt_tcr(false);
    area *cs = TCR_AUX(tcr)->cs_area;
    BytePtr current_sp = (BytePtr) current_stack_pointer();
    CONTEXT *context = exception_pointers->ContextRecord;
    
    ensure_safe_for_string_operations();

    if ((current_sp >= cs->low) &&
        (current_sp < cs->high)) {
      debug_show_registers(context, exception_pointers->ExceptionRecord, 0);
      FBug(context, "Exception on foreign stack\n");
      return EXCEPTION_CONTINUE_EXECUTION;
    }

    prepare_to_handle_windows_exception_on_foreign_stack(tcr,
                                                         context,
                                                         windows_exception_handler,
                                                         exception_pointers);
    return EXCEPTION_CONTINUE_EXECUTION;
  }
}


void
install_pmcl_exception_handlers()
{
  AddVectoredExceptionHandler(1,windows_arbstack_exception_handler);
}
#else
void
install_pmcl_exception_handlers()
{
  void *handler, *interrupt_handler;

#ifdef USE_SIGALTSTACK
  handler = (void *)altstack_signal_handler;
  interrupt_handler = (void *)altstack_interrupt_handler;
#else
  handler = (void *)arbstack_signal_handler;
  interrupt_handler = (void *)arbstack_interrupt_handler;
#endif

#ifndef DARWIN
  install_signal_handler(SIGILL, handler, RESERVE_FOR_LISP|ON_ALTSTACK);
  install_signal_handler(SIGBUS, handler, RESERVE_FOR_LISP|ON_ALTSTACK);
  install_signal_handler(SIGSEGV, handler, RESERVE_FOR_LISP|ON_ALTSTACK);
  install_signal_handler(SIGFPE, handler, RESERVE_FOR_LISP|ON_ALTSTACK);
#endif
  
  install_signal_handler(SIGNAL_FOR_PROCESS_INTERRUPT, interrupt_handler,
			 RESERVE_FOR_LISP|ON_ALTSTACK);
  signal(SIGPIPE, SIG_IGN);
}
#endif

#ifndef WINDOWS
#ifndef USE_SIGALTSTACK
void
arbstack_suspend_resume_handler(int signum, siginfo_t *info, ExceptionInformation  *context)
{
  TCR *tcr = get_interrupt_tcr(false);
  if (tcr != NULL) {
    area *vs = tcr->vs_area;
    BytePtr current_sp = (BytePtr) current_stack_pointer();
    
    if ((current_sp >= vs->low) &&
        (current_sp < vs->high)) {
      return
        handle_signal_on_foreign_stack(tcr,
                                       suspend_resume_handler,
                                       signum,
                                       info,
                                       context,
                                       (LispObj)__builtin_return_address(0)
                                       );
    } else {
      /* If we're not on the value stack, we pretty much have to be on
         the C stack.  Just run the handler. */
    }
  }
  suspend_resume_handler(signum, info, context);
}


#else /* altstack works */

void
altstack_suspend_resume_handler(int signum, siginfo_t *info, ExceptionInformation  *context)
{
  TCR* tcr = get_tcr(true);
  handle_signal_on_foreign_stack(tcr,
                                 suspend_resume_handler,
                                 signum,
                                 info,
                                 context,
                                 (LispObj)__builtin_return_address(0)
                                 );
}
#endif
#endif


/* This should only be called when the tcr_area_lock is held */
void
empty_tcr_stacks(TCR *tcr)
{
  if (tcr) {
    area *a;

    tcr->valence = TCR_STATE_FOREIGN;
    a = tcr->vs_area;
    if (a) {
      a->active = a->high;
    }
    a = tcr->ts_area;
    if (a) {
      a->active = a->high;
    }
    a = TCR_AUX(tcr)->cs_area;
    if (a) {
      a->active = a->high;
    }
  }
}

#ifdef WINDOWS
void
thread_kill_handler(int signum, siginfo_t *info, ExceptionInformation *xp)
{
}
#else
void
thread_kill_handler(int signum, siginfo_t *info, ExceptionInformation *xp)
{
  TCR *tcr = get_tcr(false);
  sigset_t mask;

  sigemptyset(&mask);

  empty_tcr_stacks(tcr);

  pthread_sigmask(SIG_SETMASK,&mask,NULL);
  pthread_exit(NULL);
}
#endif

#ifndef WINDOWS
#ifndef USE_SIGALTSTACK
void
arbstack_thread_kill_handler(int signum, siginfo_t *info, ExceptionInformation *context)
{
  TCR *tcr = get_interrupt_tcr(false);
  area *vs = tcr->vs_area;
  BytePtr current_sp = (BytePtr) current_stack_pointer();

  if ((current_sp >= vs->low) &&
      (current_sp < vs->high)) {
    handle_signal_on_foreign_stack(tcr,
                                   thread_kill_handler,
                                   signum,
                                   info,
                                   context,
                                   (LispObj)__builtin_return_address(0)
                                   );
  } else {
    /* If we're not on the value stack, we pretty much have to be on
       the C stack.  Just run the handler. */
    thread_kill_handler(signum, info, context);
  }
}


#else
void
altstack_thread_kill_handler(int signum, siginfo_t *info, ExceptionInformation *context)
{
  TCR* tcr = get_tcr(true);
  handle_signal_on_foreign_stack(tcr,
                                 thread_kill_handler,
                                 signum,
                                 info,
                                 context,
                                 (LispObj)__builtin_return_address(0)
                                 );
}
#endif
#endif

#ifdef USE_SIGALTSTACK
#define SUSPEND_RESUME_HANDLER altstack_suspend_resume_handler
#define THREAD_KILL_HANDLER altstack_thread_kill_handler
#else
#define SUSPEND_RESUME_HANDLER arbstack_suspend_resume_handler
#define THREAD_KILL_HANDLER arbstack_thread_kill_handler
#endif

#ifdef WINDOWS
void
thread_signal_setup()
{
}
#else
void
thread_signal_setup()
{
  thread_suspend_signal = SIG_SUSPEND_THREAD;
  thread_kill_signal = SIG_KILL_THREAD;

  install_signal_handler(thread_suspend_signal, (void *)SUSPEND_RESUME_HANDLER,
			 RESERVE_FOR_LISP|ON_ALTSTACK|RESTART_SYSCALLS);
  install_signal_handler(thread_kill_signal, (void *)THREAD_KILL_HANDLER,
			 RESERVE_FOR_LISP|ON_ALTSTACK);
}
#endif

void
enable_fp_exceptions()
{
}

void
exception_init()
{
  x86_early_exception_init();
#ifdef DARWIN
  darwin_early_exception_init();
#endif
  install_pmcl_exception_handlers();
}

void
adjust_exception_pc(ExceptionInformation *xp, int delta)
{
  xpPC(xp) += delta;
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
restore_soft_stack_limit(unsigned restore_tsp)
{
  TCR *tcr = get_tcr(false);
  area *a;
 
  if (restore_tsp) {
    a = tcr->ts_area;
  } else {
    a = tcr->vs_area;
  }
  adjust_soft_protection_limit(a);
}


#ifdef USE_SIGALTSTACK
void
setup_sigaltstack(area *a)
{
  stack_t stack;
  stack.ss_sp = a->low;
  a->low += SIGSTKSZ*8;
  stack.ss_size = SIGSTKSZ*8;
  stack.ss_flags = 0;
  mmap(stack.ss_sp,stack.ss_size, PROT_READ|PROT_WRITE|PROT_EXEC,MAP_FIXED|MAP_ANON|MAP_PRIVATE,-1,0);
#ifdef LINUX
  /* The ucontext pushed on the altstack may not contain the (largish)
     __fpregs_mem field; copy_ucontext() wants to copy what it thinks
     is a pointer to a full ucontext.  That'll touch a page beyond the
     bottom of the altstack, and when this happens on the initial
     thread's stack on a recent (2.6.32+?) kernel, we'll SIGBUS instead
     of mapping that page.
     It's easier to just reserve that page here than it would be to
     change copy_ucontext().
  */
  stack.ss_size -= sizeof(struct ucontext);
#endif
  if (sigaltstack(&stack, NULL) != 0) {
    perror("sigaltstack");
    exit(-1);
  }
}
#endif

extern opcode egc_write_barrier_start, egc_write_barrier_end,
  egc_set_hash_key_conditional, egc_set_hash_key_conditional_success_test,
  egc_set_hash_key_conditional_retry,
  egc_store_node_conditional_success_end, egc_store_node_conditional_retry,
  egc_store_node_conditional_success_test,egc_store_node_conditional,
  egc_set_hash_key, egc_gvset, egc_rplacd;

/* We use (extremely) rigidly defined instruction sequences for consing,
   mostly so that 'pc_luser_xp()' knows what to do if a thread is interrupted
   while consing.

   Note that we can usually identify which of these instructions is about
   to be executed by a stopped thread without comparing all of the bytes
   to those at the stopped program counter, but we generally need to
   know the sizes of each of these instructions.
*/

#ifdef X8664
opcode load_allocptr_reg_from_tcr_save_allocptr_instruction[] =
#ifdef TCR_IN_GPR
  {0x49,0x8b,0x9b,0xd8,0x00,0x00,0x00}
#else
  {0x65,0x48,0x8b,0x1c,0x25,0xd8,0x00,0x00,0x00}
#endif
;
opcode compare_allocptr_reg_to_tcr_save_allocbase_instruction[] =
#ifdef TCR_IN_GPR
  {0x49,0x3b,0x9b,0xe0,0x00,0x00,0x00}
#else
  {0x65,0x48,0x3b,0x1c,0x25,0xe0,0x00,0x00,0x00}
#endif

;
opcode branch_around_alloc_trap_instruction[] =
  {0x77,0x02};
opcode alloc_trap_instruction[] =
  {0xcd,0xc5};
opcode clear_tcr_save_allocptr_tag_instruction[] =
#ifdef TCR_IN_GPR
  {0x41,0x80,0xa3,0xd8,0x00,0x00,0x00,0xf0}
#else
  {0x65,0x80,0x24,0x25,0xd8,0x00,0x00,0x00,0xf0}
#endif
;
opcode set_allocptr_header_instruction[] =
  {0x48,0x89,0x43,0xf3};


alloc_instruction_id
recognize_alloc_instruction(pc program_counter)
{
  switch(program_counter[0]) {
  case 0xcd: return ID_alloc_trap_instruction;
  /* 0x7f is jg, which we used to use here instead of ja */
  case 0x7f:
  case 0x77: return ID_branch_around_alloc_trap_instruction;
  case 0x48: return ID_set_allocptr_header_instruction;
#ifdef TCR_IN_GPR
  case 0x41: return ID_clear_tcr_save_allocptr_tag_instruction;
  case 0x49:
    switch(program_counter[1]) {
    case 0x8b: return ID_load_allocptr_reg_from_tcr_save_allocptr_instruction;
    case 0x3b: return ID_compare_allocptr_reg_to_tcr_save_allocbase_instruction;
    }
#else
  case 0x65: 
    switch(program_counter[1]) {
    case 0x80: return ID_clear_tcr_save_allocptr_tag_instruction;
    case 0x48:
      switch(program_counter[2]) {
      case 0x3b: return ID_compare_allocptr_reg_to_tcr_save_allocbase_instruction;
      case 0x8b: return ID_load_allocptr_reg_from_tcr_save_allocptr_instruction;
      }
    }
#endif
  default: break;
  }
  return ID_unrecognized_alloc_instruction;
}
#endif
#ifdef X8632
#define TCR_SEG_PREFIX 0x64

#ifdef WIN_32
#define SAVE_ALLOCPTR 0x9c,0x0e,0x0,0x0
#define SAVE_ALLOCBASE 0x98,0x0e,0x0,0x0
#else
#define SAVE_ALLOCPTR 0x84,0x0,0x0,0x0
#define SAVE_ALLOCBASE 0x88,0x0,0x0,0x0
#endif

opcode load_allocptr_reg_from_tcr_save_allocptr_instruction[] =
  {TCR_SEG_PREFIX,0x8b,0x0d,SAVE_ALLOCPTR};
opcode compare_allocptr_reg_to_tcr_save_allocbase_instruction[] =
  {TCR_SEG_PREFIX,0x3b,0x0d,SAVE_ALLOCBASE};
opcode branch_around_alloc_trap_instruction[] =
  {0x77,0x02};
opcode alloc_trap_instruction[] =
  {0xcd,0xc5};
opcode clear_tcr_save_allocptr_tag_instruction[] =
  {TCR_SEG_PREFIX,0x80,0x25,SAVE_ALLOCPTR,0xf8};
opcode set_allocptr_header_instruction[] =
  {0x0f,0x7e,0x41,0xfa};

alloc_instruction_id
recognize_alloc_instruction(pc program_counter)
{
  switch(program_counter[0]) {
  case 0xcd: return ID_alloc_trap_instruction;
  case 0x77: return ID_branch_around_alloc_trap_instruction;
  case 0x0f: return ID_set_allocptr_header_instruction;
  case 0x64: 
    switch(program_counter[1]) {
    case 0x80: return ID_clear_tcr_save_allocptr_tag_instruction;
    case 0x3b: return ID_compare_allocptr_reg_to_tcr_save_allocbase_instruction;
    case 0x8b: return ID_load_allocptr_reg_from_tcr_save_allocptr_instruction;
    }
  }
  return ID_unrecognized_alloc_instruction;
}
#endif      

void
pc_luser_xp(ExceptionInformation *xp, TCR *tcr, signed_natural *interrupt_displacement)
{
  pc program_counter = (pc)xpPC(xp);
  int allocptr_tag = fulltag_of((LispObj)(tcr->save_allocptr));

  if (allocptr_tag != 0) {
    alloc_instruction_id state = recognize_alloc_instruction(program_counter);
    signed_natural 
      disp = (allocptr_tag == fulltag_cons) ?
      sizeof(cons) - fulltag_cons :
#ifdef X8664
      xpGPR(xp,Iimm1)
#else
      xpGPR(xp,Iimm0)
#endif
      ;
    LispObj new_vector;

    if ((state == ID_unrecognized_alloc_instruction) ||
        ((state == ID_set_allocptr_header_instruction) &&
         (allocptr_tag != fulltag_misc))) {
      Bug(xp, "Can't determine state of thread 0x" LISP ", interrupted during memory allocation", tcr);
    }
    switch(state) {
    case ID_set_allocptr_header_instruction:
      /* We were consing a vector and we won.  Set the header of the
         new vector (in the allocptr register) to the header in %rax
         (%mm0 on ia32) and skip over this instruction, then fall into
         the next case. */
      new_vector = xpGPR(xp,Iallocptr);
      deref(new_vector,0) = 
#ifdef X8664
        xpGPR(xp,Iimm0)
#else
        xpMMXreg(xp,Imm0)
#endif
        ;
      
      xpPC(xp) += sizeof(set_allocptr_header_instruction);

      /* Fall thru */
    case ID_clear_tcr_save_allocptr_tag_instruction:
      tcr->save_allocptr = (void *)(((LispObj)tcr->save_allocptr) & ~fulltagmask);
      xpPC(xp) += sizeof(clear_tcr_save_allocptr_tag_instruction);

      break;
    case ID_alloc_trap_instruction:
      /* If we're looking at another thread, we're pretty much committed to
         taking the trap.  We don't want the allocptr register to be pointing
         into the heap, so make it point to (- VOID_ALLOCPTR disp), where 'disp'
         was determined above. 
      */
      if (interrupt_displacement == NULL) {
        xpGPR(xp,Iallocptr) = VOID_ALLOCPTR - disp;
        tcr->save_allocptr = (void *)(VOID_ALLOCPTR - disp);
      } else {
        /* Back out, and tell the caller how to resume the allocation attempt */
        *interrupt_displacement = disp;
        xpGPR(xp,Iallocptr) = VOID_ALLOCPTR;
        tcr->save_allocptr += disp;
        xpPC(xp) -= (sizeof(branch_around_alloc_trap_instruction)+
                     sizeof(compare_allocptr_reg_to_tcr_save_allocbase_instruction) +
                     sizeof(load_allocptr_reg_from_tcr_save_allocptr_instruction));
      }
      break;
    case ID_branch_around_alloc_trap_instruction:
      /* If we'd take the branch - which is a "ja" - around the alloc trap,
         we might as well finish the allocation.  Otherwise, back out of the
         attempt. */
      {
        int flags = (int)eflags_register(xp);
        
        if ((!(flags & (1 << X86_ZERO_FLAG_BIT))) &&
	    (!(flags & (1 << X86_CARRY_FLAG_BIT)))) {
          /* The branch (ja) would have been taken.  Emulate taking it. */
          xpPC(xp) += (sizeof(branch_around_alloc_trap_instruction)+
                       sizeof(alloc_trap_instruction));
          if (allocptr_tag == fulltag_misc) {
            /* Slap the header on the new uvector */
            new_vector = xpGPR(xp,Iallocptr);
#ifdef X8664
            deref(new_vector,0) = xpGPR(xp,Iimm0);
#else
            deref(new_vector,0) = xpMMXreg(xp,Imm0);
#endif
            xpPC(xp) += sizeof(set_allocptr_header_instruction);
          }
          tcr->save_allocptr = (void *)(((LispObj)tcr->save_allocptr) & ~fulltagmask);
          xpPC(xp) += sizeof(clear_tcr_save_allocptr_tag_instruction);
        } else {
          /* Back up */
          xpPC(xp) -= (sizeof(compare_allocptr_reg_to_tcr_save_allocbase_instruction) +
                       sizeof(load_allocptr_reg_from_tcr_save_allocptr_instruction));
          xpGPR(xp,Iallocptr) = VOID_ALLOCPTR;
          if (interrupt_displacement) {
            *interrupt_displacement = disp;
            tcr->save_allocptr += disp;
          } else {
            tcr->save_allocptr = (void *)(VOID_ALLOCPTR-disp);
          }
        }
      }
      break;
    case ID_compare_allocptr_reg_to_tcr_save_allocbase_instruction:
      xpGPR(xp,Iallocptr) = VOID_ALLOCPTR;
      xpPC(xp) -= sizeof(load_allocptr_reg_from_tcr_save_allocptr_instruction);
      /* Fall through */
    case ID_load_allocptr_reg_from_tcr_save_allocptr_instruction:
      if (interrupt_displacement) {
        tcr->save_allocptr += disp;
        *interrupt_displacement = disp;
      } else {
        tcr->save_allocptr = (void *)(VOID_ALLOCPTR-disp);
      }
      break;
    default: 
      break;
    }
    return;
  }
  if ((program_counter >= &egc_write_barrier_start) &&
      (program_counter < &egc_write_barrier_end)) {
    LispObj *ea = 0, val, root = 0;
    bitvector refbits = (bitvector)(lisp_global(REFBITS));
    Boolean need_store = true, need_check_memo = true, need_memoize_root = false;

    if (program_counter >= &egc_set_hash_key_conditional) {
      if (program_counter <= &egc_set_hash_key_conditional_retry) {
        return;
      }
      if ((program_counter < &egc_set_hash_key_conditional_success_test) ||
          ((program_counter == &egc_set_hash_key_conditional_success_test) &&
           !(eflags_register(xp) & (1 << X86_ZERO_FLAG_BIT)))) {
        /* Back up the PC, try again */
        xpPC(xp) = (LispObj) &egc_set_hash_key_conditional_retry;
        return;
      }
      /* The conditional store succeeded.  Set the refbit, return to ra0 */
      val = xpGPR(xp,Iarg_z);
#ifdef X8664
      root = xpGPR(xp,Iarg_x);
      ea = (LispObj*)(root + (unbox_fixnum((signed_natural) xpGPR(xp,Itemp0))));
#else
      root = xpGPR(xp,Itemp1);
      ea = (LispObj *)(root + misc_data_offset + xpGPR(xp,Itemp0));
#endif
      need_memoize_root = true;
      need_store = false;
      xpGPR(xp,Iarg_z) = t_value;
    } else if (program_counter >= &egc_store_node_conditional) {
      if (program_counter <= &egc_store_node_conditional_retry) {
        return;
      }
      if ((program_counter < &egc_store_node_conditional_success_test) ||
          ((program_counter == &egc_store_node_conditional_success_test) &&
           !(eflags_register(xp) & (1 << X86_ZERO_FLAG_BIT)))) {
        /* Back up the PC, try again */
        xpPC(xp) = (LispObj) &egc_store_node_conditional_retry;
        return;
      }
      if (program_counter >= &egc_store_node_conditional_success_end) {
        return;
      }

      /* The conditional store succeeded.  Set the refbit, return to ra0 */
      val = xpGPR(xp,Iarg_z);
#ifdef X8664
      ea = (LispObj*)(xpGPR(xp,Iarg_x) + (unbox_fixnum((signed_natural)
                                                       xpGPR(xp,Itemp0))));
#else
      ea = (LispObj *)(misc_data_offset + xpGPR(xp,Itemp1) + xpGPR(xp,Itemp0));
#endif
      xpGPR(xp,Iarg_z) = t_value;
      need_store = false;
    } else if (program_counter >= &egc_set_hash_key) {
#ifdef X8664
      root = xpGPR(xp,Iarg_x);
#else
      root = xpGPR(xp,Itemp0);
#endif
      ea = (LispObj *) (root+xpGPR(xp,Iarg_y)+misc_data_offset);
      val = xpGPR(xp,Iarg_z);
      need_memoize_root = true;
    } else if (program_counter >= &egc_gvset) {
#ifdef X8664
      ea = (LispObj *) (xpGPR(xp,Iarg_x)+xpGPR(xp,Iarg_y)+misc_data_offset);
#else
      ea = (LispObj *) (xpGPR(xp,Itemp0)+xpGPR(xp,Iarg_y)+misc_data_offset);
#endif
      val = xpGPR(xp,Iarg_z);
    } else if (program_counter >= &egc_rplacd) {
      ea = (LispObj *) untag(xpGPR(xp,Iarg_y));
      val = xpGPR(xp,Iarg_z);
    } else {                      /* egc_rplaca */
      ea =  ((LispObj *) untag(xpGPR(xp,Iarg_y)))+1;
      val = xpGPR(xp,Iarg_z);
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
    {
      /* These subprimitives are called via CALL/RET; need
         to pop the return address off the stack and set
         the PC there. */
      LispObj *sp = (LispObj *)xpGPR(xp,Isp), ra = *sp++;
      xpPC(xp) = ra;
      xpGPR(xp,Isp)=(LispObj)sp;
    }
    return;
  }
}


void
normalize_tcr(ExceptionInformation *xp, TCR *tcr, Boolean is_other_tcr)
{
  void *cur_allocptr = (void *)(tcr->save_allocptr);
  LispObj lisprsp;
  area *a;

  if (xp) {
    if (is_other_tcr) {
      pc_luser_xp(xp, tcr, NULL);
    }
    a = tcr->vs_area;
    lisprsp = xpGPR(xp, Isp);
    if (((BytePtr)lisprsp >= a->low) &&
	((BytePtr)lisprsp < a->high)) {
      a->active = (BytePtr)lisprsp;
    } else {
      a->active = (BytePtr) tcr->save_vsp;
    }
    a = tcr->ts_area;
    a->active = (BytePtr) tcr->save_tsp;
  } else {
    /* In ff-call; get area active pointers from tcr */
    tcr->vs_area->active = (BytePtr) tcr->save_vsp;
    tcr->ts_area->active = (BytePtr) tcr->save_tsp;
  }
  if (cur_allocptr) {
    update_bytes_allocated(tcr, cur_allocptr);
  }
  tcr->save_allocbase = (void *)VOID_ALLOCPTR;
  if (fulltag_of((LispObj)(tcr->save_allocptr)) == 0) {
    tcr->save_allocptr = (void *)VOID_ALLOCPTR;
  }
}


/* Suspend and "normalize" other tcrs, then call a gc-like function
   in that context.  Resume the other tcrs, then return what the
   function returned */

TCR *gc_tcr = NULL;


signed_natural
gc_like_from_xp(ExceptionInformation *xp, 
                signed_natural(*fun)(TCR *, signed_natural), 
                signed_natural param)
{
  TCR *tcr = get_tcr(false), *other_tcr;
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

  /* This is generally necessary if the current thread invoked the GC
     via an alloc trap, and harmless if the GC was invoked via a GC
     trap.  (It's necessary in the first case because the "allocptr"
     register - %rbx - may be pointing into the middle of something
     below tcr->save_allocbase, and we wouldn't want the GC to see
     that bogus pointer.) */
  xpGPR(xp, Iallocptr) = VOID_ALLOCPTR; 

  normalize_tcr(xp, tcr, false);


  for (other_tcr = TCR_AUX(tcr)->next; other_tcr != tcr;
       other_tcr = TCR_AUX(other_tcr)->next) {
    if (other_tcr->pending_exception_context) {
      TCR_AUX(other_tcr)->gc_context = other_tcr->pending_exception_context;
    } else if (other_tcr->valence == TCR_STATE_LISP) {
      TCR_AUX(other_tcr)->gc_context = TCR_AUX(other_tcr)->suspend_context;
    } else {
      /* no pending exception, didn't suspend in lisp state:
	 must have executed a synchronous ff-call. 
      */
      TCR_AUX(other_tcr)->gc_context = NULL;
    }
    normalize_tcr(TCR_AUX(other_tcr)->gc_context, other_tcr, true);
  }
    


  result = fun(tcr, param);

  other_tcr = tcr;
  do {
    TCR_AUX(other_tcr)->gc_context = NULL;
    other_tcr = TCR_AUX(other_tcr)->next;
  } while (other_tcr != tcr);

  gc_tcr = NULL;

  resume_other_threads(true);

  return result;

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

/* Returns #bytes freed by invoking GC */

signed_natural
gc_from_tcr(TCR *tcr, signed_natural param)
{
  area *a;
  BytePtr oldfree, newfree;
  BytePtr oldend, newend;

#if 0
  fprintf(dbgout, "Start GC  in 0x" LISP "\n", tcr);
#endif
  a = active_dynamic_area;
  oldend = a->high;
  oldfree = a->active;
  gc(tcr, param);
  newfree = a->active;
  newend = a->high;
#if 0
  fprintf(dbgout, "End GC  in 0x" LISP "\n", tcr);
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

#ifdef DARWIN

#ifdef X8664
#define ts_pc(t) t->__rip
typedef x86_thread_state64_t native_thread_state_t;
#define NATIVE_THREAD_STATE_COUNT x86_THREAD_STATE64_COUNT
#define NATIVE_THREAD_STATE_FLAVOR x86_THREAD_STATE64
typedef x86_float_state64_t native_float_state_t;
#define NATIVE_FLOAT_STATE_COUNT x86_FLOAT_STATE64_COUNT
#define NATIVE_FLOAT_STATE_FLAVOR x86_FLOAT_STATE64
typedef x86_exception_state64_t native_exception_state_t;
#define NATIVE_EXCEPTION_STATE_COUNT x86_EXCEPTION_STATE64_COUNT
#define NATIVE_EXCEPTION_STATE_FLAVOR x86_EXCEPTION_STATE64
#else
#define ts_pc(t) t->__eip
typedef x86_thread_state32_t native_thread_state_t;
#define NATIVE_THREAD_STATE_COUNT x86_THREAD_STATE32_COUNT
#define NATIVE_THREAD_STATE_FLAVOR x86_THREAD_STATE32
typedef x86_float_state32_t native_float_state_t;
#define NATIVE_FLOAT_STATE_COUNT x86_FLOAT_STATE32_COUNT
#define NATIVE_FLOAT_STATE_FLAVOR x86_FLOAT_STATE32
typedef x86_exception_state32_t native_exception_state_t;
#define NATIVE_EXCEPTION_STATE_COUNT x86_EXCEPTION_STATE32_COUNT
#define NATIVE_EXCEPTION_STATE_FLAVOR x86_EXCEPTION_STATE32
#endif

#define TCR_FROM_EXCEPTION_PORT(p) find_tcr_from_exception_port(p)
#define TCR_TO_EXCEPTION_PORT(t) (mach_port_name_t)((natural) (((TCR *)t)->io_datum))


extern void pseudo_sigreturn(void);



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

#ifdef PPC64
#define	C_REDZONE_LEN		320
#define	C_STK_ALIGN             32
#else
#define	C_REDZONE_LEN		224
#define	C_STK_ALIGN		16
#endif
#define C_PARAMSAVE_LEN		64
#define	C_LINKAGE_LEN		48

#define TRUNC_DOWN(a,b,c)  (((((natural)a)-(b))/(c)) * (c))

void
fatal_mach_error(char *format, ...);

#define MACH_CHECK_ERROR(context,x) if (x != KERN_SUCCESS) {fatal_mach_error("Mach error while %s : %d", context, x);}


void
restore_mach_thread_state(mach_port_t thread, ExceptionInformation *pseudosigcontext, native_thread_state_t *ts)
{
  kern_return_t kret;
#if WORD_SIZE == 64
  MCONTEXT_T mc = UC_MCONTEXT(pseudosigcontext);
#else
  mcontext_t mc = UC_MCONTEXT(pseudosigcontext);
#endif

  /* Set the thread's FP state from the pseudosigcontext */
  kret = thread_set_state(thread,
                          NATIVE_FLOAT_STATE_FLAVOR,
                          (thread_state_t)&(mc->__fs),
                          NATIVE_FLOAT_STATE_COUNT);
  MACH_CHECK_ERROR("setting thread FP state", kret);
  *ts = mc->__ss;
}  

kern_return_t
do_pseudo_sigreturn(mach_port_t thread, TCR *tcr, native_thread_state_t *out)
{
  ExceptionInformation *xp;

#ifdef DEBUG_MACH_EXCEPTIONS
  fprintf(dbgout, "doing pseudo_sigreturn for 0x%x\n",tcr);
#endif
  xp = tcr->pending_exception_context;
  if (xp) {
    tcr->pending_exception_context = NULL;
    tcr->valence = TCR_STATE_LISP;
    restore_mach_thread_state(thread, xp, out);
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
			    natural *new_stack_top,
                            siginfo_t **info_ptr,
                            TCR *tcr,
                            native_thread_state_t *ts
                            )
{
  mach_msg_type_number_t thread_state_count;
  ExceptionInformation *pseudosigcontext;
#ifdef X8664
  MCONTEXT_T mc;
#else
  mcontext_t mc;
#endif
  natural stackp;

#ifdef X8664  
  stackp = (LispObj) find_foreign_rsp(ts->__rsp,tcr->cs_area,tcr);
  stackp = TRUNC_DOWN(stackp, C_REDZONE_LEN, C_STK_ALIGN);
#else
  stackp = (LispObj) find_foreign_rsp(ts->__esp, tcr->cs_area, tcr);
#endif
  stackp = TRUNC_DOWN(stackp, sizeof(siginfo_t), C_STK_ALIGN);
  if (info_ptr) {
    *info_ptr = (siginfo_t *)stackp;
  }
  stackp = TRUNC_DOWN(stackp,sizeof(*pseudosigcontext), C_STK_ALIGN);
  pseudosigcontext = (ExceptionInformation *) ptr_from_lispobj(stackp);

  stackp = TRUNC_DOWN(stackp, sizeof(*mc), C_STK_ALIGN);
  mc = (MCONTEXT_T) ptr_from_lispobj(stackp);
  
  memmove(&(mc->__ss),ts,sizeof(*ts));

  thread_state_count = NATIVE_FLOAT_STATE_COUNT;
  thread_get_state(thread,
		   NATIVE_FLOAT_STATE_FLAVOR,
		   (thread_state_t)&(mc->__fs),
		   &thread_state_count);

  thread_state_count = NATIVE_EXCEPTION_STATE_COUNT;
  thread_get_state(thread,
                   NATIVE_EXCEPTION_STATE_FLAVOR,
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
  handler" function when it resumes.  Create a fake ucontext struct
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
		   TCR *tcr,
                   native_thread_state_t *ts,
                   native_thread_state_t *new_ts
                   )
{
  ExceptionInformation *pseudosigcontext;
  int  old_valence = tcr->valence;
  natural stackp, *stackpp;
  siginfo_t *info;

#ifdef DEBUG_MACH_EXCEPTIONS
  fprintf(dbgout,"Setting up exception handling for 0x%x\n", tcr);
#endif
  pseudosigcontext = create_thread_context_frame(thread, &stackp, &info, tcr,  ts);
  bzero(info, sizeof(*info));
  info->si_code = code;
  info->si_addr = (void *)(UC_MCONTEXT(pseudosigcontext)->__es.__faultvaddr);
  info->si_signo = signum;
  pseudosigcontext->uc_onstack = 0;
  pseudosigcontext->uc_sigmask = (sigset_t) 0;
  pseudosigcontext->uc_stack.ss_sp = 0;
  pseudosigcontext->uc_stack.ss_size = 0;
  pseudosigcontext->uc_stack.ss_flags = 0;
  pseudosigcontext->uc_link = NULL;
  pseudosigcontext->uc_mcsize = sizeof(*UC_MCONTEXT(pseudosigcontext));
  tcr->pending_exception_context = pseudosigcontext;
  tcr->valence = TCR_STATE_EXCEPTION_WAIT;
  

  /* 
     It seems like we've created a  sigcontext on the thread's
     stack.  Set things up so that we call the handler (with appropriate
     args) when the thread's resumed.
  */

#ifdef X8664
  new_ts->__rip = (natural) handler_address;
  stackpp = (natural *)stackp;
  *--stackpp = (natural)pseudo_sigreturn;
  stackp = (natural)stackpp;
  new_ts->__rdi = signum;
  new_ts->__rsi = (natural)info;
  new_ts->__rdx = (natural)pseudosigcontext;
  new_ts->__rcx = (natural)tcr;
  new_ts->__r8 = (natural)old_valence;
  new_ts->__rsp = stackp;
  new_ts->__rflags = ts->__rflags;
#else
  bzero(new_ts, sizeof(*new_ts));
  new_ts->__cs = ts->__cs;
  new_ts->__ss = ts->__ss;
  new_ts->__ds = ts->__ds;
  new_ts->__es = ts->__es;
  new_ts->__fs = ts->__fs;
  new_ts->__gs = ts->__gs;

  new_ts->__eip = (natural)handler_address;
  stackpp = (natural *)stackp;
  *--stackpp = 0;		/* alignment */
  *--stackpp = 0;
  *--stackpp = 0;
  *--stackpp = (natural)old_valence;
  *--stackpp = (natural)tcr;
  *--stackpp = (natural)pseudosigcontext;
  *--stackpp = (natural)info;
  *--stackpp = (natural)signum;
  *--stackpp = (natural)pseudo_sigreturn;
  stackp = (natural)stackpp;
  new_ts->__esp = stackp;
  new_ts->__eflags = ts->__eflags;
#endif
#ifdef DEBUG_MACH_EXCEPTIONS
  fprintf(dbgout,"Set up exception context for 0x%x at 0x%x\n", tcr, tcr->pending_exception_context);
#endif
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




#define DARWIN_EXCEPTION_HANDLER signal_handler

#define EXCEPTION_PORT_BUCKETS 109

TCR *
exception_port_map[EXCEPTION_PORT_BUCKETS];

pthread_mutex_t 
exception_port_map_lock = PTHREAD_MUTEX_INITIALIZER;

TCR *
find_tcr_from_exception_port(mach_port_t port)
{
  if (use_mach_port_context_functions) {
    mach_vm_address_t addr = 0;
    kern_return_t kret;

    kret = Pmach_port_get_context(mach_task_self(),port,&addr);
    MACH_CHECK_ERROR("finding TCR from exception port",kret);
    return (TCR *)((natural)addr);
  } else {
    TCR *tcr = NULL;
    pthread_mutex_lock(&exception_port_map_lock);

    tcr = exception_port_map[(unsigned)port % EXCEPTION_PORT_BUCKETS];

    while (tcr) {
      if (TCR_TO_EXCEPTION_PORT(tcr) == port) {
        break;
      }
      tcr = (TCR *)tcr->pending_io_info;
    }
    pthread_mutex_unlock(&exception_port_map_lock);
    return tcr;
  }
}

void
associate_tcr_with_exception_port(mach_port_t port, TCR *tcr)
{
  if (use_mach_port_context_functions) {
    kern_return_t kret;
    
    kret = Pmach_port_set_context(mach_task_self(),port,(mach_vm_address_t)((natural)tcr));
    MACH_CHECK_ERROR("associating TCR with exception port",kret);
  } else {
    int b = (unsigned)port % EXCEPTION_PORT_BUCKETS;
    pthread_mutex_lock(&exception_port_map_lock);
    
    tcr->pending_io_info = (void *)(exception_port_map[b]);
    exception_port_map[b] = tcr;
    pthread_mutex_unlock(&exception_port_map_lock);
  }
}

void
disassociate_tcr_from_exception_port(mach_port_t port)
{
  if (use_mach_port_context_functions) {
    TCR **prev = &(exception_port_map[(unsigned)port % EXCEPTION_PORT_BUCKETS]),
      *tcr;
    pthread_mutex_lock(&exception_port_map_lock);

    while((tcr = *prev) != NULL) {
      if (TCR_TO_EXCEPTION_PORT(tcr) == port) {
        *prev = (TCR *)tcr->pending_io_info;
        break;
      }
      prev = (TCR **)&(tcr->pending_io_info);
    }
    pthread_mutex_unlock(&exception_port_map_lock);
  }
}
  

kern_return_t
catch_exception_raise_state(mach_port_t exception_port,
                            exception_type_t exception,
                            exception_data_t code_vector,
                            mach_msg_type_number_t code_count,
                            int * flavor,
                            thread_state_t in_state,
                            mach_msg_type_number_t in_state_count,
                            thread_state_t out_state,
                            mach_msg_type_number_t * out_state_count)
{
  int signum = 0, code = *code_vector;
  TCR *tcr = TCR_FROM_EXCEPTION_PORT(exception_port);
  mach_port_t thread = (mach_port_t)((natural)tcr->native_thread_id);
  kern_return_t kret, call_kret;

  native_thread_state_t
    *ts = (native_thread_state_t *)in_state,
    *out_ts = (native_thread_state_t*)out_state;
  mach_msg_type_number_t thread_state_count;



  if (1) {
    if (tcr->flags & (1<<TCR_FLAG_BIT_PENDING_EXCEPTION)) {
      CLR_TCR_FLAG(tcr,TCR_FLAG_BIT_PENDING_EXCEPTION);
    } 
    if ((code == EXC_I386_GPFLT) &&
        ((natural)(ts_pc(ts)) == (natural)pseudo_sigreturn)) {
      kret = do_pseudo_sigreturn(thread, tcr, out_ts);
#if 0
      fprintf(dbgout, "Exception return in 0x%x\n",tcr);
#endif
    } else if (tcr->flags & (1<<TCR_FLAG_BIT_PROPAGATE_EXCEPTION)) {
      CLR_TCR_FLAG(tcr,TCR_FLAG_BIT_PROPAGATE_EXCEPTION);
      kret = 17;
    } else {
      switch (exception) {
      case EXC_BAD_ACCESS:
        if (code == EXC_I386_GPFLT) {
          signum = SIGSEGV;
        } else {
          signum = SIGBUS;
        }
        break;
        
      case EXC_BAD_INSTRUCTION:
        if (code == EXC_I386_GPFLT) {
          signum = SIGSEGV;
        } else {
          signum = SIGILL;
        }
        break;
          
      case EXC_SOFTWARE:
        signum = SIGILL;
        break;
        
      case EXC_ARITHMETIC:
        signum = SIGFPE;
	if (code == EXC_I386_DIV)
	  code = FPE_INTDIV;
        break;
        
      default:
        break;
      }
#if WORD_SIZE==64
      if ((signum==SIGFPE) && 
          (code != FPE_INTDIV) && 
          (tcr->valence != TCR_STATE_LISP)) {
        mach_msg_type_number_t thread_state_count = x86_FLOAT_STATE64_COUNT;
        x86_float_state64_t fs;

        thread_get_state(thread,
                         x86_FLOAT_STATE64,
                         (thread_state_t)&fs,
                         &thread_state_count);
        
        if (! (tcr->flags & (1<<TCR_FLAG_BIT_FOREIGN_FPE))) {
          tcr->flags |= (1<<TCR_FLAG_BIT_FOREIGN_FPE);
          tcr->lisp_mxcsr = (fs.__fpu_mxcsr & ~MXCSR_STATUS_MASK);
        }
        fs.__fpu_mxcsr &= ~MXCSR_STATUS_MASK;
        fs.__fpu_mxcsr |= MXCSR_CONTROL_MASK;
        thread_set_state(thread,
                         x86_FLOAT_STATE64,
                         (thread_state_t)&fs,
                         x86_FLOAT_STATE64_COUNT);
        *out_state_count = NATIVE_THREAD_STATE_COUNT;
        *out_ts = *ts;
        return KERN_SUCCESS;
      }
#endif
      if (signum) {
        kret = setup_signal_frame(thread,
                                  (void *)DARWIN_EXCEPTION_HANDLER,
                                  signum,
                                  code,
                                  tcr, 
                                  ts,
                                  out_ts);
        
      } else {
        kret = 17;
      }
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




static mach_port_t mach_exception_thread = (mach_port_t)0;


/*
  The initial function for an exception-handling thread.
*/

void *
exception_handler_proc(void *arg)
{
  extern boolean_t exc_server();
  mach_port_t p = (mach_port_t)((natural)arg);

  mach_exception_thread = pthread_mach_thread_np(pthread_self());
  mach_msg_server(exc_server, 256, p, 0);
  /* Should never return. */
  abort();
}



void
mach_exception_thread_shutdown()
{
  kern_return_t kret;

  fprintf(dbgout, "terminating Mach exception thread, 'cause exit can't\n");
  kret = thread_terminate(mach_exception_thread);
  if (kret != KERN_SUCCESS) {
    fprintf(dbgout, "Couldn't terminate exception thread, kret = %d\n",kret);
  }
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
				     EXCEPTION_STATE,
#if WORD_SIZE==64
                                     x86_THREAD_STATE64,
#else
                                     x86_THREAD_STATE32,
#endif
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
  kern_return_t kret;

  if (m) {
    MACH_foreign_exception_state *fxs  = 
      (MACH_foreign_exception_state *) tcr->native_thread_info;
    int i, n = fxs->foreign_exception_port_count;
    exception_mask_t tm;

    for (i = 0; i < n; i++) {
      if ((tm = fxs->masks[i]) & m) {
	kret = thread_set_exception_ports((mach_port_t)((natural)tcr->native_thread_id),
				   tm,
				   fxs->ports[i],
				   fxs->behaviors[i],
				   fxs->flavors[i]);
	MACH_CHECK_ERROR("restoring thread exception ports", kret);
      }
    }
  }
  return KERN_SUCCESS;
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
    mach_port_t task_self = mach_task_self(),
      exception_port = TCR_TO_EXCEPTION_PORT(tcr);

    disassociate_tcr_from_exception_port(exception_port);
    mach_port_deallocate(task_self,exception_port);
    mach_port_destroy(task_self,exception_port);
  }
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




#endif

/* watchpoint stuff */

area *
new_watched_area(natural size)
{
  char *p;

  p = MapMemory(NULL, size, MEMPROTECT_RWX);
  if ((signed_natural)p == -1) {
    allocation_failure(true, size);
  }
  return new_area(p, p + size, AREA_WATCHED);
}

void
delete_watched_area(area *a, TCR *tcr)
{
  natural nbytes = a->high - a->low;
  char *base = a->low;

  condemn_area_holding_area_lock(a);

  if (nbytes) {
    int err;

    err = UnMapMemory(base, nbytes);
    if (err != 0)
      Fatal("munmap in delete_watched_area", "");
  }
}

natural
uvector_total_size_in_bytes(LispObj *u)
{
  LispObj header = header_of(u);
  natural header_tag = fulltag_of(header);
  natural subtag = header_subtag(header);
  natural element_count = header_element_count(header);
  natural nbytes = 0;

#ifdef X8632
  if ((nodeheader_tag_p(header_tag)) ||
      (subtag <= max_32_bit_ivector_subtag)) {
    nbytes = element_count << 2;
  } else if (subtag <= max_8_bit_ivector_subtag) {
    nbytes = element_count;
  } else if (subtag <= max_16_bit_ivector_subtag) {
    nbytes = element_count << 1;
  } else if (subtag == subtag_double_float_vector) {
    nbytes = element_count << 3;
  } else {
    nbytes = (element_count + 7) >> 3;
  }
  /* add 4 byte header and round up to multiple of 8 bytes */
  return ~7 & (4 + nbytes + 7);
#endif
#ifdef X8664
  if ((nodeheader_tag_p(header_tag)) || (header_tag == ivector_class_64_bit)) {
    nbytes = element_count << 3;
  } else if (header_tag == ivector_class_32_bit) {
    nbytes = element_count << 2;
  } else {
    /* ivector_class_other_bit contains 8, 16-bit arrays & bit vector */
    if (subtag == subtag_bit_vector) {
      nbytes = (element_count + 7) >> 3;
    } else if (subtag >= min_8_bit_ivector_subtag) {
      nbytes = element_count;
    } else {
      nbytes = element_count << 1;
    }
  }
  /* add 8 byte header and round up to multiple of 16 bytes */
  return ~15 & (8 + nbytes + 15);
#endif
}

extern void wp_update_references(TCR *, LispObj, LispObj);

/*
 * Other threads are suspended and pc-lusered.
 *
 * param contains a tagged pointer to a uvector or a cons cell
 */
signed_natural
watch_object(TCR *tcr, signed_natural param)
{
  LispObj object = (LispObj)param;
  unsigned tag = fulltag_of(object);
  LispObj *noderef = (LispObj *)untag(object);
  area *object_area = area_containing((BytePtr)noderef);
  natural size;

  if (tag == fulltag_cons)
    size = 2 * node_size;
  else
    size = uvector_total_size_in_bytes(noderef);

  if (object_area && object_area->code == AREA_DYNAMIC) {
    area *a = new_watched_area(size);
    LispObj old = object;
    LispObj new = (LispObj)((natural)a->low + tag);

    add_area_holding_area_lock(a);

    /* move object to watched area */
    memcpy(a->low, noderef, size);
    ProtectMemory(a->low, size);
    memset(noderef, 0, size);
    wp_update_references(tcr, old, new);
    check_all_areas(tcr);
    return 1;
  }
  return 0;
}

/*
 * We expect the watched object in arg_y, and the new uninitialized
 * object (which is just zeroed) in arg_z.
 */
signed_natural
unwatch_object(TCR *tcr, signed_natural param)
{
  ExceptionInformation *xp = tcr->xframe->curr;
  LispObj old = xpGPR(xp, Iarg_y);
  unsigned tag = fulltag_of(old);
  LispObj new = xpGPR(xp, Iarg_z);
  LispObj *oldnode = (LispObj *)untag(old);
  LispObj *newnode = (LispObj *)untag(new);
  area *a = area_containing((BytePtr)old);
  extern void update_managed_refs(area *, BytePtr, natural);

  if (a && a->code == AREA_WATCHED) {
    natural size;

    if (tag == fulltag_cons)
      size = 2 * node_size;
    else
      size = uvector_total_size_in_bytes(oldnode);

    memcpy(newnode, oldnode, size);
    delete_watched_area(a, tcr);
    wp_update_references(tcr, old, new);
    /* because wp_update_references doesn't update refbits */
    tenure_to_area(tenured_area);
    /* Unwatching can (re-)introduce managed_static->dynamic references */
    zero_bits(managed_static_area->refbits,managed_static_area->ndnodes);
    update_managed_refs(managed_static_area, low_markable_address, area_dnode(active_dynamic_area->active, low_markable_address));
    check_all_areas(tcr);
    xpGPR(xp, Iarg_z) = new;
  }
  return 0;
}

Boolean
handle_watch_trap(ExceptionInformation *xp, TCR *tcr)
{
  LispObj selector = xpGPR(xp,Iimm0);
  LispObj object = xpGPR(xp, Iarg_z);
  signed_natural result;
  
  switch (selector) {
    case WATCH_TRAP_FUNCTION_WATCH:
      result = gc_like_from_xp(xp, watch_object, object);
      if (result == 0)
	xpGPR(xp,Iarg_z) = lisp_nil;
      break;
    case WATCH_TRAP_FUNCTION_UNWATCH:
      gc_like_from_xp(xp, unwatch_object, 0);
      break;
    default:
      break;
  }
  return true;
}

