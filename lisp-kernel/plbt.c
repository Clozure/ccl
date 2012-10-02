/*
   Copyright (C) 2009 Clozure Associates
   Copyright (C) 1994-2001 Digitool, Inc
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

#include "lispdcmd.h"
#ifdef LINUX
#define __USE_GNU 1
#endif

#ifndef WINDOWS
#include <dlfcn.h>
#endif


extern Boolean lisp_frame_p(lisp_frame *);

void
print_lisp_frame(lisp_frame *frame)
{
  LispObj fun = frame->savefn, pc = frame->savelr;
  int delta = 0;
  Dl_info info;
  char *spname;

  if ((fun == 0) || (fun == fulltag_misc)) {
    spname = "unknown ?";
#ifndef STATIC
    if (dladdr((void *)ptr_from_lispobj(pc), &info)) {
      spname = (char *)(info.dli_sname);
#ifdef DARWIN
      if (spname[-1] != '_') {
        --spname;
      }
#endif
    }
#endif
#ifdef PPC64
    Dprintf("(#x%016lX) #x%016lX : (subprimitive %s)", frame, pc, spname);
#else
    Dprintf("(#x%08X) #x%08X : (subprimitive %s)", frame, pc, spname);
#endif
  } else {
    if ((fulltag_of(fun) != fulltag_misc) ||
        (header_subtag(header_of(fun)) != subtag_function)) {
#ifdef PPC64
      Dprintf("(#x%016lX) #x%016lX : (not a function!)", frame, pc);
#else
      Dprintf("(#x%08X) #x%08X : (not a function!)", frame, pc);
#endif
    } else {
      LispObj code_vector = deref(fun, 1);
      
      if ((pc >= (code_vector+misc_data_offset)) &&
          (pc < ((code_vector+misc_data_offset)+(header_element_count(header_of(code_vector))<<2)))) {
        delta = (pc - (code_vector+misc_data_offset));
      }
#ifdef PPC64
      Dprintf("(#x%016lX) #x%016lX : %s + %d", frame, pc, print_lisp_object(fun), delta);
#else
      Dprintf("(#x%08X) #x%08X : %s + %d", frame, pc, print_lisp_object(fun), delta);
#endif
    }
  }
}


void
print_foreign_frame(void *frame)
{
#ifdef LINUX
  natural pc = (natural) (((eabi_c_frame *)frame)->savelr);
#endif
#ifdef DARWIN
  natural pc = (natural) (((c_frame *)frame)->savelr);
#endif
  Dl_info foreign_info;

#ifndef STATIC
  if (dladdr((void *)pc, &foreign_info)) {
    Dprintf(
#ifdef PPC64
"(#x%016lx) #x%016lX : %s + %d"
#else
"(#x%08x) #x%08X : %s + %d"
#endif
, frame, pc, foreign_info.dli_sname,
	    pc-((long)foreign_info.dli_saddr));
  } else {
#endif
    Dprintf(
#ifdef PPC64
"(#x%016X) #x%016X : foreign code (%s)"
#else
"(#x%08X) #x%08X : foreign code (%s)"
#endif
, frame, pc, "unknown");
#ifndef STATIC
  }
#endif
}


/* Walk frames from "start" to "end". 
   Say whatever can be said about foreign frames and lisp frames.
*/

void
walk_stack_frames(lisp_frame *start, lisp_frame *end) 
{
  lisp_frame *next;
  Dprintf("\n");
  while (start < end) {

    if (lisp_frame_p(start)) {
      print_lisp_frame(start);
    } else {
#ifdef DARWIN
      print_foreign_frame((c_frame *)start);
#else
      print_foreign_frame((eabi_c_frame *)start);
#endif
    }
    
    next = start->backlink;
    if (next == 0) {
      next = end;
    }
    if (next < start) {
      fprintf(dbgout, "Bad frame! (%x < %x)\n", next, start);
      break;
    }
    start = next;
  }
}

char *
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

void
walk_other_areas()
{
  TCR *start = (TCR *)get_tcr(true), *tcr = start->next;
  area *a;
  char *ilevel = interrupt_level_description(tcr);

  while (tcr != start) {
    a = tcr->cs_area;
    Dprintf("\n\n TCR = 0x%lx, cstack area #x%lx,  native thread ID = 0x%lx, interrupts %s", tcr, a,  tcr->native_thread_id, ilevel);
    walk_stack_frames((lisp_frame *) (a->active), (lisp_frame *) (a->high));
    tcr = tcr->next;
  }
}

void
plbt_sp(LispObj currentSP)
{
  area *cs_area;
  
{
    TCR *tcr = (TCR *)get_tcr(true);
    char *ilevel = interrupt_level_description(tcr);
    cs_area = tcr->cs_area;
    if ((((LispObj) ptr_to_lispobj(cs_area->low)) > currentSP) ||
        (((LispObj) ptr_to_lispobj(cs_area->high)) < currentSP)) {
      Dprintf("\nStack pointer [#x%lX] in unknown area.", currentSP);
    } else {
      fprintf(dbgout, "current thread: tcr = 0x%lx, native thread ID = 0x%lx, interrupts %s\n", tcr, tcr->native_thread_id, ilevel);
      walk_stack_frames((lisp_frame *) ptr_from_lispobj(currentSP), (lisp_frame *) (cs_area->high));
      walk_other_areas();
    }
  } 
}

  
void
plbt(ExceptionInformation *xp)
{
  plbt_sp(xpGPR(xp, sp));
}
    
