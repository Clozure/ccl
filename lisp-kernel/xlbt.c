/*
   Copyright (C) 2005 Clozure Associates
   This file is part of OpenMCL.  

   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
   License , known as the LLGPL and distributed with OpenMCL as the
   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
   which is distributed with OpenMCL as the file "LGPL".  Where these
   conflict, the preamble takes precedence.  

   OpenMCL is referenced in the preamble as the "LIBRARY."

   The LLGPL is also available online at
   http://opensource.franz.com/preamble.html
*/

#include "lispdcmd.h"
#include <stdio.h>



void
print_lisp_frame(lisp_frame *frame)
{
  LispObj pc = frame->tra, fun=0;
  int delta = 0;

  if (pc == lisp_global(RET1VALN)) {
    pc = frame->xtra;
  }
#ifdef X8632
  if (fulltag_of(pc) == fulltag_tra) {
    if (*((unsigned char *)pc) == RECOVER_FN_OPCODE) {
      natural n = *((natural *)(pc + 1));
      fun = (LispObj)n;
    }
    if (fun && header_subtag(header_of(fun)) == subtag_function) {
      delta = pc - fun;
      Dprintf("(#x%08X) #x%08X : %s + %d", frame, pc, print_lisp_object(fun), delta);
      return;
    }
  }
  if (pc == 0) {
    fun = ((xcf *)frame)->nominal_function;
    Dprintf("(#x%08X) #x%08X : %s + ??", frame, pc, print_lisp_object(fun));
    return;
  }
#else
  if (tag_of(pc) == tag_tra) {
    if ((*((unsigned short *)pc) == RECOVER_FN_FROM_RIP_WORD0) &&
        (*((unsigned char *)(pc+2)) == RECOVER_FN_FROM_RIP_BYTE2)) {
      int sdisp = (*(int *) (pc+3));
      fun = RECOVER_FN_FROM_RIP_LENGTH+pc+sdisp;
    }
    if (fulltag_of(fun) == fulltag_function) {
      delta = pc - fun;
      Dprintf("(#x%016lX) #x%016lX : %s + %d", frame, pc, print_lisp_object(fun), delta);
      return;
    }
  }
  if (pc == 0) {
    fun = ((xcf *)frame)->nominal_function;
    Dprintf("(#x%016lX) #x%016lX : %s + ??", frame, pc, print_lisp_object(fun));
    return;
  }
#endif
}

Boolean
lisp_frame_p(lisp_frame *f)
{
  LispObj ra;

  if (f) {
    ra = f->tra;
    if (ra == lisp_global(RET1VALN)) {
      ra = f->xtra;
    }

#ifdef X8632
    if (fulltag_of(ra) == fulltag_tra) {
#else
    if (tag_of(ra) == tag_tra) {
#endif
      return true;
    } else if ((ra == lisp_global(LEXPR_RETURN)) ||
	       (ra == lisp_global(LEXPR_RETURN1V))) {
      return true;
    } else if (ra == 0) {
      return true;
    }
  }
  return false;
}

void
walk_stack_frames(lisp_frame *start, lisp_frame *end) 
{
  lisp_frame *next;
  Dprintf("\n");
  while (start < end) {

    if (lisp_frame_p(start)) {
      print_lisp_frame(start);
    } else {
      if (start->backlink) {
        fprintf(stderr, "Bogus  frame %lx\n", start);
      }
      return;
    }
    
    next = start->backlink;
    if (next == 0) {
      next = end;
    }
    if (next < start) {
      fprintf(stderr, "Bad frame! (%x < %x)\n", next, start);
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
plbt_sp(LispObj currentRBP)
{
  area *vs_area, *cs_area;
  
{
    TCR *tcr = (TCR *)get_tcr(true);
    char *ilevel = interrupt_level_description(tcr);
    vs_area = tcr->vs_area;
    cs_area = tcr->cs_area;
    if ((((LispObj) ptr_to_lispobj(vs_area->low)) > currentRBP) ||
        (((LispObj) ptr_to_lispobj(vs_area->high)) < currentRBP)) {
#ifdef X8664
      currentRBP = (LispObj) (tcr->save_rbp);
#else
      currentRBP = (LispObj) (tcr->save_ebp);
#endif
    }
    if ((((LispObj) ptr_to_lispobj(vs_area->low)) > currentRBP) ||
        (((LispObj) ptr_to_lispobj(vs_area->high)) < currentRBP)) {
      Dprintf("\nFramepointer [#x%lX] in unknown area.", currentRBP);
    } else {
      fprintf(stderr, "current thread: tcr = 0x%lx, native thread ID = 0x%lx, interrupts %s\n", tcr, tcr->native_thread_id, ilevel);
      walk_stack_frames((lisp_frame *) ptr_from_lispobj(currentRBP), (lisp_frame *) (vs_area->high));
      /*      walk_other_areas();*/
    }
  } 
}


void
plbt(ExceptionInformation *xp)
{
#ifdef X8632
  plbt_sp(xpGPR(xp,Iebp));
#else
  plbt_sp(xpGPR(xp,Irbp));
#endif
}
