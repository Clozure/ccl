/*
   Copyright (C) 2011 Clozure Associates
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
#include "x86-utils.h"

LispObj
tra_function(LispObj tra)
{
  LispObj f = 0;

#ifdef X8664
  if (tag_of(tra) == tag_tra) {
    if ((*((unsigned short *)tra) == RECOVER_FN_FROM_RIP_WORD0) &&
        (*((unsigned char *)(tra + 2)) == RECOVER_FN_FROM_RIP_BYTE2)) {
      int sdisp = (*(int *)(tra + RECOVER_FN_FROM_RIP_DISP_OFFSET));
      f = RECOVER_FN_FROM_RIP_LENGTH + tra + sdisp;
    }
  }
#else
  if (fulltag_of(tra) == fulltag_tra) {
    if (*((unsigned char *)tra) == RECOVER_FN_OPCODE) {
      natural n = *((natural *)(tra + 1));
      f = (LispObj)n;
    }
  }
#endif
  return f;
}

int
tra_offset(LispObj tra)
{
  LispObj f = tra_function(tra);
  int disp = 0;

  if (functionp(f))
    disp = tra - f;
  return disp;
}
