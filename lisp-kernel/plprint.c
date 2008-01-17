/*
   Copyright (C) 1994-2001 Digitool, Inc
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


void
plprint(ExceptionInformation *xp, LispObj obj)
{
  if (lisp_nil == (LispObj) NULL) {
    fprintf(stderr,"can't find lisp NIL; lisp process not active process ?\n");
  } else {
    Dprintf("\n%s", print_lisp_object(obj));
  }
}

