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

/*
  MCL-PPC dcmd utilities.
*/

#include "lispdcmd.h"




void
display_buffer(char *buf)
{
  fprintf(stderr, "%s\n", buf);
}

int
Dprintf(const char *format, ...)
{
  char buf[512];
  va_list args;
  int res;

  va_start(args, format);
  res = vsnprintf(buf, sizeof(buf), format, args);
  if (res >= 0) {
    display_buffer(buf);
  }
  return res;
}

