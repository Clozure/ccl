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

#ifndef X86_UTILS_H
#define X86_UTILS_H

extern LispObj tra_function(LispObj tra);
extern int tra_offset(LispObj tra);
extern int ptr_in_area(char *p, area* a);
extern area *in_any_consing_area(LispObj thing);

static inline LispObj
function_to_function_vector(LispObj f)
{
#ifdef X8664
  return f - fulltag_function + fulltag_misc;
#else
  return f;
#endif
}

static inline int
tra_p(LispObj thing)
{
#ifdef X8664
  return tag_of(thing) == tag_tra;
#else
  return fulltag_of(thing) == fulltag_tra;
#endif
}

static inline int
functionp(LispObj f)
{
#ifdef X8664
  return fulltag_of(f) == fulltag_function;
#else
  return fulltag_of(f) == fulltag_misc &&
    header_subtag(header_of(f)) == subtag_function;
#endif
}

#endif
