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

#ifndef __kernel_globals__
#define __kernel_globals__
#include "area.h"


extern area *nilreg_area, *tenured_area, *g2_area, *g1_area, *managed_static_area, *readonly_area, *static_cons_area;
extern area *all_areas;
extern int cache_block_size;







#endif /* __kernel_globals__ */
