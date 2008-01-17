/*   Copyright (C) 1994-2001 Digitool, Inc */
/*   This file is part of OpenMCL. */
 
/*   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public */
/*   License , known as the LLGPL and distributed with OpenMCL as the */
/*   file "LICENSE".  The LLGPL consists of a preamble and the LGPL, */
/*   which is distributed with OpenMCL as the file "LGPL".  Where these */
/*   conflict, the preamble takes precedence. */
 
/*   OpenMCL is referenced in the preamble as the "LIBRARY." */
 
/*   The LLGPL is also available online at */
/*   http://opensource.franz.com/preamble.html */

	include(m4macros.m4)
        ifdef([PPC],[
         include(ppc-constants.s)
         include(ppc-macros.s)
	 include(ppc-uuo.s)
        ])
	ifdef([X86],[
         include(x86-constants.s)
         include(x86-macros.s)
	 include(x86-uuo.s)
	])

