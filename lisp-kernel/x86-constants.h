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

#ifndef __x86_constants__
#define __x86_constants__ 1

#define TCR_FLAG_BIT_FOREIGN fixnumshift
#define TCR_FLAG_BIT_AWAITING_PRESET (fixnumshift+1)
#define TCR_FLAG_BIT_ALT_SUSPEND (fixnumshift+2)
#define TCR_FLAG_BIT_PROPAGATE_EXCEPTION (fixnumshift+3)
#define TCR_FLAG_BIT_SUSPEND_ACK_PENDING (fixnumshift+4)
#define TCR_FLAG_BIT_PENDING_EXCEPTION (fixnumshift+5)
#define TCR_FLAG_BIT_FOREIGN_EXCEPTION (fixnumshift+6)
#define TCR_FLAG_BIT_PENDING_SUSPEND (fixnumshift+7)
#define TCR_STATE_FOREIGN (1)
#define TCR_STATE_LISP    (0)
#define TCR_STATE_EXCEPTION_WAIT (2)
#define TCR_STATE_EXCEPTION_RETURN (4)

#ifdef X8664
#include "x86-constants64.h"
#else
#include "x86-constants32.h"
#endif

#define dnode_size (node_size*2)
#define dnode_shift (node_shift+1)

#define INTERRUPT_LEVEL_BINDING_INDEX (1)

/* FP exception mask bits */
#define MXCSR_IM_BIT (7)        /* invalid masked when set*/
#define MXCSR_DM_BIT (8)        /* denormals masked when set*/
#define MXCSR_ZM_BIT (9)        /* divide-by-zero masked when set */
#define MXCSR_OM_BIT (10)       /* overflow masked when set */
#define MXCSR_UM_BIT (11)       /* underflow masked when set */
#define MXCSR_PM_BIT (12)       /* precision masked when set */

/* Bits in the xFLAGS register */
#define X86_CARRY_FLAG_BIT (0)
#define X86_PARITY_FLAG_BIT (2)
#define X86_AUX_CARRY_FLAG_BIT (4)
#define X86_ZERO_FLAG_BIT (6)
#define X86_SIGN_FLAG_BIT (7)
#define X86_DIRECTION_FLAG_BIT (10)
#define X86_OVERFLOW_FLAG_BIT (11)

#define STATIC_BASE_ADDRESS 0x00012000


#endif /* __x86_constants__ */

