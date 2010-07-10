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

#include "constants.h"

/* MXCSR bits */

enum {
  MXCSR_IE_BIT,    /* invalid operation */
  MXCSR_DE_BIT,	   /* denormal */
  MXCSR_ZE_BIT,	   /* divide-by-zero */
  MXCSR_OE_BIT,	   /* overflow */
  MXCSR_UE_BIT,	   /* underflow */
  MXCSR_PE_BIT,	   /* precision */
  MXCSR_DAZ_BIT,   /* denorms-are-zero (not IEEE) */
  MXCSR_IM_BIT,	   /* invalid operation masked */
  MXCSR_DM_BIT,	   /* denormal masked */
  MXCSR_ZM_BIT,	   /* divide-by-zero masked */
  MXCSR_OM_BIT,	   /* overflow masked */
  MXCSR_UM_BIT,	   /* underflow masked */
  MXCSR_PM_BIT,	   /* precision masked */
  MXCSR_RC0_BIT,   /* rounding control bit 0 */
  MXCSR_RC1_BIT,   /* rounding control bit 1 */
  MXCSR_FZ_BIT	   /* flush-to-zero (not IEEE) */
};

#define MXCSR_STATUS_MASK ((1 << MXCSR_IE_BIT) | \
			   (1 << MXCSR_DE_BIT) | \
			   (1 << MXCSR_ZE_BIT) | \
			   (1 << MXCSR_OE_BIT) | \
			   (1 << MXCSR_UE_BIT) | \
			   (1 << MXCSR_PE_BIT))

#define MXCSR_CONTROL_AND_ROUNDING_MASK ((1<<MXCSR_IM_BIT) | \
                                         (1<<MXCSR_DM_BIT) | \
                                         (1<<MXCSR_ZM_BIT) | \
                                         (1<<MXCSR_OM_BIT) | \
                                         (1<<MXCSR_UM_BIT) | \
                                         (1<<MXCSR_PM_BIT) | \
                                         (1<<MXCSR_RC0_BIT) | \
                                         (1<<MXCSR_RC1_BIT))

#define MXCSR_CONTROL_MASK ((1<<MXCSR_IM_BIT) | \
                            (1<<MXCSR_DM_BIT) | \
                            (1<<MXCSR_ZM_BIT) | \
			    (1<<MXCSR_OM_BIT) | \
			    (1<<MXCSR_UM_BIT) | \
			    (1<<MXCSR_PM_BIT))

#define MXCSR_WRITE_MASK (~((1<<MXCSR_DAZ_BIT)|(1<<MXCSR_FZ_BIT)))

/* Bits in the xFLAGS register */
#define X86_CARRY_FLAG_BIT (0)
#define X86_PARITY_FLAG_BIT (2)
#define X86_AUX_CARRY_FLAG_BIT (4)
#define X86_ZERO_FLAG_BIT (6)
#define X86_SIGN_FLAG_BIT (7)
#define X86_DIRECTION_FLAG_BIT (10)
#define X86_OVERFLOW_FLAG_BIT (11)

#define STATIC_BASE_ADDRESS 0x00012000



