/*   Copyright (C) 2010 Clozure Associates */
/*   Copyright (C) 1994-2001 Digitool, Inc */
/*   This file is part of Clozure CL. */
 
/*   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public */
/*   License , known as the LLGPL and distributed with Clozure CL as the */
/*   file "LICENSE".  The LLGPL consists of a preamble and the LGPL, */
/*   which is distributed with Clozure CL as the file "LGPL".  Where these */
/*   conflict, the preamble takes precedence. */
 
/*   Clozure CL is referenced in the preamble as the "LIBRARY." */
 
/*   The LLGPL is also available online at */
/*   http://opensource.franz.com/preamble.html */


/* As of the 2005 edition of the ARM Architecture Reference Manual */
/* ("ARM ARM"), instructions I for which:        */
/* (logand i (logior (ash 255 20) (ash 15 4))) =  */
/*           (logior (ash 127 20) (ash 15 4))  is true are considered */
/* "architecturally undefined", e.g., unlikely to be implemented on */
/* future versions of the architecture.  I haven't seen anything that */
/* supersedes or contradicts this, but I'm not entirely sure that I would. */

uuo_base_opcode = ((127<<20)|(15<<4))
                        
/* Like most 32-bit ARM instructions, these instructions are only executed */
/* (and therefore only raise exceptions) if their condition field (in bits */
/* 28:31) is true.  We also have a 12-bit field at bit 20 and a 4-bit field */
/* at bit 0 in which to encode variable information.  This encoding uses */
/* the 4-bit field to describe the format of the 12-bit field, which can */
/* encode 12 bits of "error code", or 8 bits of code and a 4 bit register */
/* number, or 4 bits of code and 2 4-bit register numbers. */
        
uuo_format_nullary = 0          /* 12 bits of code */
uuo_format_unary = 1            /* 8 bits of info - NOT type info - 4-bit reg */
uuo_format_error_lisptag = 2    /* 2 bits of lisptag info, 4-bit reg */
uuo_format_error_fulltag = 3    /* 3 bits of fulltag info, 4 bit reg */

uuo_format_error_xtype = 4      /* 8 bits of extended type/subtag info, 4 bit reg */
uuo_format_binary = 7           /* 4 bits of code, r1, r0 */
uuo_format_nullary_error = 8    /* nullary, call out to lisp */
uuo_format_unary_error = 9      /* like unary, but call out to lisp */
uuo_format_cerror_lisptag = 10 /* continuable, lisptag, reg */
uuo_format_cerror_fulltag = 11 /* continuable, fulltag, reg */
uuo_format_cerror_xtype = 12   /* continuable, xtype, reg */
uuo_format_kernel_service = 13 /* 8 bits of info */  
uuo_format_ternary_error = 14   /* SLOT-UNBOUND only */              
uuo_format_binary_error = 15    /* binary format, call out to lisp */

/* Encode a UUO with cond = $1, format = $2, info = $3 */
define(`UUO',`
        .word (uuo_base_opcode|(($1)<<28)|($2)|(($3)<<8))
')
/* Nullary UUO with cond = $1, info = $2 */        
define(`nullaryUUO',`UUO($1,uuo_format_nullary,$2)')
define(`nullary_errorUUO',`UUO($1,uuo_format_nullary_error,$2)')
/* Simple (non-TYPE) unary uuo with cond = $1, reg = $2, info = $3 */
define(`unaryUUO',`UUO($1,uuo_format_unary,($2|($3<<4)))')
define(`unary_errorUUO',`UUO($1,uuo_format_unary_error,($2|($3<<4)))')

define(`binaryUUO',`UUO($1,uuo_format_binary,($2|($3<<4)|($4<<8)))')
define(`binary_errorUUO',`UUO($1,uuo_format_binary_error,($2|($3<<4)|($4<<8)))')

/* Simple type error (reg not lisptag), cond = $1, reg = $2, lisptag = $3 */
define(`uuo_error_reg_not_lisptag',`UUO($1,uuo_format_error_lisptag,$2|($3<<4))')
/* Likewise, for fulltag.  (Can distinguish between tag_list/fulltag_cons) */
define(`uuo_error_reg_not_fulltag',`UUO($1,uuo_format_error_fulltag,$2|($3<<4))')
/* As used here, an 'xtype' is an 8-bit value that's either a defined */
/* subtag/tag/lisptag value or some encoding of something like 'integer' */
define(`uuo_error_reg_not_xtype',`UUO($1,uuo_format_error_xtype,$2|($3<<4))')
/* Continuable type errors */
define(`uuo_cerror_reg_not_lisptag',`UUO($1,uuo_format_cerror_lisptag,$2|($3<<4))')
define(`uuo_cerror_reg_not_fulltag',`UUO($1,uuo_format_cerror_fulltag,$2|($3<<4))')
define(`uuo_cerror_reg_not_xtype',`UUO($1,uuo_format_cerror_xtype,$2|($3<<4))')
	
/* Nullary UUOs.  Define them as being conditional, even if the condition is */
/*  'al' (always). $1=cond, $2=8-bit-code */
define(`uuo_alloc_trap',`nullaryUUO($1,0)')
define(`uuo_error_wrong_nargs',`nullary_errorUUO($1,1)') /* can use CC field */
define(`uuo_gc_trap',`nullaryUUO($1,2)') /* probably unconditional */
define(`uuo_debug_trap',`nullaryUUO($1,3)')
define(`uuo_interrupt_now',`nullaryUUO($1,4)')
define(`uuo_suspend_now',`nullaryUUO($1,5)')

/* Unary UUOs */
define(`uuo_error_unbound',`unary_errorUUO($1,$2,0)')
define(`uuo_cerror_unbound',`unary_errorUUO($1,$2,1)')
define(`uuo_error_not_callable',`unary_errorUUO($1,$2,2)')
define(`uuo_tlb_too_small',`unaryUUO($1,$2,3)')
define(`uuo_error_no_throw_tag',`unary_errorUUO($1,$2,4)')
define(`uuo_error_udf_call',`unary_errorUUO($1,$2,5)')       
define(`uuo_error_udf',`unary_errorUUO($1,$2,6)')
        
/* Binary UUOs */
define(`uuo_error_vector_bounds',`binary_errorUUO($1,$2,$3,0)')
define(`uuo_error_array_bounds',`binary_errorUUO($1,$2,$3,1)')
define(`uuo_error_integer_divide_by_zero',`binary_errorUUO($1,$2,$3,2)')
define(`uuo_error_slot_unbound',`binary_errorUUO($1,$2,$3,3)')
define(`uuo_error_eep_unresolved',`binary_errorUUO($1,$2,$3,4)')        
define(`uuo_error_fpu_exception',`binary_errorUUO($1,$2,$3,5)')
define(`uuo_error_array_rank',`binary_errorUUO($1,$2,$3,6)')
define(`uuo_error_array_flags',`binary_errorUUO($1,$2,$3,7)')
        
/* This should never be generated (never be a legal instruction in a code
   vector); it should only be used by purify/impurify. */
define(`forward_marker',`UUO(al,uuo_format_unary,0xfff)')
/* Used by Mach exception return */
define(`uuo_pseudo_sigreturn',`UUO(al,uuo_format_unary,0xffe)')