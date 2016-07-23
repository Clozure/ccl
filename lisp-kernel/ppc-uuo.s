/*
 * Copyright 1994-2009 Clozure Associates
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */



/* A uuo looks like:  */
/*  0      5 6                  15 16   20 21          27 28  31  */
/* +--------+-----------------------------+--------------+------+  */
/* |   0    |XXXXXXXXXXXXXXXXXXXX |  RB   |  <minor op>  |  11  |  */
/* +--------+-----------------------------+--------------+------+  */
/*  */
/* e.g., the major opcode (bits 0-5) is 0, the low 4 bits (bits 28-31)  */
/* have the value "11" decimal (that's tagged as an immediate as far  */
/* as lisp is concerned, a 7-bit opcode in bits 21-27, and the format  */
/* of bits 6-20 depend on the value of the minor opcode, though typically  */
/* bits 16-20 are used to specify a register value between 0 and 31.  */
/*  */
/* There are a few cases where bits 6-15 are also used to denote registers  */
/* (RT and RA, as in an X-form PPC instruction), some where bits 6-10 are  */
/* to be interpreted as a constant (error number or type code), and some  */
/* where bits 6-15 do so.  */
/*  */
/* Since C code is typically more interested in disassembling UUOs, the  */
/* full list of UUOs is in "uuo.h".  This file contains macros for creating  */
/* them.  */
/*  */
/* Of course, there -is- no such file as "uuo.h".  That's a stale comment.  */
/* For all anyone knows, so is this one.  */

UUO_TAG = 11
UUU_MINOR_SHIFT = 4
UUO_RB_SHIFT = 11
UUO_RA_SHIFT = 16
UUO_RT_SHIFT = 21

define(`rt_ra_uuo',`
	.long (UUO_TAG|(($1)<<UUU_MINOR_SHIFT)|(($3)<<UUO_RA_SHIFT)|(($2)<<UUO_RT_SHIFT))')

define(`rt_ra_rb_uuo',`
	.long (UUO_TAG|(($1)<<UUU_MINOR_SHIFT)|(($3)<<UUO_RA_SHIFT)|(($4)<<UUO_RB_SHIFT)|(($2)<<UUO_RT_SHIFT))')
	
define(`errnum_rb_uuo',`
	.long (UUO_TAG|(($1)<<UUU_MINOR_SHIFT)|(($2)<<UUO_RA_SHIFT)|(($3)<<UUO_RB_SHIFT))')
	
define(`errnum_ra_rb_uuo',` /* minorop,errnum,ra,rb */
	.long (UUO_TAG|(($1)<<UUU_MINOR_SHIFT)|(($2)<<UUO_RA_SHIFT)|(($3)<<UUO_RB_SHIFT)|((\errnum)<<UUO_RT_SHIFT))')
	
	
	
/* Signal an internal error - type error or whatever - with error   */
/* number (0-1023) and "register" argument.  */

define(`uuo_interr',`
	errnum_rb_uuo(11,$1,$2)')
	
/* As above, but make the error continuable.  (A branch presumably  */
/* follows the UUO opcode.)  */

define(`uuo_intcerr',`
	errnum_rb_uuo(12,$1,$2)')


/* Signal an error with a much smaller error number (0-31) and  */
/* two "register" fields.  */

define(`uuo_interr2',`
	errnum_ra_rb_uuo(13,$1,$2,$3)')
	
/* Continuably ....  */

define(`uuo_intcerr2',`
	errnum_ra_rb_uuo(14,$1,$2,$3)')

	

/* A distinguished UUO: the handler should zero the FPSCR  */
define(`uuo_zero_fpscr',`
	rt_ra_rb_uuo(25,0,0,0)')
