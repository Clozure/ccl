/*   Copyright (C) 2009 Clozure Associates */
/*   Copyright (C) 1994-2001 Digitool, Inc */
/*   This file is part of Clozure CL.  */

/*   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public */
/*   License , known as the LLGPL and distributed with Clozure CL as the */
/*   file "LICENSE".  The LLGPL consists of a preamble and the LGPL, */
/*   which is distributed with Clozure CL as the file "LGPL".  Where these */
/*   conflict, the preamble takes precedence.   */

/*   Clozure CL is referenced in the preamble as the "LIBRARY." */

/*   The LLGPL is also available online at */
/*   http://opensource.franz.com/preamble.html */



	include(lisp.s)
	_beginfile

	.globl _SPmkcatch1v
	.globl _SPnthrow1value


/* This is called from a c-style context and calls a lisp function. */
/* This does the moral equivalent of */
/*   (loop  */
/*	(let* ((fn (%function_on_top_of_lisp_stack))) */
/*	  (if fn */
/*           (catch %toplevel-catch% */
/*	       (funcall fn)) */
/*            (return nil)))) */

_exportfn(toplevel_loop)
	__(mflr imm0)
        __ifdef(`POWEROPENABI')
	 __(str(imm0,c_frame.savelr(sp)))
        __else
	 __(str(imm0,eabi_c_frame.savelr(sp)))
        __endif
	__(b local_label(test))
local_label(loop):
	__(ref_nrs_value(arg_z,toplcatch))
	__(bl _SPmkcatch1v)
	__(b local_label(test))			/* cleanup address, not really a branch */

	__(set_nargs(0))
	__(bl _SPfuncall)
	__(li arg_z,nil_value)
	__(li imm0,fixnum_one)
	__(bl _SPnthrow1value)
local_label(test):
	__(ldr(temp0,0(vsp)))
	__(cmpri(cr0,temp0,nil_value))
	__(bne cr0,local_label(loop))
local_label(back_to_c):
        __ifdef(`POWEROPENABI')
	 __(ldr(imm0,c_frame.savelr(sp)))
        __else
	 __(ldr(imm0,eabi_c_frame.savelr(sp)))
        __endif
	__(mtlr imm0)
	__(blr)
	_endfn


/* This sucker gets called with R3 pointing to the current TCR. */
/* r4 is 0 if we want to start the whole thing rolling, */
/* non-zero if we want to reset the current process */
/* by throwing to toplevel */

	.globl _SPreset
_exportfn(C(start_lisp))
	__(mflr r0)
        __ifdef(`POWEROPENABI')
	 __(str(r0,c_frame.savelr(sp)))
         __ifdef(`rTOC')
          __(str(rTOC,c_frame.savetoc(sp)))
         __endif
	 __(stru(sp,-(stack_align(c_frame.minsiz+(32*node_size)))(sp)))
         __(str(r13,c_frame.minsiz+(0*node_size)(sp)))
         __(str(r14,c_frame.minsiz+(1*node_size)(sp)))
         __(str(r15,c_frame.minsiz+(2*node_size)(sp)))
         __(str(r16,c_frame.minsiz+(3*node_size)(sp)))
         __(str(r17,c_frame.minsiz+(4*node_size)(sp)))
         __(str(r18,c_frame.minsiz+(5*node_size)(sp)))
         __(str(r19,c_frame.minsiz+(6*node_size)(sp)))
         __(str(r20,c_frame.minsiz+(7*node_size)(sp)))
         __(str(r21,c_frame.minsiz+(8*node_size)(sp)))
         __(str(r22,c_frame.minsiz+(9*node_size)(sp)))
         __(str(r23,c_frame.minsiz+(10*node_size)(sp)))
         __(str(r24,c_frame.minsiz+(11*node_size)(sp)))
         __(str(r25,c_frame.minsiz+(12*node_size)(sp)))
         __(str(r26,c_frame.minsiz+(13*node_size)(sp)))
         __(str(r27,c_frame.minsiz+(14*node_size)(sp)))
         __(str(r28,c_frame.minsiz+(15*node_size)(sp)))
         __(str(r29,c_frame.minsiz+(16*node_size)(sp)))
         __(str(r30,c_frame.minsiz+(17*node_size)(sp)))
         __(str(r31,c_frame.minsiz+(18*node_size)(sp)))
	 __(stfd fp_s32conv,c_frame.minsiz+(22*node_size)(sp))
        __else
	 __(str(r0,eabi_c_frame.savelr(sp)))
	 __(stru(sp,-(eabi_c_frame.minsiz+(32*node_size))(sp)))
         __(str(r13,eabi_c_frame.minsiz+(0*node_size)(sp)))
         __(str(r14,eabi_c_frame.minsiz+(1*node_size)(sp)))
         __(str(r15,eabi_c_frame.minsiz+(2*node_size)(sp)))
         __(str(r16,eabi_c_frame.minsiz+(3*node_size)(sp)))
         __(str(r17,eabi_c_frame.minsiz+(4*node_size)(sp)))
         __(str(r18,eabi_c_frame.minsiz+(5*node_size)(sp)))
         __(str(r19,eabi_c_frame.minsiz+(6*node_size)(sp)))
         __(str(r20,eabi_c_frame.minsiz+(7*node_size)(sp)))
         __(str(r21,eabi_c_frame.minsiz+(8*node_size)(sp)))
         __(str(r22,eabi_c_frame.minsiz+(9*node_size)(sp)))
         __(str(r23,eabi_c_frame.minsiz+(10*node_size)(sp)))
         __(str(r24,eabi_c_frame.minsiz+(11*node_size)(sp)))
         __(str(r25,eabi_c_frame.minsiz+(12*node_size)(sp)))
         __(str(r26,eabi_c_frame.minsiz+(13*node_size)(sp)))
         __(str(r27,eabi_c_frame.minsiz+(14*node_size)(sp)))
         __(str(r28,eabi_c_frame.minsiz+(15*node_size)(sp)))
         __(str(r29,eabi_c_frame.minsiz+(16*node_size)(sp)))
         __(str(r30,eabi_c_frame.minsiz+(17*node_size)(sp)))
         __(str(r31,eabi_c_frame.minsiz+(18*node_size)(sp)))
	 __(stfd fp_s32conv,eabi_c_frame.minsiz+(22*node_size)(sp))
        __endif
	__(mr rcontext,r3)
	__(lwi(r30,0x43300000))
	__(lwi(r31,0x80000000))
        __ifdef(`POWEROPENABI')
	 __(stw r30,c_frame.minsiz+(20*node_size)(sp))
	 __(stw r31,c_frame.minsiz+(20*node_size)+4(sp))
	 __(lfd fp_s32conv,c_frame.minsiz+(20*node_size)(sp))
	 __(stfd fp_zero,c_frame.minsiz+(20*node_size)(sp))
        __else                
 	 __(stw r30,eabi_c_frame.minsiz+(20*node_size)(sp))
	 __(stw r31,eabi_c_frame.minsiz+(20*node_size)+4(sp))
	 __(lfd fp_s32conv,eabi_c_frame.minsiz+(20*node_size)(sp))
	 __(stfd fp_zero,eabi_c_frame.minsiz+(20*node_size)(sp))
        __endif
	__(lfs fp_zero,lisp_globals.short_float_zero(0))
	__(lfd f0,tcr.lisp_fpscr(rcontext))
        __(mtfsf 0xff,f0)
	__(li rzero,0)
	__(mr save0,rzero)
	__(mr save1,rzero)
	__(mr save2,rzero)
	__(mr save3,rzero)
	__(mr save4,rzero)
	__(mr save5,rzero)
	__(mr save6,rzero)
	__(mr save7,rzero)
	__(mr arg_z,rzero)
	__(mr arg_y,rzero)
	__(mr arg_x,rzero)
	__(mr temp0,rzero)
	__(mr temp1,rzero)
	__(mr temp2,rzero)
	__(mr temp3,rzero)
	__(li loc_pc,0)
	__(li fn,0)
	__(cmpri(cr0,r4,0))
	__(mtxer rzero)  /* start lisp with the overflow bit clear */
	__(ldr(vsp,tcr.save_vsp(rcontext)))
	__(ldr(tsp,tcr.save_tsp(rcontext)))
	__(ldr(allocptr,tcr.save_allocptr(rcontext)))
	__(ldr(allocbase,tcr.save_allocbase(rcontext)))
        __(li imm0,TCR_STATE_LISP)
        __(str(imm0,tcr.valence(rcontext)))
	__(bne cr0,1f)
	__(bl toplevel_loop)
	__(b 2f)
1:
	__(bl _SPreset)
2:
	__(str(allocptr,tcr.save_allocptr(rcontext)))
	__(str(allocbase,tcr.save_allocbase(rcontext)))
	__(str(tsp,tcr.save_tsp(rcontext)))
	__(str(vsp,tcr.save_vsp(rcontext)))
        __(li imm0,TCR_STATE_FOREIGN)
        __(str(imm0,tcr.valence(rcontext)))
        __ifdef(`POWEROPENABI')
         __(ldr(r13,c_frame.minsiz+(0*node_size)(sp)))
         __(ldr(r14,c_frame.minsiz+(1*node_size)(sp)))
         __(ldr(r15,c_frame.minsiz+(2*node_size)(sp)))
         __(ldr(r16,c_frame.minsiz+(3*node_size)(sp)))
         __(ldr(r17,c_frame.minsiz+(4*node_size)(sp)))
         __(ldr(r18,c_frame.minsiz+(5*node_size)(sp)))
         __(ldr(r19,c_frame.minsiz+(6*node_size)(sp)))
         __(ldr(r20,c_frame.minsiz+(7*node_size)(sp)))
         __(ldr(r21,c_frame.minsiz+(8*node_size)(sp)))
         __(ldr(r22,c_frame.minsiz+(9*node_size)(sp)))
         __(ldr(r23,c_frame.minsiz+(10*node_size)(sp)))
         __(ldr(r24,c_frame.minsiz+(11*node_size)(sp)))
         __(ldr(r25,c_frame.minsiz+(12*node_size)(sp)))
         __(ldr(r26,c_frame.minsiz+(13*node_size)(sp)))
         __(ldr(r27,c_frame.minsiz+(14*node_size)(sp)))
         __(ldr(r28,c_frame.minsiz+(15*node_size)(sp)))
         __(ldr(r29,c_frame.minsiz+(16*node_size)(sp)))
         __(ldr(r30,c_frame.minsiz+(17*node_size)(sp)))
         __(ldr(r31,c_frame.minsiz+(18*node_size)(sp)))
        __else
         __(ldr(r13,eabi_c_frame.minsiz+(0*node_size)(sp)))
         __(ldr(r14,eabi_c_frame.minsiz+(1*node_size)(sp)))
         __(ldr(r15,eabi_c_frame.minsiz+(2*node_size)(sp)))
         __(ldr(r16,eabi_c_frame.minsiz+(3*node_size)(sp)))
         __(ldr(r17,eabi_c_frame.minsiz+(4*node_size)(sp)))
         __(ldr(r18,eabi_c_frame.minsiz+(5*node_size)(sp)))
         __(ldr(r19,eabi_c_frame.minsiz+(6*node_size)(sp)))
         __(ldr(r20,eabi_c_frame.minsiz+(7*node_size)(sp)))
         __(ldr(r21,eabi_c_frame.minsiz+(8*node_size)(sp)))
         __(ldr(r22,eabi_c_frame.minsiz+(9*node_size)(sp)))
         __(ldr(r23,eabi_c_frame.minsiz+(10*node_size)(sp)))
         __(ldr(r24,eabi_c_frame.minsiz+(11*node_size)(sp)))
         __(ldr(r25,eabi_c_frame.minsiz+(12*node_size)(sp)))
         __(ldr(r26,eabi_c_frame.minsiz+(13*node_size)(sp)))
         __(ldr(r27,eabi_c_frame.minsiz+(14*node_size)(sp)))
         __(ldr(r28,eabi_c_frame.minsiz+(15*node_size)(sp)))
         __(ldr(r29,eabi_c_frame.minsiz+(16*node_size)(sp)))
         __(ldr(r30,eabi_c_frame.minsiz+(17*node_size)(sp)))
         __(ldr(r31,eabi_c_frame.minsiz+(18*node_size)(sp)))
        __endif
	__(li r3,nil_value)
        __ifdef(`POWEROPENABI')
	 __(lfd fp_zero,c_frame.minsiz+(20*node_size)(sp))
	 __(lfd fp_s32conv,c_frame.minsiz+(22*node_size)(sp))
	 __(ldr(r0,((stack_align(c_frame.minsiz+(32*node_size)))+c_frame.savelr)(sp)))
        __else
	 __(lfd fp_zero,eabi_c_frame.minsiz+(20*4)(sp))
	 __(lfd fp_s32conv,eabi_c_frame.minsiz+(22*4)(sp))
	 __(ldr(r0,(eabi_c_frame.minsiz+(32*node_size)+eabi_c_frame.savelr)(sp)))
        __endif
	__(mtlr r0)
	__(ldr(sp,0(sp)))
         __ifdef(`rTOC')
          __(ld rTOC,c_frame.savetoc(sp))
         __endif
	__(blr)

_exportfn(_SPsp_end)
	nop
	_endfile

