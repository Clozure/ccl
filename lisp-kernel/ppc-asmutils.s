/*   Copyright (C) 2009 Clozure Associates */
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


	

	include(lisp.s)

	_beginfile
/*  Zero R4 cache lines, starting at address in R3.  Each line is assumed to be */
/* R5 bytes wide. */
_exportfn(C(zero_cache_lines))
	__(cmpri(cr0,r4,0))
	__(mtctr r4)
	__(beqlr)
1:
	__(DCBZL(0,r3))
	__(add r3,r3,r5)
	__(bdnz 1b)
	__(blr)
_endfn

/*  Flush R4 cache lines, starting at address in R3.  Each line is */
/* assumed to be R5 bytes wide. */
_exportfn(C(flush_cache_lines))
	__(cmpri(cr0,r4,0))
	__(mtctr r4)
        __(mr r6,r3)
	__(beqlr)
1:
	__(dcbst 0,r3)
        __(add r3,r3,r5)
        __(bdnz 1b)
	__(sync)                /* wait until dcbst's get to memory */
        __(mr r3,r6)
        __(mtctr r4)
2:      
	__(icbi 0,r3)
	__(add r3,r3,r5)
	__(bdnz 2b)
        __(sync)
	__(isync)
	__(blr)
/* The strange reference to "exp" is supposed to force the kernel to */
/* load libm, so lisp code can use it.   Under Darwin, the functionality */
/* of libm is contained in libsystem, along with libc & everything else. */

        __ifndef([DARWIN])
        .data
        __ifdef([PPC64])
        .quad exp
        __else
        .long exp
        __endif
        .text        
        __endif
_endfn

_exportfn(C(touch_page))
        __(str(r3,0(r3)))
        __(li r4,0)
        __(str(r4,0(r3)))
        __(li r3,1) /* can't assume that low 32 bits of r3 are non-zero */
        .globl C(touch_page_end)
C(touch_page_end):
        __(blr)
_endfn
                                
_exportfn(C(current_stack_pointer))
	__(mr r3,sp)
	__(blr)
_endfn
	
_exportfn(C(count_leading_zeros))
        __ifdef([PPC64])
        __(cntlzd r3,r3)
        __else
	__(cntlzw r3,r3)
        __endif
	__(blr)
_endfn

_exportfn(C(noop))
	__(blr)
_endfn

_exportfn(C(set_fpscr))
	__(stru(sp,-32(sp)))
	__(stw r3,12(sp))
	__(lfd f0,8(sp))
	__(mtfsf 0xff,f0)
	__(la sp,32(sp))
	__(blr)
_endfn


_exportfn(C(get_fpscr))
	__(stru(sp,-32(sp)))
        __(mffs f0)
        __(stfd f0,8(sp))
        __(lwz r3,12(sp))
	__(la sp,32(sp))
	__(blr)
_endfn
                

/* The Linux kernel is constantly enabling and disabling the FPU and enabling */
/* FPU exceptions.  We can't touch the FPU without turning off the FPSCR[FEX] */
/* bit and we can't turn off the FPSCR[FEX] bit without touching the FPU. */
/* Force a distinguished exception, and let the handler for that exception */
/* zero the fpscr in its exception context. */

_exportfn(C(zero_fpscr))
	__(uuo_zero_fpscr())
	__(blr)
_endfn
	
	
_exportfn(C(save_fp_context))
	__(subi r4,r3,8)
	__(stfdu f0,8(r4))
	__(stfdu f1,8(r4))
	__(stfdu f2,8(r4))
	__(stfdu f3,8(r4))
	__(stfdu f4,8(r4))
	__(stfdu f5,8(r4))
	__(stfdu f6,8(r4))
	__(stfdu f7,8(r4))
	__(stfdu f8,8(r4))
	__(stfdu f9,8(r4))
	__(stfdu f10,8(r4))
	__(stfdu f11,8(r4))
	__(stfdu f12,8(r4))
	__(stfdu f13,8(r4))
	__(stfdu f14,8(r4))
	__(stfdu f15,8(r4))
	__(stfdu f16,8(r4))
	__(stfdu f17,8(r4))
	__(stfdu f18,8(r4))
	__(stfdu f19,8(r4))
	__(stfdu f20,8(r4))
	__(stfdu f21,8(r4))
	__(stfdu f22,8(r4))
	__(stfdu f23,8(r4))
	__(stfdu f24,8(r4))
	__(stfdu f25,8(r4))
	__(stfdu f26,8(r4))
	__(stfdu f27,8(r4))
	__(stfdu f28,8(r4))
	__(stfdu f29,8(r4))
	__(stfdu f30,8(r4))
	__(stfdu f31,8(r4))
	__(mffs f0)
	__(stfd f0,8(r4))
	__(lfd f0,0(r3))
	__(blr)
_endfn

_exportfn(C(restore_fp_context))
	__(mr r4,r3)
	__(lfdu f1,8(r4))
	__(lfdu f2,8(r4))
	__(lfdu f3,8(r4))
	__(lfdu f4,8(r4))
	__(lfdu f5,8(r4))
	__(lfdu f6,8(r4))
	__(lfdu f7,8(r4))
	__(lfdu f8,8(r4))
	__(lfdu f9,8(r4))
	__(lfdu f10,8(r4))
	__(lfdu f11,8(r4))
	__(lfdu f12,8(r4))
	__(lfdu f13,8(r4))
	__(lfdu f14,8(r4))
	__(lfdu f15,8(r4))
	__(lfdu f16,8(r4))
	__(lfdu f17,8(r4))
	__(lfdu f18,8(r4))
	__(lfdu f19,8(r4))
	__(lfdu f20,8(r4))
	__(lfdu f21,8(r4))
	__(lfdu f22,8(r4))
	__(lfdu f23,8(r4))
	__(lfdu f24,8(r4))
	__(lfdu f25,8(r4))
	__(lfdu f26,8(r4))
	__(lfdu f27,8(r4))
	__(lfdu f28,8(r4))
	__(lfdu f29,8(r4))
	__(lfdu f30,8(r4))
	__(lfdu f31,8(r4))
	__(lfd f0,8(r4))
	__(mtfsf 0xff,f0)
	__(lfd f0,0(r3))
	__(blr)
_endfn



/* Atomically store new value (r5) in *r3, if old value == expected. */
/* Return actual old value. */

_exportfn(C(store_conditional))
        __(mr r6,r3)
1:      __(lrarx(r3,0,r6))
        __(cmpw r3,r4)
        __(bne- 2f)
        __(strcx(r5,0,r6))
        __(bne- 1b)
        __(isync)
        __(blr)
2:      __(li r0,RESERVATION_DISCHARGE)
        __(strcx(r0,0,r0))
        __(blr)
_endfn

/* Atomically store new_value(r4) in *r3 ;  return previous contents */
/* of *r3. */

_exportfn(C(atomic_swap))
        __(sync)
1:	__(lrarx(r5,0,r3))
	__(strcx(r4,0,r3))
	__(bne- 1b)
	__(isync)
	__(mr r3,r5)
	__(blr)
_endfn

/* Logior the value in *r3 with the value in r4 (presumably a bitmask with exactly 1 */
/* bit set.)  Return non-zero if any of the bits in that bitmask were already set. */
        
_exportfn(C(atomic_ior))
        __(sync)
1:	__(lrarx(r5,0,r3))
        __(or r6,r4,r5)
	__(strcx(r6,0,r3))
	__(bne- 1b)
	__(isync)
	__(and r3,r4,r5)
	__(blr)
_endfn


/* Logand the value in *r3 with the value in r4 (presumably a bitmask with exactly 1 */
/* bit set.)  Return the value now in *r3 (for some value of "now" */

_exportfn(C(atomic_and))
        __(sync)
1:	__(lrarx(r5,0,r3))
        __(and r6,r4,r5)
	__(strcx(r6,0,r3))
	__(bne- 1b)
	__(isync)
	__(mr r3,r6)
	__(blr)
_endfn
                
	
        __ifdef([DARWIN])
_exportfn(C(enable_fp_exceptions))
        __(.long 0)
        __(blr)
_endfn
        
_exportfn(C(disable_fp_exceptions))
        __(.long 0)
        __(blr)
_endfn

_exportfn(C(pseudo_sigreturn))
	__(.long 0)
	__(b C(pseudo_sigreturn))
_endfn
        __endif
	
/* Copy all 32 Altivec registers (+ VSCR & VRSAVE) to the buffer */
/* in r3.  If the buffer's non-NULL, it's aligned and big enough, */
/* and Altivec is present. */

_exportfn(C(put_vector_registers))
	__(cmpri(r3,0))
	__(li r4,0)
	__(beqlr)
	__(stvx v0,r3,r4)
	__(la r4,16(r4))
	__(stvx v1,r3,r4)
	__(la r4,16(r4))
	__(stvx v2,r3,r4)
	__(la r4,16(r4))
	__(stvx v3,r3,r4)
	__(la r4,16(r4))
	__(stvx v4,r3,r4)
	__(la r4,16(r4))
	__(stvx v5,r3,r4)
	__(la r4,16(r4))
	__(stvx v6,r3,r4)
	__(la r4,16(r4))
	__(stvx v7,r3,r4)
	__(la r4,16(r4))
	__(stvx v8,r3,r4)
	__(la r4,16(r4))
	__(stvx v9,r3,r4)
	__(la r4,16(r4))
	__(stvx v10,r3,r4)
	__(la r4,16(r4))
	__(stvx v11,r3,r4)
	__(la r4,16(r4))
	__(stvx v12,r3,r4)
	__(la r4,16(r4))
	__(stvx v13,r3,r4)
	__(la r4,16(r4))
	__(stvx v14,r3,r4)
	__(la r4,16(r4))
	__(stvx v15,r3,r4)
	__(la r4,16(r4))
	__(stvx v16,r3,r4)
	__(la r4,16(r4))
	__(stvx v17,r3,r4)
	__(la r4,16(r4))
	__(stvx v18,r3,r4)
	__(la r4,16(r4))
	__(stvx v19,r3,r4)
	__(la r4,16(r4))
	__(stvx v20,r3,r4)
	__(la r4,16(r4))
	__(stvx v21,r3,r4)
	__(la r4,16(r4))
	__(stvx v22,r3,r4)
	__(la r4,16(r4))
	__(stvx v23,r3,r4)
	__(la r4,16(r4))
	__(stvx v24,r3,r4)
	__(la r4,16(r4))
	__(stvx v25,r3,r4)
	__(la r4,16(r4))
	__(stvx v26,r3,r4)
	__(la r4,16(r4))
	__(stvx v27,r3,r4)
	__(la r4,16(r4))
	__(stvx v28,r3,r4)
	__(la r4,16(r4))
	__(stvx v29,r3,r4)
	__(la r4,16(r4))
	__(stvx v30,r3,r4)
	__(la r4,16(r4))
	__(stvx v31,r3,r4)
	__(la r4,16(r4))
	__(mfvscr v0)
	__(stvx v0,r3,r4)
	__(mfspr r5,256)
	__(stw r5,8(r4))
	__(blr)
_endfn

_exportfn(C(get_vector_registers))
	__(cmpri(r3,0))
	__(li r4,32*16)
	__(beqlr)
	__(lvx v0,r3,r4)
	__(mtvscr v0)
	__(lwz r5,8(r4))
	__(mtspr 256,r5)
	__(la r4,-16(r4))
	__(lvx v31,r3,r4)
	__(la r4,-16(r4))
	__(lvx v30,r3,r4)
	__(la r4,-16(r4))
	__(lvx v29,r3,r4)
	__(la r4,-16(r4))
	__(lvx v28,r3,r4)
	__(la r4,-16(r4))
	__(lvx v27,r3,r4)
	__(la r4,-16(r4))
	__(lvx v26,r3,r4)
	__(la r4,-16(r4))
	__(lvx v25,r3,r4)
	__(la r4,-16(r4))
	__(lvx v24,r3,r4)
	__(la r4,-16(r4))
	__(lvx v23,r3,r4)
	__(la r4,-16(r4))
	__(lvx v22,r3,r4)
	__(la r4,-16(r4))
	__(lvx v21,r3,r4)
	__(la r4,-16(r4))
	__(lvx v20,r3,r4)
	__(la r4,-16(r4))
	__(lvx v19,r3,r4)
	__(la r4,-16(r4))
	__(lvx v18,r3,r4)
	__(la r4,-16(r4))
	__(lvx v17,r3,r4)
	__(la r4,-16(r4))
	__(lvx v16,r3,r4)
	__(la r4,-16(r4))
	__(lvx v15,r3,r4)
	__(la r4,-16(r4))
	__(lvx v14,r3,r4)
	__(la r4,-16(r4))
	__(lvx v13,r3,r4)
	__(la r4,-16(r4))
	__(lvx v12,r3,r4)
	__(la r4,-16(r4))
	__(lvx v11,r3,r4)
	__(la r4,-16(r4))
	__(lvx v10,r3,r4)
	__(la r4,-16(r4))
	__(lvx v9,r3,r4)
	__(la r4,-16(r4))
	__(lvx v8,r3,r4)
	__(la r4,-16(r4))
	__(lvx v7,r3,r4)
	__(la r4,-16(r4))
	__(lvx v6,r3,r4)
	__(la r4,-16(r4))
	__(lvx v5,r3,r4)
	__(la r4,-16(r4))
	__(lvx v4,r3,r4)
	__(la r4,-16(r4))
	__(lvx v3,r3,r4)
	__(la r4,-16(r4))
	__(lvx v2,r3,r4)
	__(la r4,-16(r4))
	__(lvx v1,r3,r4)
	__(la r4,-16(r4))
	__(lvx v0,r3,r4)
	__(blr)
_endfn

/* Some versions of Linux don't implement madvise().  That's */
/* not catastrophic, but some versions of glibc will make a */
/* big deal out of that at link time.  This is here to try */
/* to fool those versions of glibc. */

        __ifdef([LINUX])
	.globl set_errno
_exportfn(C(madvise))
	__(li r0,205)	/* _NR_madvise; see /usr/include/asm/unistd.h */
	__(sc)
	__(bnslr)
	__(b set_errno)
_endfn
        __endif

	_endfile
