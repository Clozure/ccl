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


        .syntax unified
        .arm	

	include(lisp.s)

	_beginfile

/* Force data from r0, size r1 into the icache */        
_exportfn(C(flush_cache_lines))
        __ifdef(`LINUX')
        __(add r1,r1,r0)
        __(mov r2,#0)           /* options.  Pass as 0 until we know better */
        __(mov r12,r7)          /* preserve r7 ;  r12 saved by syscall */
        __(mov r7,#0x0f0000)     /* __ARM_NR_cacheflush */
        __(add r7,r7,#2)
	__(svc #0)
        __(mov r7,r12)
        __endif
        __ifdef(`DARWIN')
        __(mov r3,#0)
        __(mov r12,#0x80000000)
        __(svc #0)
        __endif   
	__(bx lr)

_exportfn(C(touch_page))
        __(str r0,[r0,#0])
        __(mov r1,#0)
        __(str r1,[r0,#0])
        __(mov r0,#1)
        .globl C(touch_page_end)
C(touch_page_end):      
        __(bx lr)
_endfn        
                                
_exportfn(C(current_stack_pointer))
	__(mov r0,sp)
	__(bx lr)
_endfn
	
_exportfn(C(count_leading_zeros))
        __(clz r0,r0)
	__(bx lr)
_endfn

_exportfn(C(noop))
	__(bx lr)
_endfn





/* Atomically store new value (r2) in *r0, if old value == expected (r1). */
/* Return actual old value. */
        .globl C(arm_architecture_version)
_exportfn(C(store_conditional))
0:      __(ldrex r3,[r0])
        __(cmp r3,r1)
        __(bne 1f)
        __(strex ip,r2,[r0])
        __(cmp ip,#0)
        __(bne 0b)
        __(b 2f)
1:      __(adr ip,3f)
        __(ldr ip,[ip])
        __(ldr ip,[ip])
        __(cmp ip,#7)
        __(blt 2f)
        .long 0xf57ff01f
2:      __(mov r0,r3)
        __(bx lr)   
3:      .long C(arm_architecture_version)                     
_endfn

/* Atomically store new_value(r1) in *r0 ;  return previous contents */
/* of *r0. */

_exportfn(C(atomic_swap))
        __(mov r2,r0)
0:      __(ldrex r0,[r2])
        __(strex r3,r1,[r2])
        __(cmp r3,#0)
        __(bne 0b)        
        __(bx lr)
_endfn

/* Logior the value in *r0 with the value in r1 (presumably a bitmask with exactly 1 */
/* bit set.)  Return non-zero if any of the bits in that bitmask were already set. */
        
_exportfn(C(atomic_ior))
        __(stmdb sp!,{r4,lr})
0:      __(ldrex r2,[r0])
        __(orr r3,r2,r1)
        __(strex r4,r3,[r0])
        __(cmp r4,#0)
        __(bne 0b)
        __(mov r0,r2)
        __(ldmia sp!,{r4,pc})
_endfn


/* Logand the value in *r0 with the value in r1 (presumably a bitmask with exactly 1 */
/* bit set.)  Return the value now in *r0 (for some value of "now" */

_exportfn(C(atomic_and))
0:      __(ldrex r2,[r0])
        __(and r2,r2,r1)
        __(strex r3,r2,[r0])
        __(cmp r3,#0)
        __(bne 0b)
        __(mov r0,r2)
        __(bx lr)
_endfn
                
	
        __ifdef(`DARWIN')
_exportfn(C(enable_fp_exceptions))
        __(.long 0)
        __(bx lr)
_endfn
        
_exportfn(C(disable_fp_exceptions))
        __(.long 0)
        __(bx lr)
_endfn

_exportfn(C(pseudo_sigreturn))
	__(uuo_pseudo_sigreturn())
	__(b C(pseudo_sigreturn))
_endfn
        __endif
	
_exportfn(C(save_fp_context))
        __(uuo_debug_trap(al))
_endfn        	
_exportfn(C(restore_fp_context))
        __(uuo_debug_trap(al))
_endfn        	
_exportfn(C(put_vector_registers))
        __(uuo_debug_trap(al))
_endfn        	
_exportfn(C(get_vector_registers))
        __(uuo_debug_trap(al))
_endfn
	
        __ifdef(`ANDROID')
_exportfn(rt_sigprocmask)
        __(stmdb sp!,{r7,lr})
        __(mov r7,#175)
        __(svc #0)
        __(ldmia sp!,{r7,pc})
_endfn
        __endif
        

        __ifdef(`DARWIN')
/* divide the 64-bit unsigned integer in r0/r1 by the 64-bit unsigned
   integer in r2/r3; return the 64-bit quotient in r0/r1 and the 64-bit
   remainder in r2/r3.  Implement this in terms of the libgcc function: 

   unsigned long long __udivti3 (unsigned long long a, 
                                 unsigned long long b, 
                                 unsigned long long *c)
*/        
_exportfn(C(__aeabi_uldivmod))
        __(stmdb sp!,{r7,lr})
        __(mov r7,sp)
        __(sub sp,sp,#8)
        __(mov ip,sp)
        __(push1(ip,sp))
        __(push1(ip,sp))
        __(bl C(__udivmoddi4))
        __(add sp,sp,#8)
        __(ldmia sp!,{r2,r3})
        __(ldmia sp!,{r7,pc})
_endfn                
        __endif

_exportfn(call_handler_on_main_stack)
        __(ldr ip,[sp])
        __(mov lr,r3)
        __(mov sp,r1)
        __(bx ip)
_endfn                

/* zero N (r1) dnodes, starting at the dnode-aligned address in r0 */
_exportfn(C(zero_dnodes))
        __(cmp r1,#0)
        __(adr r2,2f)
        __(fldd d0,[r2,#0])
        __(b 1f)
0:      __(subs r1,r1,#1)
        __(fstd d0,[r0])
        __(add r0,r0,#dnode_size)        
1:      __(bne 0b)
        __(bx lr)
        .align 3
2:      .long 0
        .long 0        
_endfn                        
                                
	_endfile

