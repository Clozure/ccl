/*
   Copyright (C) 2005 Clozure Associates
   This file is part of OpenMCL.  

   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
   License , known as the LLGPL and distributed with OpenMCL as the
   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
   which is distributed with OpenMCL as the file "LGPL".  Where these
   conflict, the preamble takes precedence.  

   OpenMCL is referenced in the preamble as the "LIBRARY."

   The LLGPL is also available online at
   http://opensource.franz.com/preamble.html
*/

#ifndef X86_EXCEPTIONS_H
#define X86_EXCEPTIONS_H 1

typedef u8_t opcode, *pc;

#ifdef LINUX
#define xpGPRvector(x) ((natural *)(&((x)->uc_mcontext.gregs)))
#define xpGPR(x,gprno) (xpGPRvector(x)[gprno])
#define set_xpGPR(x,gpr,new) xpGPR((x),(gpr)) = (natural)(new)
#define xpPC(x) (xpGPR(x,Iip))
#define xpMMXreg(x,n)  *((natural *)(&((x)->uc_mcontext.fpregs->_st[n])))
#define eflags_register(xp) xpGPR(xp,Iflags)
#endif

#ifdef DARWIN
#define DARWIN_USE_PSEUDO_SIGRETURN 1
#include <sys/syscall.h>
#define DarwinSigReturn(context) do {\
    darwin_sigreturn(context);\
    Bug(context,"sigreturn returned");\
  } while (0)

#define xpGPRvector(x) ((natural *)(&(UC_MCONTEXT(x)->__ss)))
#define xpGPR(x,gprno) (xpGPRvector(x)[gprno])
#define set_xpGPR(x,gpr,new) xpGPR((x),(gpr)) = (natural)(new)
#define xpPC(x) (xpGPR(x,Iip))
#define eflags_register(xp) xpGPR(xp,Iflags)
#define xpFPRvector(x) ((natural *)(&(UC_MCONTEXT(x)->__fs.__fpu_xmm0)))
#define xpMMXvector(x) (&(UC_MCONTEXT(x)->__fs.__fpu_stmm0))
/* Note that this yields only the lower half of the MMX reg on x8632 */
#define xpMMXreg(x,n) *(natural *)&(xpMMXvector(x)[n])

#include <mach/mach.h>
#include <mach/mach_error.h>
#include <mach/machine/thread_state.h>
#include <mach/machine/thread_status.h>

pthread_mutex_t *mach_exception_lock;

#endif

#ifdef FREEBSD
#ifdef X8664
#include <machine/fpu.h>
#else
#include "freebsdx8632/fpu.h"
#endif
#define xpGPRvector(x) ((natural *)(&((x)->uc_mcontext)))
#define xpGPR(x,gprno) (xpGPRvector(x)[gprno])
#define set_xpGPR(x,gpr,new) xpGPR((x),(gpr)) = (natural)(new)
#define eflags_register(xp) xpGPR(xp,Iflags)
#define xpPC(x) xpGPR(x,Iip)
#ifdef X8664
#define xpMMXreg(x,n) *((natural *)(&(((struct savefpu *)(&(x)->uc_mcontext.mc_fpstate))->sv_fp[n])))
#define xpXMMregs(x)(&(((struct savefpu *)(&(x)->uc_mcontext.mc_fpstate))->sv_xmm[0]))
#else
#define xpMMXreg(x,n) *((natural *)(&(((struct ccl_savexmm *)(&(x)->uc_mcontext.mc_fpstate))->sv_fp[n])))
#define xpXMMregs(x)(&(((struct ccl_savexmm *)(&(x)->uc_mcontext.mc_fpstate))->sv_xmm[0]))
#endif
#endif

#ifdef SOLARIS
#define xpGPRvector(x) ((x)->uc_mcontext.gregs)
#define xpGPR(x,gprno) (xpGPRvector(x)[gprno])
#define set_xpGPR(x,gpr,new) xpGPR((x),(gpr)) = (natural)(new)
#define xpPC(x) xpGPR(x,Iip)
#define eflags_register(xp) xpGPR(xp,Iflags)
#define xpXMMregs(x)(&((x)->uc_mcontext.fpregs.fp_reg_set.fpchip_state.xmm[0]))
#ifdef X8632
#define xpMMXreg(x,n)*(natural *)(&(((struct fnsave_state *)(&(((x)->uc_mcontext.fpregs))))->f_st[n]))
#endif
#endif

#ifdef WINDOWS
#ifdef X8664
#define xpGPRvector(x) ((DWORD64 *)(&(x)->Rax))
#define xpGPR(x,gprno) (xpGPRvector(x)[gprno])
#define xpPC(x) xpGPR(x,Iip)
#define eflags_register(xp) xp->EFlags
#define xpMXCSRptr(x) (DWORD *)(&(x->MxCsr))
#else
#define xpGPRvector(x) ((DWORD *)(&(x)->Edi))
#define xpGPR(x,gprno) (xpGPRvector(x)[gprno])
#define xpPC(x) xpGPR(x,Iip)
#define eflags_register(xp) xp->EFlags
#define xpFPRvector(x) ((natural *)(&(x->ExtendedRegisters[10*16])))
#define xpMMXreg(x,n)  (*((u64_t *)(&(x->FloatSave.RegisterArea[10*(n)]))))
#define xpMXCSRptr(x) (DWORD *)(&(x->ExtendedRegisters[24]))
#endif
#endif

#ifdef DARWIN
#define SIGNAL_FOR_PROCESS_INTERRUPT SIGUSR1
#endif
#ifdef LINUX
#define SIGNAL_FOR_PROCESS_INTERRUPT SIGPWR
#endif
#ifdef FREEBSD
#define SIGNAL_FOR_PROCESS_INTERRUPT SIGEMT
#endif
#ifdef SOLARIS
#define SIGNAL_FOR_PROCESS_INTERRUPT SIGUSR1
#endif
#ifdef WINDOWS
#define SIGNAL_FOR_PROCESS_INTERRUPT SIGINT
#ifndef SIGBUS
#define SIGBUS 10
#endif
#ifndef CONTEXT_ALL
#define CONTEXT_ALL (CONTEXT_CONTROL | CONTEXT_INTEGER | CONTEXT_SEGMENTS | CONTEXT_FLOATING_POINT | CONTEXT_DEBUG_REGISTERS | CONTEXT_EXTENDED_REGISTERS)
#endif
#endif



void switch_to_foreign_stack(void*, ...);

#define INTN_OPCODE 0xcd

#define UUO_GC_TRAP    0xc4
#define UUO_ALLOC_TRAP 0xc5
#define UUO_DEBUG_TRAP 0xca
#define UUO_DEBUG_TRAP_WITH_STRING 0xcd

#define XUUO_OPCODE_0 0x0f
#define XUUO_OPCODE_1 0x0b

#define XUUO_TLB_TOO_SMALL 1
#define XUUO_INTERRUPT_NOW 2
#define XUUO_SUSPEND_NOW 3
#define XUUO_INTERRUPT 4
#define XUUO_SUSPEND 5
#define XUUO_SUSPEND_ALL 6
#define XUUO_RESUME 7
#define XUUO_RESUME_ALL 8
#define XUUO_KILL 9
#define XUUO_ALLOCATE_LIST 10

void
pc_luser_xp(ExceptionInformation*, TCR*, signed_natural*);


typedef enum {
  ID_unrecognized_alloc_instruction,
  ID_load_allocptr_reg_from_tcr_save_allocptr_instruction,
  ID_compare_allocptr_reg_to_tcr_save_allocbase_instruction,
  ID_branch_around_alloc_trap_instruction,
  ID_alloc_trap_instruction,
  ID_set_allocptr_header_instruction,
  ID_clear_tcr_save_allocptr_tag_instruction
} alloc_instruction_id;

#ifdef LINUX
#define SIGNUM_FOR_INTN_TRAP SIGSEGV
#define IS_MAYBE_INT_TRAP(info,xp) ((xpGPR(xp,REG_TRAPNO)==0xd)&&((xpGPR(xp,REG_ERR)&7)==2))
#define IS_PAGE_FAULT(info,xp) (xpGPR(xp,REG_TRAPNO)==0xe)
#define SIGRETURN(context)
#endif

#ifdef FREEBSD
extern void freebsd_sigreturn(ExceptionInformation *);
#define SIGNUM_FOR_INTN_TRAP SIGBUS
#define IS_MAYBE_INT_TRAP(info,xp) ((xp->uc_mcontext.mc_trapno == T_PROTFLT) && ((xp->uc_mcontext.mc_err & 7) == 2))
#define IS_PAGE_FAULT(info,xp) (xp->uc_mcontext.mc_trapno == T_PAGEFLT)
#define SIGRETURN(context) freebsd_sigreturn(context)
#endif

#ifdef DARWIN
#define SIGNUM_FOR_INTN_TRAP SIGSEGV /* Not really, but our Mach handler fakes that */
#define IS_MAYBE_INT_TRAP(info,xp) ((UC_MCONTEXT(xp)->__es.trapno == 0xd) && (((UC_MCONTEXT(xp)->__es.err)&7)==2))
#define IS_PAGE_FAULT(info,xp) (UC_MCONTEXT(xp)->__es.trapno == 0xe)
/* The x86 version of sigreturn just needs the context argument; the
   hidden, magic "flavor" argument that sigtramp uses is ignored. */
#define SIGRETURN(context) DarwinSigReturn(context)
#endif

#ifdef SOLARIS
#define SIGNUM_FOR_INTN_TRAP SIGSEGV
#ifdef X8664
#define IS_MAYBE_INT_TRAP(info,xp) ((xpGPR(xp,REG_TRAPNO)==0xd)&&((xpGPR(xp,REG_ERR)&7)==2))
#define IS_MAYBE_INT_TRAP(info,xp) (xpGPR(xp,REG_TRAPNO)==0xe)
#else
#define IS_MAYBE_INT_TRAP(info,xp) ((xpGPR(xp,TRAPNO)==0xd)&&((xpGPR(xp,ERR)&7)==2))
#define IS_PAGE_FAULT(info,xp) (xpGPR(xp,TRAPNO)==0xe)
#endif
#define SIGRETURN(context) setcontext(context)
#endif

#ifdef WINDOWS
#define SIGNUM_FOR_INTN_TRAP SIGSEGV /* Also fake */
#define IS_MAYBE_INT_TRAP(info,xp) \
  ((info->ExceptionCode == EXCEPTION_ACCESS_VIOLATION) &&       \
   (info->ExceptionInformation[0]==0) &&                       \
   (info->ExceptionInformation[1]==(ULONG_PTR)(-1L)))
#define IS_PAGE_FAULT(info,xp) (1)
#define SIGRETURN(context)      /* for now */
#endif

/* Please go away. */
#ifdef DARWIN_GS_HACK
extern Boolean ensure_gs_pthread(void);
extern void set_gs_address(void *);
#endif


/* sigaltstack isn't thread-specific on The World's Most Advanced OS */
#ifdef DARWIN
#undef USE_SIGALTSTACK
#else
#ifdef WINDOWS
#undef USE_SIGALTSTACK
#else
#define USE_SIGALTSTACK 1
#endif
#endif

#ifdef USE_SIGALTSTACK
void setup_sigaltstack(area *);
#endif

/* recognizing the function associated with a tagged return address */
/* now involves recognizinig an "(lea (@ disp (% rip)) (% rn))" */
/* instruction at the tra */

#define RECOVER_FN_FROM_RIP_LENGTH 7 /* the instruction is 7 bytes long */
#define RECOVER_FN_FROM_RIP_DISP_OFFSET 3 /* displacement word is 3 bytes in */
#define RECOVER_FN_FROM_RIP_WORD0 0x8d4c /* 0x4c 0x8d, little-endian */
#define RECOVER_FN_FROM_RIP_BYTE2 0x2d  /* third byte of opcode */

extern natural get_mxcsr();
extern void set_mxcsr(natural);

#ifdef WINDOWS
typedef struct {
  HANDLE h;
  OVERLAPPED *o;
} pending_io;
#endif

#ifdef X8632
/* The 32-bit immediate value in the instruction
 * "(mov ($ 0x12345678) (% fn))" at a tagged return address
 * refers to the associated function.
 */
#define RECOVER_FN_OPCODE 0xbf
#define RECOVER_FN_LENGTH 5
#endif

#endif /* X86_EXCEPTIONS_H */

