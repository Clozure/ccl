/*
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
#define UUO_MASK 0xfc00000f

#define IS_UUO(i) (((i) & UUO_MASK) == 0xb)
/* If an instruction is a UUO, the minor opcode is in bits 21:27 */
#define UUO_MINOR(u) (((u) >> 4) & 0x7f)

typedef u_int32_t opcode, *pc;

OSStatus
handle_uuo(ExceptionInformation *, opcode, pc);



#ifdef LINUX
/*
  Different (recent) versions of glibc disagree about how
  a ucontext is laid out (and about what an mcontext is.)
  There's something like a pointer to a pt_regs structure
  in the 12th word in both cases.  (Yes, this is an extremely
  ugly hack; it would be better to conditionalize on the values
  of GLIBC_VERSION/GLIBC_MINOR , but the discrepancy exists
  in various flavors of glibc 2.3.)
*/
#ifdef PPC64
#define XP_PTREGS(x) ((x)->uc_mcontext.regs)
#define xpGPRvector(x) ((natural *)(XP_PTREGS(x)))
#else
#define XP_PTREGS(x) (((struct pt_regs **)(x))[12])
#define xpGPRvector(x) (XP_PTREGS(x)->gpr)
#endif
#define xpGPR(x,gprno) (xpGPRvector(x)[gprno])
#define set_xpGPR(x,gpr,new) xpGPR((x),(gpr)) = (natural)(new)
#define xpPC(x) (*((pc*)(&(XP_PTREGS(x)->nip))))
#define set_xpPC(x,new) (xpPC(x) = (pc)(new))
#define xpLR(x) (*((pc*)(&(XP_PTREGS(x)->link))))
#define xpCTR(x) (*(pc*)(&(XP_PTREGS(x)->ctr)))
#define xpXER(x) (XP_PTREGS(x)->xer)
#define xpCCR(x) (XP_PTREGS(x)->ccr)
#define xpMSR(x) (XP_PTREGS(x)->msr)
#define xpDSISR(x) (XP_PTREGS(x)->dsisr)
#define xpDAR(x) (XP_PTREGS(x)->dar)
#define xpTRAP(x) (XP_PTREGS(x)->trap)
#define xpFPSCR(x) (XP_PTREGS(x)->gpr[PT_FPSCR])
#define xpFPRvector(x) ((double *)(&(XP_PTREGS(x)->gpr[PT_FPR0])))
#define xpFPR(x,fprno) (xpFPRvector(x)[fprno])

/* 
   Work around a Darwin G5 bug (present in OSX 10.2.7, 10.2.8, and later
   versions.  See below for details.
*/
#define DarwinSigReturn(context)
#define SIGRETURN(context)
#endif

#ifdef DARWIN
#define xpGPRvector(x) (&(UC_MCONTEXT(x)->ss.r0))
#define xpGPR(x,gprno) ((xpGPRvector(x))[gprno])
#define set_xpGPR(x,gpr,new) xpGPR((x),(gpr)) = (UInt32)(new)
#define xpPC(x) (*((pc*) &(UC_MCONTEXT(x)->ss.srr0)))
#define set_xpPC(x,new) (xpPC(x) = (pc)(new))
#define xpLR(x) (*((pc*)(&(UC_MCONTEXT(x)->ss.lr))))
#define xpCTR(x) (*(pc*)(&(UC_MCONTEXT(x)->ss.ctr)))
#define xpXER(x) (UC_MCONTEXT(x)->ss.xer)
#define xpCCR(x) (UC_MCONTEXT(x)->ss.cr)
#define xpMSR(x) (UC_MCONTEXT(x)->ss.srr1)
#define xpDSISR(x) (UC_MCONTEXT(x)->es.dsisr)
#define xpDAR(x) (UC_MCONTEXT(x)->es.dar)
#define xpTRAP(x) (UC_MCONTEXT(x)->es.exception)
#define xpFPSCR(x) (UC_MCONTEXT(x)->fs.fpscr)
#define xpFPRvector(x) (UC_MCONTEXT(x)->fs.fpregs)
#define xpFPR(x,fprno) (xpFPRvector(x)[fprno])
/* There's a bug in some versions of Darwin on G5 processors: FP context
   isn't restored correctly on exit from a signal handler if the integer
   context appears to be unmodified (the 64-bit context isn't set up
   correctly by the kernel: only the first N bytes are copied out of
   the kernel, where N = size of 32-bit context.

   If the kernel pushed both a 32-bit and 64-bit context, the C
   runtime "signal trampoline" code tries to determine if the 32-bit
   GPRs and user-visible SPRs in the 32-bit context contain the same
   values as their 64-bit counterparts on exit; if so, it tries to
   call sigreturn with an extra argument that indicates that the
   thread's state should be restored from the 64-bit context.
   (Apparently that's more efficient; it'd be surprising if it'd be
   more efficent when the cost of comparing values in the two contexts
   is factored in ...).  On some OS releases, the 64-bit context can't
   be reliably restored (FPRs get trashed.)

   One way to work around this is to use a deprecated, 32-bit-context-only
   version of the sigreturn syscall.  There seems to be reason to be
   reason to believe that the old sigreturn syscall will disappear
   on OS releases >10.3.

   Another way to work around this is to make a "harmless" change to
   an SPR or GPR value in the 32-bit context.  There are lots of
   "reserved" bits in the XER that make good candidates: 1's written
   to reserved XER bits can't be reliably read anyway, so this may
   or may not actually change the value in the XER in a way that
   can be reliably detected.

   Note that both the old, deprecated version of sigreturn and the
   new version take a first argument of type "struct ucontext *",
   not "struct sigcontext *" as the man page and header files claim.
   The new version takes a second argument, which is a small integer
   which defines what "flavor" of context should be restored from.
   The meaningful values that can be used here aren't defined in
   a header file; the kernel (and the libc _sigtramp() function)
   have (hopefully) matching, redundant hardwired definitions in
   the source.
*/
#ifdef PPC64
#define DarwinSigReturn(x)
#else
#define DarwinSigReturn(x) (UC_MCONTEXT(x)->ss.xer)^=0x80
#endif
#define SIGRETURN(context) DarwinSigReturn(context)
#endif






/* 
  Unconditional traps (tw, twi instructions) are used by the
  operating system.  We use conditional traps.
  */

int
is_conditional_trap(opcode);

#define kNameBufLen 256
#define TRAP_LOOKUP_TRIES 5   /* # instrs to scan before trap instr */

void
callback_for_trap (LispObj, ExceptionInformation *, pc, natural, natural, natural);

natural
register_codevector_contains_pc (natural, pc);

void
callback_to_lisp (LispObj, ExceptionInformation *, natural, natural, natural, natural, natural);

OSStatus
handle_trap(ExceptionInformation *, opcode, pc, siginfo_t *);

unsigned
scan_for_instr( unsigned, unsigned, pc );



#define UUO_INTERR (11)
#define UUO_INTCERR (12)
#define UUO_INTERR2 (13)
#define UUO_INTCERR2 (14)

#define UUO_FPUX_BINOP (22)
#define UUO_ZERO_FPSCR (25)


/* PPC instructions */
#define match_instr(instr, mask, target)   (((instr) & (mask)) == (target))
#define RS_field(instr)  (((instr) >> 21) & 0x1f)
#define RT_field(instr)  (RS_field(instr))
#define TO_field(instr)  (RT_field(instr))
#define RA_field(instr)  (((instr) >> 16) & 0x1f)
#define RB_field(instr)  (((instr) >> 11) & 0x1f)
#define D_field(instr)   ((instr) & 0xffff)
#define DS_field(instr)  ((instr) & 0xfffc)
#define DS_VARIANT_FIELD(instr) ((instr) & 3)

#define RT(val) ((val & 0x1f) << 21)
#define RS(val) (RT(val))
#define RA(val) ((val & 0x1f) << 16)
#define RB(val) ((val & 0x1f) << 11)
#define D(val) (val & 0xffff)

#define RS_MASK RS(-1)
#define RT_MASK RS_MASK
#define TO_MASK RS_MASK
#define RA_MASK RA(-1)
#define RB_MASK RB(-1)
#define D_MASK  D(-1)



#define OP(x) (((x) & 0x3f) << 26)
#define OP_MASK OP (0x3f)

/* Main opcode + TO field of a D form instruction */
#define OPTO(x,to) (OP(x) | (((to) & 0x1f) << 21))
#define OPTO_MASK (OP_MASK | TO_MASK)
#define OPTORA(x,to,ra) (OPTO(x,to) | RA(ra))
#define OPTORA_MASK (OP_TO_MASK | RA_MASK)




/* An X form instruction.  */
#define X(op, xop) (OP (op) | (((xop) & 0x3ff) << 1))

/* An X form instruction with the RC bit specified.  */
#define XRC(op, xop, rc) (X ((op), (xop)) | ((rc) & 1))

/* The mask for an X form instruction.  */
#define X_MASK XRC(0x3f, 0x3ff, 1)

/* An XO form instruction */
#define XO(op, xop, oe, rc) \
  (OP (op) | ((((unsigned long)(xop)) & 0x1ff) << 1) | ((((unsigned long)(oe)) & 1) << 10) | (((unsigned long)(rc)) & 1))
#define XO_MASK XO (0x3f, 0x1ff, 1, 1)



/* The bits in the TO field of a TW or TWI instruction */
#define TO_LT (1<<4)		/* signed < */
#define TO_GT (1<<3)		/* signed > */
#define TO_EQ (1<<2)		/* = */
#define TO_LO (1<<1)		/* unsigned < */
#define TO_HI (1<<0)		/* unsigned > */
#define TO_NE (TO_LT|TO_GT)

/* True if major opcode of "instr" is "op" */
#define major_opcode_p(instr, op) match_instr((instr),OP_MASK,OP(op))

/* True if "instr" is an X form instruction with major opcode "major"
   and minor opcode "minor" */
#define X_opcode_p(instr,major,minor) match_instr((instr),X_MASK,X(major,minor))

#define major_opcode_TDI 2
#define major_opcode_TWI 3
#ifdef PPC64
#define major_opcode_TRI major_opcode_TDI
#else
#define major_opcode_TRI major_opcode_TWI
#endif
#define major_opcode_ADDI 14
#define major_opcode_RLWINM 21
#define major_opcode_X31 31		/* an "X" form instruction; see minor opcode */
#define major_opcode_LWZ 32
#define major_opcode_LBZ 34
#define major_opcode_STW 36
#define major_opcode_STWU 37
#define major_opcode_LD_LDU_LWA 58
#define major_opcode_FPU_SINGLE 59
#define major_opcode_FPU_DOUBLE 63

#define minor_opcode_TW 4
#define minor_opcode_TD 68
#ifdef PPC64
#define minor_opcode_TR minor_opcode_TD
#else
#define minor_opcode_TR minor_opcode_TW
#endif
#define minor_opcode_SUBF 40
#define minor_opcode_STWX 151
#define minor_opcode_STWUX 183

#define major_opcode_DS_LOAD64 58
#define DS_LOAD64_VARIANT_LD 0

#define major_opcode_DS_STORE64 62
#define DS_STORE64_VARIANT_STD 0



#define D_instruction(major,rt,ra,imm) (OP(major)|((rt)<<21)|((ra)<<16)|((imm)&D_MASK))
#define DS_instruction(major,rt,ra,imm,minor) (OP(major)|((rt)<<21)|((ra)<<16)|(((imm)&D_MASK)&~3)|((minor)&3))
#define TRI_instruction(rt,ra,imm)     D_instruction(major_opcode_TRI,rt,ra,imm)
#define LBZ_instruction(rt,ra,imm)     D_instruction(major_opcode_LBZ,rt,ra,imm)
#define LWZ_instruction(rt,ra,imm)     D_instruction(major_opcode_LWZ,rt,ra,imm)
#define LD_instruction(rt,ra,imm)      DS_instruction(58,rt,ra,imm,0)

#define D_RT_IMM_MASK                  (OP_MASK|RT_MASK|D_MASK)
#define D_RA_IMM_MASK                  (OP_MASK|RA_MASK|D_MASK)

#define X_instruction(major,minor,rt,ra,rb) (X(major,minor)|((rt)<<21)|((ra)<<16)|((rb)<<11))

#define unmasked_register              0

#define LISP_BREAK_INSTRUCTION 0x7f810808
#define QUIET_LISP_BREAK_INSTRUCTION 0x7c800008

#ifdef PPC64
/* Have to use signed comparisons on PPC64; if we decrememt
   allocptr and it "wraps around" address 0, that's an 
   attempt to allocate a large object.  Note that this
   means that valid heap addresses can't have the high
   bit set. */
/* tdlt allocptr,allocbase */
#define ALLOC_TRAP_INSTRUCTION 0x7e095088
#else
/* On PPC32, we can use an unsigned comparison, as long
   as  HEAP_IMAGE_BASE+PURESPACE_RESERVE is greater than
   the maximum possible allocation (around 27 bits).
   Decrementing allocptr may cause it to wrap around
   #x80000000, but it should never wrap around 0. */
/* twllt allocptr,allocbase */
#define ALLOC_TRAP_INSTRUCTION 0x7c495008
#endif

#ifdef PPC64
/* tdlgei allocptr,0 */
#define GC_TRAP_INSTRUCTION 0x08a90000
#else
/* twlgei allocptr,0 */
#define GC_TRAP_INSTRUCTION 0x0ca90000
#endif

#ifdef PPC64
/* clrrdi allocptr,allocptr,4 */
#define UNTAG_ALLOCPTR_INSTRUCTION 0x792906e4
#else
/* clrrwi allocptr,allocptr,3 */
#define UNTAG_ALLOCPTR_INSTRUCTION 0x55290038
#endif

#ifdef PPC64
/* std rX,misc_header_offset(allocptr) */
#define STORE_HEADER_ALLOCPTR_INSTRUCTION 0xf809fff4
#else
/* stw rX,misc_header_offset(allocptr) */
#define STORE_HEADER_ALLOCPTR_INSTRUCTION 0x9009fffa
#endif
#define STORE_HEADER_ALLOCPTR_MASK D_RA_IMM_MASK

#ifdef PPC64
/* std rX,cons.cXr(allocptr) */
#define STORE_CAR_ALLOCPTR_INSTRUCTION 0xf8090004
#define STORE_CDR_ALLOCPTR_INSTRUCTION 0xf809fffc
#else
/* stw rX,cons.cXr(allocptr) */
#define STORE_CAR_ALLOCPTR_INSTRUCTION 0x90090003
#define STORE_CDR_ALLOCPTR_INSTRUCTION 0x9009ffff
#endif
#define STORE_CXR_ALLOCPTR_MASK D_RA_IMM_MASK


#ifdef PPC64
/* stdu sp,-32(sp) */
#define CREATE_LISP_FRAME_INSTRUCTION 0xf821ffe1
#else
/* stwu sp,-16(sp) */
#define CREATE_LISP_FRAME_INSTRUCTION 0x9421fff0
#endif

#ifdef PPC64
/* std tsp,tsp_frame.type(tsp) */
#define MARK_TSP_FRAME_INSTRUCTION 0xf98c0008
#else
/* stw tsp,tsp_frame.type(tsp) */
#define MARK_TSP_FRAME_INSTRUCTION 0x918c0004
#endif

#ifdef PPC64
#define INIT_CATCH_FRAME_INSTRUCTION (0xf8000000 | RA(nargs))
#define INIT_CATCH_FRAME_MASK (OP_MASK | RA_MASK)
#else
#define INIT_CATCH_FRAME_INSTRUCTION (0x90000000 | RA(nargs))
#define INIT_CATCH_FRAME_MASK (OP_MASK | RA_MASK)
#endif

OSStatus
handle_error(ExceptionInformation *, unsigned, unsigned, unsigned, pc);

typedef char* vector_buf;

void put_altivec_registers(vector_buf);
void get_altivec_registers(vector_buf);


int altivec_available;

#ifdef DARWIN
#include <mach/mach.h>
#include <mach/mach_error.h>
#include <mach/machine/thread_state.h>
#include <mach/machine/thread_status.h>

#endif

/* Yet another way to look at a branch instruction ... */
typedef union {
  struct {unsigned op:6, li:24, aa:1, lk:1;} b;
  unsigned opcode;
} branch_instruction;



  /* Enable exceptions (at least, enable another thread's attempts to
     suspend this one) by restoring the signal mask.
  */



#ifdef DARWIN
#define SIGNAL_FOR_PROCESS_INTERRUPT SIGUSR1
#endif
#ifdef LINUX
#define SIGNAL_FOR_PROCESS_INTERRUPT SIGPWR
#endif


#ifdef LINUX
register void *current_r2 __asm__("r2");
#endif

Boolean
extend_tcr_tlb(TCR *, ExceptionInformation *, unsigned, unsigned);

void 
pc_luser_xp(ExceptionInformation *, TCR *, signed_natural *);


#ifdef PPC64
#define codevec_hdr_p(value) ((value) == (('C'<<24)|('O'<<16)|('D'<<8)|'E'))
#else
/* top 6 bits will be zero, subtag will be subtag_code_vector */
#define CV_HDR_MASK     (OP_MASK | subtagmask)
#define CV_HDR_VALUE    subtag_code_vector
#define codevec_hdr_p(value)	(((value) & CV_HDR_MASK) == CV_HDR_VALUE)
#endif


