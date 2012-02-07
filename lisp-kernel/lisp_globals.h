/*
   Copyright (C) 2009 Clozure Associates
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

#ifndef __lisp_globals__
#define __lisp_globals__


extern LispObj lisp_nil;

#define GET_TCR (-1)		/* address of get_tcr() for callbacks */
#define TCR_COUNT (-2)		/* next tcr's tcr_id */
#define INTERRUPT_SIGNAL  (-3)  /* signal to use for PROCESS-INTERRUPT */
#define KERNEL_IMPORTS (-4)	/* some things we need to have imported for us. */
#define OBJC_2_PERSONALITY (-5) /* A good listener.  Doesn't say much */
#define SAVETOC (-6)	        /* Saved TOC register, for some platforms */
#define SAVER13 (-7)		/* Saved (global) r13, on some platforms */
#define SUBPRIMS_BASE (-8)	/* where the dynamic subprims wound up */
#define RET1VALN (-9)		/* magic multiple-values return address */
#define TCR_KEY (-10)     	/* tsd key for per-thread tcr */
#define TCR_AREA_LOCK (-11)       /* all_areas/tcr queue lock */
#define EXCEPTION_LOCK (-12)	/* serialize exception handling */
#define STATIC_CONSES (-13)
#define DEFAULT_ALLOCATION_QUANTUM (-14)
#define INTFLAG (-15)
#define GC_INHIBIT_COUNT (-16)
#define REFBITS (-17)
#define OLDSPACE_DNODE_COUNT (-18) /* count of dynamic dnodes older than generation 0 */
#define ALTIVEC_PRESENT (-19)   /* non-zero if AltiVec present. */
#define FWDNUM (-20)            /* fixnum: GC "forwarder" call count. */
#define GC_NUM (-21)            /* fixnum: GC call count. */
#define GCABLE_POINTERS (-22)   /* linked-list of weak macptrs. */
#define HEAP_START (-23)        /* start of lisp heap */
#define HEAP_END (-24)          /* end of lisp heap */
#define STATICALLY_LINKED (-25)        /* non-zero if -static */
#define STACK_SIZE (-26)        /* from the command line */
#define OBJC_2_BEGIN_CATCH (-27)  /* address of ObjC 2.0 objc_begin_catch() */
#define KERNEL_PATH (-28)       /* real executable name */
#define ALL_AREAS (-29)         /* doubly-linked list of stack & heap areas */
#define LEXPR_RETURN (-30)      /* magic &lexpr cleanup code */
#define LEXPR_RETURN1V (-31)    /* single-value &lexpr cleanup code */
#define IN_GC (-32)             /* non-zero when lisp addresses may be invalid */
#define FREE_STATIC_CONSES (-33)     /* length of freelist */
#define OBJC_2_END_CACTCH (-34)          /* address of ObjC 2.0 objc_end_catch() */
#define SHORT_FLOAT_ZERO (-35)  /* low half of 1.0d0 */
#define DOUBLE_FLOAT_ONE (-36)  /* high half of 1.0d0 */
#define STATIC_CONS_AREA (-37)	/* static_cons_area */
#define LISP_EXIT_HOOK (-38)	/* install foreign exception handling */
#define OLDEST_EPHEMERAL (-39)  /* doubleword address of oldest ephemeral object or 0 */
#define TENURED_AREA (-40)      /* the tenured area */
#define REF_BASE (-41)          /* start of oldest pointer-bearing area */
#define ARGV (-42)              /* pointer to &argv[0] */
#define HOST_PLATFORM (-43)	/* for platform-specific initialization */
#define BATCH_FLAG (-44)	/* -b arg */
#define UNWIND_RESUME (-45)	/* address of _Unwind_Resume from libobjc */
#define WEAK_GC_METHOD (-46)	/* weak GC algorithm */
#define IMAGE_NAME (-47)	/* --image-name arg */
#define INITIAL_TCR (-48)	/* initial thread tcr */
#define WEAKVLL (-49)           /* all populations as of last GC */

#define MIN_KERNEL_GLOBAL WEAKVLL

/* These are only non-zero when an image is being saved or loaded */

#if (WORD_SIZE==64)
#define LISP_HEAP_THRESHOLD (-511)
#define EGC_ENABLED (-510)
#define G0_THRESHOLD (-509)
#define G1_THRESHOLD (-508)
#define G2_THRESHOLD (-507)
#define GC_NOTIFY_THRESHOLD (-506)
#else
#ifdef ARM
#define LISP_HEAP_THRESHOLD (-1021)
#define EGC_ENABLED (-1020)
#define G0_THRESHOLD (-1019)
#define G1_THRESHOLD (-1018)
#define G2_THRESHOLD (-1017)
#define GC_NOTIFY_THRESHOLD (-1016)
#else
#define LISP_HEAP_THRESHOLD (-1023)
#define EGC_ENABLED (-1022)
#define G0_THRESHOLD (-1021)
#define G1_THRESHOLD (-1020)
#define G2_THRESHOLD (-1019)
#define GC_NOTIFY_THRESHOLD (-1018)
#endif
#endif

#ifdef PPC
#ifdef PPC64
#define lisp_global(g) (((LispObj *) (0x3000+(LOWMEM_BIAS)))[(g)])
#define nrs_symbol(s) (((lispsymbol *) (0x3000+(LOWMEM_BIAS)))[(s)])
#else
#define lisp_global(g) (((LispObj *) (nil_value-fulltag_nil))[(g)])
#define nrs_symbol(s) (((lispsymbol *) (nil_value+(8-fulltag_nil)+8))[(s)])
#endif
#endif

#ifdef X8664
#define lisp_global(g) (((LispObj *) (0x13000+(LOWMEM_BIAS)))[(g)])
#define nrs_symbol(s) (((lispsymbol *) (0x13020+(LOWMEM_BIAS)))[(s)])
#endif

#ifdef X8632
#define lisp_global(g) (((LispObj *) (0x13000+(LOWMEM_BIAS)))[(g)])
#define nrs_symbol(s) (((lispsymbol *) (0x13008+(LOWMEM_BIAS)))[(s)])
#endif

#ifdef ARM
#define lisp_global(g) (((LispObj *) (nil_value-fulltag_nil-dnode_size))[(g)])
#define nrs_symbol(s) (((lispsymbol *) (nil_value-fulltag_nil+dnode_size))[(s)]) 
#endif

#define nrs_T 				(nrs_symbol(0))		/* t */
#define nrs_NILSYM			(nrs_symbol(1))		/* nil */
#define nrs_ERRDISP			(nrs_symbol(2))		/* %err-disp */
#define nrs_CMAIN			(nrs_symbol(3))		/* cmain */
#define nrs_EVAL			(nrs_symbol(4))		/* eval */
#define nrs_APPEVALFN			(nrs_symbol(5))		/* apply-evaluated-function */
#define nrs_ERROR			(nrs_symbol(6))		/* error */
#define nrs_DEFUN			(nrs_symbol(7))		/* %defun */
#define nrs_DEFVAR			(nrs_symbol(8))		/* %defvar */
#define nrs_DEFCONSTANT			(nrs_symbol(9))		/* %defconstant */
#define nrs_MACRO			(nrs_symbol(10))	/* %macro */
#define nrs_KERNELRESTART		(nrs_symbol(11))	/* %kernel-restart */
#define nrs_PACKAGE			(nrs_symbol(12))	/* *package* */
#define nrs_TOTAL_BYTES_FREED           (nrs_symbol(13))        /* *total-bytes-freed* */
#define nrs_KALLOWOTHERKEYS		(nrs_symbol(14))	/* :allow-other-keys */
#define nrs_TOPLCATCH			(nrs_symbol(15))	/* %toplevel-catch% */
#define nrs_TOPLFUNC			(nrs_symbol(16))	/* %toplevel-function% */
#define nrs_CALLBACKS			(nrs_symbol(17))	/* %pascal-functions% */
#define nrs_RESTORE_LISP_POINTERS	(nrs_symbol(18))	/* restore-lisp-pointers */
#define nrs_TOTAL_GC_MICROSECONDS       (nrs_symbol(19))        /* *total-gc-microseconds* */
#define nrs_BUILTIN_FUNCTIONS           (nrs_symbol(20))        /* %builtin-functions% */
#define nrs_UDF				(nrs_symbol(21))	/* %unbound-function% */
#define nrs_INIT_MISC			(nrs_symbol(22))        /* %init-misc% */
#define nrs_MACRO_CODE                  (nrs_symbol(23))        /* %macro-code% */
#define nrs_CLOSURE_CODE		(nrs_symbol(24))        /* %closure-code% */
#define nrs_NEW_GCABLE_PTR		(nrs_symbol(25))	/* %new-gcable-ptr */
#define nrs_GC_EVENT_STATUS_BITS	(nrs_symbol(26))	/* *gc-event-status-bits* */
#define nrs_POST_GC_HOOK		(nrs_symbol(27))	/* *post-gc-hook* */
#define nrs_HANDLERS			(nrs_symbol(28))	/* %handlers% */
#define nrs_ALL_PACKAGES		(nrs_symbol(29))	/* %all-packages% */
#define nrs_KEYWORD_PACKAGE		(nrs_symbol(30))	/* *keyword-package* */
#define nrs_OS_INIT_FUNCTION		(nrs_symbol(31))	/* %os-init-function% */
#define nrs_FOREIGN_THREAD_CONTROL      (nrs_symbol(32))        /* %foreign-thread-control */
#define num_nilreg_symbols 33
#define nilreg_symbols_end ((BytePtr) &(nrs_symbol(num_nilreg_symbols)))
#endif
