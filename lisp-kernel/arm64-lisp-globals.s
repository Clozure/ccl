/* SPDX-License-Identifier: Apache-2.0 */

/*
 * arm64-specific replacement for lisp_globals.s.
 *
 * Define the offsets for the lisp globals and for the nil-relative
 * symbols that live on either side of nil.
 *
 * The symbols follow nil in memory; the globals precede it.
 *
 * ARM64 accessor macros in lisp_globals.h:
 *
 *   nrs_symbol(s) = ((lispsymbol *)(nil_value - fulltag_nil + dnode_size))[s]
 *   lisp_global(g)= ((LispObj  *)(nil_value - fulltag_nil - dnode_size))[g]
 *
 * In these macros, nil_value is the runtime value of where nil ended
 * up in memory: in other words, it's the same as the contents of
 * rnil.
 *
 * Because rnil points to a weird cons cell that straddles two dnodes,
 * the nil-relative symbols begin dnode_size above nil's untagged
 * base, and the lisp globas begin one dnode_size below it.
 *
 * The order of both records must match lisp_globals.h and the
 * *kernel-globals* / *nilreg-relative-symbols* lists in
 * compiler/ARM64/arm64-arch.lisp.
 */

#include "arm64-constants.h"

nrs_origin = dnode_size - fulltag_nil
nrs_symbol_fulltag = fulltag_symbol
lisp_globals_limit = -(fulltag_nil + dnode_size)

/* Each nrs entry is a whole symbol; the label points at the tagged
   symbol, i.e. fulltag_symbol into the object, with the rest following. */
nrs_symbol_extra = symbol.size - nrs_symbol_fulltag

_struct nrs, nrs_origin
  _struct_pad nrs_symbol_fulltag
  _struct_label tsym                     /* t */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label nilsym                   /* nil */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label errdisp                  /* %err-disp */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label cmain                    /* cmain */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label eval                     /* eval */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label appevalfn                /* apply-evaluated-function */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label error                    /* error */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label defun                    /* %defun */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label defvar                   /* %defvar */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label defconstant              /* %defconstant */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label macrosym                 /* %macro */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label kernelrestart            /* %kernel-restart */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label package                  /* *package* */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label total_bytes_freed        /* *total-bytes-freed* */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label kallowotherkeys          /* :allow-other-keys */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label toplcatch                /* %toplevel-catch% */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label toplfunc                 /* %toplevel-function% */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label callbacks                /* %pascal-functions% */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label restore_lisp_pointers    /* restore-lisp-pointers */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label total_gc_microseconds    /* *total-gc-microseconds* */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label builtin_functions        /* %builtin-functions% */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label udf                      /* %unbound-function% */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label init_misc                /* %init-misc */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label macro_code               /* %macro-code% */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label closure_code             /* %closure-code% */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label new_gcable_ptr           /* %new-gcable-ptr */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label gc_event_status_bits     /* *gc-event-status-bits* */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label post_gc_hook             /* *post-gc-hook* */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label handlers                 /* %handlers% */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label all_packages             /* %all-packages% */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label keyword_package          /* *keyword-package* */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label os_init_function         /* %os-init-function% */
  _struct_pad nrs_symbol_extra

  _struct_pad nrs_symbol_fulltag
  _struct_label foreign_thread_control   /* %foreign-thread-control */
  _struct_pad nrs_symbol_extra
 _ends

/*
 * _rnode is like _node except it pre-decrements
 */
_struct lisp_globals, lisp_globals_limit
 _rnode get_tcr                /* address of get_tcr() for callbacks */
 _rnode tcr_count              /* next tcr's tcr_id */
 _rnode interrupt_signal       /* signal to use for PROCESS-INTERRUPT */
 _rnode kernel_imports         /* things imported for us (see imports.s) */
 _rnode objc_2_personality
 _rnode savetoc                /* saved TOC register, some platforms */
 _rnode saver13                /* saved (global) r13, some platforms */
 _rnode subprims_base          /* where the dynamic subprims wound up */
 _rnode ret1valn               /* magic multiple-values return address */
 _rnode tcr_key                /* tsd key for per-thread tcr */
 _rnode tcr_area_lock          /* all_areas/tcr queue lock */
 _rnode exception_lock         /* serialize exception handling */
 _rnode static_conses
 _rnode default_allocation_quantum
 _rnode intflag
 _rnode gc_inhibit_count
 _rnode refbits
 _rnode oldspace_dnode_count   /* dynamic dnodes older than generation 0 */
 _rnode reserved_platform_word
 _rnode fwdnum                 /* fixnum: GC "forwarder" call count */
 _rnode gc_num                 /* fixnum: GC call count */
 _rnode gcable_pointers        /* linked-list of weak macptrs */
 _rnode heap_start             /* start of lisp heap */
 _rnode heap_end               /* end of lisp heap */
 _rnode statically_linked      /* non-zero if -static */
 _rnode stack_size             /* from the command line */
 _rnode objc_2_begin_catch     /* address of ObjC 2.0 objc_begin_catch() */
 _rnode kernel_path            /* real executable name */
 _rnode all_areas              /* doubly-linked list of stack & heap areas */
 _rnode lexpr_return           /* magic &lexpr cleanup code */
 _rnode lexpr_return1v         /* single-value &lexpr cleanup code */
 _rnode in_gc                  /* non-zero when lisp addrs may be invalid */
 _rnode free_static_conses     /* length of freelist */
 _rnode objc_2_end_catch       /* address of ObjC 2.0 objc_end_catch() */
 _rnode short_float_zero       /* low half of 1.0d0 */
 _rnode double_float_one       /* high half of 1.0d0 */
 _rnode static_cons_area
 _rnode lisp_exit_hook         /* install foreign exception handling */
 _rnode oldest_ephemeral       /* dnode addr of oldest ephemeral object or 0 */
 _rnode tenured_area           /* the tenured area */
 _rnode ref_base               /* start of oldest pointer-bearing area */
 _rnode argv                   /* pointer to &argv[0] */
 _rnode host_platform          /* for platform-specific initialization */
 _rnode batch_flag             /* -b arg */
 _rnode unwind_resume          /* address of _Unwind_Resume from libobjc */
 _rnode weak_gc_method         /* weak GC algorithm */
 _rnode image_name             /* --image-name arg */
 _rnode initial_tcr            /* initial thread tcr */
 _rnode weakvll                /* all populations as of last GC */
 _rnode managed_static_refbits /* refs from managed_static to dynamic */
 _rnode managed_static_dnodes  /* ndnodes in managed_static_area */
 _rnode ephemeral_refidx       /* index of refbits */
 _rnode managed_static_refidx  /* index of managed_static_refbits */
_ends

/* Traditional name, differs from C */
        .set lisp_globals.ret1val_addr, lisp_globals.ret1valn
