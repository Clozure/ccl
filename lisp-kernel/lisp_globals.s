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

/* This file should be included in a platform-specific *constants*.s file,
   which should define nrs_origin and nrs_symbol_fulltag before doing so.
*/
nrs_symbol_extra = symbol.size-nrs_symbol_fulltag
        
        _struct(nrs,nrs_origin)
	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(tsym)
	 _struct_pad(nrs_symbol_extra)	/* t */

	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(nilsym)
	 _struct_pad(nrs_symbol_extra)	/* nil */

	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(errdisp)
	 _struct_pad(nrs_symbol_extra)	/* %err-disp */

	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(cmain)
	 _struct_pad(nrs_symbol_extra)	/* cmain */

	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(eval)
	 _struct_pad(nrs_symbol_extra)	/* eval */
 
	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(appevalfn)
	 _struct_pad(nrs_symbol_extra)	/* apply-evaluated-function */

	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(error)
	 _struct_pad(nrs_symbol_extra)	/* error */

	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(defun)
	 _struct_pad(nrs_symbol_extra)	/* %defun */

	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(defvar)
	 _struct_pad(nrs_symbol_extra)	/* %defvar */

	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(defconstant)
	 _struct_pad(nrs_symbol_extra)	/* %defconstant */

	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(macrosym)
	 _struct_pad(nrs_symbol_extra)	/* %macro */

	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(kernelrestart)
	 _struct_pad(nrs_symbol_extra)	/* %kernel-restart */

	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(package)
	 _struct_pad(nrs_symbol_extra)	/* *package* */

	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(total_bytes_freed)		/* *total-bytes-freed* */
	 _struct_pad(nrs_symbol_extra)

	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(kallowotherkeys)
	 _struct_pad(nrs_symbol_extra)	/* allow-other-keys */

	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(toplcatch)
	 _struct_pad(nrs_symbol_extra)	/* %toplevel-catch% */

	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(toplfunc)
	 _struct_pad(nrs_symbol_extra)	/* %toplevel-function% */

	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(callbacks)
	 _struct_pad(nrs_symbol_extra)	/* %pascal-functions% */

	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(restore_lisp_pointers)
	 _struct_pad(nrs_symbol_extra)	/* restore-lisp-pointers */

	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(total_gc_microseconds)		/* *total-gc-microseconds* */
	 _struct_pad(nrs_symbol_extra)

	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(builtin_functions)		/* %builtin-functions% */
	 _struct_pad(nrs_symbol_extra)                

	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(udf)
	 _struct_pad(nrs_symbol_extra)	/* %unbound-function% */

	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(init_misc)
	 _struct_pad(nrs_symbol_extra)	/* %init-misc */

	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(macro_code)
	 _struct_pad(nrs_symbol_extra)	/* %macro-code% */

	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(closure_code)
	 _struct_pad(nrs_symbol_extra)      /* %closure-code% */

       	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(new_gcable_ptr) /* %new-gcable-ptr */
	 _struct_pad(nrs_symbol_extra)
	
       	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(gc_event_status_bits)
	 _struct_pad(nrs_symbol_extra)	/* *gc-event-status-bits* */

	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(post_gc_hook)
	 _struct_pad(nrs_symbol_extra)	/* *post-gc-hook* */

	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(handlers)
	 _struct_pad(nrs_symbol_extra)	/* %handlers% */


	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(all_packages)
	 _struct_pad(nrs_symbol_extra)	/* %all-packages% */

	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(keyword_package)
	 _struct_pad(nrs_symbol_extra)	/* *keyword-package* */

	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(os_init_function)
	 _struct_pad(nrs_symbol_extra)	/* %os-init-function% */

	 _struct_pad(nrs_symbol_fulltag)
	 _struct_label(foreign_thread_control)
	 _struct_pad(nrs_symbol_extra)	/* %foreign-thread-control */
        _ends


	
	_struct(lisp_globals,lisp_globals_limit)
         _rnode(get_tcr)		/* address of get_tcr() for callbacks */
         _rnode(tcr_count)		/* next tcr's tcr_id */
         _rnode(interrupt_signal)  /* signal to use for PROCESS-INTERRUPT */
         _rnode(kernel_imports)	/* some things we need to have imported for us. */
         _rnode(objc_2_personality) /* A good listener.  Doesn't say much */
         _rnode(savetoc)        /* Saved TOC register, for some platforms */
         _rnode(saver13)	/* Saved (global) r13, on some platforms */
         _rnode(subprims_base)	/* where the dynamic subprims wound up */
         _rnode(ret1valn)		/* magic multiple-values return address */
         _rnode(tcr_key)     	/* tsd key for per-thread tcr */
         _rnode(tcr_area_lock)       /* all_areas/tcr queue lock */
         _rnode(exception_lock)	/* serialize exception handling */
         _rnode(static_conses)
         _rnode(default_allocation_quantum)
         _rnode(intflag)
         _rnode(gc_inhibit_count)
         _rnode(refbits)
         _rnode(oldspace_dnode_count) /* count of dynamic dnodes older than generation 0 */
         __ifdef(`PPC')
          _rnode(altivec_present)   /* non-zero if AltiVec present. */
         __else
          _rnode(float_abi)         /* non zero when hard-float ABI in effect */
         __endif
         _rnode(fwdnum)            /* fixnum: GC "forwarder" call count. */
         _rnode(gc_num)            /* fixnum: GC call count. */
         _rnode(gcable_pointers)   /* linked-list of weak macptrs. */
         _rnode(heap_start)        /* start of lisp heap */
         _rnode(heap_end)          /* end of lisp heap */
         _rnode(statically_linked)        /* non-zero if -static */
         _rnode(stack_size)        /* from the command line */
         _rnode(objc_2_begin_catch)  /* address of ObjC 2.0 objc_begin_catch() */
         _rnode(kernel_path)       /* real executable name */
         _rnode(all_areas)         /* doubly-linked list of stack & heap areas */
         _rnode(lexpr_return)      /* magic &lexpr cleanup code */
         _rnode(lexpr_return1v)    /* single-value &lexpr cleanup code */
         _rnode(in_gc)             /* non-zero when lisp addresses may be invalid */
         _rnode(free_static_conses)     /* length of freelist */
         _rnode(objc_2_end_catch)          /* address of ObjC 2.0 objc_end_catch() */
         _rnode(short_float_zero)  /* low half of 1.0d0 */
         _rnode(double_float_one)  /* high half of 1.0d0 */
         _rnode(static_cons_area)	/* static_cons_area */
         _rnode(lisp_exit_hook)	/* install foreign exception handling */
         _rnode(oldest_ephemeral)  /* doubleword address of oldest ephemeral object or 0 */
         _rnode(tenured_area)      /* the tenured area */
         _rnode(ref_base)          /* start of oldest pointer-bearing area */
         _rnode(argv)              /* pointer to &argv[0] */
         _rnode(host_platform)	/* for platform-specific initialization */
         _rnode(batch_flag)	/* -b arg */
         _rnode(unwind_resume)	/* address of _Unwind_Resume from libobjc */
         _rnode(weak_gc_method)	/* weak GC algorithm */
         _rnode(image_name)	/* --image-name arg */
         _rnode(initial_tcr)	/* initial thread tcr */
         _rnode(weakvll)           /* all populations as of last GC */
         _rnode(managed_static_refbits) /* refs from managed_static to dynamic */
         _rnode(managed_static_dnodes) /* ndnodes in managed_static_area */
         _rnode(ephemeral_refidx) /* index of refbits */
	_ends

/* Traditional name, differs from C */
        .set lisp_globals.ret1val_addr,lisp_globals.ret1valn
        