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