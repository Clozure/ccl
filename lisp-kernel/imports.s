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

	include(m4macros.m4)
define(`PTR',`
        __ifdef(`PPC64')
        .quad $1
        __else
	 __ifdef(`X8664')
	 .quad $1
	 __else
	  .long $1
	 __endif
        __endif
')
	_beginfile

        	
	.globl C(import_ptrs_base)
define(`defimport',`
	.globl C($1)
        PTR(C($1))
                
# __line__
')
	.data
import_ptrs_start:

	defimport(fd_setsize_bytes)
	defimport(do_fd_set)
	defimport(do_fd_clr)
	defimport(do_fd_is_set)
	defimport(do_fd_zero)
	defimport(xMakeDataExecutable)
	defimport(xGetSharedLibrary)
	defimport(xFindSymbol)
	defimport(lisp_malloc)
	defimport(lisp_free)
	defimport(wait_for_signal)
	defimport(tcr_frame_ptr)
	defimport(register_xmacptr_dispose_function)
	defimport(open_debug_output)
	defimport(get_r_debug)
	defimport(restore_soft_stack_limit)
	defimport(lisp_egc_control)
	defimport(lisp_bug)
	defimport(xNewThread)
	defimport(cooperative_thread_startup)
	defimport(xDisposeThread)
	defimport(xThreadCurrentStackSpace)
	defimport(usage_exit)
	defimport(save_fp_context)
	defimport(restore_fp_context)
	defimport(put_vector_registers)
	defimport(get_vector_registers)
        defimport(new_semaphore)
	defimport(wait_on_semaphore)
	defimport(signal_semaphore)
        defimport(destroy_semaphore)
        defimport(new_recursive_lock)
        defimport(lock_recursive_lock)
        defimport(unlock_recursive_lock)
        defimport(destroy_recursive_lock)
        defimport(lisp_suspend_other_threads)
        defimport(lisp_resume_other_threads)
        defimport(lisp_suspend_tcr)
        defimport(lisp_resume_tcr)
        defimport(rwlock_new)
        defimport(rwlock_destroy)
        defimport(rwlock_rlock)
        defimport(rwlock_wlock)
        defimport(rwlock_unlock)
        defimport(recursive_lock_trylock)
	defimport(foreign_name_and_offset)
        defimport(lisp_read)
        defimport(lisp_write)
        defimport(lisp_open)
        defimport(lisp_fchmod)
        defimport(lisp_lseek)
        defimport(lisp_close)
        defimport(lisp_ftruncate)
        defimport(lisp_stat)
        defimport(lisp_fstat)
        defimport(lisp_futex)
        defimport(lisp_opendir)
        defimport(lisp_readdir)
        defimport(lisp_closedir)
        defimport(lisp_pipe)
        defimport(lisp_gettimeofday)
        defimport(lisp_sigexit)
   
        .globl C(import_ptrs_base)
C(import_ptrs_base):
	PTR(import_ptrs_start)

	__ifdef(`PPC')
        __ifdef(`LINUX')
        __ifndef(`PPC64')
        .globl __trampoline_setup
	.long  __trampoline_setup
        __endif
        __endif
	__endif

        /* Need to be sure that the kernel links against advapi32.dll ;
           the random number generator needs to call into that library */
	__ifdef(`WIN_64')
	.globl C(SystemFunction036)
	.long C(SystemFunction036)
	__endif




	_endfile
