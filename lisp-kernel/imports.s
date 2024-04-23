/*
 * Copyright 1994-2009 Clozure Associates
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
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
	defimport(do_nothing)
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
        defimport(jvm_init)
        defimport(lisp_lstat)
        defimport(lisp_realpath)
   
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

	_endfile
