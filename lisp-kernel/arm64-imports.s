/* SPDX-License-Identifier: Apache-2.0 */

/*
 * A table of pointers to kernel-provided routines that Lisp uses.
 * Lisp finds this table via the KERNEL_IMPORTS lisp_global, which is
 * initialized to import_ptrs_base.
 *
 * XXX - imports.s should be revised to use C preprocessor macros and
 * then this file should be merged into it.
 */

#include "arm64-asm.h"

#define defimport(name) .quad C(name)

        .section RELRO
        .p2align 3

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
        .quad import_ptrs_start
