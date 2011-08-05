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

#include <stdlib.h>
#ifndef WINDOWS
#include <unistd.h>
#include <sys/mman.h>
#endif
#undef __argv
#include <stdio.h>
#ifndef WINDOWS
#include <pthread.h>
#endif
#ifdef WINDOWS
#include <process.h>
#endif
#include <errno.h>
#include <limits.h>

#ifdef SOLARIS
#include <sys/syscall.h>
#include <sys/lwp.h>
#endif

#ifdef LINUX
#include <sys/syscall.h>
#endif

#undef USE_MACH_SEMAPHORES
#define USE_POSIX_SEMAPHORES
#undef USE_WINDOWS_SEMAPHORES

#ifdef DARWIN
#define USE_MACH_SEMAPHORES 1
#undef  USE_POSIX_SEMAPHORES
#endif
#ifdef WINDOWS
#define USE_WINDOWS_SEMAPHORES 1
#undef USE_POSIX_SEMAPHORES
#ifdef NEED_TIMESPEC
struct timespec {
  int tv_sec;
  int tv_nsec;
};
#endif
#endif

#ifdef USE_POSIX_SEMAPHORES
#include <semaphore.h>
#endif


#ifdef USE_MACH_SEMAPHORES
/* We have to use Mach semaphores, even if we're otherwise 
   using POSIX signals, etc. */
#include <mach/task.h>
#include <mach/semaphore.h>
#endif

#include <limits.h>

#ifdef FREEBSD
#include <pthread_np.h>
#endif

#ifndef WINDOWS
#ifndef ANDROID
#include <sched.h>
#endif
#endif

#include "lisp.h"
#include "lisp_globals.h"
#include "gc.h"

#ifdef USE_FUTEX
#ifndef FUTEX_WAIT
#define FUTEX_WAIT (0)
#endif
#ifndef FUTEX_WAKE
#define FUTEX_WAKE (1)
#endif
#include <sys/syscall.h>
#endif

#ifndef WINDOWS
#include <syslog.h>
#endif

Boolean extern threads_initialized;
Boolean extern log_tcr_info;

#define LOCK_SPINLOCK(x,tcr) get_spin_lock(&(x),tcr)
#define RELEASE_SPINLOCK(x) (x)=0

#ifdef WIN_32
#define TCR_TO_TSD(tcr) ((void *)((natural)(tcr)))
#define TCR_FROM_TSD(tsd) ((TCR *)((natural)(tsd)))
#else
#define TCR_TO_TSD(tcr) ((void *)((natural)(tcr)+TCR_BIAS))
#define TCR_FROM_TSD(tsd) ((TCR *)((natural)(tsd)-TCR_BIAS))
#endif

#ifdef USE_WINDOWS_SEMAPHORES

typedef void * SEMAPHORE;
#define SEM_WAIT(s) WaitForSingleObject(s,INFINITE)
#define SEM_RAISE(s) ReleaseSemaphore(s, 1L, NULL)
#define SEM_BROADCAST(s, count) do {while(count) {SEM_RAISE(s);(count)--;}}while(0)
#define SEM_TIMEDWAIT(s,t) WaitOnSingleObject(s,t)

#endif
#ifdef USE_POSIX_SEMAPHORES
typedef sem_t * SEMAPHORE;
#define SEM_WAIT(s) sem_wait((SEMAPHORE)s)
#define SEM_RAISE(s) sem_post((SEMAPHORE)s)
#define SEM_BROADCAST(s, count) do {while(count) {SEM_RAISE(s);(count)--;}}while(0)
#define SEM_TIMEDWAIT(s,t) sem_timedwait((SEMAPHORE)s,(struct timespec *)t)
#endif

#ifdef USE_MACH_SEMAPHORES
typedef semaphore_t SEMAPHORE;
#define SEM_WAIT(s) semaphore_wait((SEMAPHORE)(natural)s)
#define SEM_RAISE(s) semaphore_signal((SEMAPHORE)(natural)s)
#define SEM_BROADCAST(s,count)semaphore_signal_all((SEMAPHORE)(natural)s)
#define SEM_TIMEDWAIT(s,t) semaphore_timedwait((SEMAPHORE)(natural)s,t)
#endif

void signal_semaphore(SEMAPHORE s);
int wait_on_semaphore(void *s, int seconds, int millis);
void sem_wait_forever(SEMAPHORE s);

#ifdef USE_POSIX_SEMAPHORES
#define SEM_WAIT_FOREVER(s) sem_wait_forever((SEMAPHORE)s)
#endif

#ifdef USE_MACH_SEMAPHORES
#define SEM_WAIT_FOREVER(s) sem_wait_forever((SEMAPHORE)(natural)s)
#endif

#ifdef USE_WINDOWS_SEMAPHORES
#define SEM_WAIT_FOREVER(s) sem_wait_forever((SEMAPHORE)s)
#endif

typedef struct
{
  signed_natural avail;
  TCR* owner;
  signed_natural  count;
  void* signal;
  signed_natural waiting;
  void *malloced_ptr;
  signed_natural spinlock;
} _recursive_lock, *RECURSIVE_LOCK;


int lock_recursive_lock(RECURSIVE_LOCK, TCR *);
int unlock_recursive_lock(RECURSIVE_LOCK, TCR *);
RECURSIVE_LOCK new_recursive_lock(void);
void destroy_recursive_lock(RECURSIVE_LOCK);
int recursive_lock_trylock(RECURSIVE_LOCK, TCR *, int *);

#define LOCK(m, t) lock_recursive_lock((RECURSIVE_LOCK)ptr_from_lispobj(m), (TCR *)t)
#define UNLOCK(m, t) unlock_recursive_lock((RECURSIVE_LOCK)ptr_from_lispobj(m), (TCR *)t)

/* Hmm.  This doesn't look like the MacOS Thread Manager ... */
LispObj current_thread_osid(void);
void *current_native_thread_id(void);
void *new_semaphore(int);
void destroy_semaphore(void**);
void tsd_set(LispObj, void *);
void *tsd_get(LispObj);
TCR *new_tcr(natural, natural);
void thread_init_tcr(TCR *tcr, void *stack_base, natural stack_size);
TCR *initial_thread_tcr;

#define DEFAULT_THREAD_STACK_SIZE ((size_t) -1)
#define MINIMAL_THREAD_STACK_SIZE ((size_t) 0)


Boolean create_system_thread(size_t stack_size, 
			     void* stackaddr,
#ifdef WINDOWS
                             unsigned CALLBACK (*start_routine)(void *)
#else
			     void* (*start_routine)(void *)
#endif
                             ,
			     void* param);

TCR *get_tcr(Boolean);
TCR *get_interrupt_tcr(Boolean);

Boolean suspend_tcr(TCR *);
Boolean resume_tcr(TCR *);
Boolean kill_tcr(TCR *);

int raise_thread_interrupt(TCR *target);

Boolean lisp_suspend_tcr(TCR *);
Boolean lisp_resume_tcr(TCR *);
void lisp_suspend_other_threads(void);
void lisp_resume_other_threads(void);

typedef struct
{
  signed_natural spin; /* need spin lock to change fields */
  signed_natural state; /* 0 = free, positive if writer, negative if readers; */
  natural blocked_writers;
  natural blocked_readers;
  TCR  *writer;
#ifdef USE_FUTEX
  natural reader_signal;
  natural writer_signal;
#else
  void * reader_signal;
  void * writer_signal;
#endif
  void *malloced_ptr;
} rwlock;


rwlock * rwlock_new(void);
void rwlock_destroy(rwlock *);
int rwlock_rlock(rwlock *, TCR *, struct timespec *);
int rwlock_wlock(rwlock *, TCR *, struct timespec *);
int rwlock_try_wlock(rwlock *, TCR *);
int rwlock_try_rlock(rwlock *, TCR *);
int rwlock_unlock(rwlock *, TCR *);


natural 
atomic_and(natural*, natural);

natural 
atomic_ior(natural*, natural);

#define SET_TCR_FLAG(t,bit) atomic_ior(&(t->flags),(1L<<bit))
#define CLR_TCR_FLAG(t,bit) atomic_and(&(t->flags),~(1L<<bit))



extern int thread_suspend_signal, thread_kill_signal;

void *
allocate_stack(natural);

void
suspend_resume_handler(int, siginfo_t *, ExceptionInformation *);

/* Maybe later
Boolean
rwlock_try_rlock(rwlock *);

Boolean
rwlock_try_wlock(rwlock *);
*/
