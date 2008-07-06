/*
   Copyright (C) 1994-2001 Digitool, Inc
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


#include "Threads.h"

/*
   If we suspend via signals - and if the "suspend" signal is maked
   in the handler for that signal - then it's not possible to suspend
   a thread that's still waiting to be resumed (which is what
   WAIT_FOR_RESUME_ACK is all about.)
*/
#define WAIT_FOR_RESUME_ACK 0
#define RESUME_VIA_RESUME_SEMAPHORE 1
#define SUSPEND_RESUME_VERBOSE 0

typedef struct {
  TCR *tcr;
  natural vsize, tsize;
  void *created;
} thread_activation;

#ifdef HAVE_TLS
__thread TCR current_tcr;
#endif

extern natural
store_conditional(natural*, natural, natural);

extern signed_natural
atomic_swap(signed_natural*, signed_natural);

#ifdef USE_FUTEX
#define futex_wait(futex,val) syscall(SYS_futex,futex,FUTEX_WAIT,val)
#define futex_wake(futex,n) syscall(SYS_futex,futex,FUTEX_WAKE,n)
#define FUTEX_AVAIL (0)
#define FUTEX_LOCKED (1)
#define FUTEX_CONTENDED (2)
#endif

#ifdef WINDOWS
int
raise_thread_interrupt(TCR *target)
{
}
#else
int
raise_thread_interrupt(TCR *target)
{
#ifdef DARWIN_not_yet
  if (use_mach_exception_handling) {
    return mach_raise_thread_interrupt(target);
  }
#endif
 return pthread_kill((pthread_t)target->osid, SIGNAL_FOR_PROCESS_INTERRUPT);
}
#endif

signed_natural
atomic_incf_by(signed_natural *ptr, signed_natural by)
{
  signed_natural old, new;
  do {
    old = *ptr;
    new = old+by;
  } while (store_conditional((natural *)ptr, (natural) old, (natural) new) !=
           (natural) old);
  return new;
}

signed_natural
atomic_incf(signed_natural *ptr)
{
  return atomic_incf_by(ptr, 1);
}

signed_natural
atomic_decf(signed_natural *ptr)
{
  signed_natural old, new;
  do {
    old = *ptr;
    new = old == 0 ? old : old-1;
  } while (store_conditional((natural *)ptr, (natural) old, (natural) new) !=
           (natural) old);
  return old-1;
}


#ifndef USE_FUTEX
int spin_lock_tries = 1;

void
get_spin_lock(signed_natural *p, TCR *tcr)
{
  int i, n = spin_lock_tries;
  
  while (1) {
    for (i = 0; i < n; i++) {
      if (atomic_swap(p,(signed_natural)tcr) == 0) {
        return;
      }
    }
#ifndef WINDOWS
    sched_yield();
#endif
  }
}
#endif

#ifndef USE_FUTEX
int
lock_recursive_lock(RECURSIVE_LOCK m, TCR *tcr)
{

  if (tcr == NULL) {
    tcr = get_tcr(true);
  }
  if (m->owner == tcr) {
    m->count++;
    return 0;
  }
  while (1) {
    LOCK_SPINLOCK(m->spinlock,tcr);
    ++m->avail;
    if (m->avail == 1) {
      m->owner = tcr;
      m->count = 1;
      RELEASE_SPINLOCK(m->spinlock);
      break;
    }
    RELEASE_SPINLOCK(m->spinlock);
    SEM_WAIT_FOREVER(m->signal);
  }
  return 0;
}

#else /* USE_FUTEX */

static void inline
lock_futex(natural *p)
{
  
  while (1) {
    if (store_conditional(p,FUTEX_AVAIL,FUTEX_LOCKED) == FUTEX_AVAIL) {
      return;
    }
    while (1) {
      if (atomic_swap(p,FUTEX_CONTENDED) == FUTEX_AVAIL) {
        return;
      }
      futex_wait(p,FUTEX_CONTENDED);
    }
  }
}

static void inline
unlock_futex(natural *p)
{
  if (atomic_decf(p) != FUTEX_AVAIL) {
    *p = FUTEX_AVAIL;
    futex_wake(p,INT_MAX);
  }
}
    
int
lock_recursive_lock(RECURSIVE_LOCK m, TCR *tcr)
{
  natural val;
  if (tcr == NULL) {
    tcr = get_tcr(true);
  }
  if (m->owner == tcr) {
    m->count++;
    return 0;
  }
  lock_futex(&m->avail);
  m->owner = tcr;
  m->count = 1;
  return 0;
}
#endif /* USE_FUTEX */


#ifndef USE_FUTEX  
int
unlock_recursive_lock(RECURSIVE_LOCK m, TCR *tcr)
{
  int ret = EPERM, pending;

  if (tcr == NULL) {
    tcr = get_tcr(true);
  }

  if (m->owner == tcr) {
    --m->count;
    if (m->count == 0) {
      LOCK_SPINLOCK(m->spinlock,tcr);
      m->owner = NULL;
      pending = m->avail-1 + m->waiting;     /* Don't count us */
      m->avail = 0;
      --pending;
      if (pending > 0) {
        m->waiting = pending;
      } else {
        m->waiting = 0;
      }
      RELEASE_SPINLOCK(m->spinlock);
      if (pending >= 0) {
	SEM_RAISE(m->signal);
      }
    }
    ret = 0;
  }
  return ret;
}
#else /* USE_FUTEX */
int
unlock_recursive_lock(RECURSIVE_LOCK m, TCR *tcr)
{
  int ret = EPERM, pending;

   if (tcr == NULL) {
    tcr = get_tcr(true);
  }

  if (m->owner == tcr) {
    --m->count;
    if (m->count == 0) {
      m->owner = NULL;
      unlock_futex(&m->avail);
    }
    ret = 0;
  }
  return ret;
}
#endif /* USE_FUTEX */

void
destroy_recursive_lock(RECURSIVE_LOCK m)
{
#ifndef USE_FUTEX
  destroy_semaphore((void **)&m->signal);
#endif
  postGCfree((void *)(m->malloced_ptr));
}

/*
  If we're already the owner (or if the lock is free), lock it
  and increment the lock count; otherwise, return EBUSY without
  waiting.
*/

#ifndef USE_FUTEX
int
recursive_lock_trylock(RECURSIVE_LOCK m, TCR *tcr, int *was_free)
{
  TCR *owner = m->owner;

  LOCK_SPINLOCK(m->spinlock,tcr);
  if (owner == tcr) {
    m->count++;
    if (was_free) {
      *was_free = 0;
      RELEASE_SPINLOCK(m->spinlock);
      return 0;
    }
  }
  if (store_conditional((natural*)&(m->avail), 0, 1) == 0) {
    m->owner = tcr;
    m->count = 1;
    if (was_free) {
      *was_free = 1;
    }
    RELEASE_SPINLOCK(m->spinlock);
    return 0;
  }

  RELEASE_SPINLOCK(m->spinlock);
  return EBUSY;
}
#else
int
recursive_lock_trylock(RECURSIVE_LOCK m, TCR *tcr, int *was_free)
{
  TCR *owner = m->owner;

  if (owner == tcr) {
    m->count++;
    if (was_free) {
      *was_free = 0;
      return 0;
    }
  }
  if (store_conditional((natural*)&(m->avail), 0, 1) == 0) {
    m->owner = tcr;
    m->count = 1;
    if (was_free) {
      *was_free = 1;
    }
    return 0;
  }

  return EBUSY;
}
#endif

void
sem_wait_forever(SEMAPHORE s)
{
  int status;

  do {
#ifdef USE_MACH_SEMAPHORES
    mach_timespec_t q = {1,0};
    status = SEM_TIMEDWAIT(s,q);
#endif
#ifdef USE_POSIX_SEMAPHORES
    struct timespec q;
    gettimeofday((struct timeval *)&q, NULL);
    q.tv_sec += 1;
    status = SEM_TIMEDWAIT(s,&q);
#endif
  } while (status != 0);
}

int
wait_on_semaphore(void *s, int seconds, int millis)
{
  int nanos = (millis % 1000) * 1000000;
#ifdef USE_POSIX_SEMAPHORES
  int status;

  struct timespec q;
  gettimeofday((struct timeval *)&q, NULL);
  q.tv_nsec *= 1000L;  /* microseconds -> nanoseconds */
    
  q.tv_nsec += nanos;
  if (q.tv_nsec >= 1000000000L) {
    q.tv_nsec -= 1000000000L;
    seconds += 1;
  }
  q.tv_sec += seconds;
  status = SEM_TIMEDWAIT(s, &q);
  if (status < 0) {
    return errno;
  }
  return status;
#endif
#ifdef USE_MACH_SEMAPHORES
  mach_timespec_t q = {seconds, nanos};
  int status = SEM_TIMEDWAIT(s, q);

  
  switch (status) {
  case 0: return 0;
  case KERN_OPERATION_TIMED_OUT: return ETIMEDOUT;
  case KERN_ABORTED: return EINTR;
  default: return EINVAL;
  }

#endif
}


int
semaphore_maybe_timedwait(void *s, struct timespec *t)
{
  if (t) {
    return wait_on_semaphore(s, t->tv_sec, t->tv_nsec/1000000L);
  }
  SEM_WAIT_FOREVER(s);
  return 0;
}

void
signal_semaphore(SEMAPHORE s)
{
  SEM_RAISE(s);
}

  
#ifdef WINDOWS
LispObj
current_thread_osid()
{
}
#else
LispObj
current_thread_osid()
{
  return (LispObj)ptr_to_lispobj(pthread_self());
}
#endif


int thread_suspend_signal = 0, thread_resume_signal = 0;



void
linux_exception_init(TCR *tcr)
{
}


TCR *
get_interrupt_tcr(Boolean create)
{
  return get_tcr(create);
}
  
  void
suspend_resume_handler(int signo, siginfo_t *info, ExceptionInformation *context)
{
#ifdef DARWIN_GS_HACK
  Boolean gs_was_tcr = ensure_gs_pthread();
#endif
  TCR *tcr = get_interrupt_tcr(false);

  if (TCR_INTERRUPT_LEVEL(tcr) <= (-2<<fixnumshift)) {
    SET_TCR_FLAG(tcr,TCR_FLAG_BIT_PENDING_SUSPEND);
  } else {
    if (signo == thread_suspend_signal) {
#if 0
      sigset_t wait_for;
#endif

      tcr->suspend_context = context;
#if 0
      sigfillset(&wait_for);
#endif
      SEM_RAISE(tcr->suspend);
#if 0
      sigdelset(&wait_for, thread_resume_signal);
#endif
#if 1
#if RESUME_VIA_RESUME_SEMAPHORE
      SEM_WAIT_FOREVER(tcr->resume);
#if SUSPEND_RESUME_VERBOSE
      fprintf(stderr, "got  resume in 0x%x\n",tcr);
#endif
      tcr->suspend_context = NULL;
#else
      sigsuspend(&wait_for);
#endif
#else
    do {
      sigsuspend(&wait_for);
    } while (tcr->suspend_context);
#endif  
    } else {
      tcr->suspend_context = NULL;
#if SUSEPEND_RESUME_VERBOSE
      fprintf(stderr,"got  resume in in 0x%x\n",tcr);
#endif
    }
#if WAIT_FOR_RESUME_ACK
    SEM_RAISE(tcr->suspend);
#endif
  }
#ifdef DARWIN_GS_HACK
  if (gs_was_tcr) {
    set_gs_address(tcr);
  }
#endif
#ifdef DARWIN
  DarwinSigReturn(context);
#endif
#ifdef FREEBSD
  freebsd_sigreturn(context);
#endif
}

  

/*
  'base' should be set to the bottom (origin) of the stack, e.g., the
  end from which it grows.
*/
  
#ifdef WINDOWS
void
os_get_stack_bounds(LispObj q,void **base, natural *size)
{
}
#else
void
os_get_stack_bounds(LispObj q,void **base, natural *size)
{
  pthread_t p = (pthread_t)(q);
#ifdef DARWIN
  *base = pthread_get_stackaddr_np(p);
  *size = pthread_get_stacksize_np(p);
#endif
#ifdef LINUX
  pthread_attr_t attr;

  pthread_getattr_np(p,&attr);
  pthread_attr_getstack(&attr, base, size);
  *(natural *)base += *size;
#endif
#ifdef FREEBSD
  pthread_attr_t attr;
  void * temp_base;
  size_t temp_size;
  

  pthread_attr_init(&attr);  
  pthread_attr_get_np(p, &attr);
  pthread_attr_getstackaddr(&attr,&temp_base);
  pthread_attr_getstacksize(&attr,&temp_size);
  *base = (void *)((natural)temp_base + temp_size);
  *size = temp_size;
#endif

}
#endif

void *
new_semaphore(int count)
{
#ifdef USE_POSIX_SEMAPHORES
  sem_t *s = malloc(sizeof(sem_t));
  sem_init(s, 0, count);
  return s;
#endif
#ifdef USE_MACH_SEMAPHORES
  semaphore_t s = (semaphore_t)0;
  semaphore_create(mach_task_self(),&s, SYNC_POLICY_FIFO, count);
  return (void *)(natural)s;
#endif
}

RECURSIVE_LOCK
new_recursive_lock()
{
  extern int cache_block_size;
  void *p = calloc(1,sizeof(_recursive_lock)+cache_block_size-1);
  RECURSIVE_LOCK m = NULL;
#ifndef USE_FUTEX
  void *signal = new_semaphore(0);
#endif

  if (p) {
    m = (RECURSIVE_LOCK) ((((natural)p)+cache_block_size-1) & (~(cache_block_size-1)));
    m->malloced_ptr = p;
  }

#ifdef USE_FUTEX
  if (m) {
    return m;
  }
#else
  if (m && signal) {
    m->signal = signal;
    return m;
  }
  if (m) {
    free(p);
  }
  if (signal) {
    destroy_semaphore(&signal);
  }
#endif
  return NULL;
}

void
destroy_semaphore(void **s)
{
  if (*s) {
#ifdef USE_POSIX_SEMAPHORES
    sem_destroy((sem_t *)*s);
#endif
#ifdef USE_MACH_SEMAPHORES
    semaphore_destroy(mach_task_self(),((semaphore_t)(natural) *s));
#endif
    *s=NULL;
  }
}

#ifdef WINDOWS
void
tsd_set(LispObj key, void *datum)
{
}

void *
tsd_get(LispObj key)
{
}
#else
void
tsd_set(LispObj key, void *datum)
{
  pthread_setspecific((pthread_key_t)key, datum);
}

void *
tsd_get(LispObj key)
{
  return pthread_getspecific((pthread_key_t)key);
}
#endif

void
dequeue_tcr(TCR *tcr)
{
  TCR *next, *prev;

  next = tcr->next;
  prev = tcr->prev;

  prev->next = next;
  next->prev = prev;
  tcr->prev = tcr->next = NULL;
#ifdef X8664
  tcr->linear = NULL;
#endif
}
  
void
enqueue_tcr(TCR *new)
{
  TCR *head, *tail;
  
  LOCK(lisp_global(TCR_AREA_LOCK),new);
  head = (TCR *)ptr_from_lispobj(lisp_global(INITIAL_TCR));
  tail = head->prev;
  tail->next = new;
  head->prev = new;
  new->prev = tail;
  new->next = head;
  UNLOCK(lisp_global(TCR_AREA_LOCK),new);
}

TCR *
allocate_tcr()
{
  TCR *tcr, *chain = NULL, *next;
#ifdef DARWIN
  extern Boolean use_mach_exception_handling;
  kern_return_t kret;
  mach_port_t 
    thread_exception_port,
    task_self = mach_task_self();
#endif
  for (;;) {
    tcr = calloc(1, sizeof(TCR));
#ifdef DARWIN
#if WORD_SIZE == 64
    if (((unsigned)((natural)tcr)) != ((natural)tcr)) {
      tcr->next = chain;
      chain = tcr;
      continue;
    }
#endif
    if (use_mach_exception_handling) {
      thread_exception_port = (mach_port_t)((natural)tcr);
      kret = mach_port_allocate_name(task_self,
                                     MACH_PORT_RIGHT_RECEIVE,
                                     thread_exception_port);
    } else {
      kret = KERN_SUCCESS;
    }

    if (kret != KERN_SUCCESS) {
      tcr->next = chain;
      chain = tcr;
      continue;
    }
#endif
    for (next = chain; next;) {
      next = next->next;
      free(chain);
    }
    return tcr;
  }
}

#ifdef X8664
#ifdef LINUX
#include <asm/prctl.h>
#include <sys/prctl.h>
#endif
#ifdef FREEBSD
#include <machine/sysarch.h>
#endif

void
setup_tcr_extra_segment(TCR *tcr)
{
#ifdef FREEBSD
  amd64_set_gsbase(tcr);
#endif
#ifdef LINUX
  arch_prctl(ARCH_SET_GS, (natural)tcr);
#endif
#ifdef DARWIN
  /* There's no way to do this yet.  See DARWIN_GS_HACK */
  /* darwin_set_x8664_fs_reg(tcr); */
#endif
}

#endif



/*
  Caller must hold the area_lock.
*/
#ifdef WINDOWS
TCR *
new_tcr(natural vstack_size, natural tstack_size)
{
}
#else
TCR *
new_tcr(natural vstack_size, natural tstack_size)
{
  extern area
    *allocate_vstack_holding_area_lock(natural),
    *allocate_tstack_holding_area_lock(natural);
  area *a;
  int i;
  sigset_t sigmask;

  sigemptyset(&sigmask);
  pthread_sigmask(SIG_SETMASK,&sigmask, NULL);
#ifdef HAVE_TLS
  TCR *tcr = &current_tcr;
#else
  TCR *tcr = allocate_tcr();
#endif

#ifdef X8664
  setup_tcr_extra_segment(tcr);
  tcr->linear = tcr;
#endif

#if (WORD_SIZE == 64)
  tcr->single_float_convert.tag = subtag_single_float;
#endif
  lisp_global(TCR_COUNT) += (1<<fixnumshift);
  tcr->suspend = new_semaphore(0);
  tcr->resume = new_semaphore(0);
  tcr->reset_completion = new_semaphore(0);
  tcr->activate = new_semaphore(0);
  LOCK(lisp_global(TCR_AREA_LOCK),tcr);
  a = allocate_vstack_holding_area_lock(vstack_size);
  tcr->vs_area = a;
  a->owner = tcr;
  tcr->save_vsp = (LispObj *) a->active;  
  a = allocate_tstack_holding_area_lock(tstack_size);
  UNLOCK(lisp_global(TCR_AREA_LOCK),tcr);
  tcr->ts_area = a;
  a->owner = tcr;
  tcr->save_tsp = (LispObj *) a->active;
#ifdef X86
  tcr->next_tsp = tcr->save_tsp;
#endif

  tcr->valence = TCR_STATE_FOREIGN;
#ifdef PPC
  tcr->lisp_fpscr.words.l = 0xd0;
#endif
#ifdef X86
  tcr->lisp_mxcsr = (1 << MXCSR_DM_BIT) | 
#if 1                           /* Mask underflow; too hard to 
                                   deal with denorms if underflow is 
                                   enabled */
    (1 << MXCSR_UM_BIT) | 
#endif
    (1 << MXCSR_PM_BIT);
#endif
  tcr->save_allocbase = tcr->save_allocptr = (void *) VOID_ALLOCPTR;
  tcr->tlb_limit = 2048<<fixnumshift;
  tcr->tlb_pointer = (LispObj *)malloc(tcr->tlb_limit);
  for (i = 0; i < 2048; i++) {
    tcr->tlb_pointer[i] = (LispObj) no_thread_local_binding_marker;
  }
  TCR_INTERRUPT_LEVEL(tcr) = (LispObj) (-1<<fixnum_shift);
  tcr->shutdown_count = PTHREAD_DESTRUCTOR_ITERATIONS;
  return tcr;
}
#endif

void
shutdown_thread_tcr(void *arg)
{
  TCR *tcr = TCR_FROM_TSD(arg);

  area *vs, *ts, *cs;
  void *termination_semaphore;
  
  if (--(tcr->shutdown_count) == 0) {
    if (tcr->flags & (1<<TCR_FLAG_BIT_FOREIGN)) {
      LispObj callback_macptr = nrs_FOREIGN_THREAD_CONTROL.vcell,
	callback_ptr = ((macptr *)ptr_from_lispobj(untag(callback_macptr)))->address;
    
      tsd_set(lisp_global(TCR_KEY), TCR_TO_TSD(tcr));
      ((void (*)())ptr_from_lispobj(callback_ptr))(1);
      tsd_set(lisp_global(TCR_KEY), NULL);
    }
#ifdef DARWIN
    darwin_exception_cleanup(tcr);
#endif
    LOCK(lisp_global(TCR_AREA_LOCK),tcr);
    vs = tcr->vs_area;
    tcr->vs_area = NULL;
    ts = tcr->ts_area;
    tcr->ts_area = NULL;
    cs = tcr->cs_area;
    tcr->cs_area = NULL;
    if (vs) {
      condemn_area_holding_area_lock(vs);
    }
    if (ts) {
      condemn_area_holding_area_lock(ts);
    }
    if (cs) {
      condemn_area_holding_area_lock(cs);
    }
    destroy_semaphore(&tcr->suspend);
    destroy_semaphore(&tcr->resume);
    destroy_semaphore(&tcr->reset_completion);
    destroy_semaphore(&tcr->activate);
    free(tcr->tlb_pointer);
    tcr->tlb_pointer = NULL;
    tcr->tlb_limit = 0;
    tcr->osid = 0;
    tcr->interrupt_pending = 0;
    termination_semaphore = tcr->termination_semaphore;
    tcr->termination_semaphore = NULL;
#ifdef HAVE_TLS
    dequeue_tcr(tcr);
#endif
    UNLOCK(lisp_global(TCR_AREA_LOCK),tcr);
    if (termination_semaphore) {
      SEM_RAISE(termination_semaphore);
    }
  } else {
    tsd_set(lisp_global(TCR_KEY), TCR_TO_TSD(tcr));
  }
}

void
tcr_cleanup(void *arg)
{
  TCR *tcr = (TCR *)arg;
  area *a;

  a = tcr->vs_area;
  if (a) {
    a->active = a->high;
  }
  a = tcr->ts_area;
  if (a) {
    a->active = a->high;
  }
  a = tcr->cs_area;
  if (a) {
    a->active = a->high;
  }
  tcr->valence = TCR_STATE_FOREIGN;
  tcr->shutdown_count = 1;
  shutdown_thread_tcr(tcr);
  tsd_set(lisp_global(TCR_KEY), NULL);
}

void *
current_native_thread_id()
{
  return ((void *) (natural)
#ifdef LINUX
          getpid()
#endif
#ifdef DARWIN
	  mach_thread_self()
#endif
#ifdef FREEBSD
	  pthread_self()
#endif
#ifdef SOLARIS
	  pthread_self()
#endif
#ifdef WINDOWS
	  /* ThreadSelf() */ 23
#endif
	  );
}


void
thread_init_tcr(TCR *tcr, void *stack_base, natural stack_size)
{
  area *a, *register_cstack_holding_area_lock(BytePtr, natural);

  tcr->osid = current_thread_osid();
  tcr->native_thread_id = current_native_thread_id();
  LOCK(lisp_global(TCR_AREA_LOCK),tcr);
  a = register_cstack_holding_area_lock((BytePtr)stack_base, stack_size);
  UNLOCK(lisp_global(TCR_AREA_LOCK),tcr);
  tcr->cs_area = a;
  a->owner = tcr;
  if (!(tcr->flags & (1<<TCR_FLAG_BIT_FOREIGN))) {
    tcr->cs_limit = (LispObj)ptr_to_lispobj(a->softlimit);
  }
#ifdef LINUX
#ifdef PPC
#ifndef PPC64
  tcr->native_thread_info = current_r2;
#endif
#endif
#endif
  tcr->errno_loc = &errno;
  tsd_set(lisp_global(TCR_KEY), TCR_TO_TSD(tcr));
#ifdef DARWIN
  extern Boolean use_mach_exception_handling;
  if (use_mach_exception_handling) {
    darwin_exception_init(tcr);
  }
#endif
#ifdef LINUX
  linux_exception_init(tcr);
#endif
  tcr->log2_allocation_quantum = unbox_fixnum(lisp_global(DEFAULT_ALLOCATION_QUANTUM));
}

/*
  Register the specified tcr as "belonging to" the current thread.
  Under Darwin, setup Mach exception handling for the thread.
  Install cleanup handlers for thread termination.
*/
void
register_thread_tcr(TCR *tcr)
{
  void *stack_base = NULL;
  natural stack_size = 0;

  os_get_stack_bounds(current_thread_osid(),&stack_base, &stack_size);
  thread_init_tcr(tcr, stack_base, stack_size);
  enqueue_tcr(tcr);
}


  
  
#ifndef MAP_GROWSDOWN
#define MAP_GROWSDOWN 0
#endif

#ifdef WINDOWS
Ptr
create_stack(int size)
{
}
#else
Ptr
create_stack(natural size)
{
  Ptr p;
  size=align_to_power_of_2(size, log2_page_size);
  p = (Ptr) mmap(NULL,
                 (size_t)size,
                 PROT_READ | PROT_WRITE | PROT_EXEC,
                 MAP_PRIVATE | MAP_ANON | MAP_GROWSDOWN,
                 -1,	/* Darwin insists on this when not mmap()ing
                           a real fd */
                 0);
  if (p != (Ptr)(-1)) {
    *((size_t *)p) = size;
    return p;
  }
  allocation_failure(true, size);

}
#endif

void *
allocate_stack(natural size)
{
  return create_stack(size);
}

#ifdef WINDOWS
void
free_stack(void *s)
{
}
#else
void
free_stack(void *s)
{
  size_t size = *((size_t *)s);
  munmap(s, size);
}
#endif

Boolean threads_initialized = false;

#ifndef USE_FUTEX
#ifdef WINDOWS
void
count_cpus()
{
}
#else
void
count_cpus()
{
#ifdef DARWIN
  /* As of OSX 10.4, Darwin doesn't define _SC_NPROCESSORS_ONLN */
#include <mach/host_info.h>

  struct host_basic_info info;
  mach_msg_type_number_t count = HOST_BASIC_INFO_COUNT;
  
  if (KERN_SUCCESS == host_info(mach_host_self(), HOST_BASIC_INFO,(host_info_t)(&info),&count)) {
    if (info.max_cpus > 1) {
      spin_lock_tries = 1024;
    }
  }
#else
  int n = sysconf(_SC_NPROCESSORS_ONLN);
  
  if (n > 1) {
    spin_lock_tries = 1024;
  }
#endif
}
#endif
#endif

#ifdef WINDOWS
void
init_threads(void * stack_base, TCR *tcr)
{
}
void *
lisp_thread_entry(void *param)
{
}
#else
void
init_threads(void * stack_base, TCR *tcr)
{
  lisp_global(INITIAL_TCR) = (LispObj)ptr_to_lispobj(tcr);
  pthread_key_create((pthread_key_t *)&(lisp_global(TCR_KEY)), shutdown_thread_tcr);
  thread_signal_setup();

#ifndef USE_FUTEX
  count_cpus();
#endif
  threads_initialized = true;
}


void *
lisp_thread_entry(void *param)
{
  thread_activation *activation = (thread_activation *)param;
  TCR *tcr = new_tcr(activation->vsize, activation->tsize);
  sigset_t mask, old_mask;

  sigemptyset(&mask);
  pthread_sigmask(SIG_SETMASK, &mask, &old_mask);

  register_thread_tcr(tcr);

  pthread_cleanup_push(tcr_cleanup,(void *)tcr);
  tcr->vs_area->active -= node_size;
  *(--tcr->save_vsp) = lisp_nil;
  enable_fp_exceptions();
  SET_TCR_FLAG(tcr,TCR_FLAG_BIT_AWAITING_PRESET);
  activation->tcr = tcr;
  SEM_RAISE(activation->created);
  do {
    SEM_RAISE(tcr->reset_completion);
    SEM_WAIT_FOREVER(tcr->activate);
    /* Now go run some lisp code */
    start_lisp(TCR_TO_TSD(tcr),0);
  } while (tcr->flags & (1<<TCR_FLAG_BIT_AWAITING_PRESET));
  pthread_cleanup_pop(true);

}
#endif

void *
xNewThread(natural control_stack_size,
	   natural value_stack_size,
	   natural temp_stack_size)

{
  thread_activation activation;


  activation.tsize = temp_stack_size;
  activation.vsize = value_stack_size;
  activation.tcr = 0;
  activation.created = new_semaphore(0);
  if (create_system_thread(control_stack_size +(CSTACK_HARDPROT+CSTACK_SOFTPROT), 
                           NULL, 
                           lisp_thread_entry,
                           (void *) &activation)) {
    
    SEM_WAIT_FOREVER(activation.created);	/* Wait until thread's entered its initial function */
  }
  destroy_semaphore(&activation.created);  
  return TCR_TO_TSD(activation.tcr);
}

Boolean
active_tcr_p(TCR *q)
{
  TCR *head = (TCR *)ptr_from_lispobj(lisp_global(INITIAL_TCR)), *p = head;
  
  do {
    if (p == q) {
      return true;
    }
    p = p->next;
  } while (p != head);
  return false;
}

#ifdef WINDOWS
OSErr
xDisposeThread(TCR *tcr)
{
}
#else
OSErr
xDisposeThread(TCR *tcr)
{
  if (tcr != (TCR *)ptr_from_lispobj(lisp_global(INITIAL_TCR))) {
    if (active_tcr_p(tcr) && (tcr != get_tcr(false))) {
      pthread_cancel((pthread_t)(tcr->osid));
      return 0;
    }
  }
  return -50;
}
#endif

OSErr
xYieldToThread(TCR *target)
{
  Bug(NULL, "xYieldToThread ?");
  return 0;
}
  
OSErr
xThreadCurrentStackSpace(TCR *tcr, unsigned *resultP)
{
  Bug(NULL, "xThreadCurrentStackSpace ?");
  return 0;
}


#ifdef WINDOWS
LispObj
create_system_thread(size_t stack_size,
		     void* stackaddr,
		     void* (*start_routine)(void *),
		     void* param)
{
}
#else
LispObj
create_system_thread(size_t stack_size,
		     void* stackaddr,
		     void* (*start_routine)(void *),
		     void* param)
{
  pthread_attr_t attr;
  pthread_t returned_thread = (pthread_t) 0;

  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);  

  if (stack_size == MINIMAL_THREAD_STACK_SIZE) {
    stack_size = PTHREAD_STACK_MIN;
  }

  stack_size = ensure_stack_limit(stack_size);
  if (stackaddr != NULL) {
    /* Size must have been specified.  Sort of makes sense ... */
#ifdef DARWIN
    Fatal("no pthread_attr_setsetstack. "," Which end of stack does address refer to?");
#else
    pthread_attr_setstack(&attr, stackaddr, stack_size);
#endif
  } else if (stack_size != DEFAULT_THREAD_STACK_SIZE) {
    pthread_attr_setstacksize(&attr,stack_size);
  }

  /* 
     I think that's just about enough ... create the thread.
  */
  pthread_create(&returned_thread, &attr, start_routine, param);
  return (LispObj) ptr_to_lispobj(returned_thread);
}
#endif

TCR *
get_tcr(Boolean create)
{
#ifdef HAVE_TLS
  TCR *current = current_tcr.linear;
#else
  void *tsd = (void *)tsd_get(lisp_global(TCR_KEY));
  TCR *current = (tsd == NULL) ? NULL : TCR_FROM_TSD(tsd);
#endif

  if ((current == NULL) && create) {
    LispObj callback_macptr = nrs_FOREIGN_THREAD_CONTROL.vcell,
      callback_ptr = ((macptr *)ptr_from_lispobj(untag(callback_macptr)))->address;
    int i, nbindwords = 0;
    extern unsigned initial_stack_size;
    
    /* Make one. */
    current = new_tcr(initial_stack_size, MIN_TSTACK_SIZE);
    SET_TCR_FLAG(current,TCR_FLAG_BIT_FOREIGN);
    register_thread_tcr(current);
#ifdef DEBUG_TCR_CREATION
#ifndef WINDOWS
    fprintf(stderr, "\ncreating TCR for pthread 0x%x", pthread_self());
#endif
#endif
    current->vs_area->active -= node_size;
    *(--current->save_vsp) = lisp_nil;
#ifdef PPC
#define NSAVEREGS 8
#endif
#ifdef X8664
#define NSAVEREGS 4
#endif
    for (i = 0; i < NSAVEREGS; i++) {
      *(--current->save_vsp) = 0;
      current->vs_area->active -= node_size;
    }
    nbindwords = ((int (*)())ptr_from_lispobj(callback_ptr))(-1);
    for (i = 0; i < nbindwords; i++) {
      *(--current->save_vsp) = 0;
      current->vs_area->active -= node_size;
    }
    current->shutdown_count = 1;
    ((void (*)())ptr_from_lispobj(callback_ptr))(0);

  }
  
  return current;
}

#ifdef WINDOWS
Boolean
suspend_tcr(TCR *tcr)
{
}
#else
Boolean
suspend_tcr(TCR *tcr)
{
  int suspend_count = atomic_incf(&(tcr->suspend_count));
  if (suspend_count == 1) {
#if SUSPEND_RESUME_VERBOSE
    fprintf(stderr,"Suspending 0x%x\n", tcr);
#endif
#ifdef DARWIN_nope
    if (mach_suspend_tcr(tcr)) {
      SET_TCR_FLAG(tcr,TCR_FLAG_BIT_ALT_SUSPEND);
      return true;
    }
#endif
    if (pthread_kill((pthread_t)(tcr->osid), thread_suspend_signal) == 0) {
      SET_TCR_FLAG(tcr,TCR_FLAG_BIT_SUSPEND_ACK_PENDING);
    } else {
      /* A problem using pthread_kill.  On Darwin, this can happen
	 if the thread has had its signal mask surgically removed
	 by pthread_exit.  If the native (Mach) thread can be suspended,
	 do that and return true; otherwise, flag the tcr as belonging
	 to a dead thread by setting tcr->osid to 0.
      */
      tcr->osid = 0;
      return false;
    }
    return true;
  }
  return false;
}
#endif

Boolean
tcr_suspend_ack(TCR *tcr)
{
  if (tcr->flags & (1<<TCR_FLAG_BIT_SUSPEND_ACK_PENDING)) {
    SEM_WAIT_FOREVER(tcr->suspend);
    tcr->flags &= ~(1<<TCR_FLAG_BIT_SUSPEND_ACK_PENDING);
#if SUSPEND_RESUME_VERBOSE
    fprintf(stderr,"Suspend ack from 0x%x\n", tcr);
#endif

  }
  return true;
}

      


Boolean
lisp_suspend_tcr(TCR *tcr)
{
  Boolean suspended;
  TCR *current = get_tcr(true);
  
  LOCK(lisp_global(TCR_AREA_LOCK),current);
#ifdef DARWIN
#if USE_MACH_EXCEPTION_LOCK
  if (use_mach_exception_handling) {
    pthread_mutex_lock(mach_exception_lock);
  }
#endif
#endif
  suspended = suspend_tcr(tcr);
  if (suspended) {
    while (!tcr_suspend_ack(tcr));
  }
#ifdef DARWIN
#if USE_MACH_EXCEPTION_LOCK
  if (use_mach_exception_handling) {
    pthread_mutex_unlock(mach_exception_lock);
  }
#endif
#endif
  UNLOCK(lisp_global(TCR_AREA_LOCK),current);
  return suspended;
}
	 

Boolean
resume_tcr(TCR *tcr)
{
  int suspend_count = atomic_decf(&(tcr->suspend_count));
  if (suspend_count == 0) {
#ifdef DARWIN
    if (tcr->flags & (1<<TCR_FLAG_BIT_ALT_SUSPEND)) {
#if SUSPEND_RESUME_VERBOSE
    fprintf(stderr,"Mach resume to 0x%x\n", tcr);
#endif
      mach_resume_tcr(tcr);
      return true;
    }
#endif
#if RESUME_VIA_RESUME_SEMAPHORE
    SEM_RAISE(tcr->resume);
#else
    if ((err = (pthread_kill((pthread_t)(tcr->osid), thread_resume_signal))) != 0) {
      Bug(NULL, "pthread_kill returned %d on thread #x%x", err, tcr->osid);
    }
#endif
#if SUSPEND_RESUME_VERBOSE
    fprintf(stderr, "Sent resume to 0x%x\n", tcr);
#endif
    return true;
  }
  return false;
}

void
wait_for_resumption(TCR *tcr)
{
  if (tcr->suspend_count == 0) {
#ifdef DARWIN
    if (tcr->flags & (1<<TCR_FLAG_BIT_ALT_SUSPEND)) {
      tcr->flags &= ~(1<<TCR_FLAG_BIT_ALT_SUSPEND);
      return;
  }
#endif
#if WAIT_FOR_RESUME_ACK
#if SUSPEND_RESUME_VERBOSE
    fprintf(stderr, "waiting for resume in 0x%x\n",tcr);
#endif
    SEM_WAIT_FOREVER(tcr->suspend);
#endif
  }
}
    


Boolean
lisp_resume_tcr(TCR *tcr)
{
  Boolean resumed;
  TCR *current = get_tcr(true);
  
  LOCK(lisp_global(TCR_AREA_LOCK),current);
  resumed = resume_tcr(tcr);
  wait_for_resumption(tcr);
  UNLOCK(lisp_global(TCR_AREA_LOCK), current);
  return resumed;
}


TCR *freed_tcrs = NULL;

void
enqueue_freed_tcr (TCR *tcr)
{
#ifndef HAVE_TLS
  tcr->next = freed_tcrs;
  freed_tcrs = tcr;
#endif
}

/* It's not clear that we can safely condemn a dead tcr's areas, since
   we may not be able to call free() if a suspended thread owns a 
   malloc lock. At least make the areas appear to be empty. 
*/
   

void
normalize_dead_tcr_areas(TCR *tcr)
{
  area *a;

  a = tcr->vs_area;
  if (a) {
    a->active = a->high;
  }

  a = tcr->ts_area;
  if (a) {
    a->active = a->high;
  }

  a = tcr->cs_area;
  if (a) {
    a->active = a->high;
  }
}
    
void
free_freed_tcrs ()
{
  TCR *current, *next;

  for (current = freed_tcrs; current; current = next) {
    next = current->next;
#ifndef HAVE_TLS
    free(current);
#endif
  }
  freed_tcrs = NULL;
}

void
suspend_other_threads(Boolean for_gc)
{
  TCR *current = get_tcr(true), *other, *next;
  int dead_tcr_count = 0;
  Boolean all_acked;

  LOCK(lisp_global(TCR_AREA_LOCK), current);
#ifdef DARWIN
#if USE_MACH_EXCEPTION_LOCK
  if (for_gc && use_mach_exception_handling) {
#if SUSPEND_RESUME_VERBOSE
    fprintf(stderr, "obtaining Mach exception lock in GC thread 0x%x\n", current);
#endif
    pthread_mutex_lock(mach_exception_lock);
  }
#endif
#endif
  for (other = current->next; other != current; other = other->next) {
    if ((other->osid != 0)) {
      suspend_tcr(other);
      if (other->osid == 0) {
	dead_tcr_count++;
      }
    } else {
      dead_tcr_count++;
    }
  }

  do {
    all_acked = true;
    for (other = current->next; other != current; other = other->next) {
      if ((other->osid != 0)) {
        if (!tcr_suspend_ack(other)) {
          all_acked = false;
        }
      }
    }
  } while(! all_acked);

      

  /* All other threads are suspended; can safely delete dead tcrs now */
  if (dead_tcr_count) {
    for (other = current->next; other != current; other = next) {
      next = other->next;
      if ((other->osid == 0))  {
        normalize_dead_tcr_areas(other);
	dequeue_tcr(other);
	enqueue_freed_tcr(other);
      }
    }
  }
}

void
lisp_suspend_other_threads()
{
  suspend_other_threads(false);
}

void
resume_other_threads(Boolean for_gc)
{
  TCR *current = get_tcr(true), *other;
  for (other = current->next; other != current; other = other->next) {
    if ((other->osid != 0)) {
      resume_tcr(other);
    }
  }
  for (other = current->next; other != current; other = other->next) {
    if ((other->osid != 0)) {
      wait_for_resumption(other);
    }
  }
  free_freed_tcrs();
#ifdef DARWIN
#if USE_MACH_EXCEPTION_LOCK
  if (for_gc && use_mach_exception_handling) {
#if SUSPEND_RESUME_VERBOSE
    fprintf(stderr, "releasing Mach exception lock in GC thread 0x%x\n", current);
#endif
    pthread_mutex_unlock(mach_exception_lock);
  }
#endif
#endif

  UNLOCK(lisp_global(TCR_AREA_LOCK), current);
}

void
lisp_resume_other_threads()
{
  resume_other_threads(false);
}



rwlock *
rwlock_new()
{
  extern int cache_block_size;

  void *p = calloc(1,sizeof(rwlock)+cache_block_size-1);
  rwlock *rw = NULL;;
  
  if (p) {
    rw = (rwlock *) ((((natural)p)+cache_block_size-1) & (~(cache_block_size-1)));
    rw->malloced_ptr = p;
#ifndef USE_FUTEX
    rw->reader_signal = new_semaphore(0);
    rw->writer_signal = new_semaphore(0);
    if ((rw->reader_signal == NULL) || (rw->writer_signal == NULL)) {
      if (rw->reader_signal) {
        destroy_semaphore(&(rw->reader_signal));
      } else {
        destroy_semaphore(&(rw->writer_signal));
      }
      free(rw);
      rw = NULL;
    }
#endif
  }
  return rw;
}

     
/*
  Try to get read access to a multiple-readers/single-writer lock.  If
  we already have read access, return success (indicating that the
  lock is held another time.  If we already have write access to the
  lock ... that won't work; return EDEADLK.  Wait until no other
  thread has or is waiting for write access, then indicate that we
  hold read access once.
*/
#ifndef USE_FUTEX
int
rwlock_rlock(rwlock *rw, TCR *tcr, struct timespec *waitfor)
{
  int err = 0;
  
  LOCK_SPINLOCK(rw->spin, tcr);

  if (rw->writer == tcr) {
    RELEASE_SPINLOCK(rw->spin);
    return EDEADLK;
  }

  while (rw->blocked_writers || (rw->state > 0)) {
    rw->blocked_readers++;
    RELEASE_SPINLOCK(rw->spin);
    err = semaphore_maybe_timedwait(rw->reader_signal,waitfor);
    LOCK_SPINLOCK(rw->spin,tcr);
    rw->blocked_readers--;
    if (err == EINTR) {
      err = 0;
    }
    if (err) {
      RELEASE_SPINLOCK(rw->spin);
      return err;
    }
  }
  rw->state--;
  RELEASE_SPINLOCK(rw->spin);
  return err;
}
#else
int
rwlock_rlock(rwlock *rw, TCR *tcr, struct timespec *waitfor)
{
  natural waitval;

  lock_futex(&rw->spin);

  if (rw->writer == tcr) {
    unlock_futex(&rw->spin);
    return EDEADLOCK;
  }
  while (1) {
    if (rw->writer == NULL) {
      --rw->state;
      unlock_futex(&rw->spin);
      return 0;
    }
    rw->blocked_readers++;
    waitval = rw->reader_signal;
    unlock_futex(&rw->spin);
    futex_wait(&rw->reader_signal,waitval);
    lock_futex(&rw->spin);
    rw->blocked_readers--;
  }
  return 0;
}
#endif   


/*
  Try to obtain write access to the lock.
  It is an error if we already have read access, but it's hard to
  detect that.
  If we already have write access, increment the count that indicates
  that.
  Otherwise, wait until the lock is not held for reading or writing,
  then assert write access.
*/

#ifndef USE_FUTEX
int
rwlock_wlock(rwlock *rw, TCR *tcr, struct timespec *waitfor)
{
  int err = 0;

  LOCK_SPINLOCK(rw->spin,tcr);
  if (rw->writer == tcr) {
    rw->state++;
    RELEASE_SPINLOCK(rw->spin);
    return 0;
  }

  while (rw->state != 0) {
    rw->blocked_writers++;
    RELEASE_SPINLOCK(rw->spin);
    err = semaphore_maybe_timedwait(rw->writer_signal, waitfor);
    LOCK_SPINLOCK(rw->spin,tcr);
    rw->blocked_writers--;
    if (err == EINTR) {
      err = 0;
    }
    if (err) {
      RELEASE_SPINLOCK(rw->spin);
      return err;
    }
  }
  rw->state = 1;
  rw->writer = tcr;
  RELEASE_SPINLOCK(rw->spin);
  return err;
}

#else
int
rwlock_wlock(rwlock *rw, TCR *tcr, struct timespec *waitfor)
{
  int err = 0;
  natural waitval;

  lock_futex(&rw->spin);
  if (rw->writer == tcr) {
    rw->state++;
    unlock_futex(&rw->spin);
    return 0;
  }

  while (rw->state != 0) {
    rw->blocked_writers++;
    waitval = rw->writer_signal;
    unlock_futex(&rw->spin);
    futex_wait(&rw->writer_signal,waitval);
    lock_futex(&rw->spin);
    rw->blocked_writers--;
  }
  rw->state = 1;
  rw->writer = tcr;
  unlock_futex(&rw->spin);
  return err;
}
#endif

/*
  Sort of the same as above, only return EBUSY if we'd have to wait.
*/
#ifndef USE_FUTEX
int
rwlock_try_wlock(rwlock *rw, TCR *tcr)
{
  int ret = EBUSY;

  LOCK_SPINLOCK(rw->spin,tcr);
  if (rw->writer == tcr) {
    rw->state++;
    ret = 0;
  } else {
    if (rw->state == 0) {
      rw->writer = tcr;
      rw->state = 1;
      ret = 0;
    }
  }
  RELEASE_SPINLOCK(rw->spin);
  return ret;
}
#else
int
rwlock_try_wlock(rwlock *rw, TCR *tcr)
{
  int ret = EBUSY;

  lock_futex(&rw->spin);
  if (rw->writer == tcr) {
    rw->state++;
    ret = 0;
  } else {
    if (rw->state == 0) {
      rw->writer = tcr;
      rw->state = 1;
      ret = 0;
    }
  }
  unlock_futex(&rw->spin);
  return ret;
}
#endif

#ifndef USE_FUTEX
int
rwlock_try_rlock(rwlock *rw, TCR *tcr)
{
  int ret = EBUSY;

  LOCK_SPINLOCK(rw->spin,tcr);
  if (rw->state <= 0) {
    --rw->state;
    ret = 0;
  }
  RELEASE_SPINLOCK(rw->spin);
  return ret;
}
#else
int
rwlock_try_rlock(rwlock *rw, TCR *tcr)
{
  int ret = EBUSY;

  lock_futex(&rw->spin);
  if (rw->state <= 0) {
    --rw->state;
    ret = 0;
  }
  unlock_futex(&rw->spin);
  return ret;
}
#endif



#ifndef USE_FUTEX
int
rwlock_unlock(rwlock *rw, TCR *tcr)
{

  int err = 0;
  natural blocked_readers = 0;

  LOCK_SPINLOCK(rw->spin,tcr);
  if (rw->state > 0) {
    if (rw->writer != tcr) {
      err = EINVAL;
    } else {
      --rw->state;
      if (rw->state == 0) {
        rw->writer = NULL;
      }
    }
  } else {
    if (rw->state < 0) {
      ++rw->state;
    } else {
      err = EINVAL;
    }
  }
  if (err) {
    RELEASE_SPINLOCK(rw->spin);
    return err;
  }
  
  if (rw->state == 0) {
    if (rw->blocked_writers) {
      SEM_RAISE(rw->writer_signal);
    } else {
      blocked_readers = rw->blocked_readers;
      if (blocked_readers) {
        SEM_BROADCAST(rw->reader_signal, blocked_readers);
      }
    }
  }
  RELEASE_SPINLOCK(rw->spin);
  return 0;
}
#else
int
rwlock_unlock(rwlock *rw, TCR *tcr)
{

  int err = 0;

  lock_futex(&rw->spin);
  if (rw->state > 0) {
    if (rw->writer != tcr) {
      err = EINVAL;
    } else {
      --rw->state;
      if (rw->state == 0) {
        rw->writer = NULL;
      }
    }
  } else {
    if (rw->state < 0) {
      ++rw->state;
    } else {
      err = EINVAL;
    }
  }
  if (err) {
    unlock_futex(&rw->spin);
    return err;
  }
  
  if (rw->state == 0) {
    if (rw->blocked_writers) {
      ++rw->writer_signal;
      unlock_futex(&rw->spin);
      futex_wake(&rw->writer_signal,1);
      return 0;
    }
    if (rw->blocked_readers) {
      ++rw->reader_signal;
      unlock_futex(&rw->spin);
      futex_wake(&rw->reader_signal, INT_MAX);
      return 0;
    }
  }
  unlock_futex(&rw->spin);
  return 0;
}
#endif

        
void
rwlock_destroy(rwlock *rw)
{
#ifndef USE_FUTEX
  destroy_semaphore((void **)&rw->reader_signal);
  destroy_semaphore((void **)&rw->writer_signal);
#endif
  postGCfree((void *)(rw->malloced_ptr));
}



