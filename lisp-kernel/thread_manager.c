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


#include "threads.h"


typedef struct {
  TCR *tcr;
  natural vsize, tsize;
  void *created;
} thread_activation;

#ifdef HAVE_TLS
__thread char tcrbuf[sizeof(TCR)+16];
__thread TCR *current_tcr;
#endif

/* This is set to true when running a 32-bit Lisp on 64-bit FreeBSD */
Boolean rcontext_readonly = false;

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
extern pc spentry_start, spentry_end,subprims_start,subprims_end;
extern pc restore_windows_context_start, restore_windows_context_end,
  restore_windows_context_iret;


extern void interrupt_handler(int, siginfo_t *, ExceptionInformation *);

void CALLBACK 
nullAPC(ULONG_PTR arg) 
{
}
  
BOOL WINAPI (*pCancelIoEx)(HANDLE, OVERLAPPED*) = NULL;
BOOL WINAPI (*pCancelSynchronousIo)(HANDLE) = NULL;



extern void *windows_find_symbol(void*, char*);

int
raise_thread_interrupt(TCR *target)
{
  /* GCC doesn't align CONTEXT corrcectly */
  char _contextbuf[sizeof(CONTEXT)+__alignof(CONTEXT)];
  CONTEXT  *pcontext;
  HANDLE hthread = (HANDLE)(TCR_AUX(target)->osid);
  pc where;
  area *ts = target->ts_area;
  DWORD rc;
  BOOL io_pending;

  pcontext = (CONTEXT *)((((natural)&_contextbuf)+15)&~15);
  rc = SuspendThread(hthread);
  if (rc == -1) {
    return -1;
  }
  /* What if the suspend count is > 1 at this point ?  I don't think
     that that matters, but I'm not sure */
  pcontext->ContextFlags = CONTEXT_ALL;
  rc = GetThreadContext(hthread, pcontext);
  if (rc == 0) {
    return ESRCH;
  }

  where = (pc)(xpPC(pcontext));
  
  if ((target->valence != TCR_STATE_LISP) ||
      (TCR_INTERRUPT_LEVEL(target) < 0) ||
      (target->unwinding != 0) ||
      (!((where < (pc)lisp_global(HEAP_END)) &&
         (where >= (pc)lisp_global(HEAP_START))) &&
       (!((where < (pc)(managed_static_area->active)) &&
	 (where >= (pc)(readonly_area->low)))) &&
       !((where < spentry_end) && (where >= spentry_start)) &&
       !((where < subprims_end) && (where >= subprims_start)) &&
       !((where < (pc) 0x16000) &&
         (where >= (pc) 0x15000)) &&
       !((where < (pc) (ts->high)) &&
         (where >= (pc) (ts->low))))) {

    target->interrupt_pending = (1LL << (nbits_in_word - 1LL));

#if 0
    /* If the thread's in a blocking syscall, it'd be nice to
       get it out of that state here. */
    GetThreadIOPendingFlag(hthread,&io_pending);
    if (io_pending) {
      pending_io * pending = (pending_io *) (target->pending_io_info);
      if (pending) {
        if (pCancelIoEx) {
          pCancelIoEx(pending->h, pending->o);
        } else {
          CancelIo(pending->h);
        }
      }
    }
#endif
    if (pCancelSynchronousIo) {
      pCancelSynchronousIo(hthread);
    }
    QueueUserAPC(nullAPC, hthread, 0);
    ResumeThread(hthread);
    return 0;
  } else {
    /* Thread is running lisp code with interupts enabled.  Set it
       so that it calls out and then returns to the context,
       handling any necessary pc-lusering. */
    LispObj foreign_rsp = (((LispObj)(target->foreign_sp))-0x200)&~15;
    CONTEXT *icontext = ((CONTEXT *) foreign_rsp) -1;
    icontext = (CONTEXT *)(((LispObj)icontext)&~15);
    
    *icontext = *pcontext;

#ifdef WIN_64    
    xpGPR(pcontext,REG_RCX) = SIGNAL_FOR_PROCESS_INTERRUPT;
    xpGPR(pcontext,REG_RDX) = 0;
    xpGPR(pcontext,REG_R8) = (LispObj) icontext;
    xpGPR(pcontext,REG_RSP) = (LispObj)(((LispObj *)icontext)-1);
    *(((LispObj *)icontext)-1) = (LispObj)raise_thread_interrupt;
#else
    {
      LispObj *p = (LispObj *)icontext;
      p -= 4;
      p[0] = SIGNAL_FOR_PROCESS_INTERRUPT;
      p[1] = 0;
      p[2] = (DWORD)icontext;
      *(--p) = (LispObj)raise_thread_interrupt;;
      xpGPR(pcontext,Isp) = (DWORD)p;
    }
#endif
    pcontext->EFlags &= ~0x400;  /* clear direction flag */
    xpPC(pcontext) = (LispObj)interrupt_handler;
    SetThreadContext(hthread,pcontext);
    ResumeThread(hthread);
    return 0;
  }
}
#else
int
raise_thread_interrupt(TCR *target)
{
  pthread_t thread = (pthread_t)TCR_AUX(target)->osid;
#ifdef DARWIN_not_yet
  if (use_mach_exception_handling) {
    return mach_raise_thread_interrupt(target);
  }
#endif
  if (thread != (pthread_t) 0) {
    return pthread_kill(thread, SIGNAL_FOR_PROCESS_INTERRUPT);
  }
  return ESRCH;
}
#endif

void
set_thread_affinity(TCR *target, unsigned cpuno)
{
#ifdef LINUX
#ifndef ANDROID                 /* too useful to be in Android ... */
  pthread_t thread = (pthread_t)(target->osid);
  cpu_set_t mask;
  
  CPU_ZERO(&mask);
  CPU_SET(cpuno,&mask);
  pthread_setaffinity_np(thread,sizeof(mask),&mask);
#endif
#endif
}



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
lock_futex(signed_natural *p)
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
unlock_futex(signed_natural *p)
{
  if (atomic_decf(p) != FUTEX_AVAIL) {
    *p = FUTEX_AVAIL;
    futex_wake(p,INT_MAX);
  }
}
    
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
  int ret = EPERM;

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
  free((void *)(m->malloced_ptr));
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
    status = SEM_WAIT(s);
#endif
#ifdef USE_POSIX_SEMAPHORES
    status = SEM_WAIT(s);
#endif
#ifdef USE_WINDOWS_SEMAPHORES
    status = (WaitForSingleObject(s,INFINITE) == WAIT_OBJECT_0) ? 0 : 1;
#endif
  } while (status != 0);
}

int
wait_on_semaphore(void *s, int seconds, int millis)
{
#ifdef USE_POSIX_SEMAPHORES
  int nanos = (millis % 1000) * 1000000;
  int status;

  struct timespec q;
  clock_gettime(CLOCK_REALTIME,&q);
    
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
  int nanos = (millis % 1000) * 1000000;
  mach_timespec_t q = {seconds, nanos};
  int status = SEM_TIMEDWAIT(s, q);

  
  switch (status) {
  case 0: return 0;
  case KERN_OPERATION_TIMED_OUT: return ETIMEDOUT;
  case KERN_ABORTED: return EINTR;
  default: return EINVAL;
  }
#endif
#ifdef USE_WINDOWS_SEMAPHORES
  switch (WaitForSingleObjectEx(s, seconds*1000L+(DWORD)millis,true)) {
  case WAIT_OBJECT_0:
    return 0;
  case WAIT_TIMEOUT:
    return /* ETIMEDOUT */ WAIT_TIMEOUT;
  case WAIT_IO_COMPLETION:
    return EINTR;
  default:
    break;
  }
  return EINVAL;

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
  TCR *tcr = get_tcr(false);
  LispObj current = 0;

  if (tcr) {
    current = TCR_AUX(tcr)->osid;
  }
  if (current == 0) {
    DuplicateHandle(GetCurrentProcess(),
                    GetCurrentThread(),
                    GetCurrentProcess(),
                    (LPHANDLE)(&current),
                    0,
                    FALSE,
                    DUPLICATE_SAME_ACCESS);
    if (tcr) {
      TCR_AUX(tcr)->osid = current;
    }
  }
  return current;
}
#else
LispObj
current_thread_osid()
{
  return (LispObj)ptr_to_lispobj(pthread_self());
}
#endif


int thread_suspend_signal = 0, thread_kill_signal = 0;



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
  TCR *tcr = get_interrupt_tcr(false);
  
  if (tcr == NULL) {
    /* Got a suspend signal sent to the pthread. */
    extern natural initial_stack_size;
    void register_thread_tcr(TCR *);
    
    tcr = new_tcr(initial_stack_size, MIN_TSTACK_SIZE);
    TCR_AUX(tcr)->suspend_count = 1;
    tcr->vs_area->active -= node_size;
    *(--tcr->save_vsp) = lisp_nil;
    register_thread_tcr(tcr);
  }
  if (TCR_INTERRUPT_LEVEL(tcr) <= -(2 << fixnumshift)) {
    SET_TCR_FLAG(tcr,TCR_FLAG_BIT_PENDING_SUSPEND);
  } else {
    TCR_AUX(tcr)->suspend_context = context;
    SEM_RAISE(TCR_AUX(tcr)->suspend);
    SEM_WAIT_FOREVER(TCR_AUX(tcr)->resume);
    TCR_AUX(tcr)->suspend_context = NULL;
  }
  SIGRETURN(context);
}

  

/*
  'base' should be set to the bottom (origin) of the stack, e.g., the
  end from which it grows.
*/
  
#ifdef WINDOWS
void
os_get_current_thread_stack_bounds(void **base, natural *size)
{
  natural natbase;
  MEMORY_BASIC_INFORMATION info;
  void *addr = (void *)current_stack_pointer();
  
  VirtualQuery(addr, &info, sizeof(info));
  natbase = (natural)info.BaseAddress+info.RegionSize;
  *size = natbase - (natural)(info.AllocationBase);
  *base = (void *)natbase;
}
#else
void
os_get_current_thread_stack_bounds(void **base, natural *size)
{
  pthread_t p = pthread_self();
#ifdef DARWIN
  *base = pthread_get_stackaddr_np(p);
  *size = pthread_get_stacksize_np(p);
#endif
#ifdef LINUX
  pthread_attr_t attr;

  pthread_getattr_np(p,&attr);
  pthread_attr_getstack(&attr, base, size);
  pthread_attr_destroy(&attr);
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
  pthread_attr_destroy(&attr);
#endif
#ifdef SOLARIS
  stack_t st;
  
  thr_stksegment(&st);
  *size = st.ss_size;
  *base = st.ss_sp;
  
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
  kern_return_t kret;
  semaphore_t s = (semaphore_t)0;
  kret = semaphore_create(mach_task_self(),&s, SYNC_POLICY_FIFO, count);
  if (kret != KERN_SUCCESS) {
    fatal_oserr("Can't create Mach semaphore.",(OSErr)kret);
  }
  return (void *)(natural)s;
#endif
#ifdef USE_WINDOWS_SEMAPHORES
  return CreateSemaphore(NULL, count, 0x7fffL, NULL);
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
    free(*s);    
#endif
#ifdef USE_MACH_SEMAPHORES
    semaphore_destroy(mach_task_self(),((semaphore_t)(natural) *s));
#endif
#ifdef USE_WINDOWS_SEMAPHORES
    CloseHandle(*s);
#endif
    *s=NULL;
  }
}

#ifdef WINDOWS
void
tsd_set(LispObj key, void *datum)
{
  TlsSetValue((DWORD)key, datum);
}

void *
tsd_get(LispObj key)
{
  return TlsGetValue((DWORD)key);
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

  next = TCR_AUX(tcr)->next;
  prev = TCR_AUX(tcr)->prev;

  TCR_AUX(prev)->next = next;
  TCR_AUX(next)->prev = prev;
  TCR_AUX(tcr)->prev = TCR_AUX(tcr)->next = NULL;
#ifdef X86
  tcr->linear = NULL;
#endif
}
  
void
enqueue_tcr(TCR *new)
{
  TCR *head, *tail;
  
  LOCK(lisp_global(TCR_AREA_LOCK),new);
  head = (TCR *)ptr_from_lispobj(lisp_global(INITIAL_TCR));
  tail = TCR_AUX(head)->prev;
  TCR_AUX(tail)->next = new;
  TCR_AUX(head)->prev = new;
  TCR_AUX(new)->prev = tail;
  TCR_AUX(new)->next = head;
  UNLOCK(lisp_global(TCR_AREA_LOCK),new);
}

#ifdef WIN_32
TCR *
allocate_tcr()
{
  void *p = calloc(1,sizeof(struct tcr_aux));
  char *teb = (char *)NtCurrentTeb();
  TCR *tcr = (TCR *)(teb + TCR_BIAS);

  if (p == NULL)
    allocation_failure(true, sizeof(struct tcr_aux));

  if ((intptr_t)p & 03) {
    fprintf(dbgout, "%p not aligned\n", p);
    exit(1);
  }
  memset(tcr, 0, sizeof(TCR));
  tcr->aux = p;
  return tcr;
}
#else
TCR *
allocate_tcr()
{
  TCR *tcr;

  tcr = calloc(1, sizeof(TCR));
  if (tcr == NULL)
    allocation_failure(true, sizeof(TCR));

#ifdef DARWIN
  {
    kern_return_t kret;
    mach_port_t thread_exception_port;

    kret = mach_port_allocate(mach_task_self(), MACH_PORT_RIGHT_RECEIVE,
			      &thread_exception_port);
    if (kret != KERN_SUCCESS) {
      Fatal("Can't allocate Mach exception port for thread.", "");
    } else {
      tcr->io_datum = (void *)((natural)thread_exception_port);
      associate_tcr_with_exception_port(thread_exception_port,tcr);
    }
  }
#endif

  return tcr;
}
#endif

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
  /*
   * According arch_prctl(2), there's no function prototype for
   * arch_prctl().  Thus, we have to declare it ourselves.
   * Note that addr is unsigned long for SET operations.
   */
  extern int arch_prctl(int code, unsigned long addr);

  arch_prctl(ARCH_SET_GS, (natural)tcr);
#endif
#ifdef DARWIN
  /*
   * There's apparently no way to do this.  We used to use a horrible
   * and slow kludge conditionalized on DARWIN_GS_HACK (which involved
   * sharing gs between lisp and pthreads), hoping that Apple would
   * eventually provide a way to set fsbase.  We got tired of waiting,
   * and have now resigned ourselves to keeping the TCR in a GPR.
   */
  /* darwin_set_x8664_fs_reg(tcr); */
#endif
#ifdef SOLARIS
  /* Chris Curtis found this and suggested the use of syscall here */
  syscall(SYS_lwp_private,_LWP_SETPRIVATE, _LWP_GSBASE, tcr);
#endif
}

#endif

#ifdef X8632

#ifdef DARWIN
#include <architecture/i386/table.h>
#include <architecture/i386/sel.h>
#include <i386/user_ldt.h>

void setup_tcr_extra_segment(TCR *tcr)
{
    uintptr_t addr = (uintptr_t)tcr;
    unsigned int size = sizeof(*tcr);
    ldt_entry_t desc;
    sel_t sel;
    int i;

    desc.data.limit00 = (size - 1) & 0xffff;
    desc.data.limit16 = ((size - 1) >> 16) & 0xf;
    desc.data.base00 = addr & 0xffff;
    desc.data.base16 = (addr >> 16) & 0xff;
    desc.data.base24 = (addr >> 24) & 0xff;
    desc.data.type = DESC_DATA_WRITE;
    desc.data.dpl = USER_PRIV;
    desc.data.present = 1;
    desc.data.stksz = DESC_CODE_32B;
    desc.data.granular = DESC_GRAN_BYTE;
    
    i = i386_set_ldt(LDT_AUTO_ALLOC, &desc, 1);

    if (i < 0) {
	perror("i386_set_ldt");
    } else {
	sel.index = i;
	sel.rpl = USER_PRIV;
	sel.ti = SEL_LDT;
	tcr->ldt_selector = sel;
    }
}

void free_tcr_extra_segment(TCR *tcr)
{
  /* load %fs with null segement selector */
  __asm__ volatile ("mov %0,%%fs" : : "r"(0));
  if (i386_set_ldt(tcr->ldt_selector.index, NULL, 1) < 0)
    perror("i386_set_ldt");
  tcr->ldt_selector = NULL_SEL;
}
#endif

#ifdef LINUX

#include <asm/ldt.h>
#include <sys/syscall.h>

/* see desc_struct in kernel/include/asm-i386/processor.h */
typedef struct {
  uint32_t a;
  uint32_t b;
} linux_desc_struct;


#define desc_avail(d) (((d)->a) == 0)

linux_desc_struct linux_ldt_entries[LDT_ENTRIES];

/* We have to ask the Linux kernel for a copy of the ldt table
   and manage it ourselves.  It's not clear that this is 
   thread-safe in general, but we can at least ensure that
   it's thread-safe wrt lisp threads. */

pthread_mutex_t ldt_lock = PTHREAD_MUTEX_INITIALIZER;  /* simple, non-recursive mutex */

int
modify_ldt(int func, void *ptr, unsigned long bytecount)
{
  return syscall(__NR_modify_ldt, func, ptr, bytecount);
}


void
setup_tcr_extra_segment(TCR *tcr)
{
  int i, n;
  short sel;
  struct user_desc u = {1, 0, 0, 1, MODIFY_LDT_CONTENTS_DATA, 0, 0, 0, 1};
  linux_desc_struct *d = linux_ldt_entries;

  pthread_mutex_lock(&ldt_lock);
  n = modify_ldt(0,d,LDT_ENTRIES*LDT_ENTRY_SIZE)/LDT_ENTRY_SIZE;
  for (i = 0; i < n; i++,d++) {
    if (desc_avail(d)) {
      break;
    }
  }
  if (i == LDT_ENTRIES) {
    pthread_mutex_unlock(&ldt_lock);
    fprintf(dbgout, "All 8192 ldt entries in use ?\n");
    _exit(1);
  }
  u.entry_number = i;
  u.base_addr = (uint32_t)tcr;
  u.limit = sizeof(TCR);
  u.limit_in_pages = 0;
  if (modify_ldt(1,&u,sizeof(struct user_desc)) != 0) {
    pthread_mutex_unlock(&ldt_lock);
    fprintf(dbgout,"Can't assign LDT entry\n");
    _exit(1);
  }
  sel = (i << 3) | 7;
  tcr->ldt_selector = sel;
  pthread_mutex_unlock(&ldt_lock);
}

void
free_tcr_extra_segment(TCR *tcr)
{
  struct user_desc u = {0, 0, 0, 0, MODIFY_LDT_CONTENTS_DATA, 0, 0, 0, 0};
  short sel = tcr->ldt_selector;

  pthread_mutex_lock(&ldt_lock);
  /* load %fs with null segment selector */
  __asm__ volatile ("mov %0,%%fs" : : "r"(0));
  tcr->ldt_selector = 0;
  u.entry_number = (sel>>3);
  modify_ldt(1,&u,sizeof(struct user_desc));
  pthread_mutex_unlock(&ldt_lock);
  
}

#endif

#ifdef WINDOWS
bitvector ldt_entries_in_use = NULL;
HANDLE ldt_lock;

typedef struct {
  DWORD offset;
  DWORD size;
  LDT_ENTRY entry;
} win32_ldt_info;


int WINAPI (*NtQueryInformationProcess)(HANDLE,DWORD,VOID*,DWORD,DWORD*);
int WINAPI (*NtSetInformationProcess)(HANDLE,DWORD,VOID*,DWORD);

void
init_win32_ldt()
{
  HANDLE hNtdll;
  int status = 0xc0000002;
  win32_ldt_info info;
  DWORD nret;
  

  ldt_entries_in_use=malloc(8192/8);
  zero_bits(ldt_entries_in_use,8192);
  ldt_lock = CreateMutex(NULL,0,NULL);

  hNtdll = LoadLibrary("ntdll.dll");
  NtQueryInformationProcess = (void*)GetProcAddress(hNtdll, "NtQueryInformationProcess");
  NtSetInformationProcess = (void*)GetProcAddress(hNtdll, "NtSetInformationProcess");
  if (NtQueryInformationProcess != NULL) {
    info.offset = 0;
    info.size = sizeof(LDT_ENTRY);
    status = NtQueryInformationProcess(GetCurrentProcess(),10,&info,sizeof(info),&nret);
  }

  if (status) {
    fprintf(dbgout, "This application can't run under this OS version\n");
    _exit(1);
  }
}

void
setup_tcr_extra_segment(TCR *tcr)
{
}

void 
free_tcr_extra_segment(TCR *tcr)
{
}

#endif
#ifdef FREEBSD
#include <machine/segments.h>
#include <machine/sysarch.h>

/* It'd be tempting to use i386_set_fsbase() here, but there doesn't
   seem to be any way to free the GDT entry it creates.  Actually,
   it's not clear that that really sets a GDT entry; let's see */

#define FREEBSD_USE_SET_FSBASE 1
void
setup_tcr_extra_segment(TCR *tcr)
{
#if !FREEBSD_USE_SET_FSBASE
  struct segment_descriptor sd;
  uintptr_t addr = (uintptr_t)tcr;
  unsigned int size = sizeof(*tcr);
  int i;

  sd.sd_lolimit = (size - 1) & 0xffff;
  sd.sd_hilimit = ((size - 1) >> 16) & 0xf;
  sd.sd_lobase = addr & ((1<<24)-1);
  sd.sd_hibase = (addr>>24)&0xff;



  sd.sd_type = 18;
  sd.sd_dpl = SEL_UPL;
  sd.sd_p = 1;
  sd.sd_def32 = 1;
  sd.sd_gran = 0;

  i = i386_set_ldt(LDT_AUTO_ALLOC, (union descriptor *)&sd, 1);

  if (i < 0) {
    perror("i386_set_ldt");
    exit(1);
  } else {
    tcr->ldt_selector = LSEL(i,SEL_UPL);
  }
#else
  extern unsigned short get_fs_register(void);

  if (i386_set_fsbase((void*)tcr)) {
    perror("i386_set_fsbase");
    exit(1);
  }


  /* Once we've called i386_set_fsbase, we can't write to %fs. */
  tcr->ldt_selector = GSEL(GUFS_SEL, SEL_UPL);
#endif
}

void 
free_tcr_extra_segment(TCR *tcr)
{
#if FREEBSD_USE_SET_FSBASE
  /* On a 32-bit kernel, this allocates a GDT entry.  It's not clear
     what it would mean to deallocate that entry. */
  /* If we're running on a 64-bit kernel, we can't write to %fs */
#else
  int idx = tcr->ldt_selector >> 3;
  /* load %fs with null segment selector */
  __asm__ volatile ("mov %0,%%fs" : : "r"(0));
  if (i386_set_ldt(idx, NULL, 1) < 0)
    perror("i386_set_ldt");
#endif
  tcr->ldt_selector = 0;
}
#endif

#ifdef SOLARIS
#include <sys/sysi86.h>

bitvector ldt_entries_in_use = NULL;
pthread_mutex_t ldt_lock = PTHREAD_MUTEX_INITIALIZER;  /* simple, non-recursive mutex */

void
solaris_ldt_init()
{
  int fd;
  struct ssd s;

  ldt_entries_in_use=malloc(8192/8);
  zero_bits(ldt_entries_in_use,8192);
  
  fd = open("/proc/self/ldt", O_RDONLY);

  while(read(fd,&s,sizeof(s)) == sizeof(s)) {
    set_bit(ldt_entries_in_use,s.sel>>3);
  }
  close(fd);
}
    

void
setup_tcr_extra_segment(TCR *tcr)
{
  struct ssd s;
  int i;

  pthread_mutex_lock(&ldt_lock);

  for (i = 0; i < 8192; i++) {
    if (!ref_bit(ldt_entries_in_use,i)) {
      s.sel = (i<<3)|7;
      s.bo = (unsigned int)tcr;
      s.ls = sizeof(TCR);
      s.acc1 = 0xf2;
      s.acc2 = 4;

      if (sysi86(SI86DSCR, &s) >= 0) {
        set_bit(ldt_entries_in_use,i);
        tcr->ldt_selector = (i<<3)|7;
        pthread_mutex_unlock(&ldt_lock);
        return;
      }
      set_bit(ldt_entries_in_use,i);
    }
  }
  pthread_mutex_unlock(&ldt_lock);
  fprintf(dbgout, "All 8192 LDT descriptors in use\n");
  _exit(1);


  
}

void 
free_tcr_extra_segment(TCR *tcr)
{
  struct ssd s;
  int i;

  pthread_mutex_lock(&ldt_lock);
  __asm__ volatile ("mov %0,%%fs" : : "r"(0));
  s.sel = tcr->ldt_selector;
  i = s.sel>>3;
  tcr->ldt_selector = 0;
  s.bo = 0;
  s.ls = 0;
  s.acc1 = 0;
  s.acc2 = 0;
  sysi86(SI86DSCR, &s);
  clr_bit(ldt_entries_in_use,i);
  pthread_mutex_unlock(&ldt_lock);
}

#endif
#endif

#ifdef ARM
extern int arm_architecture_version;

void
init_arm_tcr_sptab(TCR *tcr)
{
  extern LispObj *sptab;
  extern LispObj *sptab_end;
  LispObj *p, *q;

  for (p=sptab,q = tcr->sptab;
       p<sptab_end;
       p++,q++) {
    *q = *p;
  }
}
#endif       
  
  


/*
  Caller must hold the area_lock.
*/
TCR *
new_tcr(natural vstack_size, natural tstack_size)
{
  extern area
    *allocate_vstack_holding_area_lock(natural),
    *allocate_tstack_holding_area_lock(natural);
  area *a;
  int i;
#ifndef WINDOWS
  sigset_t sigmask;

  sigemptyset(&sigmask);
  pthread_sigmask(SIG_SETMASK,&sigmask, NULL);
#endif

#ifdef HAVE_TLS
  TCR *tcr = (TCR *) ((((natural)&tcrbuf)+((natural)15)) & ~((natural)15));
  current_tcr = tcr;
#else /* no TLS */
  TCR *tcr = allocate_tcr();
#endif

#ifdef ARM
  init_arm_tcr_sptab(tcr);
  tcr->architecture_version = (arm_architecture_version-ARM_ARCHITECTURE_v7) << fixnumshift;
#endif
#ifdef X86
  setup_tcr_extra_segment(tcr);
  tcr->linear = tcr;
#ifdef X8632
  tcr->node_regs_mask = X8632_DEFAULT_NODE_REGS_MASK;
#endif
#endif

#if (WORD_SIZE == 64)
  tcr->single_float_convert.tag = subtag_single_float;
#endif
  TCR_AUX(tcr)->suspend = new_semaphore(0);
  TCR_AUX(tcr)->resume = new_semaphore(0);
  TCR_AUX(tcr)->reset_completion = new_semaphore(0);
  TCR_AUX(tcr)->activate = new_semaphore(0);
  LOCK(lisp_global(TCR_AREA_LOCK),tcr);
  a = allocate_vstack_holding_area_lock(vstack_size);
  tcr->vs_area = a;
  a->owner = tcr;
  tcr->save_vsp = (LispObj *) a->active;  
#ifndef ARM
  a = allocate_tstack_holding_area_lock(tstack_size);
#endif
  UNLOCK(lisp_global(TCR_AREA_LOCK),tcr);
#ifndef ARM
  tcr->ts_area = a;
  a->owner = tcr;
  tcr->save_tsp = (LispObj *) a->active;
#endif
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
#ifdef ARM
  tcr->lisp_fpscr = 
    (1 << FPSCR_IOE_BIT) | 
    (1 << FPSCR_DZE_BIT) |
    (1 << FPSCR_OFE_BIT);
#endif
  tcr->save_allocbase = tcr->save_allocptr = (void *) VOID_ALLOCPTR;
  tcr->tlb_limit = 2048<<fixnumshift;
  tcr->tlb_pointer = (LispObj *)malloc(tcr->tlb_limit);
  for (i = 0; i < 2048; i++) {
    tcr->tlb_pointer[i] = (LispObj) no_thread_local_binding_marker;
  }
  TCR_INTERRUPT_LEVEL(tcr) = (LispObj) -(1 << fixnumshift);
#ifndef WINDOWS
  tcr->shutdown_count = PTHREAD_DESTRUCTOR_ITERATIONS;
#else
  TCR_AUX(tcr)->shutdown_count = 1;
#endif
  return tcr;
}

void
#ifdef WIN_32
__declspec(dllexport)
#endif
shutdown_thread_tcr(void *arg)
{
#ifdef DARWIN
  extern void darwin_exception_cleanup(TCR *);
#endif
  TCR *tcr = TCR_FROM_TSD(arg),*current=get_tcr(0);

  area *vs, *ts, *cs;
#ifdef DARWIN
  mach_port_t kernel_thread;
#endif
  
  if (current == NULL) {
    current = tcr;
  }

  if (--(TCR_AUX(tcr)->shutdown_count) == 0) {
    if (tcr->flags & (1<<TCR_FLAG_BIT_FOREIGN)) {
      LispObj callback_macptr = nrs_FOREIGN_THREAD_CONTROL.vcell,
	callback_ptr = ((macptr *)ptr_from_lispobj(untag(callback_macptr)))->address;
      int (*foreign_thread_control)(int) = (int (*)(int))callback_ptr;

      tsd_set(lisp_global(TCR_KEY), TCR_TO_TSD(tcr));
      foreign_thread_control(1);
      //((void (*)(int))ptr_from_lispobj(callback_ptr))(1);
      tsd_set(lisp_global(TCR_KEY), NULL);
    }
#ifdef DARWIN
    darwin_exception_cleanup(tcr);
    kernel_thread = (mach_port_t) (uint32_t)(natural)( TCR_AUX(tcr)->native_thread_id);
#endif
    LOCK(lisp_global(TCR_AREA_LOCK),current);
    vs = tcr->vs_area;
    tcr->vs_area = NULL;
#ifndef ARM
    ts = tcr->ts_area;
    tcr->ts_area = NULL;
#endif
    cs = TCR_AUX(tcr)->cs_area;
    TCR_AUX(tcr)->cs_area = NULL;
    if (vs) {
      condemn_area_holding_area_lock(vs);
    }
#ifndef ARM
    if (ts) {
      condemn_area_holding_area_lock(ts);
    }
#endif
    if (cs) {
      condemn_area_holding_area_lock(cs);
    }
    /* If we use the sigaltstack mechanism, we always keep the
       altstack separate from other stacks now.
    */
#ifdef USE_SIGALTSTACK
    {
      stack_t new, current;
      new.ss_flags = SS_DISABLE;
      if (sigaltstack(&new, &current) == 0) {
        munmap(current.ss_sp, current.ss_size);
      }
    }
#endif
    destroy_semaphore(&TCR_AUX(tcr)->suspend);
    destroy_semaphore(&TCR_AUX(tcr)->resume);
    destroy_semaphore(&TCR_AUX(tcr)->reset_completion);
    destroy_semaphore(&TCR_AUX(tcr)->activate);
    tcr->tlb_limit = 0;
    free(tcr->tlb_pointer);
    tcr->tlb_pointer = NULL;
#ifdef WINDOWS
    if (TCR_AUX(tcr)->osid != 0) {
      CloseHandle((HANDLE)(TCR_AUX(tcr)->osid));
    }
#endif
    TCR_AUX(tcr)->osid = 0;
    tcr->interrupt_pending = 0;
    TCR_AUX(tcr)->termination_semaphore = NULL;
#if defined(HAVE_TLS) || defined(WIN_32) || defined(DARWIN)
    dequeue_tcr(tcr);
#endif
#ifdef X8632
    free_tcr_extra_segment(tcr);
#endif
#ifdef WINDOWS
    CloseHandle((HANDLE)TCR_AUX(tcr)->io_datum);
    TCR_AUX(tcr)->io_datum = NULL;
    free(TCR_AUX(tcr)->native_thread_info);
    TCR_AUX(tcr)->native_thread_info = NULL;
#ifdef WIN_32
    free(tcr->aux);
    tcr->aux = NULL;
#endif
#endif
#ifdef DARWIN
    free(tcr);
    tcr = NULL;
#endif
    UNLOCK(lisp_global(TCR_AREA_LOCK),current);
#ifdef HAVE_TLS
    if (current_tcr == tcr) {
      current_tcr = NULL;
      memset(tcr, 0, sizeof(TCR));
    }
#endif
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
#ifndef ARM
  a = tcr->ts_area;
  if (a) {
    a->active = a->high;
  }
#endif
  a = TCR_AUX(tcr)->cs_area;
  if (a) {
    a->active = a->high;
  }
  tcr->valence = TCR_STATE_FOREIGN;
  TCR_AUX(tcr)->shutdown_count = 1;
  shutdown_thread_tcr(tcr);
  tsd_set(lisp_global(TCR_KEY), NULL);
}

void *
current_native_thread_id()
{
  return ((void *) (natural)
#ifdef LINUX
#ifdef __NR_gettid
          syscall(__NR_gettid)
#else
          getpid()
#endif
#endif
#ifdef DARWIN
          pthread_mach_thread_np(pthread_self())
#endif
#ifdef FREEBSD
	  pthread_self()
#endif
#ifdef SOLARIS
	  pthread_self()
#endif
#ifdef WINDOWS
	  GetCurrentThreadId()
#endif
	  );
}


void
thread_init_tcr(TCR *tcr, void *stack_base, natural stack_size)
{
  area *a, *register_cstack_holding_area_lock(BytePtr, natural);

  TCR_AUX(tcr)->osid = current_thread_osid();
  TCR_AUX(tcr)->native_thread_id = current_native_thread_id();
  LOCK(lisp_global(TCR_AREA_LOCK),tcr);
  a = register_cstack_holding_area_lock((BytePtr)stack_base, stack_size);
  UNLOCK(lisp_global(TCR_AREA_LOCK),tcr);
  TCR_AUX(tcr)->cs_area = a;
  a->owner = tcr;
#ifdef ARM
  tcr->last_lisp_frame = (natural)(a->high);
#endif
  TCR_AUX(tcr)->cs_limit = (LispObj)ptr_to_lispobj(a->softlimit);
#ifdef LINUX
#ifdef PPC
#ifndef PPC64
  tcr->native_thread_info = current_r2;
#endif
#endif
#endif
  TCR_AUX(tcr)->errno_loc = (int *)(&errno);
  tsd_set(lisp_global(TCR_KEY), TCR_TO_TSD(tcr));
#ifdef DARWIN
  darwin_exception_init(tcr);
#endif
#ifdef LINUX
  linux_exception_init(tcr);
#endif
#ifdef WINDOWS
  TCR_AUX(tcr)->io_datum = (VOID *)CreateEvent(NULL, true, false, NULL);
  TCR_AUX(tcr)->native_thread_info = malloc(sizeof(CONTEXT));
#endif
  TCR_AUX(tcr)->log2_allocation_quantum = unbox_fixnum(lisp_global(DEFAULT_ALLOCATION_QUANTUM));
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

  os_get_current_thread_stack_bounds(&stack_base, &stack_size);
  thread_init_tcr(tcr, stack_base, stack_size);
  enqueue_tcr(tcr);
}


  
  

Ptr
create_stack(natural size)
{
  Ptr p;
  size=align_to_power_of_2(size, log2_page_size);
  p = (Ptr) MapMemoryForStack((size_t)size);
  if (p != (Ptr)(-1)) {
    *((size_t *)p) = size;
    return p;
  }
  allocation_failure(true, size);
  return NULL;
}

void *
allocate_stack(natural size)
{
  return create_stack(size);
}

void
free_stack(void *s)
{
  size_t size = *((size_t *)s);
  UnMapMemory(s, size);
}

Boolean threads_initialized = false;

#ifndef USE_FUTEX
#ifdef WINDOWS
void
count_cpus()
{
  SYSTEM_INFO si;

  GetSystemInfo(&si);
  if (si.dwNumberOfProcessors > 1) {
    spin_lock_tries = 1024;
  }
}
#else
void
count_cpus()
{
  int n = sysconf(_SC_NPROCESSORS_CONF);
  
  if (n > 1) {
    spin_lock_tries = 1024;
  }
}
#endif
#endif

void
init_threads(void * stack_base, TCR *tcr)
{
  lisp_global(INITIAL_TCR) = (LispObj)ptr_to_lispobj(tcr);
#ifdef WINDOWS
  lisp_global(TCR_KEY) = TlsAlloc();
  pCancelIoEx = windows_find_symbol(NULL, "CancelIoEx");
  pCancelSynchronousIo = windows_find_symbol(NULL, "CancelSynchronousIo");
#else
  pthread_key_create((pthread_key_t *)&(lisp_global(TCR_KEY)), shutdown_thread_tcr);
  thread_signal_setup();
#endif

#ifndef USE_FUTEX
  count_cpus();
#endif
  threads_initialized = true;
}


#ifdef WINDOWS
unsigned CALLBACK
#else
void *
#endif
lisp_thread_entry(void *param)
{
  thread_activation *activation = (thread_activation *)param;
  TCR *tcr = new_tcr(activation->vsize, activation->tsize);
  LispObj *start_vsp;
#ifndef WINDOWS
  sigset_t mask, old_mask;

  sigemptyset(&mask);
  pthread_sigmask(SIG_SETMASK, &mask, &old_mask);
#endif

  register_thread_tcr(tcr);

#ifndef WINDOWS
  pthread_cleanup_push(tcr_cleanup,(void *)tcr);
#endif
  tcr->vs_area->active -= node_size;
  *(--tcr->save_vsp) = lisp_nil;
  start_vsp = tcr->save_vsp;
  enable_fp_exceptions();
  SET_TCR_FLAG(tcr,TCR_FLAG_BIT_AWAITING_PRESET);
  activation->tcr = tcr;
  SEM_RAISE(activation->created);
  do {
    SEM_RAISE(TCR_AUX(tcr)->reset_completion);
    SEM_WAIT_FOREVER(TCR_AUX(tcr)->activate);
    /* Now go run some lisp code */
    start_lisp(TCR_TO_TSD(tcr),0);
    tcr->save_vsp = start_vsp;
  } while (tcr->flags & (1<<TCR_FLAG_BIT_AWAITING_PRESET));
#ifndef WINDOWS
  pthread_cleanup_pop(true);
#else
  tcr_cleanup(tcr);
#endif
#ifdef WINDOWS
  return 0;
#else
  return NULL;
#endif
}

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

#ifdef USE_DTRACE
  if (CCL_CREATE_THREAD_ENABLED() && activation.tcr) {
    CCL_CREATE_THREAD(activation.tcr->osid);
  }
#endif

  return TCR_TO_TSD(activation.tcr);
}

void *
do_nothing(void) {
  return 0;
}

Boolean
active_tcr_p(TCR *q)
{
  TCR *head = (TCR *)ptr_from_lispobj(lisp_global(INITIAL_TCR)), *p = head;
  
  do {
    if (p == q) {
      return true;
    }
    p = TCR_AUX(p)->next;
  } while (p != head);
  return false;
}


OSErr
xDisposeThread(TCR *tcr)
{
  return 0;                     /* I don't think that this is ever called. */
}

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
Boolean
create_system_thread(size_t stack_size,
		     void* stackaddr,
		     unsigned CALLBACK (*start_routine)(void *),
		     void* param)
{
  HANDLE thread_handle;
  Boolean won = false;

  stack_size = ((stack_size+(((1<<16)-1)))&~((1<<16)-1));

  thread_handle = (HANDLE)_beginthreadex(NULL, 
                                         stack_size,
                                         start_routine,
                                         param,
                                         0, 
                                         NULL);

  if (thread_handle == NULL) {
    wperror("CreateThread");
  } else {
    won = true;
    CloseHandle(thread_handle);
  }
  return won;
}
#else
Boolean
create_system_thread(size_t stack_size,  void *stackaddr,
		     void *(*start_routine)(void *), void *param)
{
  pthread_attr_t attr;
  pthread_t returned_thread;
  int err;
  TCR *current = get_tcr(true);

  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);  

  if (stack_size == MINIMAL_THREAD_STACK_SIZE) {
    stack_size = PTHREAD_STACK_MIN;
  }

  stack_size = ensure_stack_limit(stack_size);
  if (stackaddr != NULL) {
    /* Size must have been specified.  Sort of makes sense ... */
    pthread_attr_setstack(&attr, stackaddr, stack_size);
  } else if (stack_size != DEFAULT_THREAD_STACK_SIZE) {
    pthread_attr_setstacksize(&attr,stack_size);
  }

  /* 
     I think that's just about enough ... create the thread.
     Well ... not quite enough.  In Leopard (at least), many
     pthread routines grab an internal spinlock when validating
     their arguments.  If we suspend a thread that owns this
     spinlock, we deadlock.  We can't in general keep that
     from happening: if arbitrary C code is suspended while
     it owns the spinlock, we still deadlock.  It seems that
     the best that we can do is to keep -this- code from
     getting suspended (by grabbing TCR_AREA_LOCK)
  */
  LOCK(lisp_global(TCR_AREA_LOCK),current);
  err = pthread_create(&returned_thread, &attr, start_routine, param);
  UNLOCK(lisp_global(TCR_AREA_LOCK),current);
  pthread_attr_destroy(&attr);
  return (err == 0);
}
#endif

TCR *
get_tcr(Boolean create)
{
#ifdef HAVE_TLS
  TCR *current = current_tcr;
#elif defined(WIN_32)
  TCR *current = ((TCR *)((char *)NtCurrentTeb() + TCR_BIAS))->linear;
#else
  void *tsd = (void *)tsd_get(lisp_global(TCR_KEY));
  TCR *current = (tsd == NULL) ? NULL : TCR_FROM_TSD(tsd);
#endif

  if ((current == NULL) && create) {
    LispObj callback_macptr = nrs_FOREIGN_THREAD_CONTROL.vcell,
      callback_ptr = ((macptr *)ptr_from_lispobj(untag(callback_macptr)))->address;
    int (*foreign_thread_control)(int) = (int (*)(int))callback_ptr;
    int i, nbindwords = 0;
    extern natural initial_stack_size;
    
    /* Make one. */
    current = new_tcr(initial_stack_size, MIN_TSTACK_SIZE);
    SET_TCR_FLAG(current,TCR_FLAG_BIT_FOREIGN);
    register_thread_tcr(current);
#ifdef DEBUG_TCR_CREATION
#ifndef WINDOWS
    fprintf(dbgout, "\ncreating TCR for pthread 0x%x", pthread_self());
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
#ifdef X8632
#define NSAVEREGS 0
#endif
#ifdef ARM
#define NSAVEREGS 0
#endif
    for (i = 0; i < NSAVEREGS; i++) {
      *(--current->save_vsp) = 0;
      current->vs_area->active -= node_size;
    }
    nbindwords = foreign_thread_control(-1);
    for (i = 0; i < nbindwords; i++) {
      *(--current->save_vsp) = 0;
      current->vs_area->active -= node_size;
    }
    TCR_AUX(current)->shutdown_count = 1;
    foreign_thread_control(0);

  }
  
  return current;
}

#ifdef WINDOWS
void *
pc_luser_restore_windows_context(CONTEXT *pcontext, TCR *tcr, pc where)
{
  /* Thread has started to return from an exception. */
  if (where < restore_windows_context_iret) {
    /* In the process of restoring registers; context still in
       %rcx.  Just make our suspend_context be the context
       we're trying to restore, so that we'll resume from
       the suspend in the same context that we're trying to
       restore */
#ifdef WIN_64
    *pcontext = * (CONTEXT *)(pcontext->Rcx);
#else
    if (where == restore_windows_context_start) {
      *pcontext = * (CONTEXT *)((pcontext->Esp)+4);
    } else {
      *pcontext = * (CONTEXT *)(pcontext->Ecx);
    }
#endif
  } else {
    /* Most of the context has already been restored; fix %rcx
       if need be, then restore ss:rsp, cs:rip, and flags. */
#ifdef WIN_64
    x64_iret_frame *iret_frame = (x64_iret_frame *) (pcontext->Rsp);

    pcontext->Rip = iret_frame->Rip;
    pcontext->SegCs = (WORD) iret_frame->Cs;
    pcontext->EFlags = (DWORD) iret_frame->Rflags;
    pcontext->Rsp = iret_frame->Rsp;
    pcontext->SegSs = (WORD) iret_frame->Ss;
#else
    ia32_iret_frame *iret_frame = (ia32_iret_frame *) (pcontext->Esp);

    pcontext->Eip = iret_frame->Eip;
    pcontext->SegCs = (WORD) iret_frame->Cs;
    pcontext->EFlags = (DWORD) iret_frame->EFlags;
    pcontext->Esp += sizeof(ia32_iret_frame);
#endif
  }
  tcr->pending_exception_context = NULL;
  /* We basically never return from an exception unless we
     were executing lisp code when the exception returned.
     If that ever changes, we need to know what valence
     would have been restored here.*/
  tcr->valence = TCR_STATE_LISP;
}

Boolean
suspend_tcr(TCR *tcr)
{
  int suspend_count = atomic_incf(&(TCR_AUX(tcr)->suspend_count));
  DWORD rc;
  if (suspend_count == 1) {
    CONTEXT  *pcontext = (CONTEXT *)TCR_AUX(tcr)->native_thread_info;
    HANDLE hthread = (HANDLE)(TCR_AUX(tcr)->osid);
    pc where;
    area *cs = TCR_AUX(tcr)->cs_area;
    LispObj foreign_rsp;

    if (hthread == NULL) {
      return false;
    }
    rc = SuspendThread(hthread);
    if (rc == -1) {
      /* If the thread's simply dead, we should handle that here */
      return false;
    }
    pcontext->ContextFlags = CONTEXT_ALL;
    rc = GetThreadContext(hthread, pcontext);
    if (rc == 0) {
      return false;
    }
    where = (pc)(xpPC(pcontext));

    if ((where >= restore_windows_context_start) &&
        (where < restore_windows_context_end) &&
        (tcr->valence != TCR_STATE_LISP)) {
#ifdef WIN_64
      tcr->valence = xpGPR(pcontext,REG_R8);
#else
      tcr->valence = ((LispObj *)(xpGPR(pcontext,Isp)))[3];
#endif
      pcontext = tcr->pending_exception_context;
      tcr->pending_exception_context = NULL; 
      where = (pc)(xpPC(pcontext));
    }
    if (tcr->valence == TCR_STATE_LISP) {
      if ((where >= restore_windows_context_start) &&
          (where < restore_windows_context_end)) {
        pc_luser_restore_windows_context(pcontext, tcr, where);
      } else {
        area *ts = tcr->ts_area;
        /* If we're in the lisp heap, or in x86-spentry??.o, or in
           x86-subprims??.o, or in the subprims jump table at #x15000,
           or on the tstack ... we're just executing lisp code.  Otherwise,
           we got an exception while executing lisp code, but haven't
           entered the handler yet (still in Windows exception glue
           or switching stacks or something.)  In the latter case, we
           basically want to get to he handler and have it notice
           the pending exception request, and suspend the thread at that
           point. */
        if (!((where < (pc)lisp_global(HEAP_END)) &&
              (where >= (pc)lisp_global(HEAP_START))) &&
	    (!((where < (pc)(managed_static_area->active)) &&
	      (where >= (pc)(readonly_area->low)))) &&
            !((where < spentry_end) && (where >= spentry_start)) &&
            !((where < subprims_end) && (where >= subprims_start)) &&
            !((where < (pc) 0x16000) &&
              (where >= (pc) 0x15000)) &&
            !((where < (pc) (ts->high)) &&
              (where >= (pc) (ts->low)))) {
          /* The thread has lisp valence, but is not executing code
             where we expect lisp code to be and is not exiting from
             an exception handler.  That pretty much means that it's
             on its way into an exception handler; we have to handshake
             until it enters an exception-wait state. */
          /* There are likely race conditions here */
          SET_TCR_FLAG(tcr,TCR_FLAG_BIT_PENDING_SUSPEND);
          ResumeThread(hthread);
          SEM_WAIT_FOREVER(TCR_AUX(tcr)->suspend);
          pcontext = NULL;
        }
      }
    }
    /* If we're really running lisp code, there's some reason to
       suspect that Windows is lying about that; the thread may have
       already committed to processing an exception and just not have
       reentered user mode.  If there's a way to determine that more
       reliably, I don't know what it is.  We can catch some cases of
       that by looking at whether the PC is at a UUO or other
       "intentional" illegal instruction and letting the thread enter
       exception handling, treating this case just like the case
       above.  

       When people say that Windows sucks, they aren't always just
       talking about all of the other ways that it sucks.
    */
    if ((*where == INTN_OPCODE) ||
        ((*where == XUUO_OPCODE_0) && (where[1] == XUUO_OPCODE_1))) {
      SET_TCR_FLAG(tcr,TCR_FLAG_BIT_PENDING_SUSPEND);
      ResumeThread(hthread);
      SEM_WAIT_FOREVER(TCR_AUX(tcr)->suspend);
      pcontext = NULL;
    }
    TCR_AUX(tcr)->suspend_context = pcontext;
    return true;
  }
  return false;
}
#else
#ifdef DARWIN
extern ExceptionInformation *
create_thread_context_frame(mach_port_t, natural *, siginfo_t *, TCR*, native_thread_state_t *);

Boolean mach_suspend_tcr(TCR *tcr)
{
  mach_port_t thread = (mach_port_t)((intptr_t)tcr->native_thread_id);
  kern_return_t kret = thread_suspend(thread);

  if (kret == 0) {
    kret = thread_abort_safely(thread);       /* or maybe thread_abort(). Who knows? */
    if (kret == 0) {
      mach_msg_type_number_t thread_state_count = NATIVE_THREAD_STATE_COUNT;
      native_thread_state_t ts;

      thread_get_state(thread,
                       NATIVE_THREAD_STATE_FLAVOR,
                       (thread_state_t)&ts,
                       &thread_state_count);

      tcr->suspend_context = create_thread_context_frame(thread,
                                                         NULL,
                                                         NULL,
                                                         tcr,
                                                         &ts);
      SET_TCR_FLAG(tcr,TCR_FLAG_BIT_ALT_SUSPEND);
      return true;
    }
  }
  return false;
}
#endif

    

Boolean
suspend_tcr(TCR *tcr)
{
  int suspend_count = atomic_incf(&(tcr->suspend_count)), kill_return;
  pthread_t thread;
  if (suspend_count == 1) {
    thread = (pthread_t)(tcr->osid);
    if ((thread != (pthread_t) 0) &&
        (kill_return = pthread_kill(thread, thread_suspend_signal) == 0)) {
      SET_TCR_FLAG(tcr,TCR_FLAG_BIT_SUSPEND_ACK_PENDING);
    } else {
#ifdef DARWIN
      if (kill_return != ESRCH) {
        return mach_suspend_tcr(tcr);
      }
#endif
      tcr->osid = 0;
      return false;
    }
    return true;
  }
  return false;
}
#endif

#ifdef WINDOWS
Boolean
tcr_suspend_ack(TCR *tcr)
{
  return true;
}
#else
Boolean
tcr_suspend_ack(TCR *tcr)
{
  if (tcr->flags & (1<<TCR_FLAG_BIT_SUSPEND_ACK_PENDING)) {
    SEM_WAIT_FOREVER(tcr->suspend);
    tcr->flags &= ~(1<<TCR_FLAG_BIT_SUSPEND_ACK_PENDING);
  }
  return true;
}
#endif
      

Boolean
kill_tcr(TCR *tcr)
{
  TCR *current = get_tcr(true);
  Boolean result = false;

  LOCK(lisp_global(TCR_AREA_LOCK),current);
  {
    LispObj osid = TCR_AUX(tcr)->osid;
    
    if (osid) {
      result = true;
#ifdef WINDOWS
      /* What we really want to do here is (something like)
         forcing the thread to run quit_handler().  For now,
         mark the TCR as dead and kill the Windows thread. */
      /* xxx TerminateThread() bad */
      TCR_AUX(tcr)->osid = 0;
      if (!TerminateThread((HANDLE)osid, 0)) {
        CloseHandle((HANDLE)osid);
        result = false;
      } else {
        CloseHandle((HANDLE)osid);
        shutdown_thread_tcr(tcr);
      }
#else
      if (pthread_kill((pthread_t)osid,thread_kill_signal)) {
        result = false;
      }
#endif
    }
  }
  UNLOCK(lisp_global(TCR_AREA_LOCK), current);
  return result;
}

Boolean
lisp_suspend_tcr(TCR *tcr)
{
  Boolean suspended;
  TCR *current = get_tcr(true);
  
  LOCK(lisp_global(TCR_AREA_LOCK),current);
  suspended = suspend_tcr(tcr);
  if (suspended) {
    while (!tcr_suspend_ack(tcr));
  }
  UNLOCK(lisp_global(TCR_AREA_LOCK),current);
  return suspended;
}
	 
#ifdef WINDOWS
Boolean
resume_tcr(TCR *tcr)
{
  int suspend_count = atomic_decf(&(TCR_AUX(tcr)->suspend_count)), err;
  DWORD rc;
  if (suspend_count == 0) {
    CONTEXT *context = TCR_AUX(tcr)->suspend_context;
    HANDLE hthread = (HANDLE)(TCR_AUX(tcr)->osid);


    TCR_AUX(tcr)->suspend_context = NULL;
    if (context) {
      if (tcr->valence == TCR_STATE_LISP) {
        rc = SetThreadContext(hthread,context);
        if (! rc) {
          Bug(NULL,"SetThreadContext");
          return false;
        }
      }
      rc = ResumeThread(hthread);
      if (rc == -1) {
        Bug(NULL,"ResumeThread");
        return false;
      }
      return true;
    } else {
      SEM_RAISE(TCR_AUX(tcr)->resume);
      return true;
    }
  }
  return false;
}   
#else
#ifdef DARWIN
Boolean mach_resume_tcr(TCR *tcr)
{
  ExceptionInformation *xp = tcr->suspend_context;
  mach_port_t thread = (mach_port_t)((intptr_t)(tcr->native_thread_id));
#if WORD_SIZE == 64
  MCONTEXT_T mc = UC_MCONTEXT(xp);
#else
  mcontext_t mc = UC_MCONTEXT(xp);
#endif

  thread_set_state(thread,
                   NATIVE_FLOAT_STATE_FLAVOR,
                   (thread_state_t)&(mc->__fs),
                   NATIVE_FLOAT_STATE_COUNT);

  thread_set_state(thread,
                   NATIVE_THREAD_STATE_FLAVOR,
                   (thread_state_t)&(mc->__ss),
                   NATIVE_THREAD_STATE_COUNT);
  thread_resume(thread);
  return true;
}
#endif
Boolean
resume_tcr(TCR *tcr)
{
  int suspend_count = atomic_decf(&(tcr->suspend_count));
  if (suspend_count == 0) {
#ifdef DARWIN
    if (tcr->flags & (1L<<TCR_FLAG_BIT_ALT_SUSPEND)) {
      return mach_resume_tcr(tcr);
    }
#endif
    {
      void *s = (tcr->resume);
      if (s != NULL) {
        SEM_RAISE(s);
        return true;
      }
    }
  }
  return false;
}
#endif

    


Boolean
lisp_resume_tcr(TCR *tcr)
{
  Boolean resumed;
  TCR *current = get_tcr(true);
  
  LOCK(lisp_global(TCR_AREA_LOCK),current);
  resumed = resume_tcr(tcr);
  UNLOCK(lisp_global(TCR_AREA_LOCK), current);
  return resumed;
}


TCR *freed_tcrs = NULL;

void
enqueue_freed_tcr (TCR *tcr)
{
#ifndef HAVE_TLS
  TCR_AUX(tcr)->next = freed_tcrs;
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

#ifndef ARM
  a = tcr->ts_area;
  if (a) {
    a->active = a->high;
  }
#endif

  a = TCR_AUX(tcr)->cs_area;
  if (a) {
    a->active = a->high;
  }
}
    
void
free_freed_tcrs ()
{
  TCR *current, *next;

  for (current = freed_tcrs; current; current = next) {
    next = TCR_AUX(current)->next;
#ifndef HAVE_TLS
#ifdef WIN_32
    /* We sort of have TLS in that the TEB is per-thread.  We free the
     * tcr aux vector elsewhere. */
#else
    free(current);
#endif
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
  for (other = TCR_AUX(current)->next; other != current; other = TCR_AUX(other)->next) {
    if ((TCR_AUX(other)->osid != 0)) {
      suspend_tcr(other);
      if (TCR_AUX(other)->osid == 0) {
	dead_tcr_count++;
      }
    } else {
      dead_tcr_count++;
    }
  }

  do {
    all_acked = true;
    for (other = TCR_AUX(current)->next; other != current; other = TCR_AUX(other)->next) {
      if ((TCR_AUX(other)->osid != 0)) {
        if (!tcr_suspend_ack(other)) {
          all_acked = false;
        }
      }
    }
  } while(! all_acked);

      

  /* All other threads are suspended; can safely delete dead tcrs now */
  if (dead_tcr_count) {
    for (other = TCR_AUX(current)->next; other != current; other = next) {
      next = TCR_AUX(other)->next;
      if (TCR_AUX(other)->osid == 0)  {
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

  for (other = TCR_AUX(current)->next; other != current; other = TCR_AUX(other)->next) {
    if ((TCR_AUX(other)->osid != 0)) {
      resume_tcr(other);
    }
  }
  free_freed_tcrs();
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
  free((void *)(rw->malloced_ptr));
}




