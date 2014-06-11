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

#include "lisp.h"
#include "lisp-exceptions.h"
#include "lisp_globals.h"
#include "threads.h"
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#ifdef LINUX
#include <strings.h>
#endif
#ifdef DARWIN
#include <pthread.h>
#endif

#ifndef WINDOWS
#include <sys/mman.h>
#endif

#define DEBUG_MEMORY 0

void
allocation_failure(Boolean pointerp, natural size)
{
  char buf[64];
  sprintf(buf, "Can't allocate %s of size " DECIMAL " bytes.", pointerp ? "pointer" : "handle", size);
  Fatal(":   Kernel memory allocation failure.  ", buf);
}

void *
lisp_malloc(size_t size)
{
  return malloc(size);
}

void
lisp_free(void *p)
{
  free(p);
}


LogicalAddress
ReserveMemoryForHeap(LogicalAddress want, natural totalsize)
{
  void raise_limit(void);
  LogicalAddress start;
  raise_limit();
#ifdef WINDOWS
  start = VirtualAlloc((void *)want,
		       totalsize + heap_segment_size,
		       MEM_RESERVE,
		       PAGE_NOACCESS);
  if (!start) {
#if DEBUG_MEMORY    
    fprintf(dbgout, "Can't get desired heap address at 0x" LISP "\n", want);
#endif
    start = VirtualAlloc(0,
			 totalsize + heap_segment_size,
			 MEM_RESERVE,
			 PAGE_NOACCESS);
    if (!start) {
      return NULL;
    }
  }
#else
  start = mmap((void *)want,
	       totalsize + heap_segment_size,
	       PROT_NONE,
	       MAP_PRIVATE | MAP_ANON | MAP_NORESERVE,
	       -1,
	       0);
  if (start == MAP_FAILED) {
    return NULL;
  }

  if (start != want) {
    munmap(start, totalsize+heap_segment_size);
    start = (void *)((((natural)start)+heap_segment_size-1) & ~(heap_segment_size-1));
    if(mmap(start, totalsize, PROT_NONE, MAP_PRIVATE | MAP_ANON | MAP_FIXED | MAP_NORESERVE, -1, 0) != start) {
      return NULL;
    }
  }
  mprotect(start, totalsize, PROT_NONE);
#endif
#if DEBUG_MEMORY
  fprintf(dbgout, "Reserving heap at 0x" LISP ", size 0x" LISP "\n", start, totalsize);
#endif
  return start;
}

int
CommitMemory (LogicalAddress start, natural len) 
{
#if DEBUG_MEMORY
  fprintf(dbgout, "Committing memory at 0x" LISP ", size 0x" LISP "\n", start, len);
#endif
#ifdef WINDOWS
  LogicalAddress rc;

  if ((start < ((LogicalAddress)nil_value)) &&
      (((LogicalAddress)nil_value) < (start+len))) {
    /* nil area is in the executable on Windows; ensure range is
       read-write */
    DWORD as_if_i_care;
    if (!VirtualProtect(start,len,PAGE_EXECUTE_READWRITE,&as_if_i_care)) {
      return false;
    }
    return true;
  }
  rc = VirtualAlloc(start, len, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  if (!rc) {
    wperror("CommitMemory VirtualAlloc");
    return false;
  }
  return true;
#else
  int i;
  void *addr;

  for (i = 0; i < 3; i++) {
    addr = mmap(start, len, MEMPROTECT_RWX, MAP_PRIVATE|MAP_ANON|MAP_FIXED, -1, 0);
    if (addr == start) {
      return true;
    } else {
      mmap(addr, len, MEMPROTECT_NONE, MAP_PRIVATE|MAP_ANON|MAP_FIXED, -1, 0);
    }
  }
  return false;
#endif
}

void
UnCommitMemory (LogicalAddress start, natural len) {
#if DEBUG_MEMORY
  fprintf(dbgout, "Uncommitting memory at 0x" LISP ", size 0x" LISP "\n", start, len);
#endif
#ifdef WINDOWS
  int rc = VirtualFree(start, len, MEM_DECOMMIT);
  if (!rc) {
    wperror("UnCommitMemory VirtualFree");
    Fatal("mmap error", "");
    return;
  }
#else
  if (len) {
    madvise(start, len, MADV_DONTNEED);
    if (mmap(start, len, MEMPROTECT_NONE, MAP_PRIVATE|MAP_ANON|MAP_FIXED, -1, 0)
	!= start) {
      int err = errno;
      Fatal("mmap error", "");
      fprintf(dbgout, "errno = %d", err);
    }
  }
#endif
}


LogicalAddress
MapMemory(LogicalAddress addr, natural nbytes, int protection)
{
  LogicalAddress p;
#if DEBUG_MEMORY
  fprintf(dbgout, "Mapping memory at 0x" LISP ", size 0x" LISP "\n", addr, nbytes);
#endif
#ifdef WINDOWS
  p = VirtualAlloc(addr, nbytes, MEM_RESERVE|MEM_COMMIT, MEMPROTECT_RWX);
  if (p == NULL) {
    wperror("MapMemory");
  }
  return p;
#else
  {
    int flags = MAP_PRIVATE|MAP_ANON;

    if (addr > 0) flags |= MAP_FIXED;
    return mmap(addr, nbytes, protection, flags, -1, 0);
  }
#endif
}

LogicalAddress
MapMemoryForStack(natural nbytes)
{
#if DEBUG_MEMORY
  fprintf(dbgout, "Mapping stack of size 0x" LISP "\n", nbytes);
#endif
#ifdef WINDOWS
  return VirtualAlloc(0, nbytes, MEM_RESERVE|MEM_COMMIT, MEMPROTECT_RWX);
#else
  return mmap(NULL, nbytes, MEMPROTECT_RWX, MAP_PRIVATE|MAP_ANON, -1, 0);
#endif
}


/* Cause the mapped memory region at ADDR to become completely unmapped.
   ADDR should be an address returned by MapMemoryForStack() or MapMemory(),
   and NBYTES should be the size of the mapped region at that address. */
int
UnMapMemory(LogicalAddress addr, natural nbytes)
{
#if DEBUG_MEMORY
  fprintf(dbgout, "Unmapping memory at 0x" LISP ", size 0x" LISP "\n", addr, nbytes);
#endif
#ifdef WINDOWS
  return !VirtualFree(addr, 0, MEM_RELEASE);
#else
  return munmap(addr, nbytes);
#endif
}

int
ProtectMemory(LogicalAddress addr, natural nbytes)
{
#if DEBUG_MEMORY
  fprintf(dbgout, "Protecting memory at 0x" LISP ", size 0x" LISP "\n", addr, nbytes);
#endif
#ifdef WINDOWS
  DWORD oldProtect;
  BOOL status = VirtualProtect(addr, nbytes, MEMPROTECT_RX, &oldProtect);
  
  if(!status) {
    wperror("ProtectMemory VirtualProtect");
    Bug(NULL, "couldn't protect " DECIMAL " bytes at 0x" LISP ", errno = %d", nbytes, addr, status);
  }
  return status;
#else
  int status = mprotect(addr, nbytes, PROT_READ | PROT_EXEC);
  
  if (status) {
    status = errno;
    
    if (status == ENOMEM) {
      void *mapaddr = mmap(addr,nbytes, PROT_READ | PROT_EXEC, MAP_ANON|MAP_PRIVATE|MAP_FIXED,-1,0);
      if (mapaddr != MAP_FAILED) {
        return 0;
      }
    }
    Bug(NULL, "couldn't protect " DECIMAL " bytes at " LISP ", errno = %d", nbytes, addr, status);
  }
  return status;
#endif
}

int
UnProtectMemory(LogicalAddress addr, natural nbytes)
{
#if DEBUG_MEMORY
  fprintf(dbgout, "Unprotecting memory at 0x" LISP ", size 0x" LISP "\n", addr, nbytes);
#endif
#ifdef WINDOWS
  DWORD oldProtect;
  return VirtualProtect(addr, nbytes, MEMPROTECT_RWX, &oldProtect);
#else
  return mprotect(addr, nbytes, PROT_READ|PROT_WRITE|PROT_EXEC);
#endif
}

int
MapFile(LogicalAddress addr, natural pos, natural nbytes, int permissions, int fd) 
{
#ifdef WINDOWS
#if 0
  /* Lots of hair in here: mostly alignment issues, but also address space reservation */
  HANDLE hFile, hFileMapping;
  LPVOID rc;
  DWORD desiredAccess;

  if (permissions == MEMPROTECT_RWX) {
    permissions |= PAGE_WRITECOPY;
    desiredAccess = FILE_MAP_READ|FILE_MAP_WRITE|FILE_MAP_COPY|FILE_MAP_EXECUTE;
  } else {
    desiredAccess = FILE_MAP_READ|FILE_MAP_COPY|FILE_MAP_EXECUTE;
  }

  hFile = _get_osfhandle(fd);
  hFileMapping = CreateFileMapping(hFile, NULL, permissions,
				   (nbytes >> 32), (nbytes & 0xffffffff), NULL);
  
  if (!hFileMapping) {
    wperror("CreateFileMapping");
    return false;
  }

  rc = MapViewOfFileEx(hFileMapping,
		       desiredAccess,
		       (pos >> 32),
		       (pos & 0xffffffff),
		       nbytes,
		       addr);
#else
  size_t count, total = 0;
  size_t opos;

  opos = LSEEK(fd, 0, SEEK_CUR);
  CommitMemory(addr, nbytes);
  LSEEK(fd, pos, SEEK_SET);

  while (total < nbytes) {
    count = read(fd, addr + total, nbytes - total);
    total += count;
    // fprintf(dbgout, "read " DECIMAL " bytes, for a total of " DECIMAL " out of " DECIMAL " so far\n", count, total, nbytes);
    if (!(count > 0))
      return false;
  }

  LSEEK(fd, opos, SEEK_SET);

  return true;
#endif
#else
  return mmap(addr, nbytes, permissions, MAP_PRIVATE|MAP_FIXED, fd, pos) != MAP_FAILED;
#endif
}

void
unprotect_area(protected_area_ptr p)
{
  BytePtr start = p->start;
  natural nprot = p->nprot;
  
  if (nprot) {
    UnProtectMemory(start, nprot);
    p->nprot = 0;
  }
}

protected_area_ptr
new_protected_area(BytePtr start, BytePtr end, lisp_protection_kind reason, natural protsize, Boolean now)
{
  protected_area_ptr p = malloc(sizeof(protected_area));
  
  if (p == NULL) return NULL;
  p->protsize = protsize;
  p->nprot = 0;
  p->start = start;
  p->end = end;
  p->why = reason;
  p->next = AllProtectedAreas;

  AllProtectedAreas = p;
  if (now) {
    protect_area(p);
  }
  
  return p;
}




protected_area_ptr
AllProtectedAreas = NULL;


/* 
  This does a linear search.  Areas aren't created all that often;
  if there get to be very many of them, some sort of tree search
  might be justified.
*/

protected_area_ptr
find_protected_area(BytePtr addr)
{
  protected_area* p;
  
  for(p = AllProtectedAreas; p; p=p->next) {
    if ((p->start <= addr) && (p->end > addr)) {
      return p;
    }
  }
  return NULL;
}


void
zero_refbits(bitvector refidx, bitvector refbits, natural ndnodes)
{
  bitvector refbase = refbits, refword, limit = refbase + ((ndnodes + (WORD_SIZE-1)) >> node_shift), reflimit;
  natural i, n = (((ndnodes + 255) >> 8) + (WORD_SIZE-1)) >> bitmap_shift, bit, idx;

  for (i = 0; i < n; i++, refbase += WORD_SIZE * (256 / WORD_SIZE)) {
    idx = *refidx;
    
    if (idx != 0) {
      *refidx = 0;
      while (idx) {
        bit = count_leading_zeros(idx);
        idx &= ~(BIT0_MASK>>bit);
        refword = refbase + bit * (256/WORD_SIZE);
        reflimit = refword + (256/WORD_SIZE);
        if (limit < reflimit) {
          reflimit = limit;
        }
        while (refword < reflimit) {
          *refword++ = 0;
        }
      }
    }
    refidx++;
  }
#if 0
  /* Check,slowly */
  for (i=0;i<ndnodes;i++) {
    if (ref_bit(refbits,i)) {
      Bug(NULL, "Bit 0x" LISP " set unexpectedly\n", i);
    }
  }
#endif
}


  

/* 
   Grow or shrink the dynamic area.  Or maybe not.
   Whether or not the end of (mapped space in) the heap changes,
   ensure that everything between the freeptr and the heap end
   is mapped and read/write.  (It'll incidentally be zeroed.)
*/
Boolean
resize_dynamic_heap(BytePtr newfree, 
		    natural free_space_size)
{
  area *a = active_dynamic_area;
  BytePtr newlimit;

  if (free_space_size) {
    BytePtr lowptr = a->active;
    newlimit = lowptr + align_to_power_of_2(newfree-lowptr+free_space_size,
					    log2_heap_segment_size);
    if (newlimit > a->high) {
      return grow_dynamic_area(newlimit-a->high);
    } else if ((lowptr + free_space_size) < a->high) {
      shrink_dynamic_area(a->high-newlimit);
      return true;
    }
  } 
  return false;
}

void
protect_area(protected_area_ptr p)
{
  BytePtr start = p->start;
  natural n = p->protsize;

  if (n && ! p->nprot) {
    ProtectMemory(start, n);
#ifdef WINDOWS
    VirtualAlloc(start+n-page_size,page_size,MEM_COMMIT,PAGE_READWRITE|PAGE_GUARD);
#endif
    p->nprot = n;
  }
}


void
zero_page(BytePtr start)
{
  extern int page_size;
#ifdef PPC
  extern void zero_cache_lines(BytePtr, size_t, size_t);
  zero_cache_lines(start, (page_size/cache_block_size), cache_block_size);
#else
  memset(start, 0, page_size);
#endif
}

/* area management */


area *
new_area(BytePtr lowaddr, BytePtr highaddr, area_code code)
{
  area *a = calloc(1, sizeof(area));
  if (a) {
    natural ndnodes = area_dnode(highaddr, lowaddr);
    a->low = lowaddr;
    a->high = highaddr;
    a->active = (code == AREA_DYNAMIC) ? lowaddr : highaddr;
    a->code = code;
    a->ndnodes = ndnodes;
    /* Caller must allocate markbits when allocating heap ! */
    
  }
  return a;
}

static area *
add_area_before(area *new_area, area *before)
{
  area *before_before = before->pred;

  new_area->pred = before_before;
  new_area->succ = before;
  before_before->succ = new_area;
  before->pred = new_area;
  return new_area;
}

/*
  The active dynamic area comes first.
  Static areas follow dynamic areas.
  Stack areas follow static areas.
  Readonly areas come last.
*/

/*
  If we already own the area_lock (or during iniitalization), it's safe
  to add an area.
*/


void
add_area_holding_area_lock(area *new_area)
{
  area *that = all_areas;
  int
    thiscode = (int)(new_area->code),
    thatcode;

  /* Cdr down the linked list */
  do {
    that = that->succ;
    thatcode = (int)(that->code);
  } while (thiscode < thatcode);
  add_area_before(new_area, that);
}

/*
  In general, we need to own the area lock before adding an area.
*/
void
add_area(area *new_area, TCR *tcr)
{
  LOCK(lisp_global(TCR_AREA_LOCK),tcr);
  add_area_holding_area_lock(new_area);
  LOCK(lisp_global(TCR_AREA_LOCK),tcr);
}  

/*
  Search areas "forward" from the header's successor, until
  an area containing ADDR is found or an area with code < MINCODE
  is encountered.
  This walks the area list visiting heaps (dynamic, then static)
  first, then stacks.

*/
static area *
find_area_forward(BytePtr addr, area_code mincode)
{
  area *p, *header = all_areas;

  for (p = header->succ; p != header; p = p->succ) {
    area_code pcode = p->code;
    if (pcode < mincode) {
      return NULL;
    }
    if (pcode >= AREA_READONLY) {
      if ((addr >= p->low) &&
          (addr < p->active)) {
        return p;
      }
    } else {
      if ((addr >= p->active) &&
          (addr < p->high)) {
        return p;
      }
    }
  }
  return NULL;
}

static area *
find_area_backward(BytePtr addr, area_code maxcode)
{
  area *p, *header = all_areas;

  for (p = header->pred; p != header; p = p->pred) {
    area_code pcode = p->code;

    if (pcode > maxcode) {
      return NULL;
    }
    if (pcode >= AREA_READONLY) {
      if ((addr >= p->low) &&
          (addr < p->active)) {
        return p;
      }
    } else {
      if ((addr >= p->active) &&
          (addr < p->high)) {
        return p;
      }
    }
  }
  return NULL;
}

area *
area_containing(BytePtr addr)
{
  return find_area_forward(addr, AREA_VOID);
}

area *
heap_area_containing(BytePtr addr)
{
  return find_area_forward(addr, AREA_READONLY);
}

area *
stack_area_containing(BytePtr addr)
{
  return find_area_backward(addr, AREA_TSTACK);
}

/*
  Make everything "younger" than the start of the target area
  belong to that area; all younger areas will become empty, and
  the dynamic area will have to lose some of its markbits (they
  get zeroed and become part of the tenured area's refbits.)

  The active dynamic area must have been "normalized" (e.g., its
  active pointer must match the free pointer) before this is called.

  If the target area is 'tenured_area' (the oldest ephemeral generation),
  zero its refbits and update YOUNGEST_EPHEMERAL.

*/

void
tenure_to_area(area *target)
{
  area *a = active_dynamic_area, *child;
  BytePtr 
    curfree = a->active,
    target_low = target->low;
  natural new_tenured_dnodes = area_dnode(curfree, tenured_area->low);
  bitvector 
    refbits = tenured_area->refbits,
    markbits = a->markbits,
    new_markbits;

  target->high = target->active = curfree;
  target->ndnodes = area_dnode(curfree, target_low);

  for (child = target->younger; child != a; child = child->younger) {
    child->high = child->low = child->active = curfree;
    child->ndnodes = 0;
  }

  a->low = curfree;
  a->ndnodes = area_dnode(a->high, curfree);

  new_markbits = refbits + ((new_tenured_dnodes + (nbits_in_word-1)) >> bitmap_shift);
  
  if (target == tenured_area) {
    zero_refbits(global_refidx,managed_static_area->refbits, managed_static_area->ndnodes);
    zero_bits(refbits, new_tenured_dnodes);
    zero_bits(dynamic_refidx,(new_tenured_dnodes+255)>>8);
    lisp_global(OLDEST_EPHEMERAL) = ptr_to_lispobj(curfree);
  } else {
    /* Need more (zeroed) refbits & fewer markbits */
    zero_bits(markbits, ((new_markbits-markbits)<<bitmap_shift));
  }
   
  a->markbits = new_markbits;
  lisp_global(OLDSPACE_DNODE_COUNT) = area_dnode(curfree, lisp_global(REF_BASE));
}



/*
  Make everything younger than the oldest byte in 'from' belong to 
  the youngest generation.  If 'from' is 'tenured_area', this means
  that nothing's ephemeral any more (and OLDEST_EPHEMERAL can be set
  to 0 to indicate this.)
  
  Some tenured_area refbits become dynamic area markbits in the process;
  it's not necessary to zero them, since the GC will do that.
*/

void
untenure_from_area(area *from)
{
  if (lisp_global(OLDEST_EPHEMERAL) != 0) {
    area *a = active_dynamic_area, *child;
    BytePtr curlow = from->low;
    natural new_tenured_dnodes = area_dnode(curlow, tenured_area->low);
    
    for (child = from; child != a; child = child->younger) {
      child->low = child->active = child->high = curlow;
      child->ndnodes = 0;
    }
    
    a->low = curlow;
    a->ndnodes = area_dnode(a->high, curlow);
    
    a->markbits = (tenured_area->refbits) + ((new_tenured_dnodes+(nbits_in_word-1))>>bitmap_shift);
    if (from == tenured_area) {
      /* Everything's in the dynamic area */
      lisp_global(OLDEST_EPHEMERAL) = 0;
      lisp_global(OLDSPACE_DNODE_COUNT) = 0;

    }
  }
}


Boolean
egc_control(Boolean activate, BytePtr curfree)
{
  area *a = active_dynamic_area;
  Boolean egc_is_active = (a->older != NULL);

  if (activate != egc_is_active) {
    if (curfree != NULL) {
      a->active = curfree;
    }
    if (activate) {
      a->older = g1_area;
      tenure_to_area(tenured_area);
      egc_is_active = true;
    } else {
      untenure_from_area(tenured_area);
      a->older = NULL;
      egc_is_active = false;
    }
  }
  return egc_is_active;
}

/*
  Lisp ff-calls this; it needs to set the active area's active pointer
  correctly.
*/

Boolean
lisp_egc_control(Boolean activate)
{
  area *a = active_dynamic_area;
  return egc_control(activate, (BytePtr) a->active);
}



  
/* Splice the protected_area_ptr out of the list and dispose of it. */
void
delete_protected_area(protected_area_ptr p)
{
  BytePtr start = p->start;
  int nbytes = p->nprot;
  protected_area_ptr *prev = &AllProtectedAreas, q;

  if (nbytes) {
    UnProtectMemory((LogicalAddress)start, nbytes);
  }
  
  while ((q = *prev) != NULL) {
    if (p == q) {
      *prev = p->next;
      break;
    } else {
      prev = &(q->next);
    }
  }

  free(p);
}




/* 
  Unlink the area from all_areas.
  Unprotect and dispose of any hard/soft protected_areas.
  If the area has a handle, dispose of that as well.
  */

void
condemn_area_holding_area_lock(area *a)
{
  void free_stack(void *);
  area *prev = a->pred, *next = a->succ;
  Ptr h = a->h;
  protected_area_ptr p;

  prev->succ = next;
  next->pred = prev;

  p = a->softprot;
  if (p) delete_protected_area(p);

  p = a->hardprot;

  if (p) delete_protected_area(p);

  if (h) free_stack(h);
  free(a);
}



void
condemn_area(area *a, TCR *tcr)
{
  LOCK(lisp_global(TCR_AREA_LOCK),tcr);
  condemn_area_holding_area_lock(a);
  LOCK(lisp_global(TCR_AREA_LOCK),tcr);
}




/*
  condemn an area and all the other areas that can be reached
  via the area.older & area.younger links.
  This is the function in the ppc::kernel-import-condemn-area slot,
  called by free-stack-area
  */
void
condemn_area_chain(area *a, TCR *tcr)
{
  area *older;

  LOCK(lisp_global(TCR_AREA_LOCK),tcr);

  for (; a->younger; a = a->younger) ;
  for (;a;) {
    older = a->older;
    condemn_area_holding_area_lock(a);
    a = older;
  }
  UNLOCK(lisp_global(TCR_AREA_LOCK),tcr);
}


void
protect_watched_areas()
{
  area *a = active_dynamic_area;
  natural code = a->code;

  while (code != AREA_VOID) {
    if (code == AREA_WATCHED) {
      natural size = a->high - a->low;
      
      ProtectMemory(a->low, size);
    }
    a = a->succ;
    code = a->code;
  }
}

void
unprotect_watched_areas()
{
  area *a = active_dynamic_area;
  natural code = a->code;

  while (code != AREA_VOID) {
    if (code == AREA_WATCHED) {
      natural size = a->high - a->low;
      
      UnProtectMemory(a->low, size);
    }
    a = a->succ;
    code = a->code;
  }
}

LogicalAddress
ReserveMemory(natural size)
{
  LogicalAddress p;
#ifdef WINDOWS
  p = VirtualAlloc(0,
                   size,
                   MEM_RESERVE,
                   PAGE_NOACCESS);
  return p;
#else
  p = mmap(NULL,size,PROT_NONE,MAP_PRIVATE|MAP_ANON|MAP_NORESERVE,-1,0);
  if (p == MAP_FAILED) {
    return NULL;
  }
  return p;
#endif
}
