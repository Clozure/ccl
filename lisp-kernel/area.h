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

#ifndef __AREA_H__
#define __AREA_H__ 1


#include "bits.h"
#include "memprotect.h"



typedef enum {
  AREA_VOID = 0,		/* Not really an area at all */
  AREA_CSTACK = 1<<fixnumshift, /* A control stack */
  AREA_VSTACK = 2<<fixnumshift, /* A value stack.  The GC sees it as being doubleword-aligned */
  AREA_TSTACK = 3<<fixnumshift, /* A temp stack.  It -is- doubleword-aligned */
  AREA_READONLY = 4<<fixnumshift, /* A (cfm) read-only section. */
  AREA_MANAGED_STATIC = 5<<fixnumshift, /* A resizable static area */
  AREA_STATIC = 6<<fixnumshift, /* A  static section: contains
                                 roots, but not GCed */
  AREA_DYNAMIC = 7<<fixnumshift /* A heap. Only one such area is "the heap."*/
} area_code;

typedef struct area {
  struct area* pred;            /* linked list predecessor */
  struct area* succ;            /* linked list successor */
  char* low;                    /* arithmetic lower limit on addresses
                                   (inclusive) */
  char* high;                   /* arithmetic upper limit on addresses
                                   (exclusive) */
  char* active;                 /* low bound (stack) or high bound
                                   (heap) */
  char* softlimit;		/* only makes sense for dynamic heaps
                                   & stacks */
  char* hardlimit;		/* only makes sense for dynamic heaps
                                   & stacks */
  natural code;
  natural*  markbits;           /* markbits for active area */
  natural ndnodes;		/* "active" size of dynamic area or
                                   stack */
  struct area* older;		/* if ephemeral, the next older ephemeral area
				 or the dynamic area */
  struct area* younger;         /* if ephemeral, the next "younger"
                                  ephemeral area if there is one.  If
                                  dynamic, the oldest ephemeral
                                  area. */
  char*  h;			/* The pointer allocated to contain
				 this area, or NULL if the operating
				 system allocated it for us. */
  protected_area* softprot;     /* "soft" protected_area */
  protected_area* hardprot;     /* "hard" protected_area */
  TCR * owner;                  /* TCR that the area belongs to, if a stack */
  natural*  refbits;            /* intergenerational references.  May
                                               or may not be the same
                                               as markbits */
  natural threshold;            /* egc threshold (boxed "fullword
                                   count") or 0 */
  LispObj gccount;              /* boxed generation GC count. */
  natural static_dnodes;        /* for hash consing, maybe other things. */
  natural *static_used;         /* bitvector */
} area;


/*
  Areas are kept in a doubly-linked list.
  The list header is just a distinguished element of
  that list; by convention, the "active" dynamic
  area is described by that header's successor, and areas
  that may have entries in their "markbits" vector (heaps)
  precede (in the area_list->succ sense) those  that don't (stacks).
  The list header's "area" pointer is an "AREA_VOID" area; the header
  (once allocated during kernel initialization) never
  moves or changes.  Lisp code can get its hands on
  the list header via a nilreg global, and carefully,
  atomically, traverse it to do ROOM, etc.
*/


area *new_area(BytePtr, BytePtr, area_code);
void add_area(area *, TCR *);
void add_area_holding_area_lock(area *);
void condemn_area(area *, TCR *);
void condemn_area_holding_area_lock(area *);
area *area_containing(BytePtr);
area *stack_area_containing(BytePtr);
area *heap_area_containing(BytePtr);
void tenure_to_area(area *);
void untenure_from_area(area *);

/* serialize add_area/remove_area, and also the tcr queue */
void *tcr_area_lock;

#define reserved_area ((area *)(all_areas))
#define active_dynamic_area ((area *)(reserved_area->succ))

typedef struct area_list {
  area *the_area;
  struct area_list *next;
} area_list;

/* The useable size of a tsp or vsp stack segment.
  */
/* #define STACK_SEGMENT_SIZE (64<<10) */
#define MIN_CSTACK_SIZE (1<<17)
#define CSTACK_HARDPROT (100<<10)
#define CSTACK_SOFTPROT (100<<10)
#define MIN_VSTACK_SIZE (1<<16)
#define VSTACK_HARDPROT (1<<12)
#define VSTACK_SOFTPROT (1<<16)
#define MIN_TSTACK_SIZE (1<<18)
#define TSTACK_HARDPROT 0
#define TSTACK_SOFTPROT (1<<16)
#ifdef PPC
#define CS_OVERFLOW_FORCE_LIMIT ((natural)(-(sizeof(lisp_frame))))
#endif

#ifdef X86
#define CS_OVERFLOW_FORCE_LIMIT ((natural)(-16))
#endif


#ifdef PPC
#ifdef LINUX
#ifdef PPC64
#define IMAGE_BASE_ADDRESS 0x100000000L
#else
#define IMAGE_BASE_ADDRESS 0x31000000
#endif
#endif
#ifdef DARWIN
#ifdef PPC64
#define IMAGE_BASE_ADDRESS 0x100000000L
#else
#define IMAGE_BASE_ADDRESS 0x04000000
#endif
#endif
#endif

#ifdef X86
#ifdef LINUX
#ifdef X8664
#define IMAGE_BASE_ADDRESS 0x300000000000L
#else
#define IMAGE_BASE_ADDRESS 0x04000000
#endif
#endif
#ifdef FREEBSD
#define IMAGE_BASE_ADDRESS 0x300000000000L /* 0x100000000L */
#endif
#ifdef SOLARIS
#define IMAGE_BASE_ADDRESS 0x300000000000L
#endif
#ifdef DARWIN
#ifdef X8664
#define IMAGE_BASE_ADDRESS 0x300000000000L
#else
#define IMAGE_BASE_ADDRESS 0x04000000
#endif
#endif
#endif
#ifdef WINDOWS
#ifdef X8664
#define IMAGE_BASE_ADDRESS 0x100000000LL
#else
#define IMAGE_BASE_ADDRESS 0x04000000
#endif
#endif

#ifdef X8664
#define PURESPACE_RESERVE 0x40000000 /* 1GB */
#else
#define PURESPACE_RESERVE 0x04000000 /* 64MB */
#endif

#define STATIC_RESERVE heap_segment_size

#ifndef X86
#define STATIC_BASE_ADDRESS 0x00002000
#else
#define STATIC_BASE_ADDRESS 0x00012000
#endif

extern LispObj image_base;
extern BytePtr pure_space_start, pure_space_active, pure_space_limit;
extern BytePtr static_space_start, static_space_active, static_space_limit;
extern area *find_readonly_area(void);

#endif /* __AREA_H__ */
