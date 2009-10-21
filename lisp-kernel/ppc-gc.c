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
#include "lisp_globals.h"
#include "bits.h"
#include "gc.h"
#include "area.h"
#include "Threads.h"
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

/* Heap sanity checking. */

void
check_node(LispObj n)
{
  int tag = fulltag_of(n), header_tag;
  area *a;
  LispObj header;

  switch (tag) {
  case fulltag_even_fixnum:
  case fulltag_odd_fixnum:


#ifdef PPC64
  case fulltag_imm_0:
  case fulltag_imm_1:
  case fulltag_imm_2:
  case fulltag_imm_3:
#else
  case fulltag_imm:
#endif


    return;

#ifndef PPC64
  case fulltag_nil:
    if (n != lisp_nil) {
      Bug(NULL,"Object tagged as nil, not nil : 0x%08x", n);
    }
    return;
#endif


#ifdef PPC64
  case fulltag_nodeheader_0: 
  case fulltag_nodeheader_1: 
  case fulltag_nodeheader_2: 
  case fulltag_nodeheader_3: 
  case fulltag_immheader_0: 
  case fulltag_immheader_1: 
  case fulltag_immheader_2: 
  case fulltag_immheader_3: 
#else
  case fulltag_nodeheader:
  case fulltag_immheader:
#endif


    Bug(NULL, "Header not expected : 0x%lx", n);
    return;

  case fulltag_misc:
  case fulltag_cons:
    a = heap_area_containing((BytePtr)ptr_from_lispobj(n));
    
    if (a == NULL) {
      /* Can't do as much sanity checking as we'd like to
         if object is a defunct stack-consed object.
         If a dangling reference to the heap, that's
         bad .. */
      a = active_dynamic_area;
      if ((n > (ptr_to_lispobj(a->active))) &&
          (n < (ptr_to_lispobj(a->high)))) {
        Bug(NULL, "Node points to heap free space: 0x%lx", n);
      }
      return;
    }
    break;
  }
  /* Node points to heap area, so check header/lack thereof. */
  header = header_of(n);
  header_tag = fulltag_of(header);
  if (tag == fulltag_cons) {
    if ((nodeheader_tag_p(header_tag)) ||
        (immheader_tag_p(header_tag))) {
      Bug(NULL, "Cons cell at 0x%lx has bogus header : 0x%lx", n, header);
    }
    return;
  }

  if ((!nodeheader_tag_p(header_tag)) &&
      (!immheader_tag_p(header_tag))) {
    Bug(NULL,"Vector at 0x%lx has bogus header : 0x%lx", n, header);
  }
  return;
}




void
check_range(LispObj *start, LispObj *end, Boolean header_allowed)
{
  LispObj node, *current = start, *prev = NULL;
  int tag;
  natural elements;

  while (current < end) {
    prev = current;
    node = *current++;
    tag = fulltag_of(node);
    if (immheader_tag_p(tag)) {
      if (! header_allowed) {
        Bug(NULL, "Header not expected at 0x%lx\n", prev);
      }
      current = (LispObj *)skip_over_ivector((natural)prev, node);
    } else if (nodeheader_tag_p(tag)) {
      if (! header_allowed) {
        Bug(NULL, "Header not expected at 0x%lx\n", prev);
      }
      elements = header_element_count(node) | 1;
      while (elements--) {
        check_node(*current++);
      }
    } else {
      check_node(node);
      check_node(*current++);
    }
  }

  if (current != end) {
    Bug(NULL, "Overran end of memory range: start = 0x%08x, end = 0x%08x, prev = 0x%08x, current = 0x%08x",
        start, end, prev, current);
  }
}

void
check_all_areas(TCR *tcr)
{
  area *a = active_dynamic_area;
  area_code code = a->code;

  while (code != AREA_VOID) {
    switch (code) {
    case AREA_DYNAMIC:
    case AREA_STATIC:
    case AREA_MANAGED_STATIC:
      check_range((LispObj *)a->low, (LispObj *)a->active, true);
      break;

    case AREA_VSTACK:
      {
        LispObj* low = (LispObj *)a->active;
        LispObj* high = (LispObj *)a->high;
        
        if (((natural)low) & node_size) {
          check_node(*low++);
        }
        check_range(low, high, false);
      }
      break;

    case AREA_TSTACK:
      {
        LispObj *current, *next,
                *start = (LispObj *) a->active,
                *end = start,
                *limit = (LispObj *) a->high;
                 
        for (current = start;
             end != limit;
             current = next) {
          next = ptr_from_lispobj(*current);
          end = ((next >= start) && (next < limit)) ? next : limit;
          if (current[1] == 0) {
            check_range(current+2, end, true);
          }
        }
      }
      break;
    }
    a = a->succ;
    code = (a->code);
  }
}










/* Sooner or later, this probably wants to be in assembler */
/* Return false if n is definitely not an ephemeral node, true if
   it might be */
void
mark_root(LispObj n)
{
  int tag_n = fulltag_of(n);
  natural dnode, bits, *bitsp, mask;

  if (!is_node_fulltag(tag_n)) {
    return;
  }

  dnode = gc_area_dnode(n);
  if (dnode >= GCndnodes_in_area) {
    return;
  }
  set_bits_vars(GCmarkbits,dnode,bitsp,bits,mask);
  if (bits & mask) {
    return;
  }
  *bitsp = (bits | mask);

  if (tag_n == fulltag_cons) {
    cons *c = (cons *) ptr_from_lispobj(untag(n));
    rmark(c->car);
    rmark(c->cdr);
    return;
  }
  {
    LispObj *base = (LispObj *) ptr_from_lispobj(untag(n));
    natural
      header = *((natural *) base),
      subtag = header_subtag(header),
      element_count = header_element_count(header),
      total_size_in_bytes,      /* including 4/8-byte header */
      suffix_dnodes;
    tag_n = fulltag_of(header);


#ifdef PPC64
    if ((nodeheader_tag_p(tag_n)) ||
        (tag_n == ivector_class_64_bit)) {
      total_size_in_bytes = 8 + (element_count<<3);
    } else if (tag_n == ivector_class_8_bit) {
      total_size_in_bytes = 8 + element_count;
    } else if (tag_n == ivector_class_32_bit) {
      total_size_in_bytes = 8 + (element_count<<2);
    } else {
      /* ivector_class_other_bit contains 16-bit arrays & bitvector */
      if (subtag == subtag_bit_vector) {
        total_size_in_bytes = 8 + ((element_count+7)>>3);
      } else {
        total_size_in_bytes = 8 + (element_count<<1);
      }
    }
#else
    if ((tag_n == fulltag_nodeheader) ||
        (subtag <= max_32_bit_ivector_subtag)) {
      total_size_in_bytes = 4 + (element_count<<2);
    } else if (subtag <= max_8_bit_ivector_subtag) {
      total_size_in_bytes = 4 + element_count;
    } else if (subtag <= max_16_bit_ivector_subtag) {
      total_size_in_bytes = 4 + (element_count<<1);
    } else if (subtag == subtag_double_float_vector) {
      total_size_in_bytes = 8 + (element_count<<3);
    } else {
      total_size_in_bytes = 4 + ((element_count+7)>>3);
    }
#endif



    suffix_dnodes = ((total_size_in_bytes+(dnode_size-1))>>dnode_shift) -1;

    if (suffix_dnodes) {
      set_n_bits(GCmarkbits, dnode+1, suffix_dnodes);
    }

    if (nodeheader_tag_p(tag_n)) {
      if (subtag == subtag_hash_vector) {
        /* Don't invalidate the cache here.  It should get
           invalidated on the lisp side, if/when we know
           that rehashing is necessary. */
        LispObj flags = ((hash_table_vector_header *) base)->flags;

        if (flags & nhash_weak_mask) {
          ((hash_table_vector_header *) base)->cache_key = undefined;
          ((hash_table_vector_header *) base)->cache_value = lisp_nil;
          mark_weak_htabv(n);
	  return;
	}
      }

      if (subtag == subtag_pool) {
        deref(ptr_to_lispobj(base), 1) = lisp_nil;
      }
      
      if (subtag == subtag_weak) {
        natural weak_type = (natural) base[2];
        if (weak_type >> population_termination_bit) {
          element_count -= 2;
        } else {
          element_count -= 1;
        }
      }

      base += (1+element_count);


      while(element_count--) {
        rmark(*--base);
      }
      if (subtag == subtag_weak) {
        deref(ptr_to_lispobj(base),1) = GCweakvll;
        GCweakvll = n;
      }
    }
  }
}


/* 
  This marks the node if it needs to; it returns true if the node
  is either a hash table vector header or a cons/misc-tagged pointer
  to ephemeral space.
  Note that it  might be a pointer to ephemeral space even if it's
  not pointing to the current generation.
*/

Boolean
mark_ephemeral_root(LispObj n)
{
  int tag_n = fulltag_of(n);
  natural eph_dnode;

  if (nodeheader_tag_p(tag_n)) {
    return (header_subtag(n) == subtag_hash_vector);
  }
 
  if ((tag_n == fulltag_cons) ||
      (tag_n == fulltag_misc)) {
    eph_dnode = area_dnode(n, GCephemeral_low);
    if (eph_dnode < GCn_ephemeral_dnodes) {
      mark_root(n);             /* May or may not mark it */
      return true;              /* but return true 'cause it's an ephemeral node */
    }
  }
  return false;                 /* Not a heap pointer or not ephemeral */
}
  

#ifdef PPC64
/* Any register (srr0, the lr or ctr) or stack location that
   we're calling this on should have its low 2 bits clear; it'll
   be tagged as a "primary" object, but the pc/lr/ctr should
   never point to a tagged object or contain a fixnum.
   
   If the "pc" appears to be pointing into a heap-allocated
   code vector that's not yet marked, back up until we find
   the code-vector's prefix (the 32-bit word containing the
   value 'CODE' whic precedes the code-vector's first instruction)
   and mark the entire code-vector.
*/
void
mark_pc_root(LispObj xpc)
{
  if ((xpc & 3) != 0) {
    Bug(NULL, "Bad PC locative!");
  } else {
    natural dnode = gc_area_dnode(xpc);
    if ((dnode < GCndnodes_in_area) &&
        !ref_bit(GCmarkbits,dnode)) {
      LispObj
        *headerP,
        header;
      opcode *program_counter;

      for(program_counter=(opcode *)ptr_from_lispobj(xpc & ~7);
	  (LispObj)program_counter >= GCarealow;
          program_counter-=2) {
        if (*program_counter == PPC64_CODE_VECTOR_PREFIX) {
          headerP = ((LispObj *)program_counter)-1;
          header = *headerP;
	  dnode = gc_area_dnode(headerP);
          set_n_bits(GCmarkbits, dnode, (8+(header_element_count(header)<<2)+(dnode_size-1))>>dnode_shift);
          return;
        }
      }
      /*
        Expected to have found a header by now, but didn't.
        That's a bug.
        */
      Bug(NULL, "code_vector header not found!");
    }
  }
}
#else /* PPC64 */
/*
  Some objects (saved LRs on the control stack, the LR, PC, and CTR
  in exception frames) may be tagged as fixnums but are really
  locatives into code_vectors.

  If "pc" is not tagged as a fixnum, mark it as a "normal" root.
  If "pc" doesn't point at an unmarked doubleword in the area
  being GCed, return.
  Else back up until the code_vector's header is found and mark
  all doublewords in the code_vector.
*/
void
mark_pc_root(LispObj pc)
{
  if (tag_of(pc) != tag_fixnum) {
    mark_root(pc);
  } else {
    natural dnode = gc_area_dnode(pc);
    if ((dnode < GCndnodes_in_area) &&
        !ref_bit(GCmarkbits,dnode)) {
      LispObj
        *headerP,
        header;

      for(headerP = (LispObj*)ptr_from_lispobj(untag(pc));
          dnode < GCndnodes_in_area;
          headerP-=2, --dnode) {
        header = *headerP;

        if ((header & code_header_mask) == subtag_code_vector) {
          set_n_bits(GCmarkbits, dnode, (2+header_element_count(header))>>1);
          return;
        }
      }
      /*
        Expected to have found a header by now, but didn't.
        That's a bug.
        */
      Bug(NULL, "code_vector header not found!");
    }
  }
}
#endif /* PPC64 */



#ifdef PPC64
#define RMARK_PREV_ROOT fulltag_imm_3
#define RMARK_PREV_CAR fulltag_misc
#else
#define RMARK_PREV_ROOT fulltag_imm
#define RMARK_PREV_CAR fulltag_nil
#endif





/*
  This wants to be in assembler even more than "mark_root" does.
  For now, it does link-inversion: hard as that is to express in C,
  reliable stack-overflow detection may be even harder ...
*/
void
rmark(LispObj n)
{
  int tag_n = fulltag_of(n);
  bitvector markbits = GCmarkbits;
  natural dnode, bits, *bitsp, mask;

  if (!is_node_fulltag(tag_n)) {
    return;
  }

  dnode = gc_area_dnode(n);
  if (dnode >= GCndnodes_in_area) {
    return;
  }
  set_bits_vars(markbits,dnode,bitsp,bits,mask);
  if (bits & mask) {
    return;
  }
  *bitsp = (bits | mask);

  if (current_stack_pointer() > GCstack_limit) {
    if (tag_n == fulltag_cons) {
      rmark(deref(n,1));
      rmark(deref(n,0));
    } else {
      LispObj *base = (LispObj *) ptr_from_lispobj(untag(n));
      natural
        header = *((natural *) base),
        subtag = header_subtag(header),
        element_count = header_element_count(header),
        total_size_in_bytes,
        suffix_dnodes;
      tag_n = fulltag_of(header);
#ifdef PPC64
      if ((nodeheader_tag_p(tag_n)) ||
          (tag_n == ivector_class_64_bit)) {
        total_size_in_bytes = 8 + (element_count<<3);
      } else if (tag_n == ivector_class_8_bit) {
        total_size_in_bytes = 8 + element_count;
      } else if (tag_n == ivector_class_32_bit) {
        total_size_in_bytes = 8 + (element_count<<2);
      } else {
        /* ivector_class_other_bit contains 16-bit arrays & bitvector */
        if (subtag == subtag_bit_vector) {
          total_size_in_bytes = 8 + ((element_count+7)>>3);
        } else {
          total_size_in_bytes = 8 + (element_count<<1);
        }
      }
#else
      if ((tag_n == fulltag_nodeheader) ||
          (subtag <= max_32_bit_ivector_subtag)) {
        total_size_in_bytes = 4 + (element_count<<2);
      } else if (subtag <= max_8_bit_ivector_subtag) {
        total_size_in_bytes = 4 + element_count;
      } else if (subtag <= max_16_bit_ivector_subtag) {
        total_size_in_bytes = 4 + (element_count<<1);
      } else if (subtag == subtag_double_float_vector) {
        total_size_in_bytes = 8 + (element_count<<3);
      } else {
        total_size_in_bytes = 4 + ((element_count+7)>>3);
      }
#endif


      suffix_dnodes = ((total_size_in_bytes+(dnode_size-1))>>dnode_shift)-1;

      if (suffix_dnodes) {
        set_n_bits(GCmarkbits, dnode+1, suffix_dnodes);
      }

      if (!nodeheader_tag_p(tag_n)) return;

      if (subtag == subtag_hash_vector) {
        /* Splice onto weakvll, then return */
        /* In general, there's no reason to invalidate the cached
           key/value pair here.  However, if the hash table's weak,
           we don't want to retain an otherwise unreferenced key
           or value simply because they're referenced from the
           cache.  Clear the cached entries iff the hash table's
           weak in some sense.
        */
        LispObj flags = ((hash_table_vector_header *) base)->flags;

        if ((flags & nhash_keys_frozen_mask) &&
            (((hash_table_vector_header *) base)->deleted_count > 0)) {
          /* We're responsible for clearing out any deleted keys, since
             lisp side can't do it without breaking the state machine
          */
          LispObj *pairp = base + hash_table_vector_header_count;
          natural
            npairs = (element_count - (hash_table_vector_header_count - 1)) >> 1;

          while (npairs--) {
            if ((pairp[1] == unbound) && (pairp[0] != unbound)) {
              pairp[0] = slot_unbound;
            }
            pairp +=2;
          }
          ((hash_table_vector_header *) base)->deleted_count = 0;
        }


        if (flags & nhash_weak_mask) {
          ((hash_table_vector_header *) base)->cache_key = undefined;
          ((hash_table_vector_header *) base)->cache_value = lisp_nil;
	  mark_weak_htabv(n);
	  return;
	}
      }

      if (subtag == subtag_pool) {
        deref(n, 1) = lisp_nil;
      }

      if (subtag == subtag_weak) {
        natural weak_type = (natural) base[2];
        if (weak_type >> population_termination_bit)
          element_count -= 2;
        else
          element_count -= 1;
      }
      while (element_count) {
        rmark(deref(n,element_count));
        element_count--;
      }

      if (subtag == subtag_weak) {
        deref(n, 1) = GCweakvll;
        GCweakvll = n;
      }

    }
  } else {
    LispObj prev = undefined;
    LispObj this = n, next;
    /*
      This is an FSM.  The basic states are:
      (0) Just marked the cdr of a cons; mark the car next;
      (1) Just marked the car of a cons; back up.
      (2) Hit a gvector header.  Back up.
      (3) Marked a gvector element; mark the preceding one.
      (4) Backed all the way up to the object that got us here.
      
      This is all encoded in the fulltag of the "prev" pointer.
    */

    if (tag_n == fulltag_cons) goto MarkCons;
    goto MarkVector;

  ClimbCdr:
    prev = deref(this,0);
    deref(this,0) = next;

  Climb:
    next = this;
    this = prev;
    tag_n = fulltag_of(prev);
    switch(tag_n) {
    case fulltag_odd_fixnum:
    case fulltag_even_fixnum:
      goto ClimbVector;

    case RMARK_PREV_ROOT:
      return;

    case fulltag_cons:
      goto ClimbCdr;

    case RMARK_PREV_CAR:
      goto ClimbCar;

      /* default: abort() */
    }

  DescendCons:
    prev = this;
    this = next;

  MarkCons:
    next = deref(this,1);
    this += node_size;
    tag_n = fulltag_of(next);
    if (!is_node_fulltag(tag_n)) goto MarkCdr;
    dnode = gc_area_dnode(next);
    if (dnode >= GCndnodes_in_area) goto MarkCdr;
    set_bits_vars(markbits,dnode,bitsp,bits,mask);
    if (bits & mask) goto MarkCdr;
    *bitsp = (bits | mask);
    deref(this,1) = prev;
    if (tag_n == fulltag_cons) goto DescendCons;
    goto DescendVector;

  ClimbCar:
    prev = deref(this,1);
    deref(this,1) = next;

  MarkCdr:
    next = deref(this, 0);
    this -= node_size;
    tag_n = fulltag_of(next);
    if (!is_node_fulltag(tag_n)) goto Climb;
    dnode = gc_area_dnode(next);
    if (dnode >= GCndnodes_in_area) goto Climb;
    set_bits_vars(markbits,dnode,bitsp,bits,mask);
    if (bits & mask) goto Climb;
    *bitsp = (bits | mask);
    deref(this, 0) = prev;
    if (tag_n == fulltag_cons) goto DescendCons;
    /* goto DescendVector; */

  DescendVector:
    prev = this;
    this = next;

  MarkVector:
    {
      LispObj *base = (LispObj *) ptr_from_lispobj(untag(this));
      natural
        header = *((natural *) base),
        subtag = header_subtag(header),
        element_count = header_element_count(header),
        total_size_in_bytes,
        suffix_dnodes;

      tag_n = fulltag_of(header);

#ifdef PPC64
      if ((nodeheader_tag_p(tag_n)) ||
          (tag_n == ivector_class_64_bit)) {
        total_size_in_bytes = 8 + (element_count<<3);
      } else if (tag_n == ivector_class_8_bit) {
        total_size_in_bytes = 8 + element_count;
      } else if (tag_n == ivector_class_32_bit) {
        total_size_in_bytes = 8 + (element_count<<2);
      } else {
        /* ivector_class_other_bit contains 16-bit arrays & bitvector */
        if (subtag == subtag_bit_vector) {
          total_size_in_bytes = 8 + ((element_count+7)>>3);
        } else {
          total_size_in_bytes = 8 + (element_count<<1);
        }
      }
#else
      if ((tag_n == fulltag_nodeheader) ||
          (subtag <= max_32_bit_ivector_subtag)) {
        total_size_in_bytes = 4 + (element_count<<2);
      } else if (subtag <= max_8_bit_ivector_subtag) {
        total_size_in_bytes = 4 + element_count;
      } else if (subtag <= max_16_bit_ivector_subtag) {
        total_size_in_bytes = 4 + (element_count<<1);
      } else if (subtag == subtag_double_float_vector) {
        total_size_in_bytes = 8 + (element_count<<3);
      } else {
        total_size_in_bytes = 4 + ((element_count+7)>>3);
      }
#endif


      suffix_dnodes = ((total_size_in_bytes+(dnode_size-1))>>dnode_shift)-1;

      if (suffix_dnodes) {
        set_n_bits(GCmarkbits, dnode+1, suffix_dnodes);
      }

      if (!nodeheader_tag_p(tag_n)) goto Climb;

      if (subtag == subtag_hash_vector) {
        /* Splice onto weakvll, then climb */
        LispObj flags = ((hash_table_vector_header *) base)->flags;

        if (flags & nhash_weak_mask) {
          ((hash_table_vector_header *) base)->cache_key = undefined;
          ((hash_table_vector_header *) base)->cache_value = lisp_nil;
	  dws_mark_weak_htabv(this);
	  element_count = hash_table_vector_header_count;
	}
      }

      if (subtag == subtag_pool) {
        deref(this, 1) = lisp_nil;
      }

      if (subtag == subtag_weak) {
        natural weak_type = (natural) base[2];
        if (weak_type >> population_termination_bit)
          element_count -= 2;
        else
          element_count -= 1;
      }

      this = untag(this) + ((element_count+1) << node_shift);
      goto MarkVectorLoop;
    }

  ClimbVector:
    prev = *((LispObj *) ptr_from_lispobj(this));
    *((LispObj *) ptr_from_lispobj(this)) = next;

  MarkVectorLoop:
    this -= node_size;
    next = *((LispObj *) ptr_from_lispobj(this));
    tag_n = fulltag_of(next);
    if (nodeheader_tag_p(tag_n)) goto MarkVectorDone;
    if (!is_node_fulltag(tag_n)) goto MarkVectorLoop;
    dnode = gc_area_dnode(next);
    if (dnode >= GCndnodes_in_area) goto MarkVectorLoop;
    set_bits_vars(markbits,dnode,bitsp,bits,mask);
    if (bits & mask) goto MarkVectorLoop;
    *bitsp = (bits | mask);
    *(ptr_from_lispobj(this)) = prev;
    if (tag_n == fulltag_cons) goto DescendCons;
    goto DescendVector;

  MarkVectorDone:
    /* "next" is vector header; "this" is fixnum-aligned.
       If  header subtag = subtag_weak_header, put it on weakvll */
    this += fulltag_misc;

    if (header_subtag(next) == subtag_weak) {
      deref(this, 1) = GCweakvll;
      GCweakvll = this;
    }
    goto Climb;
  }
}

LispObj *
skip_over_ivector(natural start, LispObj header)
{
  natural 
    element_count = header_element_count(header),
    subtag = header_subtag(header),
    nbytes;

#ifdef PPC64
  switch (fulltag_of(header)) {
  case ivector_class_64_bit:
    nbytes = element_count << 3;
    break;
  case ivector_class_32_bit:
    nbytes = element_count << 2;
    break;
  case ivector_class_8_bit:
    nbytes = element_count;
    break;
  case ivector_class_other_bit:
  default:
    if (subtag == subtag_bit_vector) {
      nbytes = (element_count+7)>>3;
    } else {
      nbytes = element_count << 1;
    }
  }
  return ptr_from_lispobj(start+(~15 & (nbytes + 8 + 15)));
#else
  if (subtag <= max_32_bit_ivector_subtag) {
    nbytes = element_count << 2;
  } else if (subtag <= max_8_bit_ivector_subtag) {
    nbytes = element_count;
  } else if (subtag <= max_16_bit_ivector_subtag) {
    nbytes = element_count << 1;
  } else if (subtag == subtag_double_float_vector) {
    nbytes = 4 + (element_count << 3);
  } else {
    nbytes = (element_count+7) >> 3;
  }
  return ptr_from_lispobj(start+(~7 & (nbytes + 4 + 7)));
#endif



}


void
check_refmap_consistency(LispObj *start, LispObj *end, bitvector refbits)
{
  LispObj x1, *base = start;
  int tag;
  natural ref_dnode, node_dnode;
  Boolean intergen_ref;

  while (start < end) {
    x1 = *start;
    tag = fulltag_of(x1);
    if (immheader_tag_p(tag)) {
      start = skip_over_ivector(ptr_to_lispobj(start), x1);
    } else {
      intergen_ref = false;
      if ((tag == fulltag_misc) || (tag == fulltag_cons)) {        
        node_dnode = gc_area_dnode(x1);
        if (node_dnode < GCndnodes_in_area) {
          intergen_ref = true;
        }
      }
      if (intergen_ref == false) {        
        x1 = start[1];
        tag = fulltag_of(x1);
        if ((tag == fulltag_misc) || (tag == fulltag_cons)) {
          node_dnode = gc_area_dnode(x1);
          if (node_dnode < GCndnodes_in_area) {
            intergen_ref = true;
          }
        }
      }
      if (intergen_ref) {
        ref_dnode = area_dnode(start, base);
        if (!ref_bit(refbits, ref_dnode)) {
          Bug(NULL, "Missing memoization in doublenode at 0x%08X", start);
          set_bit(refbits, ref_dnode);
        }
      }
      start += 2;
    }
  }
}



void
mark_memoized_area(area *a, natural num_memo_dnodes)
{
  bitvector refbits = a->refbits;
  LispObj *p = (LispObj *) a->low, x1, x2;
  natural inbits, outbits, bits, bitidx, *bitsp, nextbit, diff, memo_dnode = 0;
  Boolean keep_x1, keep_x2;

  if (GCDebug) {
    check_refmap_consistency(p, p+(num_memo_dnodes << 1), refbits);
  }

  /* The distinction between "inbits" and "outbits" is supposed to help us
     detect cases where "uninteresting" setfs have been memoized.  Storing
     NIL, fixnums, immediates (characters, etc.) or node pointers to static
     or readonly areas is definitely uninteresting, but other cases are
     more complicated (and some of these cases are hard to detect.)

     Some headers are "interesting", to the forwarder if not to us. 

     We -don't- give anything any weak treatment here.  Weak things have
     to be seen by a full gc, for some value of 'full'.
     */

  /*
    We need to ensure that there are no bits set at or beyond
    "num_memo_dnodes" in the bitvector.  (This can happen as the EGC
    tenures/untenures things.)  We find bits by grabbing a fullword at
    a time and doing a cntlzw instruction; and don't want to have to
    check for (< memo_dnode num_memo_dnodes) in the loop.
    */

  {
    natural 
      bits_in_last_word = (num_memo_dnodes & bitmap_shift_count_mask),
      index_of_last_word = (num_memo_dnodes >> bitmap_shift);

    if (bits_in_last_word != 0) {
      natural mask = ~((1L<<(nbits_in_word-bits_in_last_word))-1L);
      refbits[index_of_last_word] &= mask;
    }
  }
        
  set_bitidx_vars(refbits, 0, bitsp, bits, bitidx);
  inbits = outbits = bits;
  while (memo_dnode < num_memo_dnodes) {
    if (bits == 0) {
      int remain = nbits_in_word - bitidx;
      memo_dnode += remain;
      p += (remain+remain);
      if (outbits != inbits) {
        *bitsp = outbits;
      }
      bits = *++bitsp;
      inbits = outbits = bits;
      bitidx = 0;
    } else {
      nextbit = count_leading_zeros(bits);
      if ((diff = (nextbit - bitidx)) != 0) {
        memo_dnode += diff;
        bitidx = nextbit;
        p += (diff+diff);
      }
      x1 = *p++;
      x2 = *p++;
      bits &= ~(BIT0_MASK >> bitidx);
      keep_x1 = mark_ephemeral_root(x1);
      keep_x2 = mark_ephemeral_root(x2);
      if ((keep_x1 == false) && 
          (keep_x2 == false)) {
        outbits &= ~(BIT0_MASK >> bitidx);
      }
      memo_dnode++;
      bitidx++;
    }
  }
  if (GCDebug) {
    p = (LispObj *) a->low;
    check_refmap_consistency(p, p+(num_memo_dnodes << 1), refbits);
  }
}



void
mark_simple_area_range(LispObj *start, LispObj *end)
{
  LispObj x1, *base;
  int tag;

  while (start < end) {
    x1 = *start;
    tag = fulltag_of(x1);
    if (immheader_tag_p(tag)) {
      start = (LispObj *)ptr_from_lispobj(skip_over_ivector(ptr_to_lispobj(start), x1));
    } else if (!nodeheader_tag_p(tag)) {
      ++start;
      mark_root(x1);
      mark_root(*start++);
    } else {
      int subtag = header_subtag(x1);
      natural element_count = header_element_count(x1);
      natural size = (element_count+1 + 1) & ~1;

      if (subtag == subtag_hash_vector) {
        LispObj flags = ((hash_table_vector_header *) start)->flags;

        if (flags & nhash_weak_mask) {
          ((hash_table_vector_header *) start)->cache_key = undefined;
          ((hash_table_vector_header *) start)->cache_value = lisp_nil;
	  mark_weak_htabv((LispObj)start);
	  element_count = 0;
	}
      }
      if (subtag == subtag_pool) {
	start[1] = lisp_nil;
      }

      if (subtag == subtag_weak) {
	natural weak_type = (natural) start[2];
	if (weak_type >> population_termination_bit)
	  element_count -= 2;
	else
	  element_count -= 1; 
	start[1] = GCweakvll;
	GCweakvll = (LispObj) (((natural) start) + fulltag_misc);    
      }

      base = start + element_count + 1;
      while(element_count--) {
	mark_root(*--base);
      }   
      start += size;
    }
  }
}


/* Mark a tstack area */
void
mark_tstack_area(area *a)
{
  LispObj
    *current,
    *next,
    *start = (LispObj *) (a->active),
    *end = start,
    *limit = (LispObj *) (a->high);

  for (current = start;
       end != limit;
       current = next) {
    next = (LispObj *) ptr_from_lispobj(*current);
    end = ((next >= start) && (next < limit)) ? next : limit;
    if (current[1] == 0) {
      mark_simple_area_range(current+2, end);
    }
  }
}

/*
  It's really important that headers never wind up in tagged registers.
  Those registers would (possibly) get pushed on the vstack and confuse
  the hell out of this routine.

  vstacks are just treated as a "simple area range", possibly with
  an extra word at the top (where the area's active pointer points.)
  */

void
mark_vstack_area(area *a)
{
  LispObj
    *start = (LispObj *) a->active,
    *end = (LispObj *) a->high;

#if 0
  fprintf(dbgout, "mark VSP range: 0x%lx:0x%lx\n", start, end);
#endif
  if (((natural)start) & (sizeof(natural))) {
    /* Odd number of words.  Mark the first (can't be a header) */
    mark_root(*start);
    ++start;
  }
  mark_simple_area_range(start, end);
}


/*
  Mark lisp frames on the control stack.
  Ignore emulator frames (odd backpointer) and C frames (size != 4).
*/

void
mark_cstack_area(area *a)
{
  BytePtr
    current,
    next,
    limit = a->high,
    low = a->low;

  for (current = a->active; (current >= low) && (current < limit); current = next) {
    next = *((BytePtr *)current);
#if 0
    if (next < current) {
      Bug(NULL, "Child stack frame older than parent");
    }
#endif
    if (next == NULL) break;
    if (((next - current) == sizeof(lisp_frame)) &&
	(((((lisp_frame *)current)->savefn) == 0) ||
	 (fulltag_of(((lisp_frame *)current)->savefn) == fulltag_misc))) {
      /* mark fn, then saved lr */
      mark_root(((lisp_frame *)current)->savefn);
      mark_pc_root(((lisp_frame *)current)->savelr);
    } else {
      /* Clear low 2 bits of "next", just in case */
      next = (BytePtr) (((natural)next) & ~3);
    }
  }
}



/* Mark the lisp objects in an exception frame */
void
mark_xp(ExceptionInformation *xp)
{
  natural *regs = (natural *) xpGPRvector(xp);

#ifdef PPC
  int r;
  /* registers >= fn should be tagged and marked as roots.
     the PC, LR, loc_pc, and CTR should be treated as "pc_locatives".

     In general, marking a locative is more expensive than marking
     a node is, since it may be neccessary to back up and find the
     containing object's header.  Since exception frames contain
     many locatives, it'd be wise to mark them *after* marking the
     stacks, nilreg-relative globals, etc.
     */

  for (r = fn; r < 32; r++) {
    mark_root((regs[r]));
  }



  mark_pc_root((regs[loc_pc]));
  mark_pc_root(ptr_to_lispobj(xpPC(xp)));
  mark_pc_root(ptr_to_lispobj(xpLR(xp)));
  mark_pc_root(ptr_to_lispobj(xpCTR(xp)));
#endif /* PPC */

}

/* A "pagelet" contains 32 doublewords.  The relocation table contains
   a word for each pagelet which defines the lowest address to which
   dnodes on that pagelet will be relocated.

   The relocation address of a given pagelet is the sum of the relocation
   address for the preceding pagelet and the number of bytes occupied by
   marked objects on the preceding pagelet.
*/

LispObj
calculate_relocation()
{
  LispObj *relocptr = GCrelocptr;
  LispObj current = GCareadynamiclow;
  bitvector 
    markbits = GCdynamic_markbits;
  qnode *q = (qnode *) markbits;
  natural npagelets = ((GCndynamic_dnodes_in_area+(nbits_in_word-1))>>bitmap_shift);
  natural thesebits;
  LispObj first = 0;

  do {
    *relocptr++ = current;
    thesebits = *markbits++;
    if (thesebits == ALL_ONES) {
      current += nbits_in_word*dnode_size;
      q += 4; /* sic */
    } else {
      if (!first) {
        first = current;
        while (thesebits & BIT0_MASK) {
          first += dnode_size;
          thesebits += thesebits;
        }
      }
      current += one_bits(*q++);
      current += one_bits(*q++);
      current += one_bits(*q++);
      current += one_bits(*q++);
    }
  } while(--npagelets);
  *relocptr++ = current;
  return first ? first : current;
}

#ifdef PPC64
LispObj
dnode_forwarding_address(natural dnode, int tag_n)
{
  natural pagelet, nbits;
  unsigned int near_bits;
  LispObj new;

  if (GCDebug) {
    if (! ref_bit(GCdynamic_markbits, dnode)) {
      Bug(NULL, "unmarked object being forwarded!\n");
    }
  }

  pagelet = dnode >> bitmap_shift;
  nbits = dnode & bitmap_shift_count_mask;
  near_bits = ((unsigned int *)GCdynamic_markbits)[dnode>>(dnode_shift+1)];

  if (nbits < 32) {
    new = GCrelocptr[pagelet] + tag_n;;
    /* Increment "new" by the count of 1 bits which precede the dnode */
    if (near_bits == 0xffffffff) {
      return (new + (nbits << 4));
    } else {
      near_bits &= (0xffffffff00000000 >> nbits);
      if (nbits > 15) {
        new += one_bits(near_bits & 0xffff);
      }
      return (new + (one_bits(near_bits >> 16))); 
    }
  } else {
    new = GCrelocptr[pagelet+1] + tag_n;
    nbits = 64-nbits;

    if (near_bits == 0xffffffff) {
      return (new - (nbits << 4));
    } else {
      near_bits &= (1<<nbits)-1;
      if (nbits > 15) {
        new -= one_bits(near_bits >> 16);
      }
      return (new -  one_bits(near_bits & 0xffff));
    }
  }
}
#else
LispObj
dnode_forwarding_address(natural dnode, int tag_n)
{
  natural pagelet, nbits;
  unsigned short near_bits;
  LispObj new;

  if (GCDebug) {
    if (! ref_bit(GCdynamic_markbits, dnode)) {
      Bug(NULL, "unmarked object being forwarded!\n");
    }
  }

  pagelet = dnode >> 5;
  nbits = dnode & 0x1f;
  near_bits = ((unsigned short *)GCdynamic_markbits)[dnode>>4];

  if (nbits < 16) {
    new = GCrelocptr[pagelet] + tag_n;;
    /* Increment "new" by the count of 1 bits which precede the dnode */
    if (near_bits == 0xffff) {
      return (new + (nbits << 3));
    } else {
      near_bits &= (0xffff0000 >> nbits);
      if (nbits > 7) {
        new += one_bits(near_bits & 0xff);
      }
      return (new + (one_bits(near_bits >> 8))); 
    }
  } else {
    new = GCrelocptr[pagelet+1] + tag_n;
    nbits = 32-nbits;

    if (near_bits == 0xffff) {
      return (new - (nbits << 3));
    } else {
      near_bits &= (1<<nbits)-1;
      if (nbits > 7) {
        new -= one_bits(near_bits >> 8);
      }
      return (new -  one_bits(near_bits & 0xff));
    }
  }
}
#endif


LispObj
locative_forwarding_address(LispObj obj)
{
  int tag_n = fulltag_of(obj);
  natural dnode;


#ifdef PPC
  /* Locatives can be tagged as conses, "fulltag_misc"
     objects, or as fixnums.  Immediates, headers, and nil
     shouldn't be "forwarded".  Nil never will be, but it
     doesn't hurt to check ... */
#ifdef PPC64
  if ((tag_n & lowtag_mask) != lowtag_primary) {
    return obj;
  }
#else
  if ((1<<tag_n) & ((1<<fulltag_immheader) |
                    (1<<fulltag_nodeheader) |
                    (1<<fulltag_imm) |
                    (1<<fulltag_nil))) {
    return obj;
  }
#endif
#endif

  dnode = gc_dynamic_area_dnode(obj);

  if ((dnode >= GCndynamic_dnodes_in_area) ||
      (obj < GCfirstunmarked)) {
    return obj;
  }

  return dnode_forwarding_address(dnode, tag_n);
}




void
forward_range(LispObj *range_start, LispObj *range_end)
{
  LispObj *p = range_start, node, new;
  int tag_n;
  natural nwords;
  hash_table_vector_header *hashp;

  while (p < range_end) {
    node = *p;
    tag_n = fulltag_of(node);
    if (immheader_tag_p(tag_n)) {
      p = (LispObj *) skip_over_ivector((natural) p, node);
    } else if (nodeheader_tag_p(tag_n)) {
      nwords = header_element_count(node);
      nwords += (1 - (nwords&1));
      if ((header_subtag(node) == subtag_hash_vector) &&
          ((((hash_table_vector_header *)p)->flags) & nhash_track_keys_mask)) {
        natural skip = (sizeof(hash_table_vector_header)/sizeof(LispObj))-1;
        hashp = (hash_table_vector_header *) p;
        p++;
        nwords -= skip;
        while(skip--) {
          update_noderef(p);
          p++;
        }
        /* "nwords" is odd at this point: there are (floor nwords 2)
           key/value pairs to look at, and then an extra word for
           alignment.  Process them two at a time, then bump "p"
           past the alignment word. */
        nwords >>= 1;
        while(nwords--) {
          if (update_noderef(p) && hashp) {
            hashp->flags |= nhash_key_moved_mask;
            hashp = NULL;
          }
          p++;
          update_noderef(p);
          p++;
        }
        *p++ = 0;
      } else {
        p++;
        while(nwords--) {
          update_noderef(p);
          p++;
        }
      }
    } else {
      new = node_forwarding_address(node);
      if (new != node) {
        *p = new;
      }
      p++;
      update_noderef(p);
      p++;
    }
  }
}




/* Forward a tstack area */
void
forward_tstack_area(area *a)
{
  LispObj
    *current,
    *next,
    *start = (LispObj *) a->active,
    *end = start,
    *limit = (LispObj *) (a->high);

  for (current = start;
       end != limit;
       current = next) {
    next = ptr_from_lispobj(*current);
    end = ((next >= start) && (next < limit)) ? next : limit;
    if (current[1] == 0) {
      forward_range(current+2, end);
    }
  }
}

/* Forward a vstack area */
void
forward_vstack_area(area *a)
{
  LispObj
    *p = (LispObj *) a->active,
    *q = (LispObj *) a->high;

#ifdef DEBUG
  fprintf(dbgout,"Forward range 0x%x/0x%x (owner 0x%x)\n",p,q,a->owner);
#endif
  if (((natural)p) & sizeof(natural)) {
    update_noderef(p);
    p++;
  }
  forward_range(p, q);
}

void
forward_cstack_area(area *a)
{
  BytePtr
    current,
    next,
    limit = a->high,
    low = a->low;

  for (current = a->active; (current >= low) && (current < limit); current = next) {
    next = *((BytePtr *)current);
    if (next == NULL) break;
    if (((next - current) == sizeof(lisp_frame)) &&
	(((((lisp_frame *)current)->savefn) == 0) ||
	 (fulltag_of(((lisp_frame *)current)->savefn) == fulltag_misc))) {
      update_noderef(&((lisp_frame *) current)->savefn);
      update_locref(&((lisp_frame *) current)->savelr);
    }
  }
}



void
forward_xp(ExceptionInformation *xp)
{
  natural *regs = (natural *) xpGPRvector(xp);

  int r;

  /* registers >= fn should be tagged and forwarded as roots.
     the PC, LR, loc_pc, and CTR should be treated as "locatives".
     */

  for (r = fn; r < 32; r++) {
    update_noderef((LispObj*) (&(regs[r])));
  }

  update_locref((LispObj*) (&(regs[loc_pc])));

  update_locref((LispObj*) (&(xpPC(xp))));
  update_locref((LispObj*) (&(xpLR(xp))));
  update_locref((LispObj*) (&(xpCTR(xp))));

}


void
forward_tcr_xframes(TCR *tcr)
{
  xframe_list *xframes;
  ExceptionInformation *xp;

  xp = tcr->gc_context;
  if (xp) {
    forward_xp(xp);
  }
  for (xframes = tcr->xframe; xframes; xframes = xframes->prev) {
    if (xframes->curr == xp) {
      Bug(NULL, "forward xframe twice ???");
    }
    forward_xp(xframes->curr);
  }
}



/*
  Compact the dynamic heap (from GCfirstunmarked through its end.)
  Return the doublenode address of the new freeptr.
  */

LispObj
compact_dynamic_heap()
{
  LispObj *src = ptr_from_lispobj(GCfirstunmarked), *dest = src, node, new;
  natural 
    elements, 
    dnode = gc_area_dnode(GCfirstunmarked), 
    node_dnodes = 0, 
    imm_dnodes = 0, 
    bitidx, 
    *bitsp, 
    bits, 
    nextbit, 
    diff;
  int tag;
  bitvector markbits = GCmarkbits;
    /* keep track of whether or not we saw any
       code_vector headers, and only flush cache if so. */
  Boolean GCrelocated_code_vector = false;

  if (dnode < GCndnodes_in_area) {
    lisp_global(FWDNUM) += (1<<fixnum_shift);
  
    set_bitidx_vars(markbits,dnode,bitsp,bits,bitidx);
    while (dnode < GCndnodes_in_area) {
      if (bits == 0) {
        int remain = nbits_in_word - bitidx;
        dnode += remain;
        src += (remain+remain);
        bits = *++bitsp;
        bitidx = 0;
      } else {
        /* Have a non-zero markbits word; all bits more significant
           than "bitidx" are 0.  Count leading zeros in "bits"
           (there'll be at least "bitidx" of them.)  If there are more
           than "bitidx" leading zeros, bump "dnode", "bitidx", and
           "src" by the difference. */
        nextbit = count_leading_zeros(bits);
        if ((diff = (nextbit - bitidx)) != 0) {
          dnode += diff;
          bitidx = nextbit;
          src += (diff+diff);
        }

        if (GCDebug) {
          if (dest != ptr_from_lispobj(locative_forwarding_address(ptr_to_lispobj(src)))) {
            Bug(NULL, "Out of synch in heap compaction.  Forwarding from 0x%lx to 0x%lx,\n expected to go to 0x%lx\n", 
                src, dest, locative_forwarding_address(ptr_to_lispobj(src)));
          }
        }

        node = *src++;
        tag = fulltag_of(node);
        if (nodeheader_tag_p(tag)) {
          elements = header_element_count(node);
          node_dnodes = (elements+2)>>1;
          dnode += node_dnodes;
          if ((header_subtag(node) == subtag_hash_vector) &&
              (((hash_table_vector_header *) (src-1))->flags & nhash_track_keys_mask)) {
            hash_table_vector_header *hashp = (hash_table_vector_header *) dest;
            int skip = (sizeof(hash_table_vector_header)/sizeof(LispObj))-1;
          
            *dest++ = node;
            elements -= skip;
            while(skip--) {
              *dest++ = node_forwarding_address(*src++);
            }
            /* There should be an even number of (key/value) pairs in elements;
               an extra alignment word follows. */
            elements >>= 1;
            while (elements--) {
              if (hashp) {
                node = *src++;
                new = node_forwarding_address(node);
                if (new != node) {
                  hashp->flags |= nhash_key_moved_mask;
                  hashp = NULL;
                  *dest++ = new;
                } else {
                  *dest++ = node;
                }
              } else {
                *dest++ = node_forwarding_address(*src++);
              }
              *dest++ = node_forwarding_address(*src++);
            }
            *dest++ = 0;
            src++;
          } else {
            *dest++ = node;
            *dest++ = node_forwarding_address(*src++);
            while(--node_dnodes) {
              *dest++ = node_forwarding_address(*src++);
              *dest++ = node_forwarding_address(*src++);
            }
          }
          set_bitidx_vars(markbits,dnode,bitsp,bits,bitidx);
        } else if (immheader_tag_p(tag)) {
          *dest++ = node;
          *dest++ = *src++;
          elements = header_element_count(node);
          tag = header_subtag(node);

#ifdef PPC
#ifdef PPC64
          switch(fulltag_of(tag)) {
          case ivector_class_64_bit:
            imm_dnodes = ((elements+1)+1)>>1;
            break;
          case ivector_class_32_bit:
            if (tag == subtag_code_vector) {
              GCrelocated_code_vector = true;
            }
            imm_dnodes = (((elements+2)+3)>>2);
            break;
          case ivector_class_8_bit:
            imm_dnodes = (((elements+8)+15)>>4);
            break;
          case ivector_class_other_bit:
            if (tag == subtag_bit_vector) {
              imm_dnodes = (((elements+64)+127)>>7);
            } else {
              imm_dnodes = (((elements+4)+7)>>3);
            }
          }
#else
          if (tag <= max_32_bit_ivector_subtag) {
            if (tag == subtag_code_vector) {
              GCrelocated_code_vector = true;
            }
            imm_dnodes = (((elements+1)+1)>>1);
          } else if (tag <= max_8_bit_ivector_subtag) {
            imm_dnodes = (((elements+4)+7)>>3);
          } else if (tag <= max_16_bit_ivector_subtag) {
            imm_dnodes = (((elements+2)+3)>>2);
          } else if (tag == subtag_bit_vector) {
            imm_dnodes = (((elements+32)+63)>>6);
          } else {
            imm_dnodes = elements+1;
          }
#endif
#endif

          dnode += imm_dnodes;
          while (--imm_dnodes) {
            *dest++ = *src++;
            *dest++ = *src++;
          }
          set_bitidx_vars(markbits,dnode,bitsp,bits,bitidx);
        } else {
          *dest++ = node_forwarding_address(node);
          *dest++ = node_forwarding_address(*src++);
          bits &= ~(BIT0_MASK >> bitidx);
          dnode++;
          bitidx++;
        }
      }
  
    }

    {
      natural nbytes = (natural)ptr_to_lispobj(dest) - (natural)GCfirstunmarked;
      if ((nbytes != 0) && GCrelocated_code_vector) {
        xMakeDataExecutable((LogicalAddress)ptr_from_lispobj(GCfirstunmarked), nbytes);
      }
    }
  }
  return ptr_to_lispobj(dest);
}




      
    
/*
  Total the (physical) byte sizes of all ivectors in the indicated memory range
*/

natural
unboxed_bytes_in_range(LispObj *start, LispObj *end)
{
    natural total=0, elements, tag, subtag, bytes;
    LispObj header;

    while (start < end) {
      header = *start;
      tag = fulltag_of(header);
    
      if ((nodeheader_tag_p(tag)) ||
          (immheader_tag_p(tag))) {
        elements = header_element_count(header);
        if (nodeheader_tag_p(tag)) {
          start += ((elements+2) & ~1);
        } else {
          subtag = header_subtag(header);

#ifdef PPC64
          switch(fulltag_of(header)) {
          case ivector_class_64_bit:
            bytes = 8 + (elements<<3);
            break;
          case ivector_class_32_bit:
            bytes = 8 + (elements<<2);
            break;
          case ivector_class_8_bit:
            bytes = 8 + elements;
            break;
          case ivector_class_other_bit:
          default:
            if (subtag == subtag_bit_vector) {
              bytes = 8 + ((elements+7)>>3);
            } else {
              bytes = 8 + (elements<<1);
            }
          }
#else
          if (subtag <= max_32_bit_ivector_subtag) {
            bytes = 4 + (elements<<2);
          } else if (subtag <= max_8_bit_ivector_subtag) {
            bytes = 4 + elements;
          } else if (subtag <= max_16_bit_ivector_subtag) {
            bytes = 4 + (elements<<1);
          } else if (subtag == subtag_double_float_vector) {
            bytes = 8 + (elements<<3);
          } else {
            bytes = 4 + ((elements+7)>>3);
          }
#endif


          bytes = (bytes+dnode_size-1) & ~(dnode_size-1);
          total += bytes;
          start += (bytes >> node_shift);
        }
      } else {
        start += 2;
      }
    }
    return total;
  }


  /* 
     This assumes that it's getting called with an ivector
     argument and that there's room for the object in the
     destination area.
  */


LispObj
purify_displaced_object(LispObj obj, area *dest, natural disp)
{
  BytePtr 
    free = dest->active,
    *old = (BytePtr *) ptr_from_lispobj(untag(obj));
  LispObj 
    header = header_of(obj), 
    new;
  natural 
    start = (natural)old,
    physbytes;

  physbytes = ((natural)(skip_over_ivector(start,header))) - start;
  dest->active += physbytes;

  new = ptr_to_lispobj(free)+disp;

  memcpy(free, (BytePtr)old, physbytes);
  /* Leave a trail of breadcrumbs.  Or maybe just one breadcrumb. */
  /* Actually, it's best to always leave a trail, for two reasons.
     a) We may be walking the same heap that we're leaving forwaring
     pointers in, so we don't want garbage that we leave behind to
     look like a header.
     b) We'd like to be able to forward code-vector locatives, and
     it's easiest to do so if we leave a {forward_marker, dnode_locative}
     pair at every doubleword in the old vector.
  */
  while(physbytes) {
    *old++ = (BytePtr) forward_marker;
    *old++ = (BytePtr) free;
    free += dnode_size;
    physbytes -= dnode_size;
  }
  return new;
}

LispObj
purify_object(LispObj obj, area *dest)
{
  return purify_displaced_object(obj, dest, fulltag_of(obj));
}



void
copy_ivector_reference(LispObj *ref, BytePtr low, BytePtr high, area *dest)
{
  LispObj obj = *ref, header;
  natural tag = fulltag_of(obj), header_tag;

  if ((tag == fulltag_misc) &&
      (((BytePtr)ptr_from_lispobj(obj)) > low) &&
      (((BytePtr)ptr_from_lispobj(obj)) < high)) {
    header = deref(obj, 0);
    if (header == forward_marker) { /* already copied */
      *ref = (untag(deref(obj,1)) + tag);
    } else {
      header_tag = fulltag_of(header);
      if (immheader_tag_p(header_tag)) {
        if (header_subtag(header) != subtag_macptr) {
          *ref = purify_object(obj, dest);
        }
      }
    }
  }
}

void
purify_locref(LispObj *locaddr, BytePtr low, BytePtr high, area *to)
{
#ifdef PPC
  LispObj
    loc = *locaddr,
    *headerP;
  opcode
    *p,
    insn;
  natural
    tag = fulltag_of(loc);

  if (((BytePtr)ptr_from_lispobj(loc) > low) &&
      ((BytePtr)ptr_from_lispobj(loc) < high)) {

    headerP = (LispObj *)ptr_from_lispobj(untag(loc));
    switch (tag) {
    case fulltag_even_fixnum:
    case fulltag_odd_fixnum:
#ifdef PPC64
    case fulltag_cons:
    case fulltag_misc:
#endif
      if (*headerP == forward_marker) {
	*locaddr = (headerP[1]+tag);
      } else {
	/* Grovel backwards until the header's found; copy
	   the code vector to to space, then treat it as if it 
	   hasn't already been copied. */
	p = (opcode *)headerP;
	do {
	  p -= 2;
	  tag += 8;
	  insn = *p;
#ifdef PPC64
	} while (insn != PPC64_CODE_VECTOR_PREFIX);
	headerP = ((LispObj*)p)-1;
	*locaddr = purify_displaced_object(((LispObj)headerP), to, tag);
#else
      } while ((insn & code_header_mask) != subtag_code_vector);
      *locaddr = purify_displaced_object(ptr_to_lispobj(p), to, tag);
#endif
    }
    break;

#ifndef PPC64
  case fulltag_misc:
    copy_ivector_reference(locaddr, low, high, to);
    break;
#endif
  }
}
#endif
}

void
purify_range(LispObj *start, LispObj *end, BytePtr low, BytePtr high, area *to)
{
  LispObj header;
  unsigned tag;

  while (start < end) {
    header = *start;
    if (header == forward_marker) {
      start += 2;
    } else {
      tag = fulltag_of(header);
      if (immheader_tag_p(tag)) {
        start = (LispObj *)skip_over_ivector((natural)start, header);
      } else {
        if (!nodeheader_tag_p(tag)) {
          copy_ivector_reference(start, low, high, to);
        }
        start++;
        copy_ivector_reference(start, low, high, to);
        start++;
      }
    }
  }
}
        
/* Purify references from tstack areas */
void
purify_tstack_area(area *a, BytePtr low, BytePtr high, area *to)
{
  LispObj
    *current,
    *next,
    *start = (LispObj *) (a->active),
    *end = start,
    *limit = (LispObj *) (a->high);

  for (current = start;
       end != limit;
       current = next) {
    next = (LispObj *) ptr_from_lispobj(*current);
    end = ((next >= start) && (next < limit)) ? next : limit;
    if (current[1] == 0) {
      purify_range(current+2, end, low, high, to);
    }
  }
}

/* Purify a vstack area */
void
purify_vstack_area(area *a, BytePtr low, BytePtr high, area *to)
{
  LispObj
    *p = (LispObj *) a->active,
    *q = (LispObj *) a->high;

  if (((natural)p) & sizeof(natural)) {
    copy_ivector_reference(p, low, high, to);
    p++;
  }
  purify_range(p, q, low, high, to);
}


void
purify_cstack_area(area *a, BytePtr low, BytePtr high, area *to)
{
  BytePtr
    current,
    next,
    limit = a->high;

  for (current = a->active; current != limit; current = next) {
    next = *((BytePtr *)current);
    if (next == NULL) break;
    if (((next - current) == sizeof(lisp_frame)) && 
	(((((lisp_frame *)current)->savefn) == 0) ||
	 (fulltag_of(((lisp_frame *)current)->savefn) == fulltag_misc))) {
      purify_locref(&((lisp_frame *) current)->savelr, low, high, to);
    } else {
      /* Clear low bits of "next", just in case */
      next = (BytePtr) (((natural)next) & ~(sizeof(natural)-1));
    }
  }
}

void
purify_xp(ExceptionInformation *xp, BytePtr low, BytePtr high, area *to)
{
  unsigned long *regs = (unsigned long *) xpGPRvector(xp);

  int r;

  /* registers >= fn should be treated as roots.
     The PC, LR, loc_pc, and CTR should be treated as "locatives".
   */

  for (r = fn; r < 32; r++) {
    copy_ivector_reference((LispObj*) (&(regs[r])), low, high, to);
  };

  purify_locref((LispObj*) (&(regs[loc_pc])), low, high, to);

  purify_locref((LispObj*) (&(xpPC(xp))), low, high, to);
  purify_locref((LispObj*) (&(xpLR(xp))), low, high, to);
  purify_locref((LispObj*) (&(xpCTR(xp))), low, high, to);
}

void
purify_tcr_tlb(TCR *tcr, BytePtr low, BytePtr high, area *to)
{
  natural n = tcr->tlb_limit;
  LispObj *start = tcr->tlb_pointer, *end = (LispObj *) ((BytePtr)start+n);

  purify_range(start, end, low, high, to);
}

void
purify_tcr_xframes(TCR *tcr, BytePtr low, BytePtr high, area *to)
{
  xframe_list *xframes;
  ExceptionInformation *xp;
  
  xp = tcr->gc_context;
  if (xp) {
    purify_xp(xp, low, high, to);
  }

  for (xframes = tcr->xframe; xframes; xframes = xframes->prev) {
    purify_xp(xframes->curr, low, high, to);
  }
}

void
purify_gcable_ptrs(BytePtr low, BytePtr high, area *to)
{
  LispObj *prev = &(lisp_global(GCABLE_POINTERS)), next;

  while ((*prev) != (LispObj)NULL) {
    copy_ivector_reference(prev, low, high, to);
    next = *prev;
    prev = &(((xmacptr *)ptr_from_lispobj(untag(next)))->link);
  }
}


void
purify_areas(BytePtr low, BytePtr high, area *target)
{
  area *next_area;
  area_code code;
      
  for (next_area = active_dynamic_area; (code = next_area->code) != AREA_VOID; next_area = next_area->succ) {
    switch (code) {
    case AREA_TSTACK:
      purify_tstack_area(next_area, low, high, target);
      break;
      
    case AREA_VSTACK:
      purify_vstack_area(next_area, low, high, target);
      break;
      
    case AREA_CSTACK:
      purify_cstack_area(next_area, low, high, target);
      break;
      
    case AREA_STATIC:
    case AREA_DYNAMIC:
      purify_range((LispObj *) next_area->low, (LispObj *) next_area->active, low, high, target);
      break;
      
    default:
      break;
    }
  }
}

/*
  So far, this is mostly for save_application's benefit.
  We -should- be able to return to lisp code after doing this,
  however.

*/


signed_natural
purify(TCR *tcr, signed_natural param)
{
  extern area *extend_readonly_area(unsigned);
  area 
    *a = active_dynamic_area,
    *new_pure_area;

  TCR  *other_tcr;
  natural max_pure_size;
  BytePtr new_pure_start;


  max_pure_size = unboxed_bytes_in_range((LispObj *)(a->low + (static_dnodes_for_area(a) << dnode_shift)), 
                                         (LispObj *) a->active);
  new_pure_area = extend_readonly_area(max_pure_size);
  if (new_pure_area) {
    new_pure_start = new_pure_area->active;
    lisp_global(IN_GC) = (1<<fixnumshift);

    
    purify_areas(a->low, a->active, new_pure_area);
    
    other_tcr = tcr;
    do {
      purify_tcr_xframes(other_tcr, a->low, a->active, new_pure_area);
      purify_tcr_tlb(other_tcr, a->low, a->active, new_pure_area);
      other_tcr = other_tcr->next;
    } while (other_tcr != tcr);

    purify_gcable_ptrs(a->low, a->active, new_pure_area);

    {
      natural puresize = (unsigned) (new_pure_area->active-new_pure_start);
      if (puresize != 0) {
        xMakeDataExecutable(new_pure_start, puresize);
  
      }
    }
    ProtectMemory(new_pure_area->low,
		  align_to_power_of_2(new_pure_area->active-new_pure_area->low,
				      log2_page_size));
    lisp_global(IN_GC) = 0;
    just_purified_p = true;
    return 0;
  }
  return -1;
}

void
impurify_locref(LispObj *p, LispObj low, LispObj high, int delta)
{
  LispObj q = *p;
  
  switch (fulltag_of(q)) {
#ifdef PPC64
  case fulltag_cons:
#endif
  case fulltag_misc:
  case fulltag_even_fixnum:
  case fulltag_odd_fixnum:
    if ((q >= low) && (q < high)) {
      *p = (q+delta);
    }
  }
}

  
void
impurify_noderef(LispObj *p, LispObj low, LispObj high, int delta)
{
  LispObj q = *p;
  
  if ((fulltag_of(q) == fulltag_misc) &&
      (q >= low) && 
      (q < high)) {
    *p = (q+delta);
  }
}
  

#ifdef PPC
void
impurify_cstack_area(area *a, LispObj low, LispObj high, int delta)
{
  BytePtr
    current,
    next,
    limit = a->high;

  for (current = a->active; current != limit; current = next) {
    next = *((BytePtr *)current);
    if (next == NULL) break;
    if (((next - current) == sizeof(lisp_frame)) && 
	(((((lisp_frame *)current)->savefn) == 0) ||
	 (fulltag_of(((lisp_frame *)current)->savefn) == fulltag_misc))) {
      impurify_locref(&((lisp_frame *) current)->savelr, low, high, delta);
    } else {
      /* Clear low bits of "next", just in case */
      next = (BytePtr) (((natural)next) & ~(sizeof(natural)-1));
    }
  }
}
#endif

void
impurify_xp(ExceptionInformation *xp, LispObj low, LispObj high, int delta)
{
  natural *regs = (natural *) xpGPRvector(xp);

#ifdef PPC
  int r;
  /* registers >= fn should be treated as roots.
     The PC, LR, loc_pc, and CTR should be treated as "locatives".
   */

  for (r = fn; r < 32; r++) {
    impurify_noderef((LispObj*) (&(regs[r])), low, high, delta);
  };

  impurify_locref((LispObj*) (&(regs[loc_pc])), low, high, delta);

  impurify_locref((LispObj*) (&(xpPC(xp))), low, high, delta);
  impurify_locref((LispObj*) (&(xpLR(xp))), low, high, delta);
  impurify_locref((LispObj*) (&(xpCTR(xp))), low, high, delta);
#endif

}


void
impurify_range(LispObj *start, LispObj *end, LispObj low, LispObj high, int delta)
{
  LispObj header;
  unsigned tag;

  while (start < end) {
    header = *start;
    tag = fulltag_of(header);
    if (immheader_tag_p(tag)) {
      start = (LispObj *)skip_over_ivector((natural)start, header);
    } else {
      if (!nodeheader_tag_p(tag)) {
        impurify_noderef(start, low, high, delta);
        }
      start++;
      impurify_noderef(start, low, high, delta);
      start++;
    }
  }
}




void
impurify_tcr_tlb(TCR *tcr,  LispObj low, LispObj high, int delta)
{
  unsigned n = tcr->tlb_limit;
  LispObj *start = tcr->tlb_pointer, *end = (LispObj *) ((BytePtr)start+n);
  
  impurify_range(start, end, low, high, delta);
}

void
impurify_tcr_xframes(TCR *tcr, LispObj low, LispObj high, int delta)
{
  xframe_list *xframes;
  ExceptionInformation *xp;
  
  xp = tcr->gc_context;
  if (xp) {
    impurify_xp(xp, low, high, delta);
  }

  for (xframes = tcr->xframe; xframes; xframes = xframes->prev) {
    impurify_xp(xframes->curr, low, high, delta);
  }
}

void
impurify_tstack_area(area *a, LispObj low, LispObj high, int delta)
{
  LispObj
    *current,
    *next,
    *start = (LispObj *) (a->active),
    *end = start,
    *limit = (LispObj *) (a->high);

  for (current = start;
       end != limit;
       current = next) {
    next = (LispObj *) ptr_from_lispobj(*current);
    end = ((next >= start) && (next < limit)) ? next : limit;
    if (current[1] == 0) {
      impurify_range(current+2, end, low, high, delta);
    }
  }
}
void
impurify_vstack_area(area *a, LispObj low, LispObj high, int delta)
{
  LispObj
    *p = (LispObj *) a->active,
    *q = (LispObj *) a->high;

  if (((natural)p) & sizeof(natural)) {
    impurify_noderef(p, low, high, delta);
    p++;
  }
  impurify_range(p, q, low, high, delta);
}


void
impurify_areas(LispObj low, LispObj high, int delta)
{
  area *next_area;
  area_code code;
      
  for (next_area = active_dynamic_area; (code = next_area->code) != AREA_VOID; next_area = next_area->succ) {
    switch (code) {
    case AREA_TSTACK:
      impurify_tstack_area(next_area, low, high, delta);
      break;
      
    case AREA_VSTACK:
      impurify_vstack_area(next_area, low, high, delta);
      break;
      
    case AREA_CSTACK:
#ifdef PPC
      impurify_cstack_area(next_area, low, high, delta);
#endif
      break;
      
    case AREA_STATIC:
    case AREA_DYNAMIC:
      impurify_range((LispObj *) next_area->low, (LispObj *) next_area->active, low, high, delta);
      break;
      
    default:
      break;
    }
  }
}

void
impurify_gcable_ptrs(LispObj low, LispObj high, signed_natural delta)
{
  LispObj *prev = &(lisp_global(GCABLE_POINTERS)), next;

  while ((*prev) != (LispObj)NULL) {
    impurify_noderef(prev, low, high, delta);
    next = *prev;
    prev = &(((xmacptr *)ptr_from_lispobj(untag(next)))->link);
  }
}

signed_natural
impurify(TCR *tcr, signed_natural param)
{
  area *r = readonly_area;

  if (r) {
    area *a = active_dynamic_area;
    BytePtr ro_base = r->low, ro_limit = r->active, oldfree = a->active,
      oldhigh = a->high, newhigh; 
    unsigned n = ro_limit - ro_base;
    int delta = oldfree-ro_base;
    TCR *other_tcr;

    if (n) {
      lisp_global(IN_GC) = 1;
      newhigh = (BytePtr) (align_to_power_of_2(oldfree+n,
                                               log2_heap_segment_size));
      if (newhigh > oldhigh) {
        grow_dynamic_area(newhigh-oldhigh);
      }
      a->active += n;
      memmove(oldfree, ro_base, n);
      munmap(ro_base, n);
      a->ndnodes = area_dnode(a, a->active);
      pure_space_active = r->active = r->low;
      r->ndnodes = 0;

      impurify_areas(ptr_to_lispobj(ro_base), ptr_to_lispobj(ro_limit), delta);

      other_tcr = tcr;
      do {
        impurify_tcr_xframes(other_tcr, ptr_to_lispobj(ro_base), ptr_to_lispobj(ro_limit), delta);
        impurify_tcr_tlb(other_tcr, ptr_to_lispobj(ro_base), ptr_to_lispobj(ro_limit), delta);
        other_tcr = other_tcr->next;
      } while (other_tcr != tcr);

      impurify_gcable_ptrs(ptr_to_lispobj(ro_base), ptr_to_lispobj(ro_limit), delta);
      lisp_global(IN_GC) = 0;
    }
    return 0;
  }
  return -1;
}

