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
#include "threads.h"
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


  case fulltag_imm:


    return;

  case fulltag_nil:
    if (n != lisp_nil) {
      Bug(NULL,"Object tagged as nil, not nil : 0x%08x", n);
    }
    return;


  case fulltag_nodeheader:
  case fulltag_immheader:


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
  int tag,subtag;
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
      subtag = header_subtag(node);
      if ((subtag == subtag_function) ||
          (subtag == subtag_pseudofunction)) {
        if (fulltag_of(current[0]) == fulltag_odd_fixnum) {
          if (untag(current[0]) != untag(current[1])) {
            Bug(NULL, "In function at 0x%lx, entrypoint (0x%lx) and codevector (0x%lx) don't match\n", (LispObj)prev,current[0],current[1]);
          }
        }
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
check_xp(ExceptionInformation *xp)
{
  natural *regs = (natural *) xpGPRvector(xp);
  LispObj lr_value;
  int r;

  for (r = arg_z; r <= Rfn; r++) {
    check_node((regs[r]));
  }
}



void
check_tcrs(TCR *first)
{
  xframe_list *xframes;
  ExceptionInformation *xp;
  
  TCR *tcr = first;
  LispObj *tlb_start,*tlb_end;

  do {
    xp = tcr->gc_context;
    if (xp) {
      check_xp(xp);
    }
    for (xframes = (xframe_list *) tcr->xframe; 
         xframes; 
         xframes = xframes->prev) {
      check_xp(xframes->curr);
    }
    tlb_start = tcr->tlb_pointer;
    if (tlb_start) {
      tlb_end = tlb_start + ((tcr->tlb_limit)>>fixnumshift);
      check_range(tlb_start,tlb_end,false);
    }
    tcr = tcr->next;
  } while (tcr != first);
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

    case AREA_CSTACK:
      check_range((LispObj *)a->active, (LispObj *)a->high, true);
      break;

    }
    a = a->succ;
    code = (a->code);
  }
  check_tcrs(tcr);
}







LispObj
code_vector_from_pc(LispObj *pcloc)
{
  /* Scan forward from pcloc until we find a 0 word (marking the start
     of a constant pool or the empty constant pool at the end of the
     code vector.  The word following the 0 contains the offset of that
     word from the code-vector header. */
  while (*pcloc) {
    pcloc++;
  }
  pcloc -= *pcloc;
  return (LispObj)pcloc+fulltag_misc;
}


/* Sooner or later, this probably wants to be in assembler */
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
        deref(n, 1) = lisp_nil;
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
        deref(n, 1) = GCweakvll;
        GCweakvll = untag(n);
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
  

/*
  Some objects (saved LRs on the control stack, the LR,
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
      mark_root(code_vector_from_pc((LispObj *)pc));
    }
  }
}


#define RMARK_PREV_ROOT fulltag_imm
#define RMARK_PREV_CAR fulltag_nil





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
        GCweakvll = untag(n);
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
    this = untag(this)+fulltag_nil;
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
    this = untag(this)+fulltag_cons;
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
      GCweakvll = untag(this);
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
}


void
check_refmap_consistency(LispObj *start, LispObj *end, bitvector refbits, bitvector refidx)
{
  LispObj x1, *base = start, *prev = start;
  int tag;
  natural ref_dnode, node_dnode;
  Boolean intergen_ref, lenient_next_dnode = false, lenient_this_dnode = false;

  while (start < end) {
    x1 = *start;
    tag = fulltag_of(x1);
    if (immheader_tag_p(tag)) {
      prev = start;
      start = skip_over_ivector(ptr_to_lispobj(start), x1);
    } else {
      if (nodeheader_tag_p(tag)) {
        prev = start;
      }
      intergen_ref = false;
      if (header_subtag(x1) == subtag_weak) {        
        lenient_next_dnode = true;
      }
      if (is_node_fulltag(tag)) {        
        node_dnode = gc_area_dnode(x1);
        if (node_dnode < GCndnodes_in_area) {
          intergen_ref = true;
        }
      }
      if (lenient_this_dnode) {
        lenient_this_dnode = false;
      } else {
        if (intergen_ref == false) {        
          x1 = start[1];
          tag = fulltag_of(x1);
          if (is_node_fulltag(tag)) {
            node_dnode = gc_area_dnode(x1);
            if (node_dnode < GCndnodes_in_area) {
              intergen_ref = true;
            }
          }
        }
      }
      if (intergen_ref) {
        ref_dnode = area_dnode(start, base);
        if (!ref_bit(refbits, ref_dnode)) {
          Bug(NULL, "Missing memoization in doublenode at 0x" LISP "\n", start);
          set_bit(refbits, ref_dnode);
          set_bit(refidx,ref_dnode>>8);
        } else {
          if (!ref_bit(refidx, ref_dnode>>8)) {
            Bug(NULL, "Memoization for doublenode at 0x" LISP " not indexed\n", start);
            set_bit(refidx,ref_dnode>>8);
          }
        }
      }
      start += 2;
      if (lenient_next_dnode) {
        lenient_this_dnode = true;
      }
      lenient_next_dnode = false;
    }
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
	GCweakvll = ptr_to_lispobj(start);
      }

      base = start + element_count + 1;
      while(element_count--) {
	mark_root(*--base);
      }   
      start += size;
    }
  }
}


void
mark_tstack_area(area *a)
{
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
  LispObj *current = (LispObj *)(a->active)
    , *limit = (LispObj*)(a->high), header;
  lisp_frame *frame;

  while(current < limit) {
    header = *current;

    if (header == lisp_frame_marker) {
      frame = (lisp_frame *)current;
      
      mark_root(frame->savevsp); /* likely a fixnum */
      mark_root(frame->savefn);
      mark_pc_root(frame->savelr);
      current += sizeof(lisp_frame)/sizeof(LispObj);
    } else if ((header == stack_alloc_marker) || (header == 0)) {
      current += 2;
    } else if (nodeheader_tag_p(fulltag_of(header))) {
      natural elements = header_element_count(header);

      current++;
      while(elements--) {
        mark_root(*current++);
      }
      if (((natural)current) & sizeof(natural)) {
        current++;
      }
    } else if (immheader_tag_p(fulltag_of(header))) {
      current=(LispObj *)skip_over_ivector((natural)current,header);
    } else {
      Bug(NULL, "Unknown stack word at 0x" LISP ":\n", current);
    }
  }
  if (current != limit) {
    Bug(NULL, "Ran off the end of cstack area\n");
  }
}



/* Mark the lisp objects in an exception frame */
void
mark_xp(ExceptionInformation *xp)
{
#ifdef LINUX
  void *find_vfp_info(ExceptionInformation *);
#endif
  natural *regs = (natural *) xpGPRvector(xp);
  int r;
  /* registers between arg_z and Rfn should be tagged and marked as
     roots.  the PC, and LR should be treated as "pc_locatives".

     In general, marking a locative is more expensive than marking
     a node is, since it may be neccessary to back up and find the
     containing object's header.  Since exception frames contain
     many locatives, it'd be wise to mark them *after* marking the
     stacks, nilreg-relative globals, etc.
     */

  for (r = arg_z; r <= Rfn; r++) {
    mark_root((regs[r]));
  }


  mark_pc_root(ptr_to_lispobj(xpPC(xp)));
  mark_pc_root(ptr_to_lispobj(xpLR(xp)));
  
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
  /* On little-endian ARM, we have to flip the low bit of dnode>>4 to
     get the near_bits from the appropriate half-word. */
  near_bits = ((unsigned short *)GCdynamic_markbits)[(dnode>>4)^1];

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
      return (new - one_bits(near_bits & 0xff));
    }
  }
}


LispObj
locative_forwarding_address(LispObj obj)
{
  int tag_n = fulltag_of(obj);
  natural dnode;



  /* Locatives can be tagged as conses, "fulltag_misc"
     objects, or as fixnums.  Immediates, headers, and nil
     shouldn't be "forwarded".  Nil never will be, but it
     doesn't hurt to check ... */
  if ((1<<tag_n) & ((1<<fulltag_immheader) |
                    (1<<fulltag_nodeheader) |
                    (1<<fulltag_imm) |
                    (1<<fulltag_nil))) {
    return obj;
  }

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
  int tag_n,subtag;
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
        subtag = header_subtag(node);
        if ((subtag == subtag_function) ||
            (subtag == subtag_pseudofunction)) {
          update_locref(p);
          p++;
          nwords--;
        }
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


void
forward_tstack_area(area *a)
{
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
  LispObj *current = (LispObj *)(a->active)
    , *limit = (LispObj*)(a->high), header;
  lisp_frame *frame;
  unsigned subtag;

  while (current < limit) {
    header = *current;

    if (header == lisp_frame_marker) {
      frame = (lisp_frame *)current;

      update_noderef(&(frame->savefn));
      update_locref(&(frame->savelr));
      current += sizeof(lisp_frame)/sizeof(LispObj);
    } else if ((header == stack_alloc_marker) || (header == 0)) {
      current += 2;
    } else if (nodeheader_tag_p(fulltag_of(header))) {
      natural elements = header_element_count(header);

      current++;
      subtag = header_subtag(header);
      if ((subtag == subtag_function) ||
          (subtag == subtag_pseudofunction)) {
        update_locref(current);
        current++;
        elements--;
      }
      while(elements--) {
        update_noderef(current);
        current++;
      }
      if (((natural)current) & sizeof(natural)) {
        current++;
      }
    } else if (immheader_tag_p(fulltag_of(header))) {
      current=(LispObj *)skip_over_ivector((natural)current,header);
    } else {
      Bug(NULL, "Unknown stack word at 0x" LISP ":\n", current);
    }
  }
}



void
forward_xp(ExceptionInformation *xp)
{
  natural *regs = (natural *) xpGPRvector(xp);

  int r;

  /* registers between arg_z and Rfn should be tagged and forwarded as roots.
     the PC and LR should be treated as "locatives".
     */

  for (r = arg_z; r <= Rfn;  r++) {
    update_noderef((LispObj*) (&(regs[r])));
  }


  update_locref((LispObj*) (&(xpPC(xp))));
  update_locref((LispObj*) (&(xpLR(xp))));

}

void
flush_code_vectors_in_range(LispObj *start,LispObj *end)
{
  LispObj *current = start,header;
  unsigned tag,subtag;
  natural nbytes, nwords;
  char *range_start;

  while (current != end) {
    header = *current;
    tag = fulltag_of(header);
    if (immheader_tag_p(tag)) {
      subtag = header_subtag(header);
      if (subtag == subtag_code_vector) {
        range_start = (char *)(current+1);
        nbytes = header_element_count(header)<<2;
        flush_cache_lines(range_start,nbytes);
      }
      current = skip_over_ivector((LispObj)current,header);
    } else if (nodeheader_tag_p(tag)) {
      nwords = header_element_count(header)+1;
      current += (nwords+(nwords&1));
    } else {
      current += 2;
    }
  }
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
  int tag, subtag;
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
            subtag = header_subtag(node);
            if ((subtag == subtag_function) ||
                (subtag == subtag_pseudofunction)) {
              *dest++ = locative_forwarding_address(*src++);
            } else {
              *dest++ = node_forwarding_address(*src++);
            }
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
        flush_code_vectors_in_range((LispObj *)GCfirstunmarked,
                                    (LispObj *)dest);
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
  LispObj
    loc = *locaddr,
    *headerP;
  natural
    tag = fulltag_of(loc);

  if (((BytePtr)ptr_from_lispobj(loc) > low) &&
      ((BytePtr)ptr_from_lispobj(loc) < high)) {

    headerP = (LispObj *)ptr_from_lispobj(untag(loc));
    switch (tag) {
    case fulltag_even_fixnum:
    case fulltag_odd_fixnum:
      if (*headerP != forward_marker) {
        LispObj code_vector = code_vector_from_pc(headerP);
        
        copy_ivector_reference(&code_vector, low, high, to);
      }
      *locaddr = (headerP[1]+tag);
      break;
    }
  }
}

void
purify_range(LispObj *start, LispObj *end, BytePtr low, BytePtr high, area *to)
{
  LispObj header;
  unsigned tag, subtag;

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
        subtag = header_subtag(header);
        if ((subtag == subtag_function) ||
            (subtag == subtag_pseudofunction)) {
          LispObj entrypt = *start;
          if ((entrypt > (LispObj)low) && 
              (entrypt < (LispObj)high) &&
              (fulltag_of(entrypt) == fulltag_odd_fixnum)) {
            *start = untag(entrypt) + fulltag_misc;
            copy_ivector_reference(start, low, high, to);
            *start = untag(*start)+fulltag_odd_fixnum;
          } else {
            copy_ivector_reference(start, low, high, to);
          }
        } else {
          copy_ivector_reference(start, low, high, to);
        }
        start++;
      }
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
  LispObj *current = (LispObj *)(a->active)
    , *limit = (LispObj*)(a->high), header;
  lisp_frame *frame;
  unsigned subtag;


  while(current < limit) {
    header = *current;

    if (header == lisp_frame_marker) {
      frame = (lisp_frame *)current;
      
      copy_ivector_reference(&(frame->savevsp), low, high, to); /* likely a fixnum */
      copy_ivector_reference(&(frame->savefn), low, high, to);
      purify_locref(&(frame->savelr), low, high, to);
      current += sizeof(lisp_frame)/sizeof(LispObj);
    } else if ((header == stack_alloc_marker) || (header == 0)) {
      current += 2;
    } else if (nodeheader_tag_p(fulltag_of(header))) {
      natural elements = header_element_count(header);

      current++;
      subtag = header_subtag(header);
      if ((subtag == subtag_function) ||
          (subtag == subtag_pseudofunction)) {
        purify_locref(current, low, high, to);
        current++;
        elements--;
      }
      while(elements--) {
        copy_ivector_reference(current, low, high, to);
        current++;
      }
      if (((natural)current) & sizeof(natural)) {
        current++;
      }
    } else if (immheader_tag_p(fulltag_of(header))) {
      current=(LispObj *)skip_over_ivector((natural)current,header);
    } else {
      Bug(NULL, "Unknown stack word at 0x" LISP ":\n", current);
    }
  }
}

void
purify_xp(ExceptionInformation *xp, BytePtr low, BytePtr high, area *to)
{
  unsigned long *regs = (unsigned long *) xpGPRvector(xp);

  int r;

  /* Node registers should be treated as roots.
     The PC and LR should be treated as "locatives".
   */

  for (r = arg_z; r <= Rfn; r++) {
    copy_ivector_reference((LispObj*) (&(regs[r])), low, high, to);
  };

  purify_locref((LispObj*) (&(xpPC(xp))), low, high, to);
  purify_locref((LispObj*) (&(xpLR(xp))), low, high, to);
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
  

void
impurify_cstack_area(area *a, LispObj low, LispObj high, int delta)
{
  LispObj *current = (LispObj *)(a->active)
    , *limit = (LispObj*)(a->high), header;
  lisp_frame *frame;
  unsigned subtag;

  while(current < limit) {
    header = *current;

    if (header == lisp_frame_marker) {
      frame = (lisp_frame *)current;
      
      impurify_noderef(&(frame->savevsp), low, high,delta); /* likely a fixnum */
      impurify_noderef(&(frame->savefn), low, high, delta);
      impurify_locref(&(frame->savelr), low, high, delta);
      current += sizeof(lisp_frame)/sizeof(LispObj);
    } else if ((header == stack_alloc_marker) || (header == 0)) {
      current += 2;
    } else if (nodeheader_tag_p(fulltag_of(header))) {
      natural elements = header_element_count(header);

      current++;
      subtag = header_subtag(header);
      if ((subtag == subtag_function) || 
          (subtag == subtag_pseudofunction)) {
        impurify_locref(current, low, high, delta);
        current++;
        elements--;
      }
      while(elements--) {
        impurify_noderef(current, low, high, delta);
        current++;
      }
      if (((natural)current) & sizeof(natural)) {
        current++;
      }
    } else if (immheader_tag_p(fulltag_of(header))) {
      current=(LispObj *)skip_over_ivector((natural)current,header);
    } else {
      Bug(NULL, "Unknown stack word at 0x" LISP ":\n", current);
    }
  }
}


void
impurify_xp(ExceptionInformation *xp, LispObj low, LispObj high, int delta)
{
  natural *regs = (natural *) xpGPRvector(xp);
  int r;

  /* node registers should be treated as roots.
     The PC and LR should be treated as "locatives".
   */

  for (r = arg_z; r <= Rfn; r++) {
    impurify_noderef((LispObj*) (&(regs[r])), low, high, delta);
  };


  impurify_locref((LispObj*) (&(xpPC(xp))), low, high, delta);
  impurify_locref((LispObj*) (&(xpLR(xp))), low, high, delta);

}


void
impurify_range(LispObj *start, LispObj *end, LispObj low, LispObj high, int delta)
{
  LispObj header;
  unsigned tag, subtag;

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
      subtag = header_subtag(header);
      if ((subtag == subtag_function) ||
          (subtag == subtag_pseudofunction)) {
        LispObj entrypt = *start;
        if ((entrypt > (LispObj)low) && 
            (entrypt < (LispObj)high) &&
            (fulltag_of(entrypt) == fulltag_odd_fixnum)) {
          *start = untag(entrypt) + fulltag_misc;
          impurify_noderef(start, low, high, delta);
          *start = untag(*start)+fulltag_odd_fixnum;
        } else {
          impurify_noderef(start, low, high, delta);
        }
        start++;
      }
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
      
    case AREA_VSTACK:
      impurify_vstack_area(next_area, low, high, delta);
      break;
      
    case AREA_CSTACK:
      impurify_cstack_area(next_area, low, high, delta);
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
    natural n = ro_limit - ro_base;
    signed_natural delta = oldfree-ro_base;
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
      UnCommitMemory(ro_base, n);
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

