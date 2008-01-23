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

  if (n == (n & 0xff)) {
    return;
  }

  switch (tag) {
  case fulltag_even_fixnum:
  case fulltag_odd_fixnum:
  case fulltag_imm_0:
  case fulltag_imm_1:
    return;

  case fulltag_nil:
    if (n != lisp_nil) {
      Bug(NULL,"Object tagged as nil, not nil : 0x%08x", n);
    }
    return;


  case fulltag_nodeheader_0: 
  case fulltag_nodeheader_1: 
  case fulltag_immheader_0: 
  case fulltag_immheader_1: 
  case fulltag_immheader_2: 
    Bug(NULL, "Header not expected : 0x%lx", n);
    return;

  case fulltag_tra_0:
  case fulltag_tra_1:
    a = heap_area_containing((BytePtr)ptr_from_lispobj(n));
    if (a == NULL) {
      a = active_dynamic_area;
      if ((n > (ptr_to_lispobj(a->active))) &&
          (n < (ptr_to_lispobj(a->high)))) {
        Bug(NULL, "TRA points to heap free space: 0x%lx", n);
      }
      return;
    }
    /* tra points into the heap.  Check displacement, then
       check the function it (should) identify.
    */
    {
      int disp = 0;
      LispObj m = n;

      if ((*((unsigned short *)n) == RECOVER_FN_FROM_RIP_WORD0) &&
          (*((unsigned char *)(n+2)) == RECOVER_FN_FROM_RIP_BYTE2)) {
        disp = (*(int *) (n+3));
        n = RECOVER_FN_FROM_RIP_LENGTH+m+disp;
      }
      if ((disp == 0) ||
          (fulltag_of(n) != fulltag_function) ||
          (heap_area_containing((BytePtr)ptr_from_lispobj(n)) != a)) {
        Bug(NULL, "TRA at 0x%lx has bad displacement %d\n", n, disp);
      }
    }
    /* Otherwise, fall through and check the header on the function
       that the tra references */

  case fulltag_misc:
  case fulltag_cons:
  case fulltag_symbol:
  case fulltag_function:
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
check_all_mark_bits(LispObj *nodepointer) 
{
}





void
check_range(LispObj *start, LispObj *end, Boolean header_allowed)
{
  LispObj node, *current = start, *prev;
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
      if (header_subtag(node) == subtag_function) {
        int skip = *(int *)current;
        current += skip;
        elements -= skip;
      }
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
check_all_areas()
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
          check_range(current+2, end, true);
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

  if (tag_of(n) == tag_tra) {
    if ((*((unsigned short *)n) == RECOVER_FN_FROM_RIP_WORD0) &&
        (*((unsigned char *)(n+2)) == RECOVER_FN_FROM_RIP_BYTE2)) {
      int sdisp = (*(int *) (n+3));
      n = RECOVER_FN_FROM_RIP_LENGTH+n+sdisp;
      tag_n = fulltag_function;
    }
    else {
      return;
    }
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
    natural prefix_nodes = 0;

    tag_n = fulltag_of(header);


    if ((nodeheader_tag_p(tag_n)) ||
        (tag_n == ivector_class_64_bit)) {
      total_size_in_bytes = 8 + (element_count<<3);
    } else if (tag_n == ivector_class_32_bit) {
      total_size_in_bytes = 8 + (element_count<<2);
    } else {
      /* ivector_class_other_bit contains 8, 16-bit arrays & bitvector */
      if (subtag == subtag_bit_vector) {
        total_size_in_bytes = 8 + ((element_count+7)>>3);
      } else if (subtag >= min_8_bit_ivector_subtag) {
	total_size_in_bytes = 8 + element_count;
      } else {
        total_size_in_bytes = 8 + (element_count<<1);
      }
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
          deref(ptr_to_lispobj(base),1) = GCweakvll;
          GCweakvll = n;
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

      if (subtag == subtag_function) {
	prefix_nodes = (natural) ((int) deref(base,1));
        if (prefix_nodes > element_count) {
          Bug(NULL, "Function 0x%lx trashed",n);
        }
      }
      base += (1+element_count);

      element_count -= prefix_nodes;

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
 
  if (is_node_fulltag (tag_n)) {
    eph_dnode = area_dnode(n, GCephemeral_low);
    if (eph_dnode < GCn_ephemeral_dnodes) {
      mark_root(n);             /* May or may not mark it */
      return true;              /* but return true 'cause it's an ephemeral node */
    }
  }
  return false;                 /* Not a heap pointer or not ephemeral */
}
  


#ifdef X8664
#define RMARK_PREV_ROOT fulltag_imm_1 /* fulltag of 'undefined' value */
#define RMARK_PREV_CAR fulltag_nil /* fulltag_nil + node_size. Coincidence ? I think not. */
#else
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
  natural dnode, bits, *bitsp, mask, original_n = n;

  if (!is_node_fulltag(tag_n)) {
    return;
  }

  if (tag_of(n) == tag_tra) {
    if ((*((unsigned short *)n) == RECOVER_FN_FROM_RIP_WORD0) &&
        (*((unsigned char *)(n+2)) == RECOVER_FN_FROM_RIP_BYTE2)) {
      int sdisp = (*(int *) (n+3));
      n = RECOVER_FN_FROM_RIP_LENGTH+n+sdisp;
      tag_n = fulltag_function;
    } else {
      return;
    }
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
        suffix_dnodes,
	nmark;

      tag_n = fulltag_of(header);

      if ((nodeheader_tag_p(tag_n)) ||
          (tag_n == ivector_class_64_bit)) {
        total_size_in_bytes = 8 + (element_count<<3);
      } else if (tag_n == ivector_class_32_bit) {
        total_size_in_bytes = 8 + (element_count<<2);
      } else {
        /* ivector_class_other_bit contains 16-bit arrays & bitvector */
        if (subtag == subtag_bit_vector) {
          total_size_in_bytes = 8 + ((element_count+7)>>3);
	} else if (subtag >= min_8_bit_ivector_subtag) {
	  total_size_in_bytes = 8 + element_count;
        } else {
          total_size_in_bytes = 8 + (element_count<<1);
        }
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
          deref(ptr_to_lispobj(base),1) = GCweakvll;
          GCweakvll = n;
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

      nmark = element_count;

      if (subtag == subtag_function) {
        if ((int)base[1] >= nmark) {
          Bug(NULL,"Bad function at 0x%lx",n);
        }
	nmark -= (int)base[1];
      }

      while (nmark--) {
        rmark(deref(n,element_count));
        element_count--;
      }

      if (subtag == subtag_weak) {
        deref(n, 1) = GCweakvll;
        GCweakvll = n;
      }

    }
  } else {

    /* This is all a bit more complicated than the PPC version:

       - a symbol-vector can be referenced via either a FULLTAG-MISC
       pointer or a FULLTAG-SYMBOL pointer.  When we've finished
       marking the symbol-vector's elements, we need to know which tag
       the object that pointed to the symbol-vector had originally.

       - a function-vector can be referenced via either a FULLTAG-MISC
       pointer or a FULLTAG-FUNCTION pointer.  That introduces pretty
       much the same set of issues, but ...

       - a function-vector can also be referenced via a TRA; the
       offset from the TRA to the function header is arbitrary (though
       we can probably put an upper bound on it, and it's certainly
       not going to be more than 32 bits.)

       - function-vectors contain a mixture of code and constants,
       with a "boundary" word (that doesn't look like a valid
       constant) in between them.  There are 56 unused bits in the
       boundary word; the low 8 bits must be = to the constant
       'function_boundary_marker'.  We can store the byte displacement
       from the address of the object which references the function
       (tagged fulltag_misc, fulltag_function, or tra) to the address
       of the boundary marker when the function vector is first marked
       and recover that offset when we've finished marking the
       function vector.  (Note that the offset is signed; it's
       probably simplest to keep it in the high 32 bits of the
       boundary word.) 

 So:

       - while marking a CONS, the 'this' pointer as a 3-bit tag of
       tag_list; the 4-bit fulltag indicates which cell is being
       marked.

       - while marking a gvector (other than a symbol-vector or
       function-vector), the 'this' pointer is tagged tag_misc.
       (Obviously, it alternates between fulltag_misc and
       fulltag_nodeheader_0, arbitrarily.)  When we encounter the
       gvector header when the 'this' pointer has been tagged as
       fulltag_misc, we can restore 'this' to the header's address +
       fulltag_misc and enter the 'climb' state.  (Note that this
       value happens to be exactly what's in 'this' when the header's
       encountered.)

       - if we encounter a symbol-vector via the FULLTAG-MISC pointer
       to the symbol (not very likely, but legal and possible), it's
       treated exactly like the gvector case above.

       - in the more likely case where a symbol-vector is referenced
       via a FULLTAG-SYMBOL, we do the same loop as in the general
       gvector case, backing up through the vector with 'this' tagged
       as 'tag_symbol' (or fulltag_nodeheader_1); when we encounter
       the symbol header, 'this' gets fulltag_symbol added to the
       dnode-aligned address of the header, and we climb.

       - if anything (fulltag_misc, fulltag_function, tra) references
       an unmarked function function vector, we store the byte offfset
       from the tagged reference to the address of the boundary word
       in the high 32 bits of the boundary word, then we back up
       through the function-vector's constants, with 'this' tagged
       tag_function/ fulltag_immheader_0, until the (specially-tagged)
       boundary word is encountered.  The displacement stored in the boundary
       word is added to the aligned address of the  boundary word (restoring
       the original 'this' pointer, and we climb.

       Not that bad.
    */
       
    LispObj prev = undefined, this = n, next, *base;
    natural header, subtag, element_count, total_size_in_bytes, suffix_dnodes, *boundary;

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
    case tag_misc:
    case fulltag_misc:
    case tag_symbol:
    case fulltag_symbol:
    case tag_function:
    case fulltag_function:
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
    if ((tag_n == fulltag_tra_0) ||
        (tag_n == fulltag_tra_1)) {
      int disp = (*(int *) (n+3)) + RECOVER_FN_FROM_RIP_LENGTH;

      base = (LispObj *) (untag(n-disp));
      header = *((natural *) base);
      subtag = header_subtag(header);
      boundary = base + (int)(base[1]);
      (((int *)boundary)[1]) = (int)(this-((LispObj)boundary));
      this = (LispObj)(base)+fulltag_function;
      /* Need to set the initial markbit here */
      dnode = gc_area_dnode(this);
      set_bit(markbits,dnode);
    } else {
      base = (LispObj *) ptr_from_lispobj(untag(this));
      header = *((natural *) base);
      subtag = header_subtag(header);
      if (subtag == subtag_function) {
        boundary = base + (int)(base[1]);
        (((int *)boundary)[1]) = (int)(this-((LispObj)boundary));
      }
    }
    element_count = header_element_count(header);
    tag_n = fulltag_of(header);

    if ((nodeheader_tag_p(tag_n)) ||
        (tag_n == ivector_class_64_bit)) {
      total_size_in_bytes = 8 + (element_count<<3);
    } else if (tag_n == ivector_class_32_bit) {
      total_size_in_bytes = 8 + (element_count<<2);
    } else {
      /* ivector_class_other_bit contains 16-bit arrays & bitvector */
      if (subtag == subtag_bit_vector) {
        total_size_in_bytes = 8 + ((element_count+7)>>3);
      } else if (subtag >= min_8_bit_ivector_subtag) {
        total_size_in_bytes = 8 + element_count;
      } else {
        total_size_in_bytes = 8 + (element_count<<1);
      }
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
        deref(ptr_to_lispobj(base),1) = GCweakvll;
        GCweakvll = this;
        goto Climb;
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

    this = (LispObj)(base) + (tag_of(this))  + ((element_count+1) << node_shift);
    goto MarkVectorLoop;

  ClimbVector:
    prev = indirect_node(this);
    indirect_node(this) = next;

  MarkVectorLoop:
    this -= node_size;
    next = indirect_node(this);
    if ((tag_of(this) == tag_function) &&
        (header_subtag(next) == function_boundary_marker)) goto MarkFunctionDone;
    tag_n = fulltag_of(next);
    if (nodeheader_tag_p(tag_n)) goto MarkVectorDone;
    if (!is_node_fulltag(tag_n)) goto MarkVectorLoop;
    dnode = gc_area_dnode(next);
    if (dnode >= GCndnodes_in_area) goto MarkVectorLoop;
    set_bits_vars(markbits,dnode,bitsp,bits,mask);
    if (bits & mask) goto MarkVectorLoop;
    *bitsp = (bits | mask);
    indirect_node(this) = prev;
    if (tag_n == fulltag_cons) goto DescendCons;
    goto DescendVector;

  MarkVectorDone:
    /* "next" is vector header; "this" tagged tag_misc or tag_symbol.
       If  header subtag = subtag_weak_header, put it on weakvll */
    this += node_size;          /* make it fulltag_misc/fulltag_symbol */

    if (header_subtag(next) == subtag_weak) {
      deref(this, 1) = GCweakvll;
      GCweakvll = this;
    }
    goto Climb;

  MarkFunctionDone:
    boundary = (LispObj *)(node_aligned(this));
    this = ((LispObj)boundary) + (((int *)boundary)[1]);
    (((int *)boundary)[1]) = 0;
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



  switch (fulltag_of(header)) {
  case ivector_class_64_bit:
    nbytes = element_count << 3;
    break;
  case ivector_class_32_bit:
    nbytes = element_count << 2;
    break;
  case ivector_class_other_bit:
  default:
    if (subtag == subtag_bit_vector) {
      nbytes = (element_count+7)>>3;
    } else if (subtag >= min_8_bit_ivector_subtag) {
      nbytes = element_count;
    } else {
      nbytes = element_count << 1;
    }
  }
  return ptr_from_lispobj(start+(~15 & (nbytes + 8 + 15)));

}


void
check_refmap_consistency(LispObj *start, LispObj *end, bitvector refbits)
{
  LispObj x1, *base = start, *prev = start;
  int tag;
  natural ref_dnode, node_dnode;
  Boolean intergen_ref;

  while (start < end) {
    x1 = *start;
    prev = start;
    tag = fulltag_of(x1);
    if (immheader_tag_p(tag)) {
      start = skip_over_ivector(ptr_to_lispobj(start), x1);
    } else {
      if (header_subtag(x1) == subtag_function) {
        int skip = (int) deref(start,1);
        start += ((1+skip)&~1);
        x1 = *start;
        tag = fulltag_of(x1);
      }
      intergen_ref = false;
      if (is_node_fulltag(tag)) {        
        node_dnode = gc_area_dnode(x1);
        if (node_dnode < GCndnodes_in_area) {
          intergen_ref = true;
        }
      }
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
  if (start > end) {
    Bug(NULL, "Overran end of range!");
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
mark_headerless_area_range(LispObj *start, LispObj *end)
{
  while (start < end) {
    mark_root(*start++);
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
	  start[1] = GCweakvll;
	  GCweakvll = (LispObj) (((natural) start) + fulltag_misc);
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
      if (subtag == subtag_function) {
	element_count -= (int)start[1];
      }
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
    mark_simple_area_range(current+2, end);
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
  fprintf(stderr, "mark VSP range: 0x%lx:0x%lx\n", start, end);
#endif
  mark_headerless_area_range(start, end);
}

/* No lisp objects on cstack on x86, at least x86-64 */
void
mark_cstack_area(area *a)
{
}


/* Mark the lisp objects in an exception frame */
void
mark_xp(ExceptionInformation *xp)
{
  natural *regs = (natural *) xpGPRvector(xp), dnode;
  LispObj rip;
    
  

  mark_root(regs[Iarg_z]);
  mark_root(regs[Iarg_y]);
  mark_root(regs[Iarg_x]);
  mark_root(regs[Isave3]);
  mark_root(regs[Isave2]);
  mark_root(regs[Isave1]);
  mark_root(regs[Isave0]);
  mark_root(regs[Ifn]);
  mark_root(regs[Itemp0]);
  mark_root(regs[Itemp1]);
  mark_root(regs[Itemp2]);
  /* If the RIP isn't pointing into a marked function,
     we can -maybe- recover from that if it's tagged as
     a TRA. */
  rip = regs[Iip];
  dnode = gc_area_dnode(rip);
  if ((dnode < GCndnodes_in_area) &&
      (! ref_bit(GCmarkbits,dnode))) {
    if (tag_of(rip) == tag_tra) {
      mark_root(rip);
    } else if ((fulltag_of(rip) == fulltag_function) &&
               (*((unsigned short *)rip) == RECOVER_FN_FROM_RIP_WORD0) &&
               (*((unsigned char *)(rip+2)) == RECOVER_FN_FROM_RIP_BYTE2) &&
               ((*(int *) (rip+3))) == -RECOVER_FN_FROM_RIP_LENGTH) {
      mark_root(rip);
    } else {
      Bug(NULL, "Can't find function for rip 0x%16lx",rip);
    }
  }
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

  if (npagelets) {
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
        /* We're counting bits in qnodes in the wrong order here, but
           that's OK.  I think ... */
        current += one_bits(*q++);
        current += one_bits(*q++);
        current += one_bits(*q++);
        current += one_bits(*q++);
      }
    } while(--npagelets);
  }
  *relocptr++ = current;
  return first ? first : current;
}


#if 0
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

/* Quicker, dirtier */
LispObj
dnode_forwarding_address(natural dnode, int tag_n)
{
  natural pagelet, nbits, marked;
  LispObj new;

  if (GCDebug) {
    if (! ref_bit(GCdynamic_markbits, dnode)) {
      Bug(NULL, "unmarked object being forwarded!\n");
    }
  }

  pagelet = dnode >> bitmap_shift;
  nbits = dnode & bitmap_shift_count_mask;
  new = GCrelocptr[pagelet] + tag_n;;
  if (nbits) {
    marked = (GCdynamic_markbits[dnode>>bitmap_shift]) >> (64-nbits);
    while (marked) {
      new += one_bits((qnode)marked);
      marked >>=16;
    }
  }
  return new;
}
#endif

LispObj
locative_forwarding_address(LispObj obj)
{
  int tag_n = fulltag_of(obj);
  natural dnode = gc_dynamic_area_dnode(obj);


  if ((dnode >= GCndynamic_dnodes_in_area) ||
      (obj < GCfirstunmarked)) {
    return obj;
  }

  return dnode_forwarding_address(dnode, tag_n);
}


void
forward_headerless_range(LispObj *range_start, LispObj *range_end)
{
  LispObj *p = range_start;

  while (p < range_end) {
    update_noderef(p);
    p++;
  }
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
	if (header_subtag(node) == subtag_function) {
	  int skip = (int)(p[1]);
	  p += skip;
	  nwords -= skip;
	}
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
    forward_range(current+2, end);
  }
}

/* Forward a vstack area */
void
forward_vstack_area(area *a)
{
  LispObj
    *p = (LispObj *) a->active,
    *q = (LispObj *) a->high;

  forward_headerless_range(p, q);
}

/* Nothing of interest on x86 cstack */
void
forward_cstack_area(area *a)
{
}


void
forward_xp(ExceptionInformation *xp)
{
  natural *regs = (natural *) xpGPRvector(xp);

  update_noderef(&(regs[Iarg_z]));
  update_noderef(&(regs[Iarg_y]));
  update_noderef(&(regs[Iarg_x]));
  update_noderef(&(regs[Isave3]));
  update_noderef(&(regs[Isave2]));
  update_noderef(&(regs[Isave1]));
  update_noderef(&(regs[Isave0]));
  update_noderef(&(regs[Ifn]));
  update_noderef(&(regs[Itemp0]));
  update_noderef(&(regs[Itemp1]));
  update_noderef(&(regs[Itemp2]));
  update_locref(&(regs[Iip]));
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
  LispObj *src = ptr_from_lispobj(GCfirstunmarked), *dest = src, node, new, *current,  *prev = NULL;
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
        prev = current;
        current = src;
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
	  if (header_subtag(node) == subtag_function) {
	    int skip = *((int *)src);
	    *dest++ = node;
	    elements -= skip;
	    while(skip--) {
	      *dest++ = *src++;
	    }
	    while(elements--) {
	      *dest++ = node_forwarding_address(*src++);
	    }
	    if (((LispObj)src) & node_size) {
	      src++;
	      *dest++ = 0;
	    }
	  } else {
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
          }
          set_bitidx_vars(markbits,dnode,bitsp,bits,bitidx);
        } else if (immheader_tag_p(tag)) {
          *dest++ = node;
          *dest++ = *src++;
          elements = header_element_count(node);
          tag = header_subtag(node);


          switch(fulltag_of(tag)) {
          case ivector_class_64_bit:
            imm_dnodes = ((elements+1)+1)>>1;
            break;
          case ivector_class_32_bit:
            imm_dnodes = (((elements+2)+3)>>2);
            break;
          case ivector_class_other_bit:
            if (tag == subtag_bit_vector) {
              imm_dnodes = (((elements+64)+127)>>7);
	    } else if (tag >= min_8_bit_ivector_subtag) {
	      imm_dnodes = (((elements+8)+15)>>4);
            } else {
              imm_dnodes = (((elements+4)+7)>>3);
            }
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

        switch(fulltag_of(header)) {
        case ivector_class_64_bit:
          bytes = 8 + (elements<<3);
          break;
        case ivector_class_32_bit:
          bytes = 8 + (elements<<2);
          break;
        case ivector_class_other_bit:
        default:
          if (subtag == subtag_bit_vector) {
            bytes = 8 + ((elements+7)>>3);
	  } else if (subtag >= min_8_bit_ivector_subtag) {
	    bytes = 8 + elements;
          } else {
            bytes = 8 + (elements<<1);
          }
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
  This assumes that it's getting called with a simple-{base,general}-string
  or code vector as an argument and that there's room for the object in the
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
    subtag = header_subtag(header), 
    element_count = header_element_count(header),
    physbytes;

  switch(subtag) {
  case subtag_simple_base_string:
    physbytes = node_size + (element_count << 2);
    break;

#ifndef X86
  case subtag_code_vector:
    physbytes = node_size + (element_count << 2);
    break;
#endif

  default:
    Bug(NULL, "Can't purify object at 0x%08x", obj);
    return obj;
  }
  physbytes = (physbytes+(dnode_size-1))&~(dnode_size-1);
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


#define FORWARD_ONLY 0
#define COPY_CODE (1<<0)
#define COPY_STRINGS (1<<1)


/*
  This may overestimate a bit, if the same symbol is accessible from multiple
  packages.
*/
natural
interned_pname_bytes_in_range(LispObj *start, LispObj *end)
{
  lispsymbol *rawsym = (lispsymbol *)(&(nrs_ALL_PACKAGES));
  LispObj pkg_list = rawsym->vcell, htab, obj, pname, pname_header;
  package *p;
  cons *c;
  natural elements, i, nbytes = 0;

  while (fulltag_of(pkg_list) == fulltag_cons) {
    c = (cons *) ptr_from_lispobj(untag(pkg_list));
    p = (package *) ptr_from_lispobj(untag(c->car));
    pkg_list = c->cdr;
    c = (cons *) ptr_from_lispobj(untag(p->itab));
    htab = c->car;
    elements = header_element_count(header_of(htab));
    for (i = 1; i<= elements; i++) {
      obj = deref(htab,i);
      if (fulltag_of(obj) == fulltag_symbol) {
        rawsym = (lispsymbol *) ptr_from_lispobj(untag(obj));
        pname = rawsym->pname;

        if ((pname >= (LispObj)start) && (pname < (LispObj)end)) {
          pname_header = header_of(pname);
          nbytes += ((8 + (header_element_count(pname_header)<<2) + 15) &~15);
        }
      }
    }
    c = (cons *) ptr_from_lispobj(untag(p->etab));
    htab = c->car;
    elements = header_element_count(header_of(htab));
    for (i = 1; i<= elements; i++) {
      obj = deref(htab,i);
      if (fulltag_of(obj) == fulltag_symbol) {
        rawsym = (lispsymbol *) ptr_from_lispobj(untag(obj));
        pname = rawsym->pname;

        if ((pname >= (LispObj)start) && (pname < (LispObj)end)) {
          pname_header = header_of(pname);
          nbytes += ((8 + (header_element_count(pname_header)<<2) + 15) &~15);
        }
      }
    }
  }
  return nbytes;
}

Boolean
copy_ivector_reference(LispObj *ref, BytePtr low, BytePtr high, area *dest, int what_to_copy)
{
  LispObj obj = *ref, header, new;
  natural tag = fulltag_of(obj), header_tag, header_subtag;
  Boolean changed = false;

  if ((tag == fulltag_misc) &&
      (((BytePtr)ptr_from_lispobj(obj)) > low) &&
      (((BytePtr)ptr_from_lispobj(obj)) < high)) {
    header = deref(obj, 0);
    if (header == forward_marker) { /* already copied */
      *ref = (untag(deref(obj,1)) + tag);
      changed = true;
    } else {
      header_tag = fulltag_of(header);
      if (immheader_tag_p(header_tag)) {
        header_subtag = header_subtag(header);
        if ((what_to_copy & COPY_STRINGS) && 
            ((header_subtag == subtag_simple_base_string))) {
          new = purify_object(obj, dest);
          *ref = new;
          changed = (new != obj);
        }
      }
    }
  }
  return changed;
}


void purify_headerless_range(LispObj *start, LispObj *end, BytePtr low, BytePtr high, area *to, int what)
{
  while (start < end) { 
    copy_ivector_reference(start, low, high, to, what);
    start++;
  }
}
   
void
purify_range(LispObj *start, LispObj *end, BytePtr low, BytePtr high, area *to, int what)
{
  LispObj header;
  unsigned tag;
  natural nwords;
  hash_table_vector_header *hashp;

  while (start < end) {
    header = *start;
    if (header == forward_marker) {
      start += 2;
    } else {
      tag = fulltag_of(header);
      if (immheader_tag_p(tag)) {
        start = (LispObj *)skip_over_ivector((natural)start, header);
      } else if (nodeheader_tag_p(tag)) {
        nwords = header_element_count(header);
        nwords += (1 - (nwords&1));
        if ((header_subtag(header) == subtag_hash_vector) &&
          ((((hash_table_vector_header *)start)->flags) & 
           nhash_track_keys_mask)) {
          natural skip = (sizeof(hash_table_vector_header)/sizeof(LispObj))-1;

          hashp = (hash_table_vector_header *) start;
          start++;
          nwords -= skip;
          while(skip--) {
            copy_ivector_reference(start, low, high, to, what);
            start++;
          }
          /* "nwords" is odd at this point: there are (floor nwords 2)
             key/value pairs to look at, and then an extra word for
             alignment.  Process them two at a time, then bump "start"
             past the alignment word. */
          nwords >>= 1;
          while(nwords--) {
            if (copy_ivector_reference(start, low, high, to, what) && hashp) {
              hashp->flags |= nhash_key_moved_mask;
              hashp = NULL;
            }
            start++;
            copy_ivector_reference(start, low, high, to, what);
            start++;
          }
          *start++ = 0;
        } else {
          if (header_subtag(header) == subtag_function) {
            int skip = (int)(start[1]);
            start += skip;
            nwords -= skip;
          }
          start++;
          while(nwords--) {
            copy_ivector_reference(start, low, high, to, what);
            start++;
          }
        }
      } else {
        /* Not a header, just a cons cell */
        copy_ivector_reference(start, low, high, to, what);
        start++;
        copy_ivector_reference(start, low, high, to, what);
        start++;
      }
    }
  }
}
        
/* Purify references from tstack areas */
void
purify_tstack_area(area *a, BytePtr low, BytePtr high, area *to, int what)
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
      purify_range(current+2, end, low, high, to, what);
    }
  }
}

/* Purify a vstack area */
void
purify_vstack_area(area *a, BytePtr low, BytePtr high, area *to, int what)
{
  LispObj
    *p = (LispObj *) a->active,
    *q = (LispObj *) a->high;
  
  purify_headerless_range(p, q, low, high, to, what);
}


void
purify_xp(ExceptionInformation *xp, BytePtr low, BytePtr high, area *to, int what)
{
  natural *regs = (natural *) xpGPRvector(xp);


#ifdef X8664
  copy_ivector_reference(&(regs[Iarg_z]), low, high, to, what);
  copy_ivector_reference(&(regs[Iarg_y]), low, high, to, what);
  copy_ivector_reference(&(regs[Iarg_x]), low, high, to, what);
  copy_ivector_reference(&(regs[Isave3]), low, high, to, what);
  copy_ivector_reference(&(regs[Isave2]), low, high, to, what);
  copy_ivector_reference(&(regs[Isave1]), low, high, to, what);
  copy_ivector_reference(&(regs[Isave0]), low, high, to, what);
  copy_ivector_reference(&(regs[Ifn]), low, high, to, what);
  copy_ivector_reference(&(regs[Itemp0]), low, high, to, what);
  copy_ivector_reference(&(regs[Itemp1]), low, high, to, what);
  copy_ivector_reference(&(regs[Itemp2]), low, high, to, what);
#if 0
  purify_locref(&(regs[Iip]), low, high, to, what);
#endif
#else
#endif
}

void
purify_tcr_tlb(TCR *tcr, BytePtr low, BytePtr high, area *to, int what)
{
  natural n = tcr->tlb_limit;
  LispObj *start = tcr->tlb_pointer, *end = (LispObj *) ((BytePtr)start+n);

  purify_range(start, end, low, high, to, what);
}

void
purify_tcr_xframes(TCR *tcr, BytePtr low, BytePtr high, area *to, int what)
{
  xframe_list *xframes;
  ExceptionInformation *xp;
  
  xp = tcr->gc_context;
  if (xp) {
    purify_xp(xp, low, high, to, what);
  }

  for (xframes = tcr->xframe; xframes; xframes = xframes->prev) {
    purify_xp(xframes->curr, low, high, to, what);
  }
}


void
purify_areas(BytePtr low, BytePtr high, area *target, int what)
{
  area *next_area;
  area_code code;
      
  for (next_area = active_dynamic_area; (code = next_area->code) != AREA_VOID; next_area = next_area->succ) {
    switch (code) {
    case AREA_TSTACK:
      purify_tstack_area(next_area, low, high, target, what);
      break;
      
    case AREA_VSTACK:
      purify_vstack_area(next_area, low, high, target, what);
      break;
      
    case AREA_CSTACK:
#ifdef PPC
      purify_cstack_area(next_area, low, high, target, what);
#endif
      break;
      
    case AREA_STATIC:
    case AREA_DYNAMIC:
      purify_range((LispObj *) next_area->low, (LispObj *) next_area->active, low, high, target, what);
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


int
purify(TCR *tcr, signed_natural param)
{
  extern area *extend_readonly_area(unsigned);
  area 
    *a = active_dynamic_area,
    *new_pure_area;

  TCR  *other_tcr;
  natural max_pure_size;
  OSErr err;
  BytePtr new_pure_start;



  max_pure_size = interned_pname_bytes_in_range((LispObj *)(a->low + (static_dnodes_for_area(a) << dnode_shift)), 
                                         (LispObj *) a->active);
  new_pure_area = extend_readonly_area(max_pure_size);
  if (new_pure_area) {
    new_pure_start = new_pure_area->active;
    lisp_global(IN_GC) = (1<<fixnumshift);

    /* 
      First, loop thru *all-packages* and purify the pnames of all
      interned symbols.  Then walk every place that could reference
      a heap-allocated object (all_areas, the xframe_list) and
      purify code_vectors (and update the odd case of a shared
      reference to a pname.)
       
      Make the new_pure_area executable, just in case.

      Caller will typically GC again (and that should recover quite a bit of
      the dynamic heap.)
      */

    {
      lispsymbol *rawsym = (lispsymbol *)(&(nrs_ALL_PACKAGES));
      LispObj pkg_list = rawsym->vcell, htab, obj;
      package *p;
      cons *c;
      natural elements, i;

      while (fulltag_of(pkg_list) == fulltag_cons) {
        c = (cons *) ptr_from_lispobj(untag(pkg_list));
        p = (package *) ptr_from_lispobj(untag(c->car));
        pkg_list = c->cdr;
        c = (cons *) ptr_from_lispobj(untag(p->itab));
        htab = c->car;
        elements = header_element_count(header_of(htab));
        for (i = 1; i<= elements; i++) {
          obj = deref(htab,i);
          if (fulltag_of(obj) == fulltag_symbol) {
            rawsym = (lispsymbol *) ptr_from_lispobj(untag(obj));
            copy_ivector_reference(&(rawsym->pname), a->low, a->active, new_pure_area, COPY_STRINGS);
          }
        }
        c = (cons *) ptr_from_lispobj(untag(p->etab));
        htab = c->car;
        elements = header_element_count(header_of(htab));
        for (i = 1; i<= elements; i++) {
          obj = deref(htab,i);
          if (fulltag_of(obj) == fulltag_symbol) {
            rawsym = (lispsymbol *) ptr_from_lispobj(untag(obj));
            copy_ivector_reference(&(rawsym->pname), a->low, a->active, new_pure_area, COPY_STRINGS);
          }
        }
      }
    }
    
    purify_areas(a->low, a->active, new_pure_area, FORWARD_ONLY);
    
    other_tcr = tcr;
    do {
      purify_tcr_xframes(other_tcr, a->low, a->active, new_pure_area, FORWARD_ONLY);
      purify_tcr_tlb(other_tcr, a->low, a->active, new_pure_area, FORWARD_ONLY);
      other_tcr = other_tcr->next;
    } while (other_tcr != tcr);


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


  
Boolean
impurify_noderef(LispObj *p, LispObj low, LispObj high, int delta)
{
  LispObj q = *p;
  
  if (is_node_fulltag(fulltag_of(q)) &&
      (q >= low) && 
      (q < high)) {
    *p = (q+delta);
    return true;
  }
  return false;
}
  


void
impurify_xp(ExceptionInformation *xp, LispObj low, LispObj high, int delta)
{
  natural *regs = (natural *) xpGPRvector(xp);


#ifdef X8664
  impurify_noderef(&(regs[Iarg_z]), low, high, delta);
  impurify_noderef(&(regs[Iarg_y]), low, high, delta);
  impurify_noderef(&(regs[Iarg_x]), low, high, delta);
  impurify_noderef(&(regs[Isave3]), low, high, delta);
  impurify_noderef(&(regs[Isave2]), low, high, delta);
  impurify_noderef(&(regs[Isave1]), low, high, delta);
  impurify_noderef(&(regs[Isave0]), low, high, delta);
  impurify_noderef(&(regs[Ifn]), low, high, delta);
  impurify_noderef(&(regs[Itemp0]), low, high, delta);
  impurify_noderef(&(regs[Itemp1]), low, high, delta);
#if 0
  impurify_locref(&(regs[Iip]), low, high, delta);
#endif
#else
#endif

}

void
impurify_headerless_range(LispObj *start, LispObj *end, LispObj low, LispObj high, int delta)
{
  while (start < end) {
    impurify_noderef(start, low, high, delta);
    start++;
  }
}


void
impurify_range(LispObj *start, LispObj *end, LispObj low, LispObj high, int delta)
{
  LispObj header;
  unsigned tag;
  natural nwords;
  hash_table_vector_header *hashp;

  while (start < end) {
    header = *start;
    if (header == forward_marker) {
      start += 2;
    } else {
      tag = fulltag_of(header);
      if (immheader_tag_p(tag)) {
        start = (LispObj *)skip_over_ivector((natural)start, header);
      } else if (nodeheader_tag_p(tag)) {
        nwords = header_element_count(header);
        nwords += (1 - (nwords&1));
        if ((header_subtag(header) == subtag_hash_vector) &&
          ((((hash_table_vector_header *)start)->flags) & 
           nhash_track_keys_mask)) {
          natural skip = (sizeof(hash_table_vector_header)/sizeof(LispObj))-1;

          hashp = (hash_table_vector_header *) start;
          start++;
          nwords -= skip;
          while(skip--) {
            impurify_noderef(start, low, high, delta);
            start++;
          }
          /* "nwords" is odd at this point: there are (floor nwords 2)
             key/value pairs to look at, and then an extra word for
             alignment.  Process them two at a time, then bump "start"
             past the alignment word. */
          nwords >>= 1;
          while(nwords--) {
            if (impurify_noderef(start, low, high, delta) && hashp) {
              hashp->flags |= nhash_key_moved_mask;
              hashp = NULL;
            }
            start++;
            impurify_noderef(start, low, high, delta);
            start++;
          }
          *start++ = 0;
        } else {
          if (header_subtag(header) == subtag_function) {
            int skip = (int)(start[1]);
            start += skip;
            nwords -= skip;
          }
          start++;
          while(nwords--) {
            impurify_noderef(start, low, high, delta);
            start++;
          }
        }
      } else {
        /* Not a header, just a cons cell */
        impurify_noderef(start, low, high, delta);
        start++;
        impurify_noderef(start, low, high, delta);
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

  impurify_headerless_range(p, q, low, high, delta);
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

int
impurify(TCR *tcr, signed_natural param)
{
  area *r = find_readonly_area();

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
      munmap((void *)ro_base, n);
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
      lisp_global(IN_GC) = 0;
    }
    return 0;
  }
  return -1;
}
