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

#ifdef X8632
natural
imm_word_count(LispObj fn)
{
  natural w = ((unsigned short *)fn)[-1];

  if (w & 0x8000) {
    /* 
     * The low 15 bits encode the number of contants.
     * Compute and return the immediate word count.
     */
    LispObj header = header_of(fn);
    natural element_count = header_element_count(header);

    return element_count - (w & 0x7fff);
  } else {
    /* The immediate word count is encoded directly. */
    return w;
  }
}
#endif

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
#ifdef X8632
  case fulltag_imm:
#endif
#ifdef X8664
  case fulltag_imm_0:
  case fulltag_imm_1:
#endif
    return;

#ifdef X8664
  case fulltag_nil:
    if (n != lisp_nil) {
      Bug(NULL,"Object tagged as nil, not nil : " LISP, n);
    }
    return;
#endif

#ifdef X8632
  case fulltag_nodeheader:
  case fulltag_immheader:
#endif
#ifdef X8664
  case fulltag_nodeheader_0: 
  case fulltag_nodeheader_1: 
  case fulltag_immheader_0: 
  case fulltag_immheader_1: 
  case fulltag_immheader_2: 
#endif
    Bug(NULL, "Header not expected : 0x" LISP, n);
    return;

#ifdef X8632
  case fulltag_tra:
#endif
#ifdef X8664
  case fulltag_tra_0:
  case fulltag_tra_1:
#endif
    a = heap_area_containing((BytePtr)ptr_from_lispobj(n));
    if (a == NULL) {
      a = active_dynamic_area;
      if ((n > (ptr_to_lispobj(a->active))) &&
          (n < (ptr_to_lispobj(a->high)))) {
        Bug(NULL, "TRA points to heap free space: 0x" LISP, n);
      }
      return;
    }
    /* tra points into the heap.  Check displacement, then
       check the function it (should) identify.
    */
#ifdef X8632
    {
      LispObj fun = 0;

      if (*(unsigned char *)n == RECOVER_FN_OPCODE)
	fun = *(LispObj *)(n + 1);
      if (fun == 0 ||
	 (header_subtag(header_of(fun)) != subtag_function) ||
	 (heap_area_containing((BytePtr)ptr_from_lispobj(fun)) != a)) {
	Bug(NULL, "TRA at 0x" LISP " has bad function address 0x" LISP "\n", n, fun);
      }
      n = fun;
    }
#endif
#ifdef X8664
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
        Bug(NULL, "TRA at 0x" LISP " has bad displacement %d\n", n, disp);
      }
    }
#endif
    /* Otherwise, fall through and check the header on the function
       that the tra references */

  case fulltag_misc:
  case fulltag_cons:
#ifdef X8664
  case fulltag_symbol:
  case fulltag_function:
#endif
    a = heap_area_containing((BytePtr)ptr_from_lispobj(n));
    
    if (a == NULL) {
      /* Can't do as much sanity checking as we'd like to
         if object is a defunct stack-consed object.
         If a dangling reference to the heap, that's
         bad .. */
      a = active_dynamic_area;
      if ((n > (ptr_to_lispobj(a->active))) &&
          (n < (ptr_to_lispobj(a->high)))) {
        Bug(NULL, "Node points to heap free space: 0x" LISP, n);
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
      Bug(NULL, "Cons cell at 0x" LISP " has bogus header : 0x" LISP, n, header);
    }
    return;
  }

  if ((!nodeheader_tag_p(header_tag)) &&
      (!immheader_tag_p(header_tag))) {
    Bug(NULL,"Vector at 0x" LISP " has bogus header : 0x" LISP, n, header);
  }
  return;
}

void
check_all_mark_bits(LispObj *nodepointer) 
{
}


void
check_readonly_range(LispObj *start, LispObj *end)
{
  LispObj node, *current = start, *prev = NULL, *next, fn;
  int tag, subtag;
  natural elements;
  area *a;
  area_code code;

  while (current < end) {
    prev = current;
    node = *current++;
    tag = fulltag_of(node);
    if (immheader_tag_p(tag)) {
      current = (LispObj *)skip_over_ivector((natural)prev, node);
    } else {
      if (nodeheader_tag_p(tag)) {
        next = current+(header_element_count(node)|1);
      } else {
        next = current+1;
      }
      subtag = header_subtag(node);
      if (subtag != subtag_function) {
        Bug(NULL, "Found 0x" LISP " in readonly area at 0x" LISP ", expected ivector or function header", node, (LispObj)current);
      } else {
        elements = header_element_count(node);
        {
#ifdef X8632
          int skip = *(unsigned short *)current;
          
          fn = (LispObj)(current-1)+fulltag_misc;
          /* XXX bootstrapping */
          if (skip & 0x8000)
            skip = elements - (skip & 0x7fff);
#else
          int skip = *(int *)current;
          fn = (LispObj)(current-1)+fulltag_function;
#endif
          current += skip;
          elements -= skip;
        }
        while (elements--) {
          node = *current;
          if (is_node_fulltag(fulltag_of(node))) {
            a = area_containing((BytePtr)node);
            if (a) {
              code = a->code;
            } else {
              a = AREA_VOID;
            }
            switch (code) {
            case AREA_READONLY:
            case AREA_MANAGED_STATIC:
            case AREA_STATIC:
              break;
            default:
              Bug(NULL, "Bad constant reference from purified function 0x" LISP " to 0x" LISP ".", fn, node);
            }
          }
          current++;
        }
      }
      current = next;
    }
  }
  if (current != end) {
    Bug(NULL, "Overran end of memory range: start = 0x" LISP ", end = 0x" LISP ", prev = 0x" LISP ", current = 0x" LISP "",
        start, end, prev, current);
  }
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
        Bug(NULL, "Header not expected at 0x" LISP "\n", prev);
      }
      current = (LispObj *)skip_over_ivector((natural)prev, node);
    } else if (nodeheader_tag_p(tag)) {
      if (! header_allowed) {
        Bug(NULL, "Header not expected at 0x" LISP "\n", prev);
      }
      elements = header_element_count(node) | 1;
      if (header_subtag(node) == subtag_function) {
#ifdef X8632
	int skip = *(unsigned short *)current;

	/* XXX bootstrapping */
	if (skip & 0x8000)
	  skip = elements - (skip & 0x7fff);
#else
        int skip = *(int *)current;
#endif
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

#ifdef X8632
void
check_xp(ExceptionInformation *xp, natural node_regs_mask)
{
  natural *regs = (natural *) xpGPRvector(xp);

  if (node_regs_mask & (1<<0)) check_node(regs[REG_EAX]);
  if (node_regs_mask & (1<<1)) check_node(regs[REG_ECX]);
  if (regs[REG_EFL] & EFL_DF) {
    /* DF set means EDX should be treated as an imm reg */
    ;
  } else
    if (node_regs_mask & (1<<2)) check_node(regs[REG_EDX]);

  if (node_regs_mask & (1<<3)) check_node(regs[REG_EBX]);
  if (node_regs_mask & (1<<4)) check_node(regs[REG_ESP]);
  if (node_regs_mask & (1<<5)) check_node(regs[REG_EBP]);
  if (node_regs_mask & (1<<6)) check_node(regs[REG_ESI]);
  if (node_regs_mask & (1<<7)) check_node(regs[REG_EDI]);
}
#else
void
check_xp(ExceptionInformation *xp)
{
  natural *regs = (natural *) xpGPRvector(xp);

  check_node(regs[Iarg_z]);
  check_node(regs[Iarg_y]);
  check_node(regs[Iarg_x]);
  check_node(regs[Isave3]);
  check_node(regs[Isave2]);
  check_node(regs[Isave1]);
  check_node(regs[Isave0]);
  check_node(regs[Ifn]);
  check_node(regs[Itemp0]);
  check_node(regs[Itemp1]);
  check_node(regs[Itemp2]);
}
#endif

void
check_tcrs(TCR *first)
{
  xframe_list *xframes;
  ExceptionInformation *xp;
  
  TCR *tcr = first;
  LispObj *tlb_start,*tlb_end;

  do {
    xp = TCR_AUX(tcr)->gc_context;
    if (xp) {
#ifdef X8632
      check_xp(xp,tcr->node_regs_mask);
#else
      check_xp(xp);
#endif
    }
#ifdef X8632
    check_node(tcr->save0);
    check_node(tcr->save1);
    check_node(tcr->save2);
    check_node(tcr->save3);
    check_node(tcr->next_method_context);
#endif
    for (xframes = (xframe_list *) tcr->xframe; 
         xframes; 
         xframes = xframes->prev) {
#ifndef X8632
      check_xp(xframes->curr);
#else
      check_xp(xframes->curr, xframes->node_regs_mask);
#endif
    }
    tlb_start = tcr->tlb_pointer;
    if (tlb_start) {
      tlb_end = tlb_start + ((tcr->tlb_limit)>>fixnumshift);
      check_range(tlb_start,tlb_end,false);
    }
    tcr = TCR_AUX(tcr)->next;
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
    case AREA_WATCHED:
    case AREA_STATIC:
    case AREA_MANAGED_STATIC:
      check_range((LispObj *)a->low, (LispObj *)a->active, true);
      break;

    case AREA_READONLY:
      check_readonly_range((LispObj *)a->low, (LispObj *)a->active);
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

    default:
      break;
    }
    a = a->succ;
    code = (a->code);
  }

  check_tcrs(tcr);
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

#ifdef X8632
  if (tag_n == fulltag_tra) {
    if (*(unsigned char *)n == RECOVER_FN_OPCODE) {
      n = *(LispObj *)(n + 1);
      tag_n = fulltag_misc;
      dnode = gc_area_dnode(n);
    } else
      return;
  }
#endif
#ifdef X8664
  if (tag_of(n) == tag_tra) {
    if ((*((unsigned short *)n) == RECOVER_FN_FROM_RIP_WORD0) &&
        (*((unsigned char *)(n+2)) == RECOVER_FN_FROM_RIP_BYTE2)) {
      int sdisp = (*(int *) (n+3));
      n = RECOVER_FN_FROM_RIP_LENGTH+n+sdisp;
      tag_n = fulltag_function;
      dnode = gc_area_dnode(n);
    }
    else {
      return;
    }
  }
#endif

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

#ifdef X8664
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
#endif
#ifdef X8632
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

      if (subtag == subtag_function) {
#ifdef X8632
	prefix_nodes = (natural) ((unsigned short) deref(base,1));

	/* XXX bootstrapping */
	if (prefix_nodes & 0x8000)
	  prefix_nodes = element_count - (prefix_nodes & 0x7fff);
#else
	prefix_nodes = (natural) ((int) deref(base,1));
#endif
        if (prefix_nodes > element_count) {
          Bug(NULL, "Function 0x" LISP " trashed",n);
        }
      }
      base += (1+element_count);

      element_count -= prefix_nodes;

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
#define RMARK_PREV_CAR fulltag_nil /* fulltag_cons + node_size. Coincidence ? I think not. */
#else
#define RMARK_PREV_ROOT fulltag_imm /* fulltag of 'undefined' value */
#define RMARK_PREV_CAR fulltag_odd_fixnum 
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

#ifdef X8632
  if (tag_n == fulltag_tra) {
    if (*(unsigned char *)n == RECOVER_FN_OPCODE) {
      n = *(LispObj *)(n + 1);
      tag_n = fulltag_misc;
      dnode = gc_area_dnode(n);
    } else {
      return;
    }
  }
#endif
#ifdef X8664
  if (tag_of(n) == tag_tra) {
    if ((*((unsigned short *)n) == RECOVER_FN_FROM_RIP_WORD0) &&
        (*((unsigned char *)(n+2)) == RECOVER_FN_FROM_RIP_BYTE2)) {
      int sdisp = (*(int *) (n+3));
      n = RECOVER_FN_FROM_RIP_LENGTH+n+sdisp;
      tag_n = fulltag_function;
      dnode = gc_area_dnode(n);
    } else {
      return;
    }
  }
#endif

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

#ifdef X8664
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

      nmark = element_count;

      if (subtag == subtag_function) {
#ifdef X8664
	int code_words = (int)base[1];
#else
	int code_words = (unsigned short)base[1];

	/* XXX bootstrapping */
	if (code_words & 0x8000)
	  code_words = element_count - (code_words & 0x7fff);
#endif
        if (code_words >= nmark) {
          Bug(NULL,"Bad function at 0x" LISP,n);
        }
	nmark -= code_words;
      }

      while (nmark--) {
        rmark(deref(n,element_count));
        element_count--;
      }

      if (subtag == subtag_weak) {
        deref(n, 1) = GCweakvll;
        GCweakvll = untag(n);
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
#ifdef X8664
    case tag_symbol:
    case fulltag_symbol:
    case tag_function:
    case fulltag_function:
#endif
      goto ClimbVector;

    case RMARK_PREV_ROOT:
      return;

    case fulltag_cons:
      goto ClimbCdr;

    case RMARK_PREV_CAR:
      goto ClimbCar;

    default: abort();
    }

  DescendCons:
    prev = this;
    this = next;

  MarkCons:
    next = deref(this,1);
#ifdef X8632
    this += (RMARK_PREV_CAR-fulltag_cons);
#else
    this += node_size;
#endif
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
#ifdef X8632
    this -= (RMARK_PREV_CAR-fulltag_cons);
#else
    this -= node_size;
#endif
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
#ifdef X8664
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
#else
    if (tag_n == fulltag_tra) {
      LispObj fn = *(LispObj *)(n + 1);

      base = (LispObj *)untag(fn);
      header = *(natural *)base;
      subtag = header_subtag(header);
      boundary = base + imm_word_count(fn);

      /*
       * On x8632, the upper 24 bits of the boundary word are zero.
       * Functions on x8632 can be no more than 2^16 words (or 2^24
       * bytes) long (including the self-reference table but excluding
       * any constants).  Therefore, we can do the same basic thing
       * that the x8664 port does: namely, we keep the byte
       * displacement from the address of the object (tagged tra or
       * fulltag_misc) that references the function to the address of
       * the boundary marker in those 24 bits, recovering it when
       * we've finished marking the function vector.
       */
      *((int *)boundary) &= 0xff;
      *((int *)boundary) |= ((this-(LispObj)boundary) << 8);
      this = (LispObj)(base)+fulltag_misc;
      dnode = gc_area_dnode(this);
      set_bit(markbits,dnode);
    } else {
      base = (LispObj *) ptr_from_lispobj(untag(this));
      header = *((natural *) base);
      subtag = header_subtag(header);
      if (subtag == subtag_function) {
        boundary = base + imm_word_count(this);

	*((int *)boundary) &= 0xff;
        *((int *)boundary) |= ((this-((LispObj)boundary)) << 8);
      }
    }
    element_count = header_element_count(header);
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

    this = (LispObj)(base) + (tag_of(this))  + ((element_count+1) << node_shift);
    goto MarkVectorLoop;

  ClimbVector:
    prev = indirect_node(this);
    indirect_node(this) = next;

  MarkVectorLoop:
    this -= node_size;
    next = indirect_node(this);
#ifdef X8664
    if ((tag_of(this) == tag_function) &&
        (header_subtag(next) == function_boundary_marker)) goto MarkFunctionDone;
#else
    if ((tag_of(this) == tag_misc) &&
        (header_subtag(next) == function_boundary_marker)) goto MarkFunctionDone;
#endif

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
      GCweakvll = untag(this);
    }
    goto Climb;

  MarkFunctionDone:
    boundary = (LispObj *)(node_aligned(this));
#ifdef X8664
    this = ((LispObj)boundary) + (((int *)boundary)[1]);
    (((int *)boundary)[1]) = 0;
#else
    this = ((LispObj)boundary) + ((*((int *)boundary)) >> 8);
    ((int *)boundary)[0] &= 0xff;
#endif
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


#ifdef X8664
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
  LispObj x1, *base = start, *prev = start;
  int tag;
  natural ref_dnode, node_dnode;
  Boolean intergen_ref, lenient_next_dnode = false, lenient_this_dnode = false;

  while (start < end) {
    x1 = *start;
    prev = start;
    tag = fulltag_of(x1);
    if (immheader_tag_p(tag)) {
      start = skip_over_ivector(ptr_to_lispobj(start), x1);
    } else {
      if (header_subtag(x1) == subtag_function) {
#ifdef X8632
        int skip = (unsigned short)deref(start,1);
        /* XXX bootstrapping */
        if (skip & 0x8000)
          skip = header_element_count(x1) - (skip & 0x7fff);
#else
        int skip = (int) deref(start,1);
#endif
        start += ((1+skip)&~1);
        x1 = *start;
        tag = fulltag_of(x1);
      } else {
        if (header_subtag(x1) == subtag_weak) {
          lenient_next_dnode = true;
        }
      }
      intergen_ref = false;
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
        }
      }
      start += 2;
      if (lenient_next_dnode) {
        lenient_this_dnode = true;
      }
      lenient_next_dnode = false;
    }
  }
  if (start > end) {
    Bug(NULL, "Overran end of range!");
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
      if (subtag == subtag_function) {
#ifdef X8632
	natural skip = (unsigned short)start[1];

	/* XXX bootstrapping */
	if (skip & 0x8000)
	  skip = element_count - (skip & 0x7fff);

	element_count -= skip;

#else
	element_count -= (int)start[1];
#endif
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
  fprintf(dbgout, "mark VSP range: 0x" LISP ":0x" LISP "\n", start, end);
#endif
  mark_headerless_area_range(start, end);
}

/* No lisp objects on cstack on x86, at least x86-64 */
void
mark_cstack_area(area *a)
{
}


/* Mark the lisp objects in an exception frame */
#ifdef X8664
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
#else
void
mark_xp(ExceptionInformation *xp, natural node_regs_mask)
{
  natural *regs = (natural *) xpGPRvector(xp), dnode;
  LispObj eip;

  if (node_regs_mask & (1<<0)) mark_root(regs[REG_EAX]);
  if (node_regs_mask & (1<<1)) mark_root(regs[REG_ECX]);
  if (regs[REG_EFL] & EFL_DF) {
    /* DF set means EDX should be treated as an imm reg */
    ;
  } else
    if (node_regs_mask & (1<<2)) mark_root(regs[REG_EDX]);

  if (node_regs_mask & (1<<3)) mark_root(regs[REG_EBX]);
  if (node_regs_mask & (1<<4)) mark_root(regs[REG_ESP]);
  if (node_regs_mask & (1<<5)) mark_root(regs[REG_EBP]);
  if (node_regs_mask & (1<<6)) mark_root(regs[REG_ESI]);
  if (node_regs_mask & (1<<7)) mark_root(regs[REG_EDI]);

  /* If the EIP isn't pointing into a marked function, we're probably
     in trouble.  We can -maybe- recover from that if it's tagged as a
     TRA. */
  eip = regs[Ieip];
  dnode = gc_area_dnode(eip);
  if ((dnode < GCndnodes_in_area) &&
      (! ref_bit(GCmarkbits,dnode))) {
    if (fulltag_of(eip) == fulltag_tra) {
      mark_root(eip);
    } else if ((fulltag_of(eip) == fulltag_misc) &&
               (header_subtag(header_of(eip)) == subtag_function) &&
               (*(unsigned char *)eip == RECOVER_FN_OPCODE) &&
	       (*(LispObj *)(eip + 1)) == eip) {
      mark_root(eip);
    } else {
      Bug(NULL, "Can't find function for eip 0x%4x", eip);
    }
  }
}
#endif

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
#ifdef X8664
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
#ifdef X8632
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
  /* On little-endian x86, we have to flip the low bit of dnode>>4 to
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
#endif
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
        natural skip = hash_table_vector_header_count-1;
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
#ifdef X8632
	  int skip = (unsigned short)(p[1]);

	  /* XXX bootstrapping */
	  if (skip & 0x8000)
	    skip = header_element_count(node) - (skip & 0x7fff);

#else
	  int skip = (int)(p[1]);
#endif
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

#ifdef X8664
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
#else
void
forward_xp(ExceptionInformation *xp, natural node_regs_mask)
{
  natural *regs = (natural *) xpGPRvector(xp);

  if (node_regs_mask & (1<<0)) update_noderef(&regs[REG_EAX]);
  if (node_regs_mask & (1<<1)) update_noderef(&regs[REG_ECX]);

  if (regs[REG_EFL] & EFL_DF) {
    /* then EDX is an imm reg */
    ;
  } else
    if (node_regs_mask & (1<<2)) update_noderef(&regs[REG_EDX]);

  if (node_regs_mask & (1<<3)) update_noderef(&regs[REG_EBX]);
  if (node_regs_mask & (1<<4)) update_noderef(&regs[REG_ESP]);
  if (node_regs_mask & (1<<5)) update_noderef(&regs[REG_EBP]);
  if (node_regs_mask & (1<<6)) update_noderef(&regs[REG_ESI]);
  if (node_regs_mask & (1<<7)) update_noderef(&regs[REG_EDI]);

  update_locref(&(regs[Iip]));
}
#endif


void
forward_tcr_xframes(TCR *tcr)
{
  xframe_list *xframes;
  ExceptionInformation *xp;

  xp = TCR_AUX(tcr)->gc_context;
  if (xp) {
#ifdef X8664
    forward_xp(xp);
#else
    forward_xp(xp, tcr->node_regs_mask);

    update_noderef(&tcr->save0);
    update_noderef(&tcr->save1);
    update_noderef(&tcr->save2);
    update_noderef(&tcr->save3);
    update_noderef(&tcr->next_method_context);
#endif
  }
  for (xframes = tcr->xframe; xframes; xframes = xframes->prev) {
#ifdef X8664
    forward_xp(xframes->curr);
#else
    forward_xp(xframes->curr, xframes->node_regs_mask);
#endif
  }
}


#ifdef X8632
void
update_self_references(LispObj *node)
{
  LispObj fn = fulltag_misc + (LispObj)node;
  unsigned char *p = (unsigned char *)node;
  natural i = imm_word_count(fn);

  if (i) {
    natural offset = node[--i];

    while (offset) {
      *(LispObj *)(p + offset) = fn;
      offset = node[--i];
    }
  }    
}

void
update_self_references_in_range(LispObj *start, LispObj *end)
{
  LispObj x1;
  int tag;
  
  while (start < end) {
    x1 = *start;
    tag = fulltag_of(x1);
    if (immheader_tag_p(tag)) {
      start = (LispObj *)ptr_from_lispobj(skip_over_ivector(ptr_to_lispobj(start), x1));
    } else if (!nodeheader_tag_p(tag)) {
      start += 2;
    } else {
      natural element_count = header_element_count(x1);
      natural size = (element_count+1+1) &~1;

      if (header_subtag(x1) == subtag_function) {
        update_self_references(start);
      }
      start += size;
    }
  }
}

#endif

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
            Bug(NULL, "Out of synch in heap compaction.  Forwarding from 0x" LISP " to 0x" LISP ",\n expected to go to 0x" LISP "\n", 
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
#ifdef X8632
	    LispObj *f = dest;
	    int skip = imm_word_count(fulltag_misc + (LispObj)current);
#else
	    int skip = *((int *)src);
#endif
	    *dest++ = node;
            if (skip) {
              elements -= skip;
              while(skip--) {
                *dest++ = *src++;
              }
#ifdef X8632
              update_self_references(f);
#endif
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

#ifdef X8664
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
#endif
#ifdef X8632
          if (tag <= max_32_bit_ivector_subtag) {
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


#define PURIFY_IVECTORS (1<<0)
#define PURIFY_FUNCTIONS (1<<1)
#define PURIFY_ALL (-1)
#define PURIFY_NOTHING (0)      /* update forwarding pointers, don't copy */



Boolean
immutable_function_p(LispObj thing)
{
  LispObj header = header_of(thing), lfbits;
  if (header_subtag(header) == subtag_function) {
    lfbits = deref(thing,header_element_count(header));
    if (((lfbits & (lfbits_cm_mask | lfbits_method_mask)) !=
         lfbits_cm_mask) &&
        ((lfbits & (lfbits_gfn_mask | lfbits_method_mask)) !=
         lfbits_gfn_mask) &&
        ((lfbits & lfbits_trampoline_mask) == 0)) {
      return true;
    }
  }
  return false;
}

    
/*
  Total the (physical) byte sizes of all ivectors in the indicated memory range
*/

natural
unboxed_bytes_in_range(LispObj *start, LispObj *end, Boolean include_functions)
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
        if (include_functions && immutable_function_p((LispObj)start)) {
          total += (((elements+2)&~1)<<node_shift);
        }
        start += ((elements+2) & ~1);
      } else {
        subtag = header_subtag(header);

#ifdef X8664
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
#endif
#ifdef X8632
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


void
ensure_writable_space(area *target, natural need)
{
  BytePtr
    oldlimit = (BytePtr)align_to_power_of_2(target->active,log2_page_size),
    newlimit = (BytePtr)align_to_power_of_2(target->active+need,log2_page_size);
  if (newlimit > oldlimit) {
    CommitMemory(oldlimit,newlimit-oldlimit);
  }
}

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
  int
    header_tag = fulltag_of(header);
#ifdef X8632
  Boolean
    is_function = (header_subtag(header)==subtag_function);
#endif

  if (1) {
    if ((header_subtag(header) == subtag_weak) ||
        (header_subtag(header) == subtag_hash_vector)) {
      Bug(NULL, "purifying weak vector "LISP "\n", obj);
    }
  }

  if (immheader_tag_p(header_tag)) {
    physbytes = ((natural)(skip_over_ivector(start,header))) - start;
  } else if (nodeheader_tag_p(header_tag)) {
    physbytes = ((header_element_count(header)+2)&~1) << node_shift;
  } else {
    physbytes = dnode_size;
  }
  
  ensure_writable_space(dest, physbytes);
  dest->active += physbytes;

  new = ptr_to_lispobj(free)+disp;

  memcpy(free, (BytePtr)old, physbytes);

#ifdef X8632
  if (is_function) {
    update_self_references((LispObj *)free);
  }
#endif


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

Boolean
purify_locref(LispObj *ref,  BytePtr low, BytePtr high, area *dest, int what)
{
  LispObj obj = *ref, header, new;
  natural tag = fulltag_of(obj), header_tag;
  Boolean changed = false;

  if ((((BytePtr)ptr_from_lispobj(obj)) > low) &&
      (((BytePtr)ptr_from_lispobj(obj)) < high)) {
    header = deref(obj, 0);
    if (header == forward_marker) { /* already copied */
      *ref = (untag(deref(obj,1)) + tag);
      changed = true;
    } else {
      header_tag = fulltag_of(header);
      if ((what == PURIFY_ALL) ||
          ((what & PURIFY_IVECTORS) &&
           immheader_tag_p(header_tag) &&
           header_subtag(header) != subtag_macptr) ||
          ((what & PURIFY_FUNCTIONS) &&
           immutable_function_p(obj))) {
        new = purify_object(obj, dest);
        *ref = new;
        changed = (new != obj);
      }
    }
  }
  return changed;
}

Boolean
purify_noderef(LispObj *ref,  BytePtr low, BytePtr high, area *dest, int what)
{
  LispObj obj = *ref;
  natural tag = fulltag_of(obj);

  if (is_node_fulltag(tag)) {
    return purify_locref(ref,low,high,dest,what);
  }
  return false;
}


Boolean
copy_reference(LispObj *ref, BytePtr low, BytePtr high, area *dest, int what, Boolean recursive)
{
  LispObj obj = *ref, *scan = NULL;
  natural tag = fulltag_of(obj);

  if (recursive) {
    scan = (LispObj *)(dest->active);
  }

  if (
#ifdef X8664
      (tag == fulltag_tra_0) || (tag == fulltag_tra_1)
#endif
#ifdef X8632
      tag == fulltag_tra
#endif
      ) {
    what = PURIFY_NOTHING;
  }
  if (is_node_fulltag(tag)) {
    if (purify_locref(ref,low,high,dest,what)) {
      if (recursive) {
        LispObj header;
        unsigned tag;
        natural nwords;
        
        while (scan < (LispObj *)(dest->active)) {
          header = *scan;
          tag = fulltag_of(header);
          if (immheader_tag_p(tag)) {
            scan = (LispObj *)skip_over_ivector((natural)scan, header);
          } else if (nodeheader_tag_p(tag)) {
            if ((header_subtag(header) == subtag_simple_vector) ||
#if 0
                false
#else
                (header_subtag(header) == subtag_struct)
#endif
                ) {
              scan++;
              purify_noderef(scan,low,high,dest,what);
              scan++;
            } else {
              nwords = (header_element_count(header)+2)&~1;
              scan += nwords;
            }
          } else {
            purify_noderef(scan,low,high,dest,what);
            scan++;
            purify_noderef(scan,low,high,dest,what);
            scan++;
          }
        }            
      }
    }
    return true;
  }
  return false;
}



void
purify_gcable_ptrs(BytePtr low, BytePtr high, area *to, int what)
{
  LispObj *prev = &(lisp_global(GCABLE_POINTERS)), next;

  while ((*prev) != (LispObj)NULL) {
    copy_reference(prev, low, high, to, what, false);
    next = *prev;
    prev = &(((xmacptr *)ptr_from_lispobj(untag(next)))->link);
  }
}

void 
purify_headerless_range(LispObj *start, LispObj *end, BytePtr low, BytePtr high, area *to, int what)
{
  while (start < end) { 
    copy_reference(start, low, high, to, what, false);
    start++;
  }
}
   
void
purify_range(LispObj *start, LispObj *end, BytePtr low, BytePtr high, area *to, int what, Boolean recursive)
{
  LispObj header, pfn;
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
        if ((what & PURIFY_IVECTORS) &&
            (to == readonly_area) &&
            (header_subtag(header) != subtag_macptr) &&
            ((BytePtr)start >= low) &&
            ((BytePtr)start < high)) {
          purify_object((LispObj)start,to);
        }
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
            copy_reference(start, low, high, to, what, recursive);
            start++;
          }
          /* "nwords" is odd at this point: there are (floor nwords 2)
             key/value pairs to look at, and then an extra word for
             alignment.  Process them two at a time, then bump "start"
             past the alignment word. */
          nwords >>= 1;
          while(nwords--) {

            if (copy_reference(start, low, high, to, what, recursive) 
                && hashp) {
              hashp->flags |= nhash_key_moved_mask;
              hashp = NULL;
            }
            start++;
            copy_reference(start, low, high, to, what, recursive);
            start++;
          }
          *start++ = 0;
        } else {
          pfn = 0;
          if (header_subtag(header) == subtag_function) {
#ifdef X8632
            int skip = (unsigned short)(start[1]);

	    /* XXX bootstrapping */
	    if (skip & 0x8000)
	      skip = header_element_count(header) - (skip & 0x7fff);
#else
            int skip = (int)(start[1]);
#endif
            pfn = (LispObj)start;
            start += skip;
            nwords -= skip;
          }
          start++;
          while(nwords--) {
            copy_reference(start, low, high, to, what, recursive);
            start++;
          }
          if (((BytePtr)pfn >= low) &&
              ((BytePtr)pfn < high) &&
              (what & PURIFY_FUNCTIONS) &&
              (to == readonly_area) &&
              (immutable_function_p(pfn))) {
            purify_object(pfn, to);
          }
        }
      } else {
        /* Not a header, just a cons cell */
        copy_reference(start, low, high, to, what, recursive);
        start++;
        copy_reference(start, low, high, to, what, recursive);
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
    purify_range(current+2, end, low, high, to, what, false);
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
purify_xp(ExceptionInformation *xp, BytePtr low, BytePtr high, area *to, int what
#ifdef X8632
          ,natural node_regs_mask
#endif
)
{
  natural *regs = (natural *) xpGPRvector(xp);


#ifdef X8664
  copy_reference(&(regs[Iarg_z]), low, high, to, what, false);
  copy_reference(&(regs[Iarg_y]), low, high, to, what, false);
  copy_reference(&(regs[Iarg_x]), low, high, to, what, false);
  copy_reference(&(regs[Isave3]), low, high, to, what, false);
  copy_reference(&(regs[Isave2]), low, high, to, what, false);
  copy_reference(&(regs[Isave1]), low, high, to, what, false);
  copy_reference(&(regs[Isave0]), low, high, to, what, false);
  copy_reference(&(regs[Ifn]), low, high, to, what, false);
  copy_reference(&(regs[Itemp0]), low, high, to, what, false);
  copy_reference(&(regs[Itemp1]), low, high, to, what, false);
  copy_reference(&(regs[Itemp2]), low, high, to, what, false);

  purify_locref(&(regs[Iip]), low, high, to, PURIFY_NOTHING);

#else
  if (node_regs_mask & (1<<0)) {
    copy_reference(&(regs[REG_EAX]), low, high, to, what, false);
  }
  if (node_regs_mask & (1<<1)) {
    copy_reference(&(regs[REG_ECX]), low, high, to, what, false);
  }
  if (! (regs[REG_EFL] & EFL_DF)) {
    if (node_regs_mask & (1<<2)) {
      copy_reference(&(regs[REG_EDX]), low, high, to, what, false);
    }
  }
  if (node_regs_mask & (1<<3)) {
    copy_reference(&(regs[REG_EBX]), low, high, to, what, false);
  }
  if (node_regs_mask & (1<<4)) {
    copy_reference(&(regs[REG_ESP]), low, high, to, what, false);
  }
  if (node_regs_mask & (1<<5)) {
    copy_reference(&(regs[REG_EBP]), low, high, to, what, false);
  }
  if (node_regs_mask & (1<<6)) {
    copy_reference(&(regs[REG_ESI]), low, high, to, what, false);
  }
  if (node_regs_mask & (1<<7)) {
    copy_reference(&(regs[REG_EDI]), low, high, to, what, false);
  }
  purify_locref(&regs[REG_EIP], low, high, to, what);
#endif
}

void
purify_tcr_tlb(TCR *tcr, BytePtr low, BytePtr high, area *to, int what)
{
  natural n = tcr->tlb_limit;
  LispObj *start = tcr->tlb_pointer, *end = (LispObj *) ((BytePtr)start+n);

  purify_range(start, end, low, high, to, what, false);
}

void
purify_tcr_xframes(TCR *tcr, BytePtr low, BytePtr high, area *to, int what)
{
  xframe_list *xframes;
  ExceptionInformation *xp;
  
  xp = TCR_AUX(tcr)->gc_context;
  if (xp) {
#ifdef X8632
    purify_xp(xp, low, high, to, what, tcr->node_regs_mask);
#else
    purify_xp(xp, low, high, to, what);
#endif
  }
#ifdef X8632
  copy_reference(&tcr->save0, low, high, to, what, false);
  copy_reference(&tcr->save1, low, high, to, what, false);
  copy_reference(&tcr->save2, low, high, to, what, false);
  copy_reference(&tcr->save3, low, high, to, what, false);
  copy_reference(&tcr->next_method_context, low, high, to, what, false);
#endif

  for (xframes = tcr->xframe; xframes; xframes = xframes->prev) {
    purify_xp(xframes->curr, low, high, to, what
#ifdef X8632
              , xframes->node_regs_mask
#endif
              );
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
      break;
      
    case AREA_STATIC:
    case AREA_DYNAMIC:
    case AREA_MANAGED_STATIC:
      purify_range((LispObj *) next_area->low, (LispObj *) next_area->active, low, high, target, what, false);
      break;
      
    default:
      break;
    }
  }
}

void
update_managed_refs(area *a, BytePtr low_dynamic_address, natural ndynamic_dnodes)
{
  LispObj 
    *start = (LispObj *)a->low,
    *end = (LispObj *)a->active,
    x1, 
    *base = start, *prev = start;
  int tag;
  bitvector refbits = managed_static_refbits;
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
#ifdef X8632
	int skip = (unsigned short)deref(start,1);
	/* XXX bootstrapping */
	if (skip & 0x8000)
	  skip = header_element_count(x1) - (skip & 0x7fff);
#else
        int skip = (int) deref(start,1);
#endif
        start += ((1+skip)&~1);
        x1 = *start;
        tag = fulltag_of(x1);
      }
      intergen_ref = false;
      if (is_node_fulltag(tag)) {        
        node_dnode = area_dnode(x1, low_dynamic_address);
        if (node_dnode < ndynamic_dnodes) {
          intergen_ref = true;
        }
      }
      if (intergen_ref == false) {        
        x1 = start[1];
        tag = fulltag_of(x1);
        if (is_node_fulltag(tag)) {        
          node_dnode = area_dnode(x1, low_dynamic_address);
          if (node_dnode < ndynamic_dnodes) {
            intergen_ref = true;
          }
        }
      }
      if (intergen_ref) {
        ref_dnode = area_dnode(start, base);
        set_bit(refbits, ref_dnode);
      }
      start += 2;
    }
  }
  if (start > end) {
    Bug(NULL, "Overran end of range!");
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
  extern area *extend_readonly_area(natural);
  area 
    *a = active_dynamic_area,
    *pure_area;

  TCR  *other_tcr;
  natural max_pure_size;
  BytePtr new_pure_start,
    low = (a->low + (static_dnodes_for_area(a) << dnode_shift)),
    high = a->active;
  Boolean purify_functions = (param != 0);
  int flags = PURIFY_IVECTORS | (purify_functions ? PURIFY_FUNCTIONS : 0);

  max_pure_size = unboxed_bytes_in_range((LispObj *) low, (LispObj *) high, purify_functions);
  pure_area = extend_readonly_area(max_pure_size);
  if (pure_area) {
    new_pure_start = pure_area->active;
    lisp_global(IN_GC) = (1<<fixnumshift);

    /* 
      Caller will typically GC again (and that should recover quite a bit of
      the dynamic heap.)
      */


    
    purify_areas(low, high, pure_area, flags);
    
    other_tcr = tcr;
    do {
      purify_tcr_xframes(other_tcr, low, high, pure_area, flags);
      purify_tcr_tlb(other_tcr, low, high, pure_area, flags);
      other_tcr = TCR_AUX(other_tcr)->next;
    } while (other_tcr != tcr);

    purify_gcable_ptrs(low, high, pure_area, flags);
    if (purify_functions) {
      /* We're likely to copy a lot of symbols to the managed static
         area.  Lots of symbols will have incidental references to
         a relatively small number of things that happen to initialy
         be in dynamic space: the UNDEFINED-FUNCTION object, packages,
         etc.  Doing a shallow copy of those things to the managed-static
         area will reduce the number of static->dynamic references. */
      LispObj package_list;

      copy_reference(&nrs_UDF.vcell,low,high,managed_static_area,PURIFY_ALL, false);
      for (package_list = nrs_ALL_PACKAGES.vcell;
           package_list != lisp_nil;
           package_list = deref(package_list,0)) {
        copy_reference(&(deref(package_list,1)),low,high,managed_static_area,PURIFY_ALL, false);
      }

        

      /* Do a shallow copy of the constants of all purified functions
         from the dynamic area to the managed static area */
      purify_range((LispObj*)(pure_area->low),
                   (LispObj*)(pure_area->active),
                   low,
                   high,
                   managed_static_area,
                   PURIFY_ALL,
                   true);
      /* Go back through all areas, resolving forwarding pointers
         (but without copying anything.) */
      purify_areas(low, high, NULL, PURIFY_NOTHING);
      other_tcr = tcr;
      do {
        purify_tcr_xframes(other_tcr, low, high, NULL, PURIFY_NOTHING);
        purify_tcr_tlb(other_tcr, low, high, NULL, PURIFY_NOTHING);
        other_tcr = TCR_AUX(other_tcr)->next;
      } while (other_tcr != tcr);
      
      purify_gcable_ptrs(low, high, NULL, PURIFY_NOTHING);

      /* Update refbits for managed static area */
      {
        natural 
          managed_dnodes = area_dnode(managed_static_area->active,
                                      managed_static_area->low),
          refbytes = align_to_power_of_2((managed_dnodes+7)>>3,log2_page_size);
        
        managed_static_area->ndnodes = managed_dnodes;
        lisp_global(MANAGED_STATIC_DNODES) = managed_dnodes;
        CommitMemory(managed_static_area->refbits, refbytes); /* zeros them */
        CommitMemory(managed_static_refbits,refbytes); /* zeroes them, too */
        update_managed_refs(managed_static_area, low_markable_address, area_dnode(a->active,low_markable_address));
      }
      managed_static_area->high = managed_static_area->active;
    }
    ProtectMemory(pure_area->low,
		  align_to_power_of_2(pure_area->active-pure_area->low,
				      log2_page_size));
    lisp_global(IN_GC) = 0;
    just_purified_p = true;
    return 0;
  }
  return -1;
}

Boolean
impurify_locref(LispObj *p, LispObj low, LispObj high, signed_natural delta)
{
  LispObj q = *p;

  if ((q >= low) && 
      (q < high)) {
    *p = (q+delta);
    return true;
  }
  return false;
}
  
Boolean
impurify_noderef(LispObj *p, LispObj low, LispObj high, signed_natural delta)
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
impurify_gcable_ptrs(LispObj low, LispObj high, signed_natural delta)
{
  LispObj *prev = &(lisp_global(GCABLE_POINTERS)), next;

  while ((*prev) != (LispObj)NULL) {
    impurify_noderef(prev, low, high, delta);
    next = *prev;
    prev = &(((xmacptr *)ptr_from_lispobj(untag(next)))->link);
  }
}


void
impurify_xp(ExceptionInformation *xp, LispObj low, LispObj high, signed_natural delta
#ifdef X8632
            ,natural node_regs_mask
#endif
)
{
  natural *regs = (natural *) xpGPRvector(xp);


#ifdef X8664
  impurify_noderef(&(regs[Iarg_z]), low, high, delta);
  impurify_noderef(&(regs[Iarg_y]), low, high, delta);
  impurify_noderef(&(regs[Iarg_x]), low, high, delta);
#ifndef TCR_IN_GPR
  impurify_noderef(&(regs[Isave3]), low, high, delta);
#endif
  impurify_noderef(&(regs[Isave2]), low, high, delta);
  impurify_noderef(&(regs[Isave1]), low, high, delta);
  impurify_noderef(&(regs[Isave0]), low, high, delta);
  impurify_noderef(&(regs[Ifn]), low, high, delta);
  impurify_noderef(&(regs[Itemp0]), low, high, delta);
  impurify_noderef(&(regs[Itemp1]), low, high, delta);

  impurify_locref(&(regs[Iip]), low, high, delta);
#else
  if (node_regs_mask & (1<<0)) {
    impurify_noderef(&(regs[REG_EAX]), low, high, delta);
  }
  if (node_regs_mask & (1<<1)) {
    impurify_noderef(&(regs[REG_ECX]), low, high, delta);
  }
  if (! (regs[REG_EFL] & EFL_DF)) {
    if (node_regs_mask & (1<<2)) {
      impurify_noderef(&(regs[REG_EDX]), low, high, delta);
    }
  }
  if (node_regs_mask & (1<<3)) {
    impurify_noderef(&(regs[REG_EBX]), low, high, delta);
  }
  if (node_regs_mask & (1<<4)) {
    impurify_noderef(&(regs[REG_ESP]), low, high, delta);
  }
  if (node_regs_mask & (1<<5)) {
    impurify_noderef(&(regs[REG_EBP]), low, high, delta);
  }
  if (node_regs_mask & (1<<6)) {
    impurify_noderef(&(regs[REG_ESI]), low, high, delta);
  }
  if (node_regs_mask & (1<<7)) {
    impurify_noderef(&(regs[REG_EDI]), low, high, delta);
  }
  impurify_locref(&(regs[REG_EIP]), low, high, delta);

#endif

}

void
impurify_headerless_range(LispObj *start, LispObj *end, LispObj low, LispObj high, signed_natural delta)
{
  while (start < end) {
    impurify_noderef(start, low, high, delta);
    start++;
  }
}


void
impurify_range(LispObj *start, LispObj *end, LispObj low, LispObj high, signed_natural delta)
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
#ifdef X8632
	    int skip = (unsigned short)start[1];
#else
            int skip = (int)(start[1]);
#endif
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
impurify_tcr_tlb(TCR *tcr,  LispObj low, LispObj high, signed_natural delta)
{
  unsigned n = tcr->tlb_limit;
  LispObj *start = tcr->tlb_pointer, *end = (LispObj *) ((BytePtr)start+n);
  
  impurify_range(start, end, low, high, delta);
}

void
impurify_tcr_xframes(TCR *tcr, LispObj low, LispObj high, signed_natural delta)
{
  xframe_list *xframes;
  ExceptionInformation *xp;
  
  xp = TCR_AUX(tcr)->gc_context;
  if (xp) {
#ifdef X8632
    impurify_xp(xp, low, high, delta, tcr->node_regs_mask);
#else
    impurify_xp(xp, low, high, delta);
#endif
  }

#ifdef X8632
  impurify_noderef(&tcr->save0, low, high, delta);
  impurify_noderef(&tcr->save1, low, high, delta);
  impurify_noderef(&tcr->save2, low, high, delta);
  impurify_noderef(&tcr->save3, low, high, delta);
  impurify_noderef(&tcr->next_method_context, low, high, delta);
#endif

  for (xframes = tcr->xframe; xframes; xframes = xframes->prev) {
    impurify_xp(xframes->curr, low, high, delta
#ifdef X8632
                ,xframes->node_regs_mask
#endif
);
  }
}

void
impurify_tstack_area(area *a, LispObj low, LispObj high, signed_natural delta)
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
    impurify_range(current+2, end, low, high, delta);
  }
}
void
impurify_vstack_area(area *a, LispObj low, LispObj high, signed_natural delta)
{
  LispObj
    *p = (LispObj *) a->active,
    *q = (LispObj *) a->high;

  impurify_headerless_range(p, q, low, high, delta);
}


void
impurify_areas(LispObj low, LispObj high, signed_natural delta)
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
      break;
      
    case AREA_STATIC:
    case AREA_DYNAMIC:
    case AREA_MANAGED_STATIC:
      impurify_range((LispObj *) next_area->low, (LispObj *) next_area->active, low, high, delta);
      break;
      
    default:
      break;
    }
  }
}

void
impurify_from_area(TCR *tcr, area *src)
{
  area *a = active_dynamic_area;
  BytePtr base = src->low, limit = src->active, oldfree = a->active,
    oldhigh = a->high, newhigh;
  natural n = limit-base;
  signed_natural delta = oldfree-base;
  TCR *other_tcr;

  newhigh = (BytePtr) (align_to_power_of_2(oldfree+n,
                                           log2_heap_segment_size));
  if (newhigh > oldhigh) {
    grow_dynamic_area(newhigh-oldhigh);
  }
  a->active += n;
  memmove(oldfree, base, n);
  UnCommitMemory((void *)base, n);
#ifdef X8632
  update_self_references_in_range((LispObj *)oldfree,(LispObj *)(oldfree+n));
#endif
  a->ndnodes = area_dnode(a, a->active);
  src->active = src->low;
  if (src == readonly_area) {
    pure_space_active = src->low;
  }
  src->ndnodes = 0;
  
  impurify_areas(ptr_to_lispobj(base), ptr_to_lispobj(limit), delta);
  
  other_tcr = tcr;
  do {
    impurify_tcr_xframes(other_tcr, ptr_to_lispobj(base), ptr_to_lispobj(limit), delta);
    impurify_tcr_tlb(other_tcr, ptr_to_lispobj(base), ptr_to_lispobj(limit), delta);
    other_tcr = TCR_AUX(other_tcr)->next;
  } while (other_tcr != tcr);
  
  impurify_gcable_ptrs(ptr_to_lispobj(base), ptr_to_lispobj(limit), delta);
}

signed_natural
impurify(TCR *tcr, signed_natural param)
{
  lisp_global(IN_GC)=1;
  impurify_from_area(tcr, readonly_area);
  impurify_from_area(tcr, managed_static_area);
  lisp_global(MANAGED_STATIC_DNODES)=0;
  lisp_global(IN_GC)=0;
  return 0;
}

/*
 * This stuff is all adapted from the forward_xxx functions for use by
 * the watchpoint code.  It's a lot of duplicated code, and it would
 * be nice to generalize it somehow.
 */

static inline int
wp_maybe_update(LispObj *p, LispObj old, LispObj new)
{
  if (*p == old) {
    *p = new;
    return true;
  }
  return false;
}

static void
wp_update_headerless_range(LispObj *start, LispObj *end,
			   LispObj old, LispObj new)
{
  LispObj *p = start;

  while (p < end) {
    wp_maybe_update(p, old, new);
    p++;
  }
}

static void
wp_update_range(LispObj *start, LispObj *end, LispObj old, LispObj new)
{
  LispObj *p = start, node;
  int tag_n;
  natural nwords;

  while (p < end) {
    node = *p;
    tag_n = fulltag_of(node);

    if (immheader_tag_p(tag_n)) {
      p = (LispObj *)skip_over_ivector(ptr_to_lispobj(p), node);
    } else if (nodeheader_tag_p(tag_n)) {
      nwords = header_element_count(node);
      nwords += 1 - (nwords & 1);

      if ((header_subtag(node) == subtag_hash_vector) &&
          ((((hash_table_vector_header *)p)->flags) & nhash_track_keys_mask)) {
        natural skip = hash_table_vector_header_count - 1;
	hash_table_vector_header *hashp = (hash_table_vector_header *)p;

        p++;
        nwords -= skip;
        while(skip--) {
	  wp_maybe_update(p, old, new);
          p++;
        }
        /* "nwords" is odd at this point: there are (floor nwords 2)
           key/value pairs to look at, and then an extra word for
           alignment.  Process them two at a time, then bump "p"
           past the alignment word. */
        nwords >>= 1;
        while(nwords--) {
          if (wp_maybe_update(p, old, new) && hashp) {
            hashp->flags |= nhash_key_moved_mask;
            hashp = NULL;
          }
          p++;
	  wp_maybe_update(p, old, new);
          p++;
        }
        *p++ = 0;
      } else {
	if (header_subtag(node) == subtag_function) {
#ifdef X8632
	  int skip = (unsigned short)(p[1]);

	  /* XXX bootstrapping */
	  if (skip & 0x8000)
	    skip = header_element_count(node) - (skip & 0x7fff);

#else
	  int skip = (int)(p[1]);
#endif
	  p += skip;
	  nwords -= skip;
	}
        p++;
        while(nwords--) {
	  wp_maybe_update(p, old, new);
          p++;
        }
      }
    } else {
      /* a cons cell */
      wp_maybe_update(p, old, new);
      p++;
      wp_maybe_update(p, old, new);
      p++;
    }
  }
}

#ifdef X8664
static void
wp_update_xp(ExceptionInformation *xp, LispObj old, LispObj new)
{
  natural *regs = (natural *)xpGPRvector(xp);

  wp_maybe_update(&regs[Iarg_z], old, new);
  wp_maybe_update(&regs[Iarg_y], old, new);
  wp_maybe_update(&regs[Iarg_x], old, new);
  wp_maybe_update(&regs[Isave3], old, new);
  wp_maybe_update(&regs[Isave2], old, new);
  wp_maybe_update(&regs[Isave1], old, new);
  wp_maybe_update(&regs[Isave0], old, new);
  wp_maybe_update(&regs[Ifn], old, new);
  wp_maybe_update(&regs[Itemp0], old, new);
  wp_maybe_update(&regs[Itemp1], old, new);
  wp_maybe_update(&regs[Itemp2], old, new);

#if 0
  /* 
   * We don't allow watching functions, so this presumably doesn't
   * matter.
   */
  update_locref(&(regs[Iip]));
#endif
}
#else
static void
wp_update_xp(ExceptionInformation *xp, LispObj old, LispObj new, natural node_regs_mask)
{
  natural *regs = (natural *)xpGPRvector(xp);

  if (node_regs_mask & (1<<0)) wp_maybe_update(&regs[REG_EAX], old, new);
  if (node_regs_mask & (1<<1)) wp_maybe_update(&regs[REG_ECX], old, new);

  if (regs[REG_EFL] & EFL_DF) {
    /* then EDX is an imm reg */
    ;
  } else
    if (node_regs_mask & (1<<2)) wp_maybe_update(&regs[REG_EDX], old, new);

  if (node_regs_mask & (1<<3)) wp_maybe_update(&regs[REG_EBX], old, new);
  if (node_regs_mask & (1<<4)) wp_maybe_update(&regs[REG_ESP], old, new);
  if (node_regs_mask & (1<<5)) wp_maybe_update(&regs[REG_EBP], old, new);
  if (node_regs_mask & (1<<6)) wp_maybe_update(&regs[REG_ESI], old, new);
  if (node_regs_mask & (1<<7)) wp_maybe_update(&regs[REG_EDI], old, new);
  /* we shouldn't watch functions, so no need to update PC */
}
#endif

static void
wp_update_tcr_xframes(TCR *tcr, LispObj old, LispObj new)
{
  xframe_list *xframes;
  ExceptionInformation *xp;

  xp = TCR_AUX(tcr)->gc_context;
  if (xp) {
#ifdef X8664
    wp_update_xp(xp, old, new);
#else
    wp_update_xp(xp, old, new, tcr->node_regs_mask);
    wp_maybe_update(&tcr->save0, old, new);
    wp_maybe_update(&tcr->save1, old, new);
    wp_maybe_update(&tcr->save2, old, new);
    wp_maybe_update(&tcr->save3, old, new);
    wp_maybe_update(&tcr->next_method_context, old, new);
#endif
  }
  for (xframes = tcr->xframe; xframes; xframes = xframes->prev) {
#ifdef X8664
    wp_update_xp(xframes->curr, old, new);
#else
    wp_update_xp(xframes->curr, old, new, xframes->node_regs_mask);
#endif
  }
}

/*
 * Scan all pointer-bearing areas, updating all references to
 * "old" to "new".
 */
static void
wp_update_all_areas(LispObj old, LispObj new)
{
  area *a = active_dynamic_area;
  natural code = a->code;

  while (code != AREA_VOID) {
    switch (code) {
      case AREA_DYNAMIC:
      case AREA_STATIC:
      case AREA_MANAGED_STATIC:
      case AREA_WATCHED:
	wp_update_range((LispObj *)a->low, (LispObj *)a->active, old, new);
	break;
      case AREA_VSTACK:
      {
	LispObj *low = (LispObj *)a->active;
	LispObj *high = (LispObj *)a->high;
	
	wp_update_headerless_range(low, high, old, new);
      }
      break;
      case AREA_TSTACK:
      {
	LispObj *current, *next;
	LispObj *start = (LispObj *)a->active, *end = start;
	LispObj *limit = (LispObj *)a->high;
	
	for (current = start; end != limit; current = next) {
	  next = ptr_from_lispobj(*current);
	  end = ((next >= start) && (next < limit)) ? next : limit;
	  wp_update_range(current+2, end, old, new);
	}
      break;
      }
      default:
	break;
    }
    a = a->succ;
    code = a->code;
  }
}

static void
wp_update_tcr_tlb(TCR *tcr, LispObj old, LispObj new)
{
  natural n = tcr->tlb_limit;
  LispObj *start = tcr->tlb_pointer;
  LispObj *end = start + (n >> fixnumshift);

  while (start < end) {
    wp_maybe_update(start, old, new);
    start++;
  }
}

void
wp_update_references(TCR *tcr, LispObj old, LispObj new)
{
  TCR *other_tcr = tcr;

  do {
    wp_update_tcr_xframes(other_tcr, old, new);
    wp_update_tcr_tlb(other_tcr, old, new);
    other_tcr = TCR_AUX(other_tcr)->next;
  } while (other_tcr != tcr);
  unprotect_watched_areas();
  wp_update_all_areas(old, new);
  protect_watched_areas();
}
