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

#ifndef WINDOWS
#include <sys/time.h>
#endif

#ifndef timeradd
# define timeradd(a, b, result)						      \
  do {									      \
    (result)->tv_sec = (a)->tv_sec + (b)->tv_sec;			      \
    (result)->tv_usec = (a)->tv_usec + (b)->tv_usec;			      \
    if ((result)->tv_usec >= 1000000)					      \
      {									      \
	++(result)->tv_sec;						      \
	(result)->tv_usec -= 1000000;					      \
      }									      \
  } while (0)
#endif
#ifndef timersub
# define timersub(a, b, result)						      \
  do {									      \
    (result)->tv_sec = (a)->tv_sec - (b)->tv_sec;			      \
    (result)->tv_usec = (a)->tv_usec - (b)->tv_usec;			      \
    if ((result)->tv_usec < 0) {					      \
      --(result)->tv_sec;						      \
      (result)->tv_usec += 1000000;					      \
    }									      \
  } while (0)
#endif

void
comma_output_decimal(char *buf, int len, natural n) 
{
  int nout = 0;

  buf[--len] = 0;
  do {
    buf[--len] = n%10+'0';
    n = n/10;
    if (n == 0) {
      while (len) {
        buf[--len] = ' ';
      }
      return;
    }
    if (len == 0) return;
    nout ++;
    if (nout == 3) {
      buf[--len] = ',';
      nout = 0;
    }
  } while (len >= 0);
}


natural
static_dnodes_for_area(area *a)
{
  if (a->low == tenured_area->low) {
    return tenured_area->static_dnodes;
  }
  return 0;
}

Boolean GCDebug = false, GCverbose = false;
bitvector GCmarkbits = NULL, GCdynamic_markbits = NULL, managed_static_refbits = NULL;
LispObj GCarealow = 0, GCareadynamiclow = 0;
natural GCndnodes_in_area = 0, GCndynamic_dnodes_in_area = 0;
LispObj GCweakvll = (LispObj)NULL;
LispObj GCdwsweakvll = (LispObj)NULL;
LispObj GCephemeral_low = 0;
natural GCn_ephemeral_dnodes = 0;
natural GCstack_limit = 0;

void
check_static_cons_freelist(char *phase)
{
  LispObj 
    n,
    base = (LispObj)static_cons_area->low, 
    limit = static_cons_area->ndnodes;
  natural i=0;

  for (n=lisp_global(STATIC_CONSES);n!=lisp_nil;n=((cons *)untag(n))->cdr, i++) {
    if ((fulltag_of(n) != fulltag_cons) ||
        (area_dnode(n,base) >= limit)) {
      Bug(NULL, "%s: static cons freelist has invalid element 0x" LISP "\n",
          phase, i);
    }
  }
}

void
reapweakv(LispObj weakv)
{
  /*
    element 2 of the weak vector should be tagged as a cons: if it
    isn't, just mark it as a root.  if it is, cdr through it until a
    "marked" cons is encountered.  If the car of any unmarked cons is
    marked, mark the cons which contains it; otherwise, splice the
    cons out of the list.  N.B. : elements 0 and 1 are already marked
    (or are immediate, etc.)
  */
  LispObj *prev = ((LispObj *) ptr_from_lispobj(untag(weakv))+(1+2)), cell = *prev;
  LispObj termination_list = lisp_nil;
  natural weak_type = (natural) deref(weakv,2);
  Boolean alistp = ((weak_type & population_type_mask) == population_weak_alist),
    terminatablep = ((weak_type >> population_termination_bit) != 0);
  Boolean done = false;
  cons *rawcons;
  natural dnode, car_dnode;
  bitvector markbits = GCmarkbits;

  if (terminatablep) {
    termination_list = deref(weakv,1+3);
  }

  if (fulltag_of(cell) != fulltag_cons) {
    mark_root(cell);
  } else if (alistp) {
    /* weak alist */
    while (! done) {
      dnode = gc_area_dnode(cell);
      if ((dnode >= GCndnodes_in_area) ||
          (ref_bit(markbits, dnode))) {
        done = true;
      } else {
        /* Cons cell is unmarked. */
        LispObj alist_cell, thecar;
        unsigned cell_tag;

        rawcons = (cons *) ptr_from_lispobj(untag(cell));
        alist_cell = rawcons->car;
        cell_tag = fulltag_of(alist_cell);

        if ((cell_tag == fulltag_cons) &&
            ((car_dnode = gc_area_dnode(alist_cell)) < GCndnodes_in_area) &&
            (! ref_bit(markbits, car_dnode)) &&
            (is_node_fulltag(fulltag_of(thecar = car(alist_cell)))) &&
            ((car_dnode = gc_area_dnode(thecar)) < GCndnodes_in_area) &&
            (! ref_bit(markbits, car_dnode))) {
          *prev = rawcons->cdr;
          if (terminatablep) {
            rawcons->cdr = termination_list;
            termination_list = cell;
          }
        } else {
          set_bit(markbits, dnode);
          prev = (LispObj *)(&(rawcons->cdr));
          mark_root(alist_cell);
        }
        cell = *prev;
      }
    }
  } else {
    /* weak list */
    while (! done) {
      dnode = gc_area_dnode(cell);
      if ((dnode >= GCndnodes_in_area) ||
          (ref_bit(markbits, dnode))) {
        done = true;
      } else {
        /* Cons cell is unmarked. */
        LispObj thecar;
        unsigned cartag;

        rawcons = (cons *) ptr_from_lispobj(untag(cell));
        thecar = rawcons->car;
        cartag = fulltag_of(thecar);

        if (is_node_fulltag(cartag) &&
            ((car_dnode = gc_area_dnode(thecar)) < GCndnodes_in_area) &&
            (! ref_bit(markbits, car_dnode))) {
          *prev = rawcons->cdr;
          if (terminatablep) {
            rawcons->cdr = termination_list;
            termination_list = cell;
          }
        } else {
          set_bit(markbits, dnode);
          prev = (LispObj *)(&(rawcons->cdr));
        }
        cell = *prev;
      }
    }
  }

  if (terminatablep) {
    deref(weakv,1+3) = termination_list;
  }
  if (termination_list != lisp_nil) {
    deref(weakv,1) = GCweakvll;
    GCweakvll = untag(weakv);
  } else {
    deref(weakv,1) = lisp_global(WEAKVLL);
    lisp_global(WEAKVLL) = untag(weakv);
  }
}

/* 
  Screw: doesn't deal with finalization.
  */

void
reaphashv(LispObj hashv)
{
  hash_table_vector_header
    *hashp = (hash_table_vector_header *) ptr_from_lispobj(untag(hashv));
  natural
    dnode;
  signed_natural
    npairs = (header_element_count(hashp->header) - 
              (hash_table_vector_header_count -1)) >> 1;
  LispObj *pairp = (LispObj*) (hashp+1), weakelement;
  int weak_index = (((hashp->flags & nhash_weak_value_mask) == 0) ? 0 : 1);
  Boolean
    keys_frozen = ((hashp->flags & nhash_keys_frozen_mask) != 0);
  // Probably no reason why the non-keys_frozen case couldn't use slot_unbound as well,
  // but I don't want to risk it.
  LispObj empty_value = (keys_frozen ? slot_unbound : lisp_nil);
  bitvector markbits = GCmarkbits;
  int tag;

  natural *tenured_low = (LispObj *)tenured_area->low;
  natural tenured_dnodes = area_dnode(GCarealow, tenured_low);
  natural memo_dnode = area_dnode(ptr_to_lispobj(pairp+weak_index), tenured_low);
  Boolean
    hashv_tenured = (memo_dnode < tenured_dnodes);
  natural bits, bitidx, *bitsp;

  if (hashv_tenured) {
    set_bitidx_vars(tenured_area->refbits, memo_dnode, bitsp, bits, bitidx);
  }

  while (true) {
    if (hashv_tenured) {
      while (bits == 0) {
        int skip = nbits_in_word - bitidx;
        npairs -= skip;
        if (npairs <= 0) break;
        pairp += (skip+skip);
        bitidx = 0;
        bits = *++bitsp;
      }
      if (bits != 0) {
        int skip = (count_leading_zeros(bits) - bitidx);
        if (skip != 0) {
          npairs -= skip;
          pairp += (skip+skip);
          bitidx += skip;
        }
      }
    }

    if (npairs <= 0) break;

    weakelement = pairp[weak_index];
    tag = fulltag_of(weakelement);
    if (is_node_fulltag(tag)) {
      dnode = gc_area_dnode(weakelement);
      if ((dnode < GCndnodes_in_area) && 
          ! ref_bit(markbits, dnode)) {
	pairp[0] = slot_unbound;
	pairp[1] = empty_value;
        hashp->count += (1<<fixnumshift);
        if (!keys_frozen) {
          hashp->deleted_count += (1<<fixnumshift);
        }
      }
    }
    pairp += 2;
    --npairs;
  }
  deref(hashv, 1) = lisp_global(WEAKVLL);
  lisp_global(WEAKVLL) = untag(hashv);
}

void
traditional_dws_mark_htabv(LispObj htabv)
{
  /* Do nothing, just add htabv to GCweakvll */
  LispObj *base = (LispObj *) ptr_from_lispobj(untag(htabv));

  base[1] = GCweakvll;
  GCweakvll = ptr_to_lispobj(base);
}

void
ncircle_dws_mark_htabv(LispObj htabv)
{
  /* Do nothing, just add htabv to GCdwsweakvll */
  deref(htabv,1) = GCdwsweakvll;
  GCdwsweakvll = htabv;
}

void
traditional_mark_weak_htabv(LispObj htabv)
{
  int i, skip = hash_table_vector_header_count;;
  LispObj *base = (LispObj *) ptr_from_lispobj(untag(htabv));

  for (i = 2; i <= skip; i++) {
    rmark(base[i]);
  }
  base[1] = GCweakvll;
  GCweakvll = ptr_to_lispobj(base);
}

void
ncircle_mark_weak_htabv(LispObj htabv)
{
  int i, skip = hash_table_vector_header_count;
  hash_table_vector_header *hashp = (hash_table_vector_header *)(untag(htabv));
  natural
    npairs = (header_element_count(hashp->header) - 
              (hash_table_vector_header_count - 1)) >> 1;
  LispObj *pairp = (LispObj*) (hashp+1);
  Boolean 
    weak_on_value = ((hashp->flags & nhash_weak_value_mask) != 0);


  for (i = 2; i <= skip; i++) {
    rmark(deref(htabv,i));
  }
  
  if (!weak_on_value) {
    pairp++;
  }
  /* unconditionally mark the non-weak element of each pair */
  while (npairs--) {
    rmark(*pairp);
    pairp += 2;
  }
  deref(htabv,1)  = GCweakvll;
  GCweakvll = (LispObj)untag(htabv);
}


Boolean
mark_weak_hash_vector(hash_table_vector_header *hashp, natural elements)
{
  natural flags = hashp->flags, weak_dnode, nonweak_dnode;
  Boolean 
    marked_new = false, 
    weak_marked;
  int non_weak_index = (((flags & nhash_weak_value_mask) != 0) ? 0 : 1);
  int 
    skip = hash_table_vector_header_count-1,
    weak_tag,
    nonweak_tag,
    i;
  signed_natural
    npairs = (elements - skip) >> 1;
  LispObj 
    *pairp = (LispObj*) (hashp+1),
    weak,
    nonweak;

  natural *tenured_low = (LispObj *)tenured_area->low;
  natural tenured_dnodes = area_dnode(GCarealow, tenured_low);
  natural memo_dnode = area_dnode(ptr_to_lispobj(pairp+non_weak_index), tenured_low);
  Boolean hashv_tenured = (memo_dnode < tenured_dnodes);
  natural bits, bitidx, *bitsp;

  if (hashv_tenured) {
    set_bitidx_vars(tenured_area->refbits, memo_dnode, bitsp, bits, bitidx);
  }

  /* Mark everything in the header */
  
  for (i = 2; i<= skip; i++) {
    mark_root(deref(ptr_to_lispobj(hashp),i));
  }

  while (true) {
    if (hashv_tenured) {
      while (bits == 0) {
        int skip = nbits_in_word - bitidx;
        npairs -= skip;
        if (npairs <= 0) break;
        pairp += (skip+skip);
        bitidx = 0;
        bits = *++bitsp;
      }
      if (bits != 0) {
        int skip = count_leading_zeros(bits) - bitidx;
        if (skip != 0) {
          npairs -= skip;
          pairp += (skip+skip);
          bitidx += skip;
        }
      }
    }
    if (npairs <= 0) break;

    nonweak = pairp[non_weak_index];
    weak = pairp[1-non_weak_index];

    nonweak_tag = fulltag_of(nonweak);
    if (is_node_fulltag(nonweak_tag)) {
      nonweak_dnode = gc_area_dnode(nonweak);
      if ((nonweak_dnode < GCndnodes_in_area) &&
          ! ref_bit(GCmarkbits,nonweak_dnode)) {
        weak_marked = true;
        weak_tag = fulltag_of(weak);
        if (is_node_fulltag(weak_tag)) {
          weak_dnode = gc_area_dnode(weak);
          if ((weak_dnode < GCndnodes_in_area) &&
              ! ref_bit(GCmarkbits, weak_dnode)) {
            weak_marked = false;
          }
        }
        if (weak_marked) {
          mark_root(nonweak);
          marked_new = true;
        }
      }
    }

    pairp+=2;
    --npairs;
  }
  return marked_new;
}


Boolean
mark_weak_alist(LispObj weak_alist, int weak_type)
{
  natural
    elements = header_element_count(header_of(weak_alist)),
    dnode;
  int pair_tag;
  Boolean marked_new = false;
  LispObj alist, pair, key, value;
  bitvector markbits = GCmarkbits;

  if (weak_type >> population_termination_bit) {
    elements -= 1;
  }
  for(alist = deref(weak_alist, elements);
      (fulltag_of(alist) == fulltag_cons) &&
      ((dnode = gc_area_dnode(alist)) < GCndnodes_in_area) &&
      (! ref_bit(markbits,dnode));
      alist = cdr(alist)) {
    pair = car(alist);
    pair_tag = fulltag_of(pair);
    if ((is_node_fulltag(pair_tag)) &&
        ((dnode = gc_area_dnode(pair)) < GCndnodes_in_area) &&
        (! ref_bit(markbits,dnode))) {
      if (pair_tag == fulltag_cons) {
        key = car(pair);
        if ((! is_node_fulltag(fulltag_of(key))) ||
            ((dnode = gc_area_dnode(key)) >= GCndnodes_in_area) ||
            ref_bit(markbits,dnode)) {
          /* key is marked, mark value if necessary */
          value = cdr(pair);
          if (is_node_fulltag(fulltag_of(value)) &&
              ((dnode = gc_area_dnode(value)) < GCndnodes_in_area) &&
              (! ref_bit(markbits,dnode))) {
            mark_root(value);
            marked_new = true;
          }
        }
      } else {
          mark_root(pair);
          marked_new = true;
      }
    }
  }
  return marked_new;
}
  
void
mark_termination_lists()
{
  /* 
     Mark the termination lists in all terminatable weak vectors, which
     are now linked together on GCweakvll, and add them to WEAKVLL,
     which already contains all other weak vectors.
  */
  LispObj pending = GCweakvll,
          *base = (LispObj *)NULL;

  while (pending) {
    base = ptr_from_lispobj(pending);
    pending = base[1];

    mark_root(base[1+3]);
  }
  if (base) {
    base[1] = lisp_global(WEAKVLL);
    lisp_global(WEAKVLL) = GCweakvll;
  }

}


void
traditional_markhtabvs()
{
  LispObj *base, this, header, pending;
  int subtag;
  hash_table_vector_header *hashp;
  Boolean marked_new;

  do {
    pending = (LispObj) NULL;
    marked_new = false;
    
    while (GCweakvll) {
      base = ptr_from_lispobj(GCweakvll);
      GCweakvll = base[1];
      
      header = base[0];
      subtag = header_subtag(header);
      
      if (subtag == subtag_weak) {
        natural weak_type = base[2];
        this = ptr_to_lispobj(base) + fulltag_misc;
        base[1] = pending;
        pending = ptr_to_lispobj(base);
        if ((weak_type & population_type_mask) == population_weak_alist) {
          if (mark_weak_alist(this, weak_type)) {
            marked_new = true;
          }
        }
      } else if (subtag == subtag_hash_vector) {
        natural elements = header_element_count(header);

        hashp = (hash_table_vector_header *) base;
        if (hashp->flags & nhash_weak_mask) {
          base[1] = pending;
          pending = ptr_to_lispobj(base);
          if (mark_weak_hash_vector(hashp, elements)) {
            marked_new = true;
          }
        } 
      } else {
        Bug(NULL, "Strange object on weak vector linked list: " LISP "\n", base);
      }
    }

    if (marked_new) {
      GCweakvll = pending;
    }
  } while (marked_new);

  /* Now, everything's marked that's going to be,  and "pending" is a list
     of populations and weak hash tables.  CDR down that list and free
     anything that isn't marked.
     */

  while (pending) {
    base = ptr_from_lispobj(pending);
    pending = base[1];
    base[1] = (LispObj)NULL;

    this = ptr_to_lispobj(base) + fulltag_misc;

    subtag = header_subtag(base[0]);
    if (subtag == subtag_weak) {
      reapweakv(this);
    } else {
      reaphashv(this);
    }
  }
  mark_termination_lists();
}

void
ncircle_markhtabvs()
{
  LispObj *base, this, header, pending = 0;
  int subtag;

  /* First, process any weak hash tables that may have
     been encountered by the link-inverting marker; we
     should have more stack space now. */

  while (GCdwsweakvll) {
    this = GCdwsweakvll;
    GCdwsweakvll = deref(this,1);
    ncircle_mark_weak_htabv(this);
  }

  while (GCweakvll) {
    base = ptr_from_lispobj(GCweakvll);
    GCweakvll = base[1];
    base[1] = (LispObj)NULL;

    this = ptr_to_lispobj(base) + fulltag_misc;

    header = base[0];
    subtag = header_subtag(header);
      
    if (subtag == subtag_weak) {
      natural weak_type = base[2];
      base[1] = pending;
      pending = ptr_to_lispobj(base);
      if ((weak_type & population_type_mask) == population_weak_alist) {
        mark_weak_alist(this, weak_type);
      }
    } else if (subtag == subtag_hash_vector) {
      reaphashv(this);
    }
  }

  /* Now, everything's marked that's going to be,  and "pending" is a list
     of populations.  CDR down that list and free
     anything that isn't marked.
     */

  while (pending) {
    base = ptr_from_lispobj(pending);
    pending = base[1];
    base[1] = (LispObj)NULL;

    this = ptr_to_lispobj(base) + fulltag_misc;

    subtag = header_subtag(base[0]);
    if (subtag == subtag_weak) {
      reapweakv(this);
    } else {
      Bug(NULL, "Bad object on pending list: %s\n", this);
    }
  }

  mark_termination_lists();
}

void
mark_tcr_tlb(TCR *tcr)
{
  natural n = tcr->tlb_limit;
  LispObj 
    *start = tcr->tlb_pointer,
    *end = (LispObj *) ((BytePtr)start+n),
    node;

  while (start < end) {
    node = *start;
    if (node != no_thread_local_binding_marker) {
      mark_root(node);
    }
    start++;
  }
}

/*
  Mark things that're only reachable through some (suspended) TCR.
  (This basically means the tcr's gc_context and the exception
  frames on its xframe_list.)
*/

void
mark_tcr_xframes(TCR *tcr)
{
  xframe_list *xframes;
  ExceptionInformation *xp;

  xp = TCR_AUX(tcr)->gc_context;
  if (xp) {
#ifndef X8632
    mark_xp(xp);
#else
    mark_xp(xp, tcr->node_regs_mask);
#endif
  }
#ifdef X8632
  mark_root(tcr->save0);
  mark_root(tcr->save1);
  mark_root(tcr->save2);
  mark_root(tcr->save3);
  mark_root(tcr->next_method_context);
#endif
  
  for (xframes = (xframe_list *) tcr->xframe; 
       xframes; 
       xframes = xframes->prev) {
#ifndef X8632
      mark_xp(xframes->curr);
#else
      mark_xp(xframes->curr, xframes->node_regs_mask);
#endif
  }
}
      

struct xmacptr *user_postGC_macptrs = NULL;



void
postGCfreexmacptr(struct xmacptr *p)
{
  p->link = (LispObj) user_postGC_macptrs;
  user_postGC_macptrs = p;
}


xmacptr_dispose_fn xmacptr_dispose_functions[xmacptr_flag_user_last-xmacptr_flag_user_first];



void
freeGCptrs()
{
  void *p, *next, *addr;
  struct xmacptr *x, *xnext;
  int flags;
  xmacptr_dispose_fn dfn;

  
  for (x = user_postGC_macptrs; x; x = xnext) {
    xnext = (xmacptr *) (x->link);
    flags = x->flags;
    addr = (void *)x->address;
    x->address = 0;
    x->flags = 0;
    x->link = 0;
    x->class = 0;
    if (addr) {
      switch(flags) {
      case xmacptr_flag_recursive_lock:
        destroy_recursive_lock((RECURSIVE_LOCK)addr);
        break;
      case xmacptr_flag_ptr:
        free(addr);
        break;
      case xmacptr_flag_none:   /* ?? */
        break;
      case xmacptr_flag_rwlock:
        rwlock_destroy((rwlock *)addr);
        break;
      case xmacptr_flag_semaphore:
        destroy_semaphore((void **)&addr);
        break;
      default:
        if ((flags >= xmacptr_flag_user_first) &&
            (flags < xmacptr_flag_user_last)) {
          flags -= xmacptr_flag_user_first;
          dfn = xmacptr_dispose_functions[flags];
          if (dfn && addr) {
            dfn(addr);
          }
        }
      }
    }
  }

  user_postGC_macptrs = NULL;
}

int
register_xmacptr_dispose_function(void *dfn)
{
  int i, k;
  
  for( i = 0, k = xmacptr_flag_user_first; k < xmacptr_flag_user_last; i++, k++) {
    if (xmacptr_dispose_functions[i]==NULL) {
      xmacptr_dispose_functions[i] = dfn;
      return k;
    }
    if (xmacptr_dispose_functions[i] == dfn) {
      return k;
    }
  }
  return 0;
}

void
reap_gcable_ptrs()
{
  LispObj *prev = &(lisp_global(GCABLE_POINTERS)), next, ptr;
  natural dnode;
  xmacptr *x;

  while((next = *prev) != (LispObj)NULL) {
    dnode = gc_area_dnode(next);
    x = (xmacptr *) ptr_from_lispobj(untag(next));

    if ((dnode >= GCndnodes_in_area) ||
        (ref_bit(GCmarkbits,dnode))) {
      prev = &(x->link);
    } else {
      *prev = x->link;
      ptr = x->address;

      if (ptr) {
        set_n_bits(GCmarkbits,dnode,3);
        postGCfreexmacptr(x);
      }
    }
  }
}



#if  WORD_SIZE == 64
unsigned short *_one_bits = NULL;

unsigned short
logcount16(unsigned short n)
{
  unsigned short c=0;
  
  while(n) {
    n = n & (n-1);
    c++;
  }
  return c;
}

void
gc_init()
{
  int i;
  
  _one_bits = malloc(sizeof(unsigned short) * (1<<16));

  for (i = 0; i < (1<<16); i++) {
    _one_bits[i] = dnode_size*logcount16(i);
  }
}


#else
const unsigned char _one_bits[256] = {
    0*8,1*8,1*8,2*8,1*8,2*8,2*8,3*8,1*8,2*8,2*8,3*8,2*8,3*8,3*8,4*8,
    1*8,2*8,2*8,3*8,2*8,3*8,3*8,4*8,2*8,3*8,3*8,4*8,3*8,4*8,4*8,5*8,
    1*8,2*8,2*8,3*8,2*8,3*8,3*8,4*8,2*8,3*8,3*8,4*8,3*8,4*8,4*8,5*8,
    2*8,3*8,3*8,4*8,3*8,4*8,4*8,5*8,3*8,4*8,4*8,5*8,4*8,5*8,5*8,6*8,
    1*8,2*8,2*8,3*8,2*8,3*8,3*8,4*8,2*8,3*8,3*8,4*8,3*8,4*8,4*8,5*8,
    2*8,3*8,3*8,4*8,3*8,4*8,4*8,5*8,3*8,4*8,4*8,5*8,4*8,5*8,5*8,6*8,
    2*8,3*8,3*8,4*8,3*8,4*8,4*8,5*8,3*8,4*8,4*8,5*8,4*8,5*8,5*8,6*8,
    3*8,4*8,4*8,5*8,4*8,5*8,5*8,6*8,4*8,5*8,5*8,6*8,5*8,6*8,6*8,7*8,
    1*8,2*8,2*8,3*8,2*8,3*8,3*8,4*8,2*8,3*8,3*8,4*8,3*8,4*8,4*8,5*8,
    2*8,3*8,3*8,4*8,3*8,4*8,4*8,5*8,3*8,4*8,4*8,5*8,4*8,5*8,5*8,6*8,
    2*8,3*8,3*8,4*8,3*8,4*8,4*8,5*8,3*8,4*8,4*8,5*8,4*8,5*8,5*8,6*8,
    3*8,4*8,4*8,5*8,4*8,5*8,5*8,6*8,4*8,5*8,5*8,6*8,5*8,6*8,6*8,7*8,
    2*8,3*8,3*8,4*8,3*8,4*8,4*8,5*8,3*8,4*8,4*8,5*8,4*8,5*8,5*8,6*8,
    3*8,4*8,4*8,5*8,4*8,5*8,5*8,6*8,4*8,5*8,5*8,6*8,5*8,6*8,6*8,7*8,
    3*8,4*8,4*8,5*8,4*8,5*8,5*8,6*8,4*8,5*8,5*8,6*8,5*8,6*8,6*8,7*8,
    4*8,5*8,5*8,6*8,5*8,6*8,6*8,7*8,5*8,6*8,6*8,7*8,6*8,7*8,7*8,8*8
};


void
gc_init()
{
}

#endif


weak_mark_fun dws_mark_weak_htabv = traditional_dws_mark_htabv;
weak_mark_fun mark_weak_htabv = traditional_mark_weak_htabv;
weak_process_fun markhtabvs = traditional_markhtabvs;

void
install_weak_mark_functions(natural set) {
  switch(set) {
  case 0:
  default:
    dws_mark_weak_htabv = traditional_dws_mark_htabv;
    mark_weak_htabv = traditional_mark_weak_htabv;
    markhtabvs = traditional_markhtabvs;
    break;
  case 1:
    dws_mark_weak_htabv = ncircle_dws_mark_htabv;
    mark_weak_htabv = ncircle_mark_weak_htabv;
    markhtabvs = ncircle_markhtabvs;
    break;
  }
}

void
init_weakvll ()
{
  LispObj this = lisp_global(WEAKVLL); /* all weak vectors as of last gc */

  GCweakvll = (LispObj)NULL;
  lisp_global(WEAKVLL) = (LispObj)NULL;

  if (GCn_ephemeral_dnodes) {
    /* For egc case, initialize GCweakvll with weak vectors not in the
       GC area.  Weak vectors in the GC area will be added during marking.
    */

    LispObj *tenured_low = (LispObj *)tenured_area->low;
    natural tenured_dnodes = area_dnode(GCarealow, tenured_low);
    bitvector refbits = tenured_area->refbits;

    while (this) {
      LispObj *base = ptr_from_lispobj(this);
      LispObj next = base[1];
      natural dnode = gc_dynamic_area_dnode(this);
      if (dnode < GCndynamic_dnodes_in_area) {
        base[1] = (LispObj)NULL; /* drop it, might be garbage */
      } else {
        base[1] = GCweakvll;
        GCweakvll = ptr_to_lispobj(base);
        if (header_subtag(base[0]) == subtag_weak) {
          dnode = area_dnode(&base[3], tenured_low);
          if (dnode < tenured_dnodes) {
            clr_bit(refbits, dnode); /* Don't treat population.data as root */
          }
        } else {
          if (header_subtag(base[0]) != subtag_hash_vector)
            Bug(NULL, "Unexpected entry " LISP " -> " LISP " on WEAKVLL", base, base[0]);
          dnode = area_dnode(base, tenured_low);
          if ((dnode < tenured_dnodes) && !ref_bit(refbits, dnode)) {
            Boolean drop = true;
            /* hash vectors get marked headers if they have any ephemeral keys */
            /* but not if they have ephemeral values. */
            if (((hash_table_vector_header *)base)->flags & nhash_weak_value_mask) {
              signed_natural count = (header_element_count(base[0]) + 2) >> 1;
              natural bits, bitidx, *bitsp;
              set_bitidx_vars(refbits, dnode, bitsp, bits, bitidx);
              while ((0 < count) && (bits == 0)) {
                int skip = nbits_in_word - bitidx;
                count -= skip;
                bits = *++bitsp;
                bitidx = 0;
              }
              count -=  (count_leading_zeros(bits) - bitidx);

              if (0 < count) {
                set_bit(refbits, dnode); /* has ephemeral values, mark header */
                drop = false;
              }
            }
            if (drop) { /* if nothing ephemeral, drop it from GCweakvll. */
              GCweakvll = base[1];
              base[1] = lisp_global(WEAKVLL);
              lisp_global(WEAKVLL) = ptr_to_lispobj(base);
            }
          }
        }
      }
      this = next;
    }
  }
}

  
void
preforward_weakvll ()
{
  /* reset population refbits for forwarding */
  if (GCn_ephemeral_dnodes) {
    LispObj this = lisp_global(WEAKVLL);
    LispObj *tenured_low = (LispObj *)tenured_area->low;
    natural tenured_dnodes = area_dnode(GCarealow, tenured_low);
    bitvector refbits = tenured_area->refbits;

    while (this) {
      LispObj *base = ptr_from_lispobj(this);
      if (header_subtag(base[0]) == subtag_weak) {
        natural dnode = area_dnode(&base[3], tenured_low);
        if (base[3] >= GCarealow) {
          if (dnode < tenured_dnodes) {
            set_bit(refbits, dnode);
          }
        }
        /* might have set termination list to a new pointer */
        if ((base[2] >> population_termination_bit) && (base[4] >= GCarealow)) {
          if ((dnode + 1) < tenured_dnodes) {
            set_bit(refbits, dnode+1);
          }
        }
      }
      this = base[1];
    }
  }
}


void
forward_weakvll_links()
{
  LispObj *ptr = &(lisp_global(WEAKVLL)), this, new, old;

  while ((this = *ptr)) {
    old = this + fulltag_misc;
    new = node_forwarding_address(old);
    if (old != new) {
      *ptr = untag(new);
    }
    ptr = &(deref(new,1));
  }
}





LispObj
node_forwarding_address(LispObj node)
{
  int tag_n;
  natural dnode = gc_dynamic_area_dnode(node);

  if ((dnode >= GCndynamic_dnodes_in_area) ||
      (node < GCfirstunmarked)) {
    return node;
  }

  tag_n = fulltag_of(node);
  if (!is_node_fulltag(tag_n)) {
    return node;
  }

  return dnode_forwarding_address(dnode, tag_n);
}

Boolean
update_noderef(LispObj *noderef)
{
  LispObj
    node = *noderef,
    new = node_forwarding_address(node);

  if (new != node) {
    *noderef = new;
    return true;
  }
  return false;
}

void
update_locref(LispObj *locref)
{
  LispObj
    obj = *locref,
    new = locative_forwarding_address(obj);

  if (new != obj) {
    *locref = new;
  }
}

void
forward_gcable_ptrs()
{
  LispObj *prev = &(lisp_global(GCABLE_POINTERS)), next, new;
  struct xmacptr **xprev, *xnext, *xnew;

  while ((next = *prev) != (LispObj)NULL) {
    new = node_forwarding_address(next);
    if (new != next) {
      *prev = new;
    }
    prev = &(((xmacptr *)ptr_from_lispobj(untag(next)))->link);
  }
  xprev = &user_postGC_macptrs;
  while ((xnext = *xprev)) {
    xnew = (struct xmacptr *)locative_forwarding_address((LispObj)xnext);
    if (xnew != xnext) {
      *xprev = xnew;
    }
    xprev = (struct xmacptr **)(&(xnext->link));
  }
}

void
forward_memoized_area(area *a, natural num_memo_dnodes, bitvector refbits)
{
  LispObj *p = (LispObj *) a->low, x1, x2, new;
#ifdef ARM
  LispObj *p0 = p;
#endif
  natural bits, bitidx, *bitsp, nextbit, diff, memo_dnode = 0, hash_dnode_limit = 0;
  int tag_x1;
  hash_table_vector_header *hashp = NULL;
  Boolean header_p;



  if (num_memo_dnodes) {
    if (GCDebug) {
      check_refmap_consistency(p, p+(num_memo_dnodes << 1), refbits);
    }

    /* This is pretty straightforward, but we have to note
       when we move a key in a hash table vector that wants
       us to tell it about that. */

    set_bitidx_vars(refbits, 0, bitsp, bits, bitidx);
    while (memo_dnode < num_memo_dnodes) {
      if (bits == 0) {
        int remain = nbits_in_word - bitidx;
        memo_dnode += remain;
        p += (remain+remain);
        if (memo_dnode < num_memo_dnodes) {
          bits = *++bitsp;
        }
        bitidx = 0;
      } else {
        nextbit = count_leading_zeros(bits);
        if ((diff = (nextbit - bitidx)) != 0) {
          memo_dnode += diff;
          bitidx = nextbit;
          p += (diff+diff);
        }
        x1 = p[0];
        x2 = p[1];
        tag_x1 = fulltag_of(x1);
        bits &= ~(BIT0_MASK >> bitidx);
        header_p = (nodeheader_tag_p(tag_x1));

        if (header_p &&
            (header_subtag(x1) == subtag_hash_vector)) {
          hashp = (hash_table_vector_header *) p;
          if (hashp->flags & nhash_track_keys_mask) {
            hash_dnode_limit = memo_dnode + ((header_element_count(x1)+2)>>1);
          } else {
            hashp = NULL;
          }
        }


        if (! header_p) {
          new = node_forwarding_address(x1);
          if (new != x1) {
            *p = new;
#ifdef ARM
            if (p != p0) {
              if(header_subtag(p[-2]) == subtag_function) {
                /* Just updated the code vector; fix the entrypoint */
                if (p[-1] == (untag(x1)+fulltag_odd_fixnum)) {
                  p[-1] = (untag(new)+fulltag_odd_fixnum);
                }
              }
            }
#endif
          }
        }
        p++;

        new = node_forwarding_address(x2);
        if (new != x2) {
          *p = new;
          if (memo_dnode < hash_dnode_limit) {
            /* If this code is reached, 'hashp' is non-NULL and pointing
               at the header of a hash_table_vector, and 'memo_dnode' identifies
               a pair of words inside the hash_table_vector.  It may be
               hard for program analysis tools to recognize that, but I
               believe that warnings about 'hashp' being NULL here can
               be safely ignored. */
            hashp->flags |= nhash_key_moved_mask;
            hash_dnode_limit = 0;
            hashp = NULL;
          }
        }
        p++;
        memo_dnode++;
        bitidx++;

      }
    }
  }
}

void
forward_tcr_tlb(TCR *tcr)
{
  natural n = tcr->tlb_limit;
  LispObj 
    *start = tcr->tlb_pointer, 
    *end = (LispObj *) ((BytePtr)start+n),
    node;

  while (start < end) {
    node = *start;
    if (node != no_thread_local_binding_marker) {
      update_noderef(start);
    }
    start++;
  }
}

void
reclaim_static_dnodes()
{
  natural nstatic = tenured_area->static_dnodes, 
    i, 
    bits, 
    bitnum,
    nfree = 0,
    nstatic_conses = area_dnode(static_cons_area->high, static_cons_area->low);
  cons *c = (cons *)tenured_area->low, *d;
  bitvector bitsp = GCmarkbits;
  LispObj head = lisp_global(STATIC_CONSES);

  for (i = 0; i < nstatic; i+= nbits_in_word, c+= nbits_in_word) {
    bits = *bitsp++;
    if (bits != ALL_ONES) {
      for (bitnum = 0; bitnum < nbits_in_word; bitnum++) {
        if (! (bits & (BIT0_MASK>>bitnum))) {
          d = c + bitnum;
          if (i < nstatic_conses) {                
            d->car = unbound;
            d->cdr = head;
            head = ((LispObj)d)+fulltag_cons;
            nfree++;
          } else {
            d->car = 0;
            d->cdr = 0;
          }
        }
      }
    }
  }
  lisp_global(STATIC_CONSES) = head;
  lisp_global(FREE_STATIC_CONSES)+=(nfree<<fixnumshift);
}

Boolean
youngest_non_null_area_p (area *a)
{
  if (a->active == a->high) {
    return false;
  } else {
    for (a = a->younger; a; a = a->younger) {
      if (a->active != a->high) {
        return false;
      }
    }
  };
  return true;
}

Boolean just_purified_p = false;

/*
  All thread's stack areas have been "normalized", as
  has the dynamic heap.  (The "active" pointer in these areas
  matches the stack pointer/freeptr value at the time that
  the exception occurred.)
*/

#define get_time(when) gettimeofday(&when, NULL)



#ifdef FORCE_DWS_MARK
#warning recursive marker disabled for testing; remember to re-enable it
#endif


Boolean
mark_static_ref(LispObj n, BytePtr dynamic_start, natural ndynamic_dnodes)
{
  int tag_n = fulltag_of(n);
  natural dyn_dnode;

  if (nodeheader_tag_p(tag_n)) {
    return (header_subtag(n) == subtag_hash_vector);
  }
 
  if (is_node_fulltag (tag_n)) {
    dyn_dnode = area_dnode(n, dynamic_start);
    if (dyn_dnode < ndynamic_dnodes) {
      mark_root(n);             /* May or may not mark it */
      return true;              /* but return true 'cause it's a dynamic node */
    }
  }
  return false;                 /* Not a heap pointer or not dynamic */
}

void
mark_managed_static_refs(area *a, BytePtr low_dynamic_address, natural ndynamic_dnodes)
{
  bitvector refbits = managed_static_refbits;
  LispObj *p = (LispObj *) a->low, x1, x2;
  natural inbits, outbits, bits, bitidx, *bitsp, nextbit, diff, memo_dnode = 0,
    num_memo_dnodes = a->ndnodes;
  Boolean keep_x1, keep_x2;

  if (num_memo_dnodes) {
    if (GCDebug) {
      check_refmap_consistency(p, p+(num_memo_dnodes << 1), refbits);
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
        if (memo_dnode < num_memo_dnodes) {
          bits = *++bitsp;
        }
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
        keep_x1 = mark_static_ref(x1, low_dynamic_address, ndynamic_dnodes);
        keep_x2 = mark_static_ref(x2, low_dynamic_address, ndynamic_dnodes);
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
}

natural
ephemeral_ref=0;

void
mark_memoized_area(area *a, natural num_memo_dnodes)
{
  bitvector refbits = a->refbits;
  LispObj *p = (LispObj *) a->low, x1, x2;
  natural inbits, outbits, bits, bitidx, *bitsp, nextbit, diff, memo_dnode = 0;
  Boolean keep_x1, keep_x2;
  natural hash_dnode_limit = 0;
  hash_table_vector_header *hashp = NULL;
  int mark_method = 3;

  if (num_memo_dnodes) {
    if (GCDebug) {
      check_refmap_consistency(p, p+(num_memo_dnodes << 1), refbits);
    }

    /* The distinction between "inbits" and "outbits" is supposed to help us
       detect cases where "uninteresting" setfs have been memoized.  Storing
       NIL, fixnums, immediates (characters, etc.) or node pointers to static
       or readonly areas is definitely uninteresting, but other cases are
       more complicated (and some of these cases are hard to detect.)

       Some headers are "interesting", to the forwarder if not to us. 

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
        natural mask = ~((NATURAL1<<(nbits_in_word-bits_in_last_word))- NATURAL1);
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
        if (memo_dnode < num_memo_dnodes) {
          bits = *++bitsp;
        } 
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


        if (hashp) {
          Boolean force_x1 = false;
          if ((memo_dnode >= hash_dnode_limit) && (mark_method == 3)) {
            /* if vector_header_count is odd, x1 might be the last word of the header */
            force_x1 = (hash_table_vector_header_count & 1) && (memo_dnode == hash_dnode_limit);
            /* was marking header, switch to data */
            hash_dnode_limit = area_dnode(((LispObj *)hashp)
                                          + 1
                                          + header_element_count(hashp->header),
                                          a->low);
            /* In traditional weak method, don't mark vector entries at all. */
            /* Otherwise mark the non-weak elements only */
            mark_method = ((lisp_global(WEAK_GC_METHOD) == 0) ? 0 :
                           ((hashp->flags & nhash_weak_value_mask)
                            ? (1 + (hash_table_vector_header_count & 1))
                            : (2 - (hash_table_vector_header_count & 1))));
          }

          if (memo_dnode < hash_dnode_limit) {
            /* perhaps ignore one or both of the elements */
            if (!force_x1 && !(mark_method & 1)) x1 = 0;
            if (!(mark_method & 2)) x2 = 0;
          } else {
            hashp = NULL;
          }
        }

        if (header_subtag(x1) == subtag_hash_vector) {
          if (hashp) Bug(NULL, "header inside hash vector?");
          hash_table_vector_header *hp = (hash_table_vector_header *)(p - 2);
          if (hp->flags & nhash_weak_mask) {
            /* Work around the issue that seems to cause ticket:817,
               which is that tenured hash vectors that are weak on value
               aren't always maintained on GCweakvll.  If they aren't and
               we process them weakly here, nothing will delete the unreferenced
               elements. */
            if (!(hp->flags & nhash_weak_value_mask)) {
              /* If header_count is odd, this cuts off the last header field */
              /* That case is handled specially above */
              hash_dnode_limit = memo_dnode + ((hash_table_vector_header_count) >>1);
              hashp = hp;
              mark_method = 3;
            }
          }
        }

        keep_x1 = mark_ephemeral_root(x1);
        keep_x2 = mark_ephemeral_root(x2);
        if ((keep_x1 == false) && 
            (keep_x2 == false) &&
            (hashp == NULL)) {
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
}

extern void zero_dnodes(void *,natural);

void 
gc(TCR *tcr, signed_natural param)
{
  struct timeval start, stop;
  area *a = active_dynamic_area, *to = NULL, *from = NULL, *note = NULL;
  unsigned timeidx = 1;
  paging_info paging_info_start;
  LispObj
    pkg = 0,
    itabvec = 0;
  BytePtr oldfree = a->active, last_zeroed_addr;
  TCR *other_tcr;
  natural static_dnodes;
  natural weak_method = lisp_global(WEAK_GC_METHOD) >> fixnumshift;

#ifndef FORCE_DWS_MARK
  if ((natural) (TCR_AUX(tcr)->cs_limit) == CS_OVERFLOW_FORCE_LIMIT) {
    GCstack_limit = CS_OVERFLOW_FORCE_LIMIT;
  } else {
    GCstack_limit = (natural)(TCR_AUX(tcr)->cs_limit)+(natural)page_size;
  }
#else
  GCstack_limit = CS_OVERFLOW_FORCE_LIMIT;
#endif

  GCephemeral_low = lisp_global(OLDEST_EPHEMERAL);
  if (GCephemeral_low) {
    GCn_ephemeral_dnodes=area_dnode(oldfree, GCephemeral_low);
  } else {
    GCn_ephemeral_dnodes = 0;
  }
  
  if (GCn_ephemeral_dnodes) {
    GCverbose = ((nrs_GC_EVENT_STATUS_BITS.vcell & egc_verbose_bit) != 0);
  } else {
    GCverbose = ((nrs_GC_EVENT_STATUS_BITS.vcell & gc_verbose_bit) != 0);
  }

  if (GCephemeral_low) {
    if ((oldfree-g1_area->low) < g1_area->threshold) {
      to = g1_area;
      note = a;
      timeidx = 4;
    } else if ((oldfree-g2_area->low) < g2_area->threshold) {
      to = g2_area;
      from = g1_area;
      note = g1_area;
      timeidx = 3;
    } else {
      to = tenured_area;
      from = g2_area;
      note = g2_area;
      timeidx = 2;
    } 
  } else {
    note = tenured_area;
  }

  install_weak_mark_functions(weak_method);
  
  if (GCverbose) {
    char buf[16];

    sample_paging_info(&paging_info_start);
    comma_output_decimal(buf,16,area_dnode(oldfree,a->low) << dnode_shift);
    if (GCephemeral_low) {
      fprintf(dbgout,
              "\n\n;;; Starting Ephemeral GC of generation %d",
              (from == g2_area) ? 2 : (from == g1_area) ? 1 : 0); 
    } else {
      fprintf(dbgout,"\n\n;;; Starting full GC");
    }
    fprintf(dbgout, ", %s bytes allocated.\n", buf);
  }

#ifdef USE_DTRACE
  if (GCephemeral_low) {
    if (CCL_EGC_START_ENABLED()) {
      natural bytes_used = area_dnode(oldfree, a->low) << dnode_shift;
      unsigned generation = (from == g2_area) ? 2 : (from == g1_area) ? 1 : 0;
      CCL_EGC_START(bytes_used, generation);
    }
  } else {
    if (CCL_GC_START_ENABLED()) {
      natural bytes_used = area_dnode(oldfree, a->low) << dnode_shift;
      CCL_GC_START(bytes_used);
    }
  }
#endif

  get_time(start);

  /* The link-inverting marker might need to write to watched areas */
  unprotect_watched_areas();

  lisp_global(IN_GC) = (1<<fixnumshift);

  if (just_purified_p) {
    just_purified_p = false;
    GCDebug = false;
  } else {
    GCDebug = ((nrs_GC_EVENT_STATUS_BITS.vcell & gc_integrity_check_bit) != 0);
    if (GCDebug) {
      check_all_areas(tcr);
      check_static_cons_freelist("in pre-gc static-cons check");
    }
  }

  if (from) {
    untenure_from_area(from);
  }
  static_dnodes = static_dnodes_for_area(a);
  GCmarkbits = a->markbits;
  GCarealow = ptr_to_lispobj(a->low);
  GCareadynamiclow = GCarealow+(static_dnodes << dnode_shift);
  GCndnodes_in_area = gc_area_dnode(oldfree);

  if (GCndnodes_in_area) {
    GCndynamic_dnodes_in_area = GCndnodes_in_area-static_dnodes;
    GCdynamic_markbits = 
      GCmarkbits + ((GCndnodes_in_area-GCndynamic_dnodes_in_area)>>bitmap_shift);

    zero_bits(GCmarkbits, GCndnodes_in_area);

    init_weakvll();

    if (GCn_ephemeral_dnodes == 0) {
      /* For GCTWA, mark the internal package hash table vector of
       *PACKAGE*, but don't mark its contents. */
      {
        LispObj
          itab,
          pkgidx = nrs_PACKAGE.binding_index;
        natural
          dnode, ndnodes;
      
        if ((pkgidx >= tcr->tlb_limit) ||
            ((pkg = tcr->tlb_pointer[pkgidx>>fixnumshift]) == 
             no_thread_local_binding_marker)) {
          pkg = nrs_PACKAGE.vcell;
        }
        if ((fulltag_of(pkg) == fulltag_misc) &&
            (header_subtag(header_of(pkg)) == subtag_package)) {
          itab = ((package *)ptr_from_lispobj(untag(pkg)))->itab;
          itabvec = car(itab);
          dnode = gc_area_dnode(itabvec);
          if (dnode < GCndnodes_in_area) {
            ndnodes = (header_element_count(header_of(itabvec))+1) >> 1;
            set_n_bits(GCmarkbits, dnode, ndnodes);
          }
        }
      }
    }

    mark_root(lisp_global(STATIC_CONSES));

    {
      area *next_area;
      area_code code;

      /* Could make a jump table instead of the typecase */

      for (next_area = a->succ; (code = next_area->code) != AREA_VOID; next_area = next_area->succ) {
        switch (code) {
        case AREA_TSTACK:
          mark_tstack_area(next_area);
          break;

        case AREA_VSTACK:
          mark_vstack_area(next_area);
          break;
          
        case AREA_CSTACK:
          mark_cstack_area(next_area);
          break;

        case AREA_STATIC:
	case AREA_WATCHED:
        case AREA_DYNAMIC:                  /* some heap that isn't "the" heap */
          /* In both of these cases, we -could- use the area's "markbits"
             bitvector as a reference map.  It's safe (but slower) to
             ignore that map and process the entire area.
          */
          if (next_area->younger == NULL) {
            mark_simple_area_range((LispObj *) next_area->low, (LispObj *) next_area->active);
          }
          break;

        default:
          break;
        }
      }
    }

    if (GCephemeral_low) {
      mark_memoized_area(tenured_area, area_dnode(a->low,tenured_area->low));
      mark_memoized_area(managed_static_area,managed_static_area->ndnodes);
    } else {
      mark_managed_static_refs(managed_static_area,low_markable_address,area_dnode(a->active,low_markable_address));
    }
    other_tcr = tcr;
    do {
      mark_tcr_xframes(other_tcr);
      mark_tcr_tlb(other_tcr);
      other_tcr = TCR_AUX(other_tcr)->next;
    } while (other_tcr != tcr);




    /* Go back through *package*'s internal symbols, marking
       any that aren't worthless.
    */
    
    if (itabvec) {
      natural
        i,
        n = header_element_count(header_of(itabvec));
      LispObj
        sym,
        *raw = 1+((LispObj *)ptr_from_lispobj(untag(itabvec)));

      for (i = 0; i < n; i++) {
        sym = *raw++;
        if (is_symbol_fulltag(sym)) {
          lispsymbol *rawsym = (lispsymbol *)ptr_from_lispobj(untag(sym));
          natural dnode = gc_area_dnode(sym);
          
          if ((dnode < GCndnodes_in_area) &&
              (!ref_bit(GCmarkbits,dnode))) {
            /* Symbol is in GC area, not marked.
               Mark it if fboundp, boundp, or if
               it has a plist or another home package.
            */
            
            if (FBOUNDP(rawsym) ||
                BOUNDP(rawsym) ||
                (rawsym->flags != 0) || /* SPECIAL, etc. */
                (rawsym->plist != lisp_nil) ||
                ((rawsym->package_predicate != pkg) &&
                 (rawsym->package_predicate != lisp_nil))) {
              mark_root(sym);
            }
          }
        }
      }
    }

    (void)markhtabvs();

    if (itabvec) {
      natural
        i,
        n = header_element_count(header_of(itabvec));
      LispObj
        sym,
        *raw = 1+((LispObj *)ptr_from_lispobj(untag(itabvec)));

      for (i = 0; i < n; i++, raw++) {
        sym = *raw;
        if (is_symbol_fulltag(sym)) {
          natural dnode = gc_area_dnode(sym);

          if ((dnode < GCndnodes_in_area) &&
              (!ref_bit(GCmarkbits,dnode))) {
            *raw = unbound_marker;
          }
        }
      }
    }
  
    reap_gcable_ptrs();

    preforward_weakvll();

    GCrelocptr = global_reloctab;
    GCfirstunmarked = calculate_relocation();

    if (!GCephemeral_low) {
      reclaim_static_dnodes();
    }

    forward_range((LispObj *) ptr_from_lispobj(GCarealow), (LispObj *) ptr_from_lispobj(GCfirstunmarked));

    other_tcr = tcr;
    do {
      forward_tcr_xframes(other_tcr);
      forward_tcr_tlb(other_tcr);
      other_tcr = TCR_AUX(other_tcr)->next;
    } while (other_tcr != tcr);

  
    forward_gcable_ptrs();



    {
      area *next_area;
      area_code code;

      /* Could make a jump table instead of the typecase */

      for (next_area = a->succ; (code = next_area->code) != AREA_VOID; next_area = next_area->succ) {
        switch (code) {
        case AREA_TSTACK:
          forward_tstack_area(next_area);
          break;

        case AREA_VSTACK:
          forward_vstack_area(next_area);
          break;

        case AREA_CSTACK:
          forward_cstack_area(next_area);
          break;

        case AREA_STATIC:
	case AREA_WATCHED:
        case AREA_DYNAMIC:                  /* some heap that isn't "the" heap */
          if (next_area->younger == NULL) {
            forward_range((LispObj *) next_area->low, (LispObj *) next_area->active);
          }
          break;

        default:
          break;
        }
      }
    }

    if (GCephemeral_low) {
      forward_memoized_area(tenured_area, area_dnode(a->low, tenured_area->low), tenured_area->refbits);
      forward_memoized_area(managed_static_area,managed_static_area->ndnodes, managed_static_area->refbits);
    } else {
      forward_memoized_area(managed_static_area,area_dnode(managed_static_area->active,managed_static_area->low),managed_static_refbits);
    }
    a->active = (BytePtr) ptr_from_lispobj(compact_dynamic_heap());

    forward_weakvll_links();

    if (to) {
      tenure_to_area(to);
    }


    resize_dynamic_heap(a->active,
                        (GCephemeral_low == 0) ? lisp_heap_gc_threshold : 0);

    if (oldfree < a->high) {
      last_zeroed_addr = oldfree;
    } else {
      last_zeroed_addr = a->high;
    }
    zero_dnodes(a->active, area_dnode(last_zeroed_addr,a->active));

    /*
      If the EGC is enabled: If there's no room for the youngest
      generation, untenure everything.  If this was a full GC and
      there's now room for the youngest generation, tenure everything.
    */
    if (a->older != NULL) {
      natural nfree = (a->high - a->active);


      if (nfree < a->threshold) {
        untenure_from_area(tenured_area);
      } else {
        if (GCephemeral_low == 0) {
          tenure_to_area(tenured_area);
        }
      }
    }
  }
  lisp_global(GC_NUM) += (1<<fixnumshift);
  if (note) {
    note->gccount += (1<<fixnumshift);
  }

  if (GCDebug) {
    check_all_areas(tcr);
    check_static_cons_freelist("in post-gc static-cons check");
  }

  
  lisp_global(IN_GC) = 0;
  
  protect_watched_areas();

  nrs_GC_EVENT_STATUS_BITS.vcell |= gc_postgc_pending;
  get_time(stop);

  {
    lispsymbol * total_gc_microseconds = (lispsymbol *) &(nrs_TOTAL_GC_MICROSECONDS);
    lispsymbol * total_bytes_freed = (lispsymbol *) &(nrs_TOTAL_BYTES_FREED);
    LispObj val;
    struct timeval *timeinfo, elapsed = {0, 0};

    val = total_gc_microseconds->vcell;
    if ((fulltag_of(val) == fulltag_misc) &&
        (header_subtag(header_of(val)) == subtag_macptr)) {
      timersub(&stop, &start, &elapsed);
      timeinfo = (struct timeval *) ptr_from_lispobj(((macptr *) ptr_from_lispobj(untag(val)))->address);
      timeradd(timeinfo,  &elapsed, timeinfo);
      timeradd(timeinfo+timeidx,  &elapsed, timeinfo+timeidx);
    }

    val = total_bytes_freed->vcell;
    if ((fulltag_of(val) == fulltag_misc) &&
        (header_subtag(header_of(val)) == subtag_macptr)) {
      long long justfreed = oldfree - a->active;
      *( (long long *) ptr_from_lispobj(((macptr *) ptr_from_lispobj(untag(val)))->address)) += justfreed;

#ifdef USE_DTRACE
      if (note == tenured_area) {
	if (CCL_GC_FINISH_ENABLED()) {
	  natural bytes_freed = justfreed <= heap_segment_size ? 0 : justfreed;
	  CCL_GC_FINISH(bytes_freed);
	}
      } else {
	if (CCL_EGC_FINISH_ENABLED()) {
	  natural bytes_freed = justfreed <= heap_segment_size ? 0 : justfreed;
	  unsigned generation = (from == g2_area) ? 2 : (from == g1_area) ? 1 : 0;
	  CCL_EGC_FINISH(bytes_freed, generation);
	}
      }
#endif

      if (GCverbose) {
        char buf[16];
        paging_info paging_info_stop;

        sample_paging_info(&paging_info_stop);
        if (justfreed <= heap_segment_size) {
          justfreed = 0;
        }
        comma_output_decimal(buf,16,justfreed);
        if (note == tenured_area) {
          fprintf(dbgout,";;; Finished full GC. %s bytes freed in %d.%06d s\n\n", buf, elapsed.tv_sec, elapsed.tv_usec);
        } else {
          fprintf(dbgout,";;; Finished EGC of generation %d. %s bytes freed in %d.%06d s\n\n", 
                  (from == g2_area) ? 2 : (from == g1_area) ? 1 : 0,
                  buf, 
                  elapsed.tv_sec, elapsed.tv_usec);
        }
        report_paging_info_delta(dbgout, &paging_info_start, &paging_info_stop);
      }
    }
  }
}
