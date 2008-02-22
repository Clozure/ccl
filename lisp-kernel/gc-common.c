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
bitvector GCmarkbits = NULL, GCdynamic_markbits = NULL;
LispObj GCarealow = 0, GCareadynamiclow = 0;
natural GCndnodes_in_area = 0, GCndynamic_dnodes_in_area = 0;
LispObj GCweakvll = (LispObj)NULL;
LispObj GCdwsweakvll = (LispObj)NULL;
LispObj GCephemeral_low = 0;
natural GCn_ephemeral_dnodes = 0;
natural GCstack_limit = 0;


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
    if (termination_list != lisp_nil) {
      deref(weakv,1) = GCweakvll;
      GCweakvll = weakv;
    }
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
    dnode,
    npairs = (header_element_count(hashp->header) - 
              (hash_table_vector_header_count -1)) >> 1;
  LispObj *pairp = (LispObj*) (hashp+1), weakelement;
  Boolean 
    weak_on_value = ((hashp->flags & nhash_weak_value_mask) != 0);
  bitvector markbits = GCmarkbits;
  int tag;

  while (npairs--) {
    if (weak_on_value) {
      weakelement = pairp[1];
    } else {
      weakelement = pairp[0];
    }
    tag = fulltag_of(weakelement);
    if (is_node_fulltag(tag)) {
      dnode = gc_area_dnode(weakelement);
      if ((dnode < GCndnodes_in_area) && 
          ! ref_bit(markbits, dnode)) {
        pairp[0] = slot_unbound;
        pairp[1] = lisp_nil;
        hashp->weak_deletions_count += (1<<fixnumshift);
      }
    }
    pairp += 2;
  }
}

void
traditional_dws_mark_htabv(LispObj htabv)
{
  /* Do nothing, just add htabv to GCweakvll */
  LispObj *base = (LispObj *) ptr_from_lispobj(untag(htabv));
  
  deref(base,1) = GCweakvll;
  GCweakvll = htabv;
}

void
ncircle_dws_mark_htabv(LispObj htabv)
{
  /* Do nothing, just add htabv to GCdwsweakvll */
  LispObj *base = (LispObj *) ptr_from_lispobj(untag(htabv));
  
  deref(base,1) = GCdwsweakvll;
  GCdwsweakvll = htabv;
}

void
traditional_mark_weak_htabv(LispObj htabv)
{
  int i, skip = hash_table_vector_header_count;;

  for (i = 2; i <= skip; i++) {
    rmark(deref(htabv,i));
  }

  deref(htabv,1) = GCweakvll;
  GCweakvll = htabv;
}

void
ncircle_mark_weak_htabv(LispObj htabv)
{
  int i, skip = hash_table_vector_header_count;
  hash_table_vector_header *hashp = (hash_table_vector_header *)(untag(htabv));
  natural
    dnode,
    npairs = (header_element_count(hashp->header) - 
              (hash_table_vector_header_count - 1)) >> 1;
  LispObj *pairp = (LispObj*) (hashp+1), weakelement;
  Boolean 
    weak_on_value = ((hashp->flags & nhash_weak_value_mask) != 0);
  bitvector markbits = GCmarkbits;
  int tag;

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

  deref(htabv,1) = GCweakvll;
  GCweakvll = htabv;
}


Boolean
mark_weak_hash_vector(hash_table_vector_header *hashp, natural elements)
{
  natural flags = hashp->flags, key_dnode, val_dnode;
  Boolean 
    marked_new = false, 
    key_marked,
    val_marked,
    weak_value = ((flags & nhash_weak_value_mask) != 0);
  int 
    skip = hash_table_vector_header_count-1,
    key_tag,
    val_tag,
    i;
  LispObj 
    *pairp = (LispObj*) (hashp+1),
    key,
    val;

  /* Mark everything in the header */
  
  for (i = 2; i<= skip; i++) {
    mark_root(deref(ptr_to_lispobj(hashp),i));
  }

  elements -= skip;

  for (i = 0; i<elements; i+=2, pairp+=2) {
    key = pairp[0];
    val = pairp[1];
    key_marked = val_marked = true;
    key_tag = fulltag_of(key);
    val_tag = fulltag_of(val);
    if (is_node_fulltag(key_tag)) {
      key_dnode = gc_area_dnode(key);
      if ((key_dnode < GCndnodes_in_area) &&
          ! ref_bit(GCmarkbits,key_dnode)) {
        key_marked = false;
      }
    }
    if (is_node_fulltag(val_tag)) {
      val_dnode = gc_area_dnode(val);
      if ((val_dnode < GCndnodes_in_area) &&
          ! ref_bit(GCmarkbits,val_dnode)) {
        val_marked = false;
      }
    }

    if (weak_value) {
      if (val_marked & !key_marked) {
        mark_root(key);
        marked_new = true;
      }
    } else {
      if (key_marked & !val_marked) {
        mark_root(val);
        marked_new = true;
      }
    }
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
        ((dnode = gc_area_dnode(pair_tag)) < GCndnodes_in_area) &&
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
traditional_markhtabvs()
{
  LispObj this, header, pending;
  int subtag;
  bitvector markbits = GCmarkbits;
  hash_table_vector_header *hashp;
  Boolean marked_new;

  do {
    pending = (LispObj) NULL;
    marked_new = false;
    
    while (GCweakvll) {
      this = GCweakvll;
      GCweakvll = deref(this,1);
      
      header = header_of(this);
      subtag = header_subtag(header);
      
      if (subtag == subtag_weak) {
        natural weak_type = deref(this,2);
        deref(this,1) = pending;
        pending = this;
        if ((weak_type & population_type_mask) == population_weak_alist) {
          if (mark_weak_alist(this, weak_type)) {
            marked_new = true;
          }
        }
      } else if (subtag == subtag_hash_vector) {
        natural elements = header_element_count(header), i;

        hashp = (hash_table_vector_header *) ptr_from_lispobj(untag(this));
        if (hashp->flags & nhash_weak_mask) {
          deref(this,1) = pending;
          pending = this;
          if (mark_weak_hash_vector(hashp, elements)) {
            marked_new = true;
          }
        } 
      } else {
        Bug(NULL, "Strange object on weak vector linked list: 0x~08x\n", this);
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
    this = pending;
    pending = deref(this,1);
    deref(this,1) = (LispObj)NULL;

    subtag = header_subtag(header_of(this));
    if (subtag == subtag_weak) {
      reapweakv(this);
    } else {
      reaphashv(this);
    }
  }

  /* Finally, mark the termination lists in all terminatable weak vectors
     They are now linked together on GCweakvll.
     This is where to store  lisp_global(TERMINATION_LIST) if we decide to do that,
     but it will force terminatable popualations to hold on to each other
     (set TERMINATION_LIST before clearing GCweakvll, and don't clear deref(this,1)).
     */
  pending = GCweakvll;
  GCweakvll = (LispObj)NULL;
  while (pending) {
    this = pending;
    pending = deref(this,1);
    deref(this,1) = (LispObj)NULL;
    mark_root(deref(this,1+3));
  }
}

void
ncircle_markhtabvs()
{
  LispObj this, header, pending = 0;
  int subtag;
  bitvector markbits = GCmarkbits;
  hash_table_vector_header *hashp;
  Boolean marked_new;

  /* First, process any weak hash tables that may have
     been encountered by the link-inverting marker; we
     should have more stack space now. */

  while (GCdwsweakvll) {
    this = GCdwsweakvll;
    GCdwsweakvll = deref(this,1);
    ncircle_mark_weak_htabv(this);
  }

  while (GCweakvll) {
    this = GCweakvll;
    GCweakvll = deref(this,1);
      
    header = header_of(this);
    subtag = header_subtag(header);
      
    if (subtag == subtag_weak) {
      natural weak_type = deref(this,2);
      deref(this,1) = pending;
      pending = this;
      if ((weak_type & population_type_mask) == population_weak_alist) {
        if (mark_weak_alist(this, weak_type)) {
          marked_new = true;
          }
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
    this = pending;
    pending = deref(this,1);
    deref(this,1) = (LispObj)NULL;

    subtag = header_subtag(header_of(this));
    if (subtag == subtag_weak) {
      reapweakv(this);
    } else {
      Bug(NULL, "Bad object on pending list: %s\n", this);
    }
  }

  /* Finally, mark the termination lists in all terminatable weak vectors
     They are now linked together on GCweakvll.
     This is where to store  lisp_global(TERMINATION_LIST) if we decide to do that,
     but it will force terminatable popualations to hold on to each other
     (set TERMINATION_LIST before clearing GCweakvll, and don't clear deref(this,1)).
     */
  pending = GCweakvll;
  GCweakvll = (LispObj)NULL;
  while (pending) {
    this = pending;
    pending = deref(this,1);
    deref(this,1) = (LispObj)NULL;
    mark_root(deref(this,1+3));
  }
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

  xp = tcr->gc_context;
  if (xp) {
    mark_xp(xp);
  }
  
  for (xframes = (xframe_list *) tcr->xframe; 
       xframes; 
       xframes = xframes->prev) {
      mark_xp(xframes->curr);
  }
}
      

void *postGCptrs = NULL;

void
postGCfree(void *p)
{
  *(void **)p = postGCptrs;
  postGCptrs = p;
}

void
freeGCptrs()
{
  void *p, *next;

  for (p = postGCptrs; p; p = next) {
    next = *((void **)p);
    free(p);
  }
  postGCptrs = NULL;
}

void
reap_gcable_ptrs()
{
  LispObj *prev = &(lisp_global(GCABLE_POINTERS)), next, ptr;
  xmacptr_flag flag;
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
      flag = (xmacptr_flag)(x->flags);
      ptr = x->address;

      if (ptr) {
        switch (flag) {
        case xmacptr_flag_recursive_lock:
	  destroy_recursive_lock((RECURSIVE_LOCK)ptr_from_lispobj(ptr));
          break;

        case xmacptr_flag_ptr:
	  postGCfree((void *)ptr_from_lispobj(ptr));
          break;

        case xmacptr_flag_rwlock:
          rwlock_destroy((rwlock *)ptr_from_lispobj(ptr));
          break;

        case xmacptr_flag_semaphore:
	  destroy_semaphore((void**)&(x->address));
          break;

        default:
          /* (warn "unknown xmacptr_flag: ~s" flag) */
          /* Unknowd, and perhaps unknowdable. */
          /* Fall in: */
        case xmacptr_flag_none:
          break;
        }
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

  while ((next = *prev) != (LispObj)NULL) {
    new = node_forwarding_address(next);
    if (new != next) {
      *prev = new;
    }
    prev = &(((xmacptr *)ptr_from_lispobj(untag(next)))->link);
  }
}

void
forward_memoized_area(area *a, natural num_memo_dnodes)
{
  bitvector refbits = a->refbits;
  LispObj *p = (LispObj *) a->low, x1, x2, new;
  natural bits, bitidx, *bitsp, nextbit, diff, memo_dnode = 0, hash_dnode_limit = 0;
  int tag_x1;
  hash_table_vector_header *hashp = NULL;
  Boolean header_p;

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
      bits = *++bitsp;
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
        }
      }
      p++;

      new = node_forwarding_address(x2);
      if (new != x2) {
        *p = new;
        if (memo_dnode < hash_dnode_limit) {
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
  natural nstatic = tenured_area->static_dnodes, i, bits, mask, bitnum;
  cons *c = (cons *)tenured_area->low, *d;
  bitvector bitsp = GCmarkbits;
  LispObj head = lisp_global(STATIC_CONSES);

  if (nstatic) {
    if (head) {
      for (i = 0; i < nstatic; i+= nbits_in_word, c+= nbits_in_word) {
        bits = *bitsp++;
        if (bits != ALL_ONES) {
          for (bitnum = 0; bitnum < nbits_in_word; bitnum++) {
            if (! (bits & (BIT0_MASK>>bitnum))) {
              d = c + bitnum;
              d->car = 0;
              d->cdr = head;
              head = ((LispObj)d)+fulltag_cons;
            }
          }
        }
      }
      lisp_global(STATIC_CONSES) = head;
    } else {
      for (i = 0; i < nstatic; i+= nbits_in_word, c+= nbits_in_word) {
        bits = *bitsp++;
        if (bits != ALL_ONES) {
          for (bitnum = 0; bitnum < nbits_in_word; bitnum++) {
            if (! (bits & (BIT0_MASK>>bitnum))) {
              d = c + bitnum;
              d->car = 0;
              d->cdr = 0;
            }
          }
        }
      }
    }
  }
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

#ifdef WINDOWS
#define get_time(when) /* FIXME */
#else
#define get_time(when) gettimeofday(&when, NULL)
#endif


#ifdef FORCE_DWS_MARK
#warning recursive marker disabled for testing; remember to re-enable it
#endif


void 
gc(TCR *tcr, signed_natural param)
{
  xframe_list *xframes = (tcr->xframe);
  struct timeval start, stop;
  area *a = active_dynamic_area, *to = NULL, *from = NULL, *note = NULL;
  unsigned timeidx = 1;
  paging_info paging_info_start;
  xframe_list *x;
  LispObj
    pkg,
    itabvec = 0;
  BytePtr oldfree = a->active;
  TCR *other_tcr;
  natural static_dnodes;

  install_weak_mark_functions(lisp_global(WEAK_GC_METHOD) >> fixnumshift);



#ifndef FORCE_DWS_MARK
  if ((natural) (tcr->cs_limit) == CS_OVERFLOW_FORCE_LIMIT) {
    GCstack_limit = CS_OVERFLOW_FORCE_LIMIT;
  } else {
    GCstack_limit = (natural)(tcr->cs_limit)+(natural)page_size;
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

  if (GCverbose) {
    char buf[16];

    sample_paging_info(&paging_info_start);
    comma_output_decimal(buf,16,area_dnode(oldfree,a->low) << dnode_shift);
    if (GCephemeral_low) {
      fprintf(stderr,
              "\n\n;;; Starting Ephemeral GC of generation %d",
              (from == g2_area) ? 2 : (from == g1_area) ? 1 : 0); 
    } else {
      fprintf(stderr,"\n\n;;; Starting full GC");
    }
    fprintf(stderr, ", %s bytes allocated.\n", buf);
  }

  get_time(start);
  lisp_global(IN_GC) = (1<<fixnumshift);

  if (just_purified_p) {
    just_purified_p = false;
    GCDebug = false;
  } else {
    GCDebug = ((nrs_GC_EVENT_STATUS_BITS.vcell & gc_integrity_check_bit) != 0);
    if (GCDebug) {
      check_all_areas();
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
    GCweakvll = (LispObj)NULL;

    if (GCn_ephemeral_dnodes == 0) {
      /* For GCTWA, mark the internal package hash table vector of
       *PACKAGE*, but don't mark its contents. */
      {
        LispObj
          itab;
        natural
          dnode, ndnodes;
      
        pkg = nrs_PACKAGE.vcell;
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
  
    if (lisp_global(OLDEST_EPHEMERAL)) {
      mark_memoized_area(tenured_area, area_dnode(a->low,tenured_area->low));
    }

    other_tcr = tcr;
    do {
      mark_tcr_xframes(other_tcr);
      mark_tcr_tlb(other_tcr);
      other_tcr = other_tcr->next;
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
          lispsymbol *rawsym = (lispsymbol *)ptr_from_lispobj(untag(sym));
          natural dnode = gc_area_dnode(sym);

          if ((dnode < GCndnodes_in_area) &&
              (!ref_bit(GCmarkbits,dnode))) {
            *raw = unbound_marker;
          }
        }
      }
    }
  
    reap_gcable_ptrs();

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
      other_tcr = other_tcr->next;
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
      forward_memoized_area(tenured_area, area_dnode(a->low, tenured_area->low));
    }
  
    a->active = (BytePtr) ptr_from_lispobj(compact_dynamic_heap());
    if (to) {
      tenure_to_area(to);
    }

    zero_memory_range(a->active, oldfree);

    resize_dynamic_heap(a->active,
                        (GCephemeral_low == 0) ? lisp_heap_gc_threshold : 0);

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
    check_all_areas();
  }

  
  lisp_global(IN_GC) = 0;

  nrs_GC_EVENT_STATUS_BITS.vcell |= gc_postgc_pending;
  get_time(stop);

  {
    lispsymbol * total_gc_microseconds = (lispsymbol *) &(nrs_TOTAL_GC_MICROSECONDS);
    lispsymbol * total_bytes_freed = (lispsymbol *) &(nrs_TOTAL_BYTES_FREED);
    LispObj val;
    struct timeval *timeinfo, elapsed;

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
      if (GCverbose) {
        char buf[16];
        paging_info paging_info_stop;

        sample_paging_info(&paging_info_stop);
        if (justfreed <= heap_segment_size) {
          justfreed = 0;
        }
        comma_output_decimal(buf,16,justfreed);
        if (note == tenured_area) {
          fprintf(stderr,";;; Finished full GC. %s bytes freed in %d.%06d s\n\n", buf, elapsed.tv_sec, elapsed.tv_usec);
        } else {
          fprintf(stderr,";;; Finished EGC of generation %d. %s bytes freed in %d.%06d s\n\n", 
                  (from == g2_area) ? 2 : (from == g1_area) ? 1 : 0,
                  buf, 
                  elapsed.tv_sec, elapsed.tv_usec);
        }
        report_paging_info_delta(stderr, &paging_info_start, &paging_info_stop);
      }
    }
  }
}
