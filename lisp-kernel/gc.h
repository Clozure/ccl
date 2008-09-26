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

#ifndef __GC_H__
#define __GC_H__ 1

#include "lisp.h"
#include "bits.h"
#include "lisp-exceptions.h"
#include "memprotect.h"



#ifdef PPC
#define is_node_fulltag(f)  ((1<<(f))&((1<<fulltag_cons)|(1<<fulltag_misc)))
#ifdef PPC64
#define PPC64_CODE_VECTOR_PREFIX (('C'<< 24) | ('O' << 16) | ('D' << 8) | 'E')
#else
/*
  A code-vector's header can't look like a valid instruction or UUO:
  the low 8 bits must be subtag_code_vector, and the top 6 bits
  must be 0.  That means that the maximum length of a code vector
  is 18 bits worth of elements (~1MB.)
*/

#define code_header_mask ((0x3f<<26) | subtag_code_vector)
#endif
#endif

#ifdef X86
#ifdef X8664
#define is_node_fulltag(f)  ((1<<(f))&((1<<fulltag_cons)    | \
				       (1<<fulltag_tra_0)   | \
				       (1<<fulltag_tra_1)   | \
				       (1<<fulltag_misc)    | \
				       (1<<fulltag_symbol)  | \
				       (1<<fulltag_function)))
#else
#define is_node_fulltag(f)  ((1<<(f))&((1<<fulltag_cons) | \
				       (1<<fulltag_misc) | \
				       (1<<fulltag_tra)))
#endif
#endif


extern void zero_memory_range(BytePtr,BytePtr);
extern LispObj GCarealow, GCareadynamiclow;
extern natural GCndnodes_in_area, GCndynamic_dnodes_in_area;
extern bitvector GCmarkbits, GCdynamic_markbits;
LispObj *global_reloctab, *GCrelocptr;
LispObj GCfirstunmarked;

extern natural lisp_heap_gc_threshold;
void mark_root(LispObj);
void mark_pc_root(LispObj);
void mark_locative_root(LispObj);
void rmark(LispObj);
void postGCfree(void *);
LispObj *skip_over_ivector(LispObj, LispObj);
void mark_simple_area_range(LispObj *,LispObj *);
LispObj calculate_relocation();
LispObj locative_forwarding_address(LispObj);
LispObj node_forwarding_address(LispObj);
void forward_range(LispObj *, LispObj *);
void note_memoized_references(ExceptionInformation *,LogicalAddress, LogicalAddress, BytePtr *, BytePtr *);
void gc(TCR *, signed_natural);
int  purify(TCR *, signed_natural);
int impurify(TCR *, signed_natural);
int change_hons_area_size(TCR *, signed_natural);
void delete_protected_area(protected_area_ptr);
Boolean egc_control(Boolean, BytePtr);
Boolean free_segments_zero_filled_by_OS;

/* an type representing 1/4 of a natural word */
#if WORD_SIZE == 64
typedef unsigned short qnode;
#else
typedef unsigned char qnode;
#endif


#ifdef fulltag_symbol
#define is_symbol_fulltag(x) (fulltag_of(x) == fulltag_symbol)
#else
#define is_symbol_fulltag(x) (fulltag_of(x) == fulltag_misc)
#endif

#define area_dnode(w,low) ((natural)(((ptr_to_lispobj(w)) - ptr_to_lispobj(low))>>dnode_shift))
#define gc_area_dnode(w)  area_dnode(w,GCarealow)
#define gc_dynamic_area_dnode(w) area_dnode(w,GCareadynamiclow)

#if defined(PPC64) || defined(X8632)
#define forward_marker subtag_forward_marker
#else
#define forward_marker fulltag_nil
#endif

#ifdef PPC64
#define VOID_ALLOCPTR ((LispObj)(0x8000000000000000-dnode_size))
#else
#define VOID_ALLOCPTR ((LispObj)(-dnode_size))
#endif

#ifdef DARWIN
#include <mach/task_info.h>
typedef struct task_events_info paging_info;
#else
#ifndef WINDOWS
#include <sys/resource.h>
typedef struct rusage paging_info;
#else
typedef natural paging_info;
#endif
#endif

#undef __argv
#include <stdio.h>

void sample_paging_info(paging_info *);
void report_paging_info_delta(FILE*, paging_info *, paging_info *);


#define GC_TRAP_FUNCTION_IMMEDIATE_GC (-1)
#define GC_TRAP_FUNCTION_GC 0
#define GC_TRAP_FUNCTION_PURIFY 1
#define GC_TRAP_FUNCTION_IMPURIFY 2
#define GC_TRAP_FUNCTION_SAVE_APPLICATION 8

#define GC_TRAP_FUNCTION_GET_LISP_HEAP_THRESHOLD 16
#define GC_TRAP_FUNCTION_SET_LISP_HEAP_THRESHOLD 17
#define GC_TRAP_FUNCTION_USE_LISP_HEAP_THRESHOLD 18
#define GC_TRAP_FUNCTION_EGC_CONTROL 32
#define GC_TRAP_FUNCTION_CONFIGURE_EGC 64
#define GC_TRAP_FUNCTION_SET_HONS_AREA_SIZE 128 /* deprecated */
#define GC_TRAP_FUNCTION_FREEZE 129
#define GC_TRAP_FUNCTION_THAW 130

Boolean GCDebug, GCverbose, just_purified_p;
bitvector GCmarkbits, GCdynamic_markbits;
LispObj GCarealow, GCareadynamiclow;
natural GCndnodes_in_area, GCndynamic_dnodes_in_area;
LispObj GCweakvll,GCdwsweakvll;
LispObj GCephemeral_low;
natural GCn_ephemeral_dnodes;
natural GCstack_limit;

#if WORD_SIZE == 64
unsigned short *_one_bits;
#else
const unsigned char _one_bits[256];
#endif

#define one_bits(x) _one_bits[x]

natural static_dnodes_for_area(area *a);
void reapweakv(LispObj weakv);
void reaphashv(LispObj hashv);
Boolean mark_weak_hash_vector(hash_table_vector_header *hashp, natural elements);
Boolean mark_weak_alist(LispObj weak_alist, int weak_type);
void mark_tcr_tlb(TCR *);
void mark_tcr_xframes(TCR *);
void postGCfree(void *);
void freeGCptrs(void);
void reap_gcable_ptrs(void);
unsigned short logcount16(unsigned short);
void gc_init(void);
LispObj node_forwarding_address(LispObj);
Boolean update_noderef(LispObj *);
void update_locref(LispObj *);
void forward_gcable_ptrs(void);
void forward_memoized_area(area *, natural);
void forward_tcr_tlb(TCR *);
void reclaim_static_dnodes(void);
Boolean youngest_non_null_area_p(area *);
void gc(TCR *, signed_natural);

/* backend-interface */

typedef void (*weak_mark_fun) (LispObj);
weak_mark_fun mark_weak_htabv, dws_mark_weak_htabv;

typedef void (*weak_process_fun)(void);

weak_process_fun markhtabvs;


#define hash_table_vector_header_count (sizeof(hash_table_vector_header)/sizeof(LispObj))

void mark_root(LispObj);
void rmark(LispObj);
#ifdef X8632
void mark_xp(ExceptionInformation *, natural);
#else
void mark_xp(ExceptionInformation *);
#endif
LispObj dnode_forwarding_address(natural, int);
LispObj locative_forwarding_address(LispObj);
void check_refmap_consistency(LispObj *, LispObj *, bitvector);
void check_all_areas(void);
void mark_tstack_area(area *);
void mark_vstack_area(area *);
void mark_cstack_area(area *);
void mark_simple_area_range(LispObj *, LispObj *);
void mark_memoized_area(area *, natural);
LispObj calculate_relocation(void);
void forward_range(LispObj *, LispObj *);
void forward_tstack_area(area *);
void forward_vstack_area(area *);
void forward_cstack_area(area *);
LispObj compact_dynamic_heap(void);
int purify(TCR *, signed_natural);
int impurify(TCR *, signed_natural);

#endif                          /* __GC_H__ */
