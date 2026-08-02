/* SPDX-License-Identifier: Apache-2.0 */

/* PPC64 LINE-PORT (source: vendor/ccl/lisp-kernel/ppc-gc.c)
 *
 * Arch-dependent GC for Matt Emerson's upstream ARM64 (low-tag) design;
 * the missing arm64-gc.o in linuxarm64/Makefile:57 (compiles alongside
 * the arch-independent gc-common.c).  Tree pin: upstream-arm64-tip @
 * 115b7aa.  Companion report (contract, decisions, open items):
 * drafts/arm64-gc-report.md.
 *
 * PORT-NOTE â€” deviations from the PPC64 source (each tagged inline):
 *  1. Tag scheme: Matt's arm64 fulltags mirror X8664's *shape* (symbols
 *     and functions have their OWN pointer tags; cons=3, nil=11,
 *     RMARK_PREV_CAR = fulltag_cons + node_size = fulltag_nil exactly as
 *     on X8664).  Where PPC64 logic is tag-scheme-specific (rmark's
 *     link-inversion FSM, is_node_fulltag, mark_ephemeral_root), the
 *     X8664 rendering of the same logic is the analog and is cited as
 *     x86-gc.c:NNN.
 *  2. pc locatives: PPC64's backward grovel for the 32-bit 'CODE' prefix
 *     becomes a grovel for 0x00000000 = the `udf #0' sentinel that starts
 *     every arm64 code vector (arm64-uuo.s format 0).  No CTR, no loc_pc
 *     register on ARM64: pc roots are xpPC/xpLR only.
 *  3. cstack: ARM64 uses MARKER lisp_frames {marker,savevsp,savefn,savelr}
 *     (arm64-constants.h:359-364), not PPC backlink frames; the four
 *     cstack walkers follow arm-gc.c's marker walk, with the case ORDER
 *     corrected for arm64 header fulltags (see mark_cstack_area).
 *  4. tstack: Matt's design HAS a tsp/tstack (unlike ARM32) â€” the tstack
 *     walkers port from PPC verbatim.
 *  5. Function objects are {header, entrypoint, codevector, ...}
 *     (arm64-constants.h:300-304).  Every site that must treat the
 *     entrypoint slot as a locative is tagged OPEN-ENTRYPOINT: the
 *     call-side convention is unsettled upstream; the ARM32 convention
 *     (entrypoint = untag(codevector)+fulltag_odd_fixnum) is assumed,
 *     under which the plain node walkers skip it as a fixnum.
 *  6. VENDOR BUG FIXED (see report Â§5): ppc-gc.c:1795-1798 (PPC64 branch
 *     of purify_locref) forgets to include the header word in the
 *     displacement when groveling (the PPC32 branch, which finds the
 *     header itself rather than the prefix, is correct).  Our grovel adds
 *     the missing node_size.
 *
 * Register-number authority: arm64-exceptions.c enum (map unified
 * upstream @ 01d73c3): imm0-5=x0-5, nargs=x6, fn=x7, arg_w-z=x8-11,
 * temp0-5=x12-17, save0-3=x19-22, rnil=x23, tsp=x24, vsp=x25,
 * allocptr=x26, allocbase=x27, rcontext=x28.
 *
 * PROPOSED register SET marked by mark_xp/forward_xp/purify_xp/
 * impurify_xp (ratify with Matt): the BOXED node registers
 * fn(7)..rnil(23) EXCEPT x18 (platform register).  Excluded: imm0-5 and
 * nargs (unboxed/fixnum), tsp/vsp/rcontext/fp (raw pointers),
 * allocptr/allocbase (mid-allocation state is normalized by
 * pc_luser_xp on the exception side before gc runs, as on ARM32).
 * pc locatives: xpPC, xpLR.
 */

#include "lisp.h"                 /* ppc-gc.c:17 */
#include "lisp_globals.h"         /* ppc-gc.c:18 */
#include "bits.h"                 /* ppc-gc.c:19 */
#include "gc.h"                   /* ppc-gc.c:20 */
#include "area.h"                 /* ppc-gc.c:21 */
#include "threads.h"              /* ppc-gc.c:22 */
#include <stddef.h>               /* ppc-gc.c:23 */
#include <stdlib.h>               /* ppc-gc.c:24 */
#include <string.h>               /* ppc-gc.c:25 */
#include <sys/time.h>             /* ppc-gc.c:26 */
#include <sys/mman.h>             /* for munmap in impurify (ppc-gc.c:2283
                                     calls munmap; PPC got the prototype
                                     transitively) */

/* ------------------------------------------------------------------ */
/* Guarded shims for headers that lack an ARM64 branch at the pin.
 * (linuxarm64/Makefile:27 defines -DARM64 only â€” NOT -DARM â€” so the
 * PPC/X86/ARM ladders in gc.h/macros.h/lisp_globals.h all fall through.)
 * Each is #ifndef-guarded so the real upstream branch supersedes it.   */

/* lisp_globals.h grew a real ARM64 branch @ 93d72a0 (nil-anchored via
 * the runtime lisp_nil) -- the fixed-address shim retired. */

/* PROPOSED upstream gc.h ARM64 branch: node fulltags are cons, misc,
 * symbol (fulltag_function removed, patch 0055: a function is an ordinary
 * miscobj).  No x86 tra tags: arm64 return addresses are RAW
 * 4-byte-aligned locatives, not node-tagged.
 * fulltag_nil excluded: only NIL carries it and NIL is static
 * (x8632/x8664 precedent). */
#ifndef is_node_fulltag
#define is_node_fulltag(f)  ((1<<(f))&((1<<fulltag_cons)     | \
                                       (1<<fulltag_misc)     | \
                                       (1<<fulltag_symbol)))
#endif

/* PROPOSED upstream macros.h ARM64 branch (macros.h:53-83 ladder has no
 * ARM64 case).  arm64 nodeheader fulltags = {6,14}, immheader = {5,12,13}
 * (arm64-constants.h:120-135); X8664-mask style (macros.h:64-73). */
#ifndef nodeheader_tag_p
#define NODEHEADER_MASK ((1<<fulltag_nodeheader_0) | \
                         (1<<fulltag_nodeheader_1))
#define nodeheader_tag_p(tag) ((1<<(tag)) & NODEHEADER_MASK)
#define IMMHEADER_MASK ((1<<fulltag_immheader_0) | \
                        (1<<fulltag_immheader_1) | \
                        (1<<fulltag_immheader_2))
#define immheader_tag_p(tag) ((1<<(tag)) & IMMHEADER_MASK)
#endif

/* 16m10: a corrupt/garbage uvector header decodes to an absurd element
 * count and the suffix set_n_bits memsets far past the markbits buffer
 * (observed: a pointer misread as a header claimed a 51GB object; the
 * ~400MB memset ran off the mapping and the SEGV handler looped at
 * 100% CPU).  Any claimed extent beyond the GC area is proof of a
 * corrupt header - die loudly at the object, not in the storm. */
static void
check_marked_extent(LispObj n, natural dnode, natural suffix_dnodes)
{
  if ((dnode + 1 + suffix_dnodes) > GCndnodes_in_area) {
    Bug(NULL, "GC: object 0x%lx (dnode 0x%lx) claims 0x%lx suffix dnodes"
        " but the area has only 0x%lx - corrupt uvector header?",
        (unsigned long)n, (unsigned long)dnode,
        (unsigned long)suffix_dnodes, (unsigned long)GCndnodes_in_area);
  }
}

/* forward_marker: gc.h:106-114 falls through to `fulltag_nil' (11) under
 * -DARM64.  Usable as-is: 0xB as a raw slot word would be a fulltag_nil
 * "pointer" to address 0 â€” never a legal object, header, or 4-aligned
 * locative.  (Had -DARM been defined we'd have inherited ARM32's udf
 * encoding, which would be wrong here.)  No local definition needed.   */

/* The udf #0 sentinel word that STARTS every arm64 code vector
 * (arm64-uuo.s:25-27 format 0: "udf #0 is the sentinel at the start of
 * every code vector"; A64 UDF encodes as 0x0000imm16 â†’ udf #0 is
 * 0x00000000).  Replaces PPC64_CODE_VECTOR_PREFIX ('CODE', gc.h:30) in
 * the pc-locative â†’ header grovel.  Alignment: code-vector headers are
 * dnode(16)-aligned, so the sentinel (element 0) sits at header+8, an
 * 8-aligned address that the 8-byte-stepped grovel always lands on.
 * ASSUMPTION (report OPEN #8): no other 0x00000000 word at an 8-aligned
 * offset inside the instruction stream (no inline literal pools).      */
#define ARM64_CODE_VECTOR_SENTINEL 0

/* PROPOSED (ratify with Matt): arm64-constants.h defines
 * subtag_lisp_frame_marker = SUBTAG(fulltag_imm_1,5) but no
 * stack_alloc_marker yet (it exists only in the STALE high-tag
 * arm64-constants.s:112).  ARM32 keeps the two adjacent
 * (arm-constants.h:245-246); propose the next imm_1 slot.              */
#ifndef stack_alloc_marker
#define stack_alloc_marker SUBTAG(fulltag_imm_1, 6)
#endif

/* The marker lisp_frame C overlay lives in platform-linuxarm64.h
 * (albt.c and arm64-exceptions.c consume it too). */

/* ===== cstack walk trail: make a walk failure name its own cause ==========
 *
 * KEEP (16m41).  The bare Bug() this replaced named the function and NOTHING
 * else -- not which of the walk's five strides overshot, not the word it strode
 * on, not whether the region leading in was a frame, an nfp ivector or a
 * stack-consed vector -- and that cost two sessions.  With the trail, ONE run
 * named the cause: three marker frames, then an x29 hop and four zero words
 * (i.e. C frames), then a spilled 0.9d0 read as an ivector header.  That is the
 * whole diagnosis of the missing lisp<->foreign boundary protocol (see
 * spentry-E-ffi.s), and it cost a single stage-11 cycle instead of a build per
 * question.  The cost on the healthy path is a few stores per stride over a
 * walk that is hundreds of steps long: keep it.
 *
 * Reports, then Bug()s -- the walk has no way to continue correctly, and the
 * clamp-and-continue used during the 16m41 diagnosis leaves marking INCOMPLETE
 * (it exists only so a diagnostic run can show the pattern across GCs; set
 * CSTACK_MAX_REPORTS > 1 to get it back).                                    */
enum {
  CB_FRAME = 0,                 /* lisp_frame_marker: stride sizeof(lisp_frame) */
  CB_MARKER0,                   /* stack_alloc_marker or 0: stride 2 */
  CB_NODE,                      /* nodeheader: stride 1+elements (+pad) */
  CB_IMM,                       /* immheader: skip_over_ivector (nfp frames) */
  CB_BACKLINK,                  /* (word & fixnummask)==0: current = word */
  CB_NBRANCH
};

static const char *cstack_branch_name[CB_NBRANCH] = {
  "lisp_frame", "marker/zero", "nodeheader", "immheader", "backlink"
};

typedef struct {
  LispObj at, word, next;
  int br;
} cstack_step;

#define CSTACK_TRAIL 12
#define CSTACK_MAX_REPORTS 1    /* >1 = diagnostic mode: clamp and keep going */

static natural cstack_walk_reports = 0;

#define CSTACK_TRAIL_DECL \
  cstack_step _ctrail[CSTACK_TRAIL]; \
  natural _cnsteps = 0, _chisto[CB_NBRANCH] = {0, 0, 0, 0, 0}; \
  LispObj *_cfrom = NULL

/* The parameter is _br, not br: a macro parameter is substituted after `->'
   too, so naming it `br' turns `_s->br' into `_s->CB_FRAME'. */
#define CSTACK_TRAIL_STEP(_br)                                  \
  do {                                                          \
    cstack_step *_s = _ctrail + (_cnsteps % CSTACK_TRAIL);       \
    _s->at = (LispObj)_cfrom;                                   \
    _s->word = header;                                          \
    _s->next = (LispObj)current;                                \
    _s->br = (_br);                                             \
    _cnsteps++;                                                 \
    _chisto[_br]++;                                             \
  } while (0)

/* Returns 1 if the caller may clamp and keep walking, 0 if it must Bug out. */
static int
cstack_walk_report(const char *who, const char *why, area *a,
                   LispObj *current, LispObj *limit,
                   cstack_step *trail, natural nsteps, natural *histo)
{
  natural i, first = (nsteps > CSTACK_TRAIL) ? nsteps - CSTACK_TRAIL : 0;
  LispObj *p;

  fprintf(dbgout, "\n*** %s: %s (report %lu of %d)\n",
          who, why, (unsigned long)cstack_walk_reports + 1, CSTACK_MAX_REPORTS);
  fprintf(dbgout, "  area    low=0x%lx active=0x%lx high=0x%lx  (walked range = %ld words)\n",
          (unsigned long)a->low, (unsigned long)a->active, (unsigned long)a->high,
          (long)(((LispObj *)a->high) - ((LispObj *)a->active)));
  fprintf(dbgout, "  walk    current=0x%lx limit=0x%lx  past-limit=%ld words  steps=%lu\n",
          (unsigned long)current, (unsigned long)limit,
          (long)(current - limit), (unsigned long)nsteps);
  /* Owner state.  nfp says whether an nfp ivector frame was live at the
     safepoint (compare it against the step bases below), last_lisp_frame and an
     odd valence say whether this thread was in foreign code -- which is the
     other way a->active can name a word the walk cannot classify. */
  if (a->owner) {
    TCR *tcr = a->owner;
    fprintf(dbgout, "  owner   tcr=0x%lx valence=%ld%s nfp=0x%lx last_lisp_frame=0x%lx\n"
                    "          save_vsp=0x%lx save_tsp=0x%lx cs_limit=0x%lx\n",
            (unsigned long)tcr, (long)tcr->valence,
            (tcr->valence & 1) ? " (ODD = in foreign code)" : "",
            (unsigned long)tcr->nfp, (unsigned long)tcr->last_lisp_frame,
            (unsigned long)tcr->save_vsp, (unsigned long)tcr->save_tsp,
            (unsigned long)tcr->cs_limit);
  }
  fprintf(dbgout, "  strides taken:");
  for (i = 0; i < CB_NBRANCH; i++) {
    fprintf(dbgout, " %s=%lu", cstack_branch_name[i], (unsigned long)histo[i]);
  }
  fprintf(dbgout, "\n");

  if (nsteps == 0) {
    fprintf(dbgout, "  NO steps taken -- the FIRST word at a->active failed; a->active is the suspect\n");
  } else {
    fprintf(dbgout, "  last %lu steps (oldest first):\n", (unsigned long)(nsteps - first));
    for (i = first; i < nsteps; i++) {
      cstack_step *s = trail + (i % CSTACK_TRAIL);
      fprintf(dbgout, "    at 0x%lx word 0x%-18lx %-12s -> 0x%lx (%+ld words)\n",
              (unsigned long)s->at, (unsigned long)s->word,
              cstack_branch_name[s->br], (unsigned long)s->next,
              (long)(((LispObj *)s->next) - ((LispObj *)s->at)));
    }
    /* The region the overshooting stride flew over, read raw. */
    p = (LispObj *)(trail[(nsteps - 1) % CSTACK_TRAIL].at);
    fprintf(dbgout, "  raw words from the last step's base:\n");
    for (i = 0; i < 10 && (p + i) < limit; i++) {
      fprintf(dbgout, "    [0x%lx] = 0x%lx\n",
              (unsigned long)(p + i), (unsigned long)p[i]);
    }
  }
  fprintf(dbgout, "  raw words at the top of the area:\n");
  for (p = limit - 6; p < limit; p++) {
    if (p >= (LispObj *)a->low) {
      fprintf(dbgout, "    [0x%lx] = 0x%lx%s\n", (unsigned long)p,
              (unsigned long)*p,
              (*p == lisp_frame_marker) ? "   <- lisp_frame_marker" : "");
    }
  }
  fflush(dbgout);
  return (++cstack_walk_reports < CSTACK_MAX_REPORTS);
}
/* ===== end 16m41 TEMPORARY DIAG ========================================== */

/* Heap sanity checking. */

void
check_node(LispObj n)                                /* ppc-gc.c:30-115 */
{
  int tag = fulltag_of(n), header_tag;
  area *a;
  LispObj header;

  switch (tag) {
  case fulltag_even_fixnum:                          /* ppc-gc.c:38-39 */
  case fulltag_odd_fixnum:
  /* arm64 immediate fulltags (ppc-gc.c:43-46 lists PPC64's four imm
     tags; arm64's are single_float, imm_0, imm_1) */
  case fulltag_single_float:
  case fulltag_imm_0:
  case fulltag_imm_1:
    return;

  case fulltag_nil:                                  /* ppc-gc.c:55-59 (PPC32
       branch; PPC64 has no fulltag_nil, arm64 does â€” x86-gc.c precedent) */
    if (n != lisp_nil) {
      Bug(NULL,"Object tagged as nil, not nil : " LISP, n);
    }
    return;

  case fulltag_nodeheader_0:                         /* ppc-gc.c:64-78 */
  case fulltag_nodeheader_1:
  case fulltag_immheader_0:
  case fulltag_immheader_1:
  case fulltag_immheader_2:
    Bug(NULL, "Header not expected : 0x" LISP, n);
    return;

  case fulltag_reserved:      /* ARM64-DEVIATION: arm64-constants.h:129
       reserves fulltag 9; nothing may carry it */
    Bug(NULL, "Reserved tag not expected : 0x" LISP, n);
    return;

  case fulltag_misc:                                 /* ppc-gc.c:81-97 */
  case fulltag_cons:
  case fulltag_symbol:      /* ARM64-DEVIATION: own-tag pointers are heap
       nodes too (x86-gc.c:153-154 precedent).  fulltag_function removed
       (patch 0055): functions arrive here as fulltag_misc. */
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
  header = header_of(n);                             /* ppc-gc.c:99-114 */
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
check_range(LispObj *start, LispObj *end, Boolean header_allowed)
{                                                    /* ppc-gc.c:120-154 */
  LispObj node, *current = start, *prev = NULL;
  int tag, subtag;
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
      /* OPEN-ENTRYPOINT (arm-gc.c:121-129, ARM64-DEVIATION): consistency
         check for the function entrypoint/codevector pair; only fires
         under the ARM32-style odd_fixnum entrypoint convention. */
      subtag = header_subtag(node);
      if (subtag == subtag_function) {
        if (fulltag_of(current[0]) == fulltag_odd_fixnum) {
          if (untag(current[0]) != untag(current[1])) {
            Bug(NULL, "In function at 0x" LISP ", entrypoint (0x" LISP ") and codevector (0x" LISP ") don't match\n", (LispObj)prev, current[0], current[1]);
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
    Bug(NULL, "Overran end of memory range: start = 0x" LISP ", end = 0x" LISP ", prev = 0x" LISP ", current = 0x" LISP,
        start, end, prev, current);
  }
}

void
check_all_areas(TCR *tcr)                            /* ppc-gc.c:156-204 */
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

    case AREA_TSTACK:                                /* ppc-gc.c:182-198 */
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
void
mark_root(LispObj n)                                 /* ppc-gc.c:216-334 */
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
      total_size_in_bytes,      /* including 8-byte header */
      suffix_dnodes;
    tag_n = fulltag_of(header);

    /* ppc-gc.c:253-268 (PPC64 ivector classes), re-keyed to arm64's
       three classes (arm64-constants.h:149-151): there is NO
       ivector_class_8_bit on arm64 â€” 8-bit subtags live in other_bit
       (x86-gc.c:526-546 same shape).
       16m41: the note here said subtag_complex_double_float_vector and
       subtag_s16_vector COLLIDE at SUBTAG(other_bit,9), so no cdf branch
       was possible (report OPEN #2).  STALE â€” Matt fixed it upstream
       (s16 moved to index 10); at this pin arm64-constants.h:176/178 are
       9 and 10, distinct, and cdf is min_cl_ivector_subtag.  So the cdf
       leg below is now both possible and REQUIRED: cdf is index 9, i.e.
       BELOW subtag_s8_vector, so it used to fall into the 16-bit leg and
       every complex-double-float vector mis-sized the heap walk.
       Element = 2 doubles = 16 bytes, data immediately after the header
       (see the layout note in skip_over_ivector). */
    if ((nodeheader_tag_p(tag_n)) ||
        (tag_n == ivector_class_64_bit)) {
      /* includes complex_single_float_vector (index 11): 2 singles = 8
         bytes per element, so the generic 8n is already correct. */
      total_size_in_bytes = 8 + (element_count<<3);
    } else if (tag_n == ivector_class_32_bit) {
      total_size_in_bytes = 8 + (element_count<<2);
    } else {
      /* ivector_class_other_bit: complex-double, 8/16-bit arrays, bitvector */
      if (subtag == subtag_bit_vector) {
        total_size_in_bytes = 8 + ((element_count+7)>>3);
      } else if (subtag >= subtag_s8_vector) { /* s8/u8;
              bit_vector already handled */
        total_size_in_bytes = 8 + element_count;
      } else if (subtag == subtag_complex_double_float_vector) {
        total_size_in_bytes = 8 + (element_count<<4);
      } else {                  /* s16, u16 */
        total_size_in_bytes = 8 + (element_count<<1);
      }
    }

    suffix_dnodes = ((total_size_in_bytes+(dnode_size-1))>>dnode_shift) -1;

    if (suffix_dnodes) {
      check_marked_extent(n, dnode, suffix_dnodes);
      set_n_bits(GCmarkbits, dnode+1, suffix_dnodes);
    }

    if (nodeheader_tag_p(tag_n)) {                   /* ppc-gc.c:294-332 */
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

      /* OPEN-ENTRYPOINT: for subtag_function (pseudofunction removed upstream @ a6314ba) gvectors,
         slot 1 (entrypoint) is visited by this loop; under the assumed
         ARM32 convention it is fixnum-tagged so rmark ignores it.  If
         the settled convention makes it a node-tagged or 4-aligned-raw
         value, functions need a prefix-skip here (x86-gc.c:603-618
         shape). */
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
mark_ephemeral_root(LispObj n)                       /* ppc-gc.c:345-364 */
{
  int tag_n = fulltag_of(n);
  natural eph_dnode;

  if (nodeheader_tag_p(tag_n)) {
    return (header_subtag(n) == subtag_hash_vector);
  }

  /* ARM64-DEVIATION (x86-gc.c:650): is_node_fulltag, not cons|misc â€”
     symbol/function-tagged ephemeral references count too. */
  if (is_node_fulltag(tag_n)) {
    eph_dnode = area_dnode(n, GCephemeral_low);
    if (eph_dnode < GCn_ephemeral_dnodes) {
      mark_root(n);             /* May or may not mark it */
      return true;              /* but return true 'cause it's an ephemeral node */
    }
  }
  return false;                 /* Not a heap pointer or not ephemeral */
}


/* ppc-gc.c:367-378 (PPC64 comment, adapted): any register or stack
   location that we're calling this on should have its low 2 bits clear;
   the pc/lr should never point to a tagged object or contain a fixnum
   with the low bits set.

   If the "pc" appears to be pointing into a heap-allocated code vector
   that's not yet marked, back up until we find the code vector's
   sentinel (the 32-bit word 0x00000000 = `udf #0' which is the vector's
   first element) and mark the entire code vector. */
void
mark_pc_root(LispObj xpc)                            /* ppc-gc.c:379-411 */
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
        if (*program_counter == ARM64_CODE_VECTOR_SENTINEL) {
          headerP = ((LispObj *)program_counter)-1;
          header = *headerP;
          dnode = gc_area_dnode(headerP);
          /* code vectors are 32-bit ivectors on arm64 too
             (subtag_code_vector âˆˆ ivector_class_32_bit,
             arm64-constants.h:181) â€” same size formula. */
          {
            natural code_dnodes =
              (8+(header_element_count(header)<<2)+(dnode_size-1))>>dnode_shift;
            if (code_dnodes) {
              check_marked_extent((LispObj)headerP, dnode, code_dnodes - 1);
            }
            set_n_bits(GCmarkbits, dnode, code_dnodes);
          }
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

/* C-side register numbers used by the xp walkers â€” subset of the
 * authority enum in arm64-exceptions.c:86-120 (arm64-constants.h R*
 * numbers @ pin, map unified upstream @ 01d73c3). */
enum {
  fn = 7,          /* first BOXED node register */
  arg_w = 8, arg_x = 9, arg_y = 10, arg_z = 11,
  temp0 = 12, temp1 = 13, temp2 = 14, temp3 = 15, temp4 = 16, temp5 = 17,
  /* x18: platform register â€” NEVER a lisp register */
  save0 = 19, save1 = 20, save2 = 21, save3 = 22,
  rnil = 23        /* last BOXED node register */
};

/* FSM state tags for the link-inverting marker.
   ARM64-DEVIATION (x86-gc.c:664-665 â€” same tag layout: cons=3, nil=11,
   imm_1=10): RMARK_PREV_ROOT is the fulltag of the `undefined' value
   (SUBTAG(fulltag_imm_1,1), arm64-constants.h:205-207); RMARK_PREV_CAR =
   fulltag_cons + node_size = fulltag_nil ("Coincidence?  I think not.")
   PPC64 used fulltag_imm_3/fulltag_misc (ppc-gc.c:459-461) â€” different
   tag space. */
#define RMARK_PREV_ROOT fulltag_imm_1
#define RMARK_PREV_CAR fulltag_nil

/*
  This wants to be in assembler even more than "mark_root" does.
  For now, it does link-inversion: hard as that is to express in C,
  reliable stack-overflow detection may be even harder ...
*/
void
rmark(LispObj n)                                     /* ppc-gc.c:476-787 */
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

  if (current_stack_pointer() > GCstack_limit) {     /* ppc-gc.c:497 */
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
      /* ppc-gc.c:510-525 sizing, arm64 classes (see mark_root) */
      if ((nodeheader_tag_p(tag_n)) ||
          (tag_n == ivector_class_64_bit)) {
        total_size_in_bytes = 8 + (element_count<<3);
      } else if (tag_n == ivector_class_32_bit) {
        total_size_in_bytes = 8 + (element_count<<2);
      } else {
        if (subtag == subtag_bit_vector) {
          total_size_in_bytes = 8 + ((element_count+7)>>3);
        } else if (subtag >= subtag_s8_vector) {
          total_size_in_bytes = 8 + element_count;
        } else if (subtag == subtag_complex_double_float_vector) {
          total_size_in_bytes = 8 + (element_count<<4); /* 16m41; see mark_root */
        } else {                /* 16-bit: s16, u16 */
          total_size_in_bytes = 8 + (element_count<<1);
        }
      }

      suffix_dnodes = ((total_size_in_bytes+(dnode_size-1))>>dnode_shift)-1;

      if (suffix_dnodes) {
        check_marked_extent(n, dnode, suffix_dnodes);
        set_n_bits(GCmarkbits, dnode+1, suffix_dnodes);
      }

      if (!nodeheader_tag_p(tag_n)) return;

      if (subtag == subtag_hash_vector) {            /* ppc-gc.c:552-569 */
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
    /* Link-inversion FSM (ppc-gc.c:593-786 structure; tag mechanics from
       x86-gc.c:975-1189 â€” the same own-tag-symbol/function layout):

       - marking a CONS: `this' is fulltag_cons (3) while its cdr is
         being marked, fulltag_nil (11 = cons+node_size) while its car is.
       - marking a gvector: `this' = base + tag_of(orig) + element
         offset; steps of node_size keep the 3-bit tag: tag 4 for
         fulltag_misc references (fulltags alternate 4/12), tag 7 for
         fulltag_symbol references (fulltags alternate 7/15; 15 is just
         the bit-3 flip of a symbol climb -- fulltag_function removed,
         patch 0055).  When the header is reached, the original fulltag
         is restored: tag 4 â†’ fulltag_misc, tag 7 â†’ fulltag_symbol (a
         misc-tagged reference to a symbol -- or to a function, which is
         an ordinary miscobj now -- keeps tag 4 and is restored to
         fulltag_misc, exactly what it was).
       - prev = `undefined' (fulltag_imm_1) marks the FSM root.

       ARM64-DEVIATION vs x86: no function_boundary_marker / in-object
       code â€” arm64 function gvectors are {header, entrypoint,
       codevector, constants...}; the back-scan visits the entrypoint
       slot like any other (OPEN-ENTRYPOINT: skipped as a fixnum under
       the assumed ARM32 convention). */
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

  ClimbCdr:                                          /* ppc-gc.c:610-612 */
    prev = deref(this,0);
    deref(this,0) = next;

  Climb:                                             /* ppc-gc.c:614-633 */
    next = this;
    this = prev;
    tag_n = fulltag_of(prev);
    switch(tag_n) {
    /* Vector states.  Climbing a gvector walks BACKWARD one slot at a time,
       and node_size is 8 while fulltagmask is 15, so each step FLIPS BIT 3 of
       the pointer's fulltag: a vector reference is seen under BOTH members of
       a {t, t+8} pair, and both must be cased.  x86-gc.c:925-935 spells this
       as `case tag_misc: case fulltag_misc:' (5/13), `case tag_symbol: case
       fulltag_symbol:' (6/14), `case tag_function: case fulltag_function:'
       (7/15) -- always the 3-bit tag AND its fulltag.
         arm64 (arm64-constants.h:123-147): fulltag_misc=12 pairs with
       tag_4=4; fulltag_symbol=7 pairs with 15 = fulltag_symbol+node_size,
       the bit-3 flip alias of the SAME symbol climb (15 stopped being a
       pointer tag when fulltag_function was removed, patch 0055).
         This case used to read fulltag_immheader_1, which is 5 on arm64, not
       4 -- so the misc pair was {12,5}, the real tag-4 half of every
       misc-vector climb fell to `default:', and rmark abort()ed.  16m42
       OBSERVED exactly that: "unexpected prev fulltag 4" after ~4194 ANSI
       tests.  The comment above this switch always said {4,12}; only the
       constant was wrong.  Spelled tag_4 rather than fulltag_immheader_0
       (numerically identical) because this state is a MISC POINTER minus a
       slot, not a header -- the same reason x86 writes tag_misc there. */
    case fulltag_misc:
    case tag_4:
    case fulltag_symbol:
    case fulltag_symbol + node_size:    /* 15: symbol climb, bit-3 flip */
      goto ClimbVector;

    case RMARK_PREV_ROOT:
      return;

    case fulltag_cons:
      goto ClimbCdr;

    case RMARK_PREV_CAR:
      goto ClimbCar;

    default:
      /* KEEP (16m42), same reason as the sizing Bug() above: this used to be a
         bare abort(), which died as SIGABRT with NO output whatsoever -- not
         the fulltag, not a pointer, not even the function name.  Stage 11
         reaches it after ~4194 ANSI tests and a gdb boot could only report
         "stopped in lisp code", because the suite fires thousands of benign
         UUO SIGTRAPs before the real death and there was no C symbol worth
         breaking on.  An unreachable-state assert must say which state it saw.

         The FSM's state IS prev's fulltag, so landing here means either a
         link-inversion state we never encode, or a `prev' corrupted while
         inverted.  Encoded states (RMARK_PREV_* above + arm64-constants.h:
         123-147): vector refs tag_4(4)/fulltag_misc(12) and
         fulltag_symbol(7)/its bit-3 flip(15); RMARK_PREV_ROOT =
         fulltag_imm_1(10); fulltag_cons(3); RMARK_PREV_CAR = fulltag_nil(11).

         Print the whole FSM triple, not just the tag: `this' and `next' say
         where in the object graph the walk was, prev's FULL word says what it
         tried to climb into, and the root `n' says which mark_root call owns
         the walk. */
      Bug(NULL, "rmark FSM: unexpected prev fulltag %d (not one of "
                "cons/3 tag_4/4 symbol/7 imm_1/10 nil/11 misc/12 "
                "symbol-flip/15)\n"
                "  prev = 0x" LISP "\n  this = 0x" LISP "\n  next = 0x" LISP
                "\n  root = 0x" LISP "\n",
          tag_n, prev, this, next, n);
      /* Bug() is NOT noreturn -- it returns if lisp_Debugger does, and falling
         out of this switch would drop into DescendCons with a corrupt FSM.
         Keep the original abort() so only the DIAGNOSTIC is new. */
      abort();
    }

  DescendCons:                                       /* ppc-gc.c:635-637 */
    prev = this;
    this = next;

  MarkCons:                                          /* ppc-gc.c:639-651 */
    next = deref(this,1);
    this += node_size;          /* fulltag_cons â†’ fulltag_nil (RMARK_PREV_CAR) */
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

  ClimbCar:                                          /* ppc-gc.c:653-655 */
    prev = deref(this,1);
    deref(this,1) = next;

  MarkCdr:                                           /* ppc-gc.c:657-669 */
    next = deref(this, 0);
    this -= node_size;          /* fulltag_nil â†’ fulltag_cons */
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

  DescendVector:                                     /* ppc-gc.c:671-673 */
    prev = this;
    this = next;

  MarkVector:                                        /* ppc-gc.c:675-755 */
    {
      LispObj *base = (LispObj *) ptr_from_lispobj(untag(this));
      natural
        header = *((natural *) base),
        subtag = header_subtag(header),
        element_count = header_element_count(header),
        total_size_in_bytes,
        suffix_dnodes;

      tag_n = fulltag_of(header);

      /* ppc-gc.c:687-702 sizing, arm64 classes (see mark_root) */
      if ((nodeheader_tag_p(tag_n)) ||
          (tag_n == ivector_class_64_bit)) {
        total_size_in_bytes = 8 + (element_count<<3);
      } else if (tag_n == ivector_class_32_bit) {
        total_size_in_bytes = 8 + (element_count<<2);
      } else {
        if (subtag == subtag_bit_vector) {
          total_size_in_bytes = 8 + ((element_count+7)>>3);
        } else if (subtag >= subtag_s8_vector) {
          total_size_in_bytes = 8 + element_count;
        } else if (subtag == subtag_complex_double_float_vector) {
          total_size_in_bytes = 8 + (element_count<<4); /* 16m41; see mark_root */
        } else {                /* 16-bit: s16, u16 */
          total_size_in_bytes = 8 + (element_count<<1);
        }
      }

      suffix_dnodes = ((total_size_in_bytes+(dnode_size-1))>>dnode_shift)-1;

      if (suffix_dnodes) {
        check_marked_extent(this, dnode, suffix_dnodes);
        set_n_bits(GCmarkbits, dnode+1, suffix_dnodes);
      }

      if (!nodeheader_tag_p(tag_n)) goto Climb;

      if (subtag == subtag_hash_vector) {            /* ppc-gc.c:729-739 */
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

      /* ARM64-DEVIATION (x86-gc.c:1128): keep the reference's 3-bit tag
         in the scan pointer (PPC64 used a raw untagged pointer,
         ppc-gc.c:753 â€” its tag space differs). */
      this = (LispObj)(base) + (tag_of(this)) + ((element_count+1) << node_shift);
      goto MarkVectorLoop;
    }

  ClimbVector:                                       /* ppc-gc.c:757-759 */
    prev = indirect_node(this);
    indirect_node(this) = next;

  MarkVectorLoop:                                    /* ppc-gc.c:761-774 */
    this -= node_size;
    next = indirect_node(this);
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

  MarkVectorDone:                                    /* ppc-gc.c:776-785 */
    /* "next" is the vector header; "this" = header address + the 3-bit tag of
       the original reference (indirect_node masks ~tagmask, so the loop stops
       at the residue that node-aligns onto the header: 4 for a misc climb, 7
       for a symbol climb).  Restore the original fulltag.

       fulltag_function removed (patch 0055): tag residue 7 can ONLY be a
       symbol reference now, so the old header-subtag disambiguation (7 ->
       function iff subtag_function -- needed when fulltag_symbol(7) and
       fulltag_function(15) shared one {t, t+8} pair) collapses to an
       unconditional fulltag_symbol.  Function references are misc-tagged
       and take the tag_4 arm like every other misc climb.

       The MISC arm history (16m42): it tested `tag_of(this) == fulltag_misc',
       and tag_of() masks with tagmask=7 while fulltag_misc is 12 -- a 3-bit
       value compared against 12 is NEVER equal, so the branch was dead and
       every misc vector was restored as fulltag_symbol(7).  Spelled tag_4
       since then. */
    if (tag_of(this) == tag_4) {
      this = node_aligned(this) + fulltag_misc;
    } else {
      this = node_aligned(this) + fulltag_symbol;
    }

    if (header_subtag(next) == subtag_weak) {
      deref(this, 1) = GCweakvll;
      GCweakvll = untag(this);
    }
    goto Climb;
  }
}

LispObj *
skip_over_ivector(natural start, LispObj header)     /* ppc-gc.c:789-836 */
{
  natural
    element_count = header_element_count(header),
    subtag = header_subtag(header),
    nbytes;

  /* ppc-gc.c:798-818 (PPC64 switch), arm64 classes (see mark_root) */
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
    } else if (subtag >= subtag_s8_vector) {
      nbytes = element_count;
    } else if (subtag == subtag_complex_double_float_vector) {
      /* 16m41: 2 doubles = 16 bytes per element.  LAYOUT NOTE (this is the
         authority the other four sizing sites and the allocators agree with):
         data starts immediately after the header, NOT after an 8-byte pad as
         on x8664 (l0-array.lisp:862-865).  ARM64-DEVIATION, deliberate: both
         our allocators compute 16n with no pad (spentry-A misc_alloc label 3
         and stack_misc_alloc label 6, `add imm1,arg_y,arg_y'), subtag-bytes
         (patch 0015) says (ash n 4), and aarch64 needs only 8-byte alignment
         for LDP d/LDR q on normal memory, so x8664's pad buys nothing here.
         The dnode round-up below puts the slack at the END instead, so the
         total object size matches x8664's 16n+16 either way.  UPSTREAM ITEM:
         confirm with Matt that arm64 wants no pad. */
      nbytes = element_count << 4;
    } else {                    /* 16-bit: s16, u16 */
      nbytes = element_count << 1;
    }
  }
  return ptr_from_lispobj(start+(~15 & (nbytes + 8 + 15)));
}


void
check_refmap_consistency(LispObj *start, LispObj *end, bitvector refbits, bitvector refidx)
{                                                    /* ppc-gc.c:839-905 */
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
        /* Consumer side of the spentry-B/D write-barrier memoization:
           dnode math on the doubleword address, bitnumber>>8 into the
           refidx (same producer protocol). */
        ref_dnode = area_dnode(start, base);
        if (!ref_bit(refbits, ref_dnode)) {
          Bug(NULL, "Missing memoization in doublenode at 0x" LISP "\n", start);
          set_bit(refbits, ref_dnode);
          if (refidx) {
            set_bit(refidx,ref_dnode>>8);
          }
        } else {
          if (refidx) {
            if (!ref_bit(refidx, ref_dnode>>8)) {
              Bug(NULL, "Memoization for doublenode at 0x" LISP " not indexed\n", start);
              set_bit(refidx,ref_dnode>>8);
            }
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
mark_simple_area_range(LispObj *start, LispObj *end) /* ppc-gc.c:912-963 */
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

      /* OPEN-ENTRYPOINT: function gvectors' entrypoint slot is visited
         here; a fixnum-tagged locative (assumed convention) no-ops. */
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
mark_tstack_area(area *a)                            /* ppc-gc.c:967-986 */
{
  /* Matt's arm64 HAS a tsp/tstack (tsp=x24; tcr.save_tsp/next_tsp/
     ts_area) â€” PPC-shaped {backlink, type, data...} frames; the walk
     ports verbatim (ARM32's empty stub, arm-gc.c:851, does NOT apply). */
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
mark_vstack_area(area *a)                            /* ppc-gc.c:997-1013 */
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

  ARM64-DEVIATION (whole function; ppc-gc.c:1021-1049 walks PPC backlink
  frames): arm64 uses MARKER frames â€” the walk is arm-gc.c:889-928, with
  one correction: the nodeheader/immheader tests are moved BEFORE the
  `(header & fixnummask) == 0' raw-pointer test.  ARM32's order is unsafe
  here because arm64 immheader_1(12) â‰¡ 0 (mod 4): a stack-consed ivector
  header (e.g. simple_base_string, header fulltag 12) would be misread as
  a raw backlink.  Raw stack addresses are 16-aligned (fulltag 0 =
  even_fixnum), so they can never be mistaken for headers by the moved
  tests.
*/

void
mark_cstack_area(area *a)                            /* arm-gc.c:889-928 */
{
  LispObj *current = (LispObj *)(a->active)
    , *limit = (LispObj*)(a->high), header;
  lisp_frame *frame;
  CSTACK_TRAIL_DECL;                                 /* 16m41 DIAG */

  while(current < limit) {
    header = *current;
    _cfrom = current;                                /* 16m41 DIAG */

    if (header == lisp_frame_marker) {
      frame = (lisp_frame *)current;

      mark_root(frame->savevsp); /* likely a fixnum */
      mark_root(frame->savefn);
      mark_pc_root(frame->savelr);
      current += sizeof(lisp_frame)/sizeof(LispObj);
      CSTACK_TRAIL_STEP(CB_FRAME);                   /* 16m41 DIAG */
    } else if ((header == stack_alloc_marker) || (header == 0)) {
      current += 2;
      CSTACK_TRAIL_STEP(CB_MARKER0);                 /* 16m41 DIAG */
    } else if (nodeheader_tag_p(fulltag_of(header))) { /* REORDERED, see above */
      natural elements = header_element_count(header);

      current++;
      while(elements--) {
        mark_root(*current++);
      }
      if (((natural)current) & sizeof(natural)) {
        current++;
      }
      CSTACK_TRAIL_STEP(CB_NODE);                    /* 16m41 DIAG */
    } else if (immheader_tag_p(fulltag_of(header))) {  /* REORDERED, see above */
      current=(LispObj *)skip_over_ivector((natural)current,header);
      CSTACK_TRAIL_STEP(CB_IMM);                     /* 16m41 DIAG */
    } else if ((header & fixnummask) == 0) {
      /* 16m40: the note here claimed fixnummask=3 was "suspected ARM32
         copy-pasta, report OPEN #3", justified as a 4-aligned test.  STALE:
         at this pin arm64-constants.h:32 is DEFCONST(fixnummask, 7), correct
         for fixnumshift=3, so this is an 8-aligned test and OPEN #3 is moot.
         Do NOT re-open it.
         What IS true, and is the live hazard: this branch cannot tell a boxed
         FIXNUM from a raw backlink -- with fixnumshift=3 both have the low 3
         bits clear -- so a fixnum on the cstack is followed as a pointer.
         That is a property of this linear-scan shape, not of the mask.  See
         comms/ARM64-CSTACK-WALK-DECISION.md before touching it. */
      current = (LispObj *)header;
      CSTACK_TRAIL_STEP(CB_BACKLINK);                /* 16m41 DIAG */
    } else {
      /* 16m41 DIAG: dump the trail before dying -- the shape leading in says
         more about an unclassifiable word than the word itself does. */
      cstack_walk_report("mark_cstack_area", "UNKNOWN STACK WORD", a,
                         current, limit, _ctrail, _cnsteps, _chisto);
      Bug(NULL, "Unknown stack word at 0x" LISP ":\n", current);
    }
    /* 16m41 DIAG: report AT the overshooting step, not after the loop. */
    if (current > limit) {
      if (!cstack_walk_report("mark_cstack_area", "RAN OFF THE END of cstack area",
                              a, current, limit, _ctrail, _cnsteps, _chisto)) {
        Bug(NULL, "Ran off the end of cstack area\n");
      }
      current = limit;                  /* clamp: marking is now INCOMPLETE */
    }
  }
  if (current != limit) {
    Bug(NULL, "Ran off the end of cstack area\n");
  }
}



/* Mark the lisp objects in an exception frame */
void
mark_xp(ExceptionInformation *xp)                    /* ppc-gc.c:1054-1083 */
{
  natural *regs = (natural *) xpGPRvector(xp);
  int r;
  /* PPC marks regs[fn..31] as node roots and PC/LR/CTR/loc_pc as pc
     locatives (ppc-gc.c:1071-1080).  The arm64 BOXED-node-register SET
     (PROPOSED â€” see file header): fn(x7)..rnil(x23), skipping x18 (the
     platform register).  pc locatives: PC and LR only (no CTR/loc_pc on
     ARM64 â€” ARM64-DEVIATION, arm-gc.c:956-957).

     In general, marking a locative is more expensive than marking
     a node is, since it may be neccessary to back up and find the
     containing object's header.  Since exception frames contain
     many locatives, it'd be wise to mark them *after* marking the
     stacks, nilreg-relative globals, etc.
     */

  for (r = fn; r <= rnil; r++) {
    if (r != 18) {
      mark_root((regs[r]));
    }
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

   (On 64-bit platforms a markbits word covers 64 dnodes =
   nbits_in_word; "32" above is the 32-bit heritage of the comment â€”
   the code below is the PPC64/WORD_SIZE-64 version: qnode = unsigned
   short, 4 qnodes per markbits word.)
*/

LispObj
calculate_relocation()                               /* ppc-gc.c:1094-1128 */
{
  LispObj *relocptr = GCrelocptr;
  LispObj current = GCareadynamiclow;
  bitvector
    markbits = GCdynamic_markbits;
  qnode *q = (qnode *) markbits;
  natural npagelets = ((GCndynamic_dnodes_in_area+(nbits_in_word-1))>>bitmap_shift);
  natural thesebits;
  LispObj first = 0;

  /* Endianness note: summing one_bits over all four qnodes of a word is
     order-independent, and `thesebits'/BIT0_MASK work on the whole
     natural â€” no little-endian fix needed here (unlike
     dnode_forwarding_address below). */
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

/* 16m38: WHICH ROOT held the reference?  dnode_forwarding_address knows the
   object but not the referrer, and the referrer is the whole question: an
   "unmarked object being forwarded" means the forwarding pass reached a
   reference the MARK pass never followed, so naming the area or register that
   held it names the gap.  Set on entry to each forward_* walker; costs one
   store per area (not per slot) and is read only inside the GCDebug branch. */
const char *GCforward_context = "(none yet)";
int GCforward_reg = -1;
/* 16m39: WHICH SLOT, and does the MARK side agree?  The 16m38 Bug() named the
   area (cstack) but not the referring slot, and it read only the FORWARD side's
   view of the markbits (GCdynamic_markbits, dynamiclow-relative).  mark_root
   writes GCmarkbits at a GCarealow-relative dnode; those views coincide only if
   the static_dnodes prefix arithmetic is exact, so the Bug below now evaluates
   BOTH views of the failing node.  Slot/branch are set only in the cold cstack
   and xp walkers (per-slot stores there are a handful of frames, not the heap
   sweep), so the hot forward_range path still pays one store per area. */
LispObj *GCforward_slot = NULL;
const char *GCforward_branch = "(unset)";

LispObj
dnode_forwarding_address(natural dnode, int tag_n)   /* ppc-gc.c:1131-1174 (PPC64) */
{
  natural pagelet, nbits;
  unsigned int near_bits;
  LispObj new;

  if (GCDebug) {
    if (! ref_bit(GCdynamic_markbits, dnode)) {
      /* 16m38: the bare assert (ppc-gc.c:1140) says WHAT went wrong and nothing
         about WHICH object, and one boot buys one Bug() -- so spend it.
         dnode here is relative to GCareadynamiclow, because every caller reaches
         this through gc_dynamic_area_dnode (gc.h:111 -- area_dnode(w,low) is
         ((w - low) >> dnode_shift), gc.h:109), so the object's first word sits at
         GCareadynamiclow + (dnode << dnode_shift).
         The NEIGHBOURING mark bits are the discriminating fact: if dnode-1 is
         marked then this reference points INTO a live object -- an interior or
         off-by-one pointer, whose cause is a tag/offset error at the referring
         site -- whereas an unmarked run either side means the mark phase never
         reached the object at all, i.e. a root-set or subtag-dispatch gap. Those
         two want opposite investigations, and guessing between them is what this
         line exists to avoid. */
      LispObj addr = GCareadynamiclow + (((natural)dnode) << dnode_shift);
      LispObj w0 = 0, w1 = 0;
      int readable = ((addr >= GCareadynamiclow) &&
                      ((addr + node_size) <
                       ptr_to_lispobj(active_dynamic_area->high)));
      /* 16m39: the MARK side's view of this same node.  mark_root computes
         gc_area_dnode (GCarealow-relative) and tests/sets GCmarkbits; the
         check above read GCdynamic_markbits at a GCareadynamiclow-relative
         dnode.  node reconstructs the tagged ref exactly: dnode came from
         (node - GCareadynamiclow) >> dnode_shift, and tag_n restores the
         low bits that shift discarded. */
      LispObj node = addr + tag_n;
      natural mdnode = gc_area_dnode(node);
      int m_in_bounds = (mdnode < GCndnodes_in_area);
      int m_bit = m_in_bounds ? (ref_bit(GCmarkbits, mdnode) ? 1 : 0) : -1;
      LispObj s0 = 0, s1 = 0, s2 = 0, s3 = 0;

      if (readable) {
        w0 = *(LispObj *)ptr_from_lispobj(addr);
        w1 = *(LispObj *)ptr_from_lispobj(addr + node_size);
      }
      if (GCforward_slot != NULL) {
        s0 = GCforward_slot[-1];
        s1 = GCforward_slot[0];
        s2 = GCforward_slot[1];
        s3 = GCforward_slot[2];
      }
      Bug(NULL, "unmarked object being forwarded!\n"
          "  dnode 0x" LISP "  tag_n %d  addr 0x" LISP "  readable %d\n"
          "  w0 0x" LISP "  w1 0x" LISP "  (w0 as header: subtag 0x%02x)\n"
          "  markbits prev/self/next = %d/%d/%d\n"
          "  forwarding context: %s  reg %d\n"
          "  referring slot 0x" LISP "  branch %s  slot[-1..2] 0x" LISP
          " 0x" LISP " 0x" LISP " 0x" LISP "\n"
          "  mark-side view: mdnode 0x" LISP "  in-bounds %d  GCmarkbits bit %d\n"
          "  GCmarkbits %p  GCdynamic_markbits %p  static-prefix bytes 0x" LISP "\n"
          "  GCfirstunmarked 0x" LISP "  GCephemeral_low 0x" LISP
          "  GCn_ephemeral_dnodes 0x" LISP "\n"
          "  GCareadynamiclow 0x" LISP "  GCarealow 0x" LISP
          "  dynamic active 0x" LISP " high 0x" LISP "\n",
          (LispObj)dnode, tag_n, addr, readable,
          w0, w1, (unsigned)(header_subtag(w0) & 0xff),
          (dnode ? (ref_bit(GCdynamic_markbits, dnode - 1) ? 1 : 0) : -1),
          (ref_bit(GCdynamic_markbits, dnode) ? 1 : 0),
          (ref_bit(GCdynamic_markbits, dnode + 1) ? 1 : 0),
          GCforward_context, GCforward_reg,
          (LispObj)(natural)GCforward_slot, GCforward_branch, s0, s1, s2, s3,
          (LispObj)mdnode, m_in_bounds, m_bit,
          (void *)GCmarkbits, (void *)GCdynamic_markbits,
          (LispObj)(GCareadynamiclow - GCarealow),
          GCfirstunmarked, GCephemeral_low, (LispObj)GCn_ephemeral_dnodes,
          GCareadynamiclow, GCarealow,
          ptr_to_lispobj(active_dynamic_area->active),
          ptr_to_lispobj(active_dynamic_area->high));
    }
  }

  pagelet = dnode >> bitmap_shift;
  nbits = dnode & bitmap_shift_count_mask;
  /* ARM64-DEVIATION (arm-gc.c:1021-1023 idiom, widened to uint32):
     markbits words are 64-bit naturals with MSB-first bit numbering
     (BIT0_MASK = 1<<63, bits.h:28), so dnodes 0-31 of a word live in
     its HIGH uint32.  PPC64 (big-endian) indexes uint32s with
     dnode>>(dnode_shift+1) directly (ppc-gc.c:1146); on little-endian
     AArch64 the uint32 index must be XORed with 1. */
  near_bits = ((unsigned int *)GCdynamic_markbits)[(dnode>>(dnode_shift+1))^1];

  if (nbits < 32) {
    new = GCrelocptr[pagelet] + tag_n;
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
      /* VENDOR-UB FIXED: ppc-gc.c:1167 computes (1<<nbits)-1 with int
         arithmetic; nbits can be 32 here (dnode â‰¡ 32 mod 64), and
         1<<32 is UB / wrong on AArch64.  (natural)1 gives the intended
         all-ones 32-bit mask. */
      near_bits &= ((natural)1<<nbits)-1;
      if (nbits > 15) {
        new -= one_bits(near_bits >> 16);
      }
      return (new -  one_bits(near_bits & 0xffff));
    }
  }
}


LispObj
locative_forwarding_address(LispObj obj)             /* ppc-gc.c:1223-1257 */
{
  int tag_n = fulltag_of(obj);
  natural dnode;

  /* Locatives can be tagged as misc objects, as fixnums, or be raw
     4-byte-aligned instruction addresses (residues 0/4/8/12 mod 16 =
     fulltags even_fixnum/misc/odd_fixnum/immheader_1).  Immediates,
     node headers at other residues, and nil shouldn't be "forwarded".
     ARM64-DEVIATION: `(obj & 3) == 0' is the arm64 rendering of PPC64's
     `(tag_n & lowtag_mask) == lowtag_primary' (ppc-gc.c:1236) â€” on
     PPC64 the four 4-aligned residues were exactly its four primary
     tags; here they are the four tags listed above.  fulltag_cons(3) is
     NOT accepted: arm64 conses are never pc locatives, and
     update_locref is only applied to known locative slots. */
  if ((obj & 3) != 0) {
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
{                                                    /* ppc-gc.c:1262-1320 */
  LispObj *p = range_start, node, new;
  int tag_n, subtag;
  natural nwords;
  hash_table_vector_header *hashp;

  GCforward_context = "range (dynamic/static sweep)"; GCforward_reg = -1;
  GCforward_slot = NULL; GCforward_branch = "(range)";

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
        /* OPEN-ENTRYPOINT (ARM64-DEVIATION, arm-gc.c:1129-1135): the
           first slot of a function (pseudofunction removed upstream @ a6314ba) is the entrypoint â€”
           a locative into its code vector, not a node. */
        subtag = header_subtag(node);
        if (subtag == subtag_function) {
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




/* Forward a tstack area */
void
forward_tstack_area(area *a)                         /* ppc-gc.c:1326-1345 */
{
  LispObj
    *current,
    *next,
    *start = (LispObj *) a->active,
    *end = start,
    *limit = (LispObj *) (a->high);

  GCforward_context = "tstack"; GCforward_reg = -1;
  GCforward_slot = NULL; GCforward_branch = "(tstack)";

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
forward_vstack_area(area *a)                         /* ppc-gc.c:1348-1363 */
{
  LispObj
    *p = (LispObj *) a->active,
    *q = (LispObj *) a->high;

  GCforward_context = "vstack"; GCforward_reg = -1;
  GCforward_slot = NULL; GCforward_branch = "(vstack)";

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
forward_cstack_area(area *a)                         /* arm-gc.c:1179-1223 */
{
  /* ARM64-DEVIATION (whole function; ppc-gc.c:1365-1384 walks backlink
     frames): marker-frame walk, case order corrected as in
     mark_cstack_area. */
  LispObj *current = (LispObj *)(a->active)
    , *limit = (LispObj*)(a->high), header;
  lisp_frame *frame;
  unsigned subtag;
  CSTACK_TRAIL_DECL;                                 /* 16m41 DIAG */

  GCforward_context = "cstack"; GCforward_reg = -1;
  GCforward_slot = NULL; GCforward_branch = "(cstack, pre-slot)";

  while (current < limit) {
    header = *current;
    _cfrom = current;                                /* 16m41 DIAG */

    if (header == lisp_frame_marker) {
      frame = (lisp_frame *)current;

      GCforward_slot = &(frame->savefn);
      GCforward_branch = "lisp_frame.savefn";
      update_noderef(&(frame->savefn));
      GCforward_slot = &(frame->savelr);
      GCforward_branch = "lisp_frame.savelr";
      update_locref(&(frame->savelr));
      current += sizeof(lisp_frame)/sizeof(LispObj);
      CSTACK_TRAIL_STEP(CB_FRAME);                   /* 16m41 DIAG */
    } else if ((header == stack_alloc_marker) || (header == 0)) {
      current += 2;
      CSTACK_TRAIL_STEP(CB_MARKER0);                 /* 16m41 DIAG */
    } else if (nodeheader_tag_p(fulltag_of(header))) { /* REORDERED, see mark_cstack_area */
      natural elements = header_element_count(header);

      current++;
      /* OPEN-ENTRYPOINT (arm-gc.c:1203-1209) */
      subtag = header_subtag(header);
      if (subtag == subtag_function) {
        GCforward_slot = current;
        GCforward_branch = "stack gvector fn entrypoint";
        update_locref(current);
        current++;
        elements--;
      }
      GCforward_branch = "stack gvector slot";
      while(elements--) {
        GCforward_slot = current;
        update_noderef(current);
        current++;
      }
      if (((natural)current) & sizeof(natural)) {
        current++;
      }
      CSTACK_TRAIL_STEP(CB_NODE);                    /* 16m41 DIAG */
    } else if (immheader_tag_p(fulltag_of(header))) {  /* REORDERED */
      current=(LispObj *)skip_over_ivector((natural)current,header);
      CSTACK_TRAIL_STEP(CB_IMM);                     /* 16m41 DIAG */
    } else if ((header & fixnummask) == 0) {
      current = (LispObj *)header;
      CSTACK_TRAIL_STEP(CB_BACKLINK);                /* 16m41 DIAG */
    } else {
      cstack_walk_report("forward_cstack_area", "UNKNOWN STACK WORD", a,
                         current, limit, _ctrail, _cnsteps, _chisto);
      Bug(NULL, "Unknown stack word at 0x" LISP ":\n", current);
    }
    /* ARM64-DEVIATION (16m41): the reference walk has no end-of-area assertion
       here (asymmetric with mark_cstack_area), so an overshoot in the FORWARD
       pass was silent -- and a forward pass that skips a region leaves stale
       pointers into freed space, i.e. silent corruption instead of a crash we
       can read.  Report and die: loud failure beats a corrupted heap. */
    if (current > limit) {
      cstack_walk_report("forward_cstack_area", "RAN OFF THE END of cstack area",
                         a, current, limit, _ctrail, _cnsteps, _chisto);
      Bug(NULL, "Ran off the end of cstack area (forward)\n");
    }
  }
}



void
forward_xp(ExceptionInformation *xp)                 /* ppc-gc.c:1388-1409 */
{
  natural *regs = (natural *) xpGPRvector(xp);

  int r;

  /* Same register SET as mark_xp (see file header); PC and LR are
     treated as "locatives" (no CTR/loc_pc on ARM64 â€” ARM64-DEVIATION,
     arm-gc.c:1243-1244). */

  GCforward_context = "xp GPR";
  GCforward_branch = "(xp)";
  for (r = fn; r <= rnil; r++) {
    if (r != 18) {
      GCforward_reg = r;
      GCforward_slot = (LispObj *) (&(regs[r]));
      update_noderef((LispObj*) (&(regs[r])));
    }
  }
  GCforward_reg = -1; GCforward_slot = NULL;

  GCforward_context = "xp PC";
  update_locref((LispObj*) (&(xpPC(xp))));
  GCforward_context = "xp LR";
  update_locref((LispObj*) (&(xpLR(xp))));
}


void
forward_tcr_xframes(TCR *tcr)                        /* ppc-gc.c:1412-1428 */
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
compact_dynamic_heap()                               /* ppc-gc.c:1437-1607 */
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

  /* 16m39: self-attribute.  This function Bug()ed while GCforward_context
     still said "cstack" (the walkers' residue), and the 16m38 handoff
     chased a stack-walker gap for it.  An unmarked ref found HERE means a
     MARKED object's slot references an unmarked object: mark-closure gap
     or a dead entry a weak/GCTWA pass failed to scrub -- a different
     investigation from a root-set gap.  NB the gc-common.c sites between
     the area loop and here (forward_memoized_area) and after (
     forward_weakvll_links) still can't self-attribute; a Bug tagged with a
     walker name that the slot value contradicts is one of THOSE. */
  GCforward_context = "compact (slot of a MARKED object)";
  GCforward_slot = NULL; GCforward_branch = "(compact)";
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
        if (nodeheader_tag_p(tag)) {                 /* ppc-gc.c:1490-1533 */
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
            /* OPEN-ENTRYPOINT (ARM64-DEVIATION, arm-gc.c:1391-1398):
               the first slot of a function (pseudofunction removed upstream @ a6314ba) is its
               entrypoint locative. */
            subtag = header_subtag(node);
            if (subtag == subtag_function) {
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
        } else if (immheader_tag_p(tag)) {           /* ppc-gc.c:1534-1587 */
          LispObj *xiv_dest = dest;
          Boolean xiv_is_code = false;
          *dest++ = node;
          *dest++ = *src++;
          elements = header_element_count(node);
          tag = header_subtag(node);

          /* ppc-gc.c:1541-1561 (PPC64 class switch) re-keyed to arm64
             classes; a subtag's low 4 bits ARE its immheader class.
             PPC64's ivector_class_8_bit case folds into other_bit. */
          switch(fulltag_of(tag)) {
          case ivector_class_64_bit:
            imm_dnodes = ((elements+1)+1)>>1;
            break;
          case ivector_class_32_bit:
            if (tag == subtag_code_vector) {
              GCrelocated_code_vector = true;
              xiv_is_code = true;
            }
            imm_dnodes = (((elements+2)+3)>>2);
            break;
          case ivector_class_other_bit:
          default:
            if (tag == subtag_bit_vector) {
              imm_dnodes = (((elements+64)+127)>>7);
            } else if (tag >= subtag_s8_vector) {
              imm_dnodes = (((elements+8)+15)>>4);
            } else if (tag == subtag_complex_double_float_vector) {
              /* 16m41: 16 bytes/element + the 8-byte header, rounded up =
                 ceil((16n+8)/16) = n+1.  This site COPIES imm_dnodes, so a
                 wrong count here corrupts the heap during compaction rather
                 than merely mis-striding a walk. */
              imm_dnodes = elements + 1;
            } else {            /* 16-bit: s16, u16 */
              imm_dnodes = (((elements+4)+7)>>3);
            }
          }

          dnode += imm_dnodes;
          while (--imm_dnodes) {
            *dest++ = *src++;
            *dest++ = *src++;
          }
          if (xiv_is_code) {
            /* ARM64-DEVIATION (perf, measured 16m62): flush the relocated
               code vector's new extent NOW instead of one whole-region
               flush after compaction.  I-cache maintenance is only needed
               for bytes that will be FETCHED AS INSTRUCTIONS: every such
               byte is inside some code vector, each relocated code vector
               is flushed here, and code vectors born after this GC are
               flushed at creation (%make-code-executable).  The PPC-shaped
               whole-region flush (ppc-gc.c:1599-1604) ic-ivau'd every line
               of the compacted region -- data included -- which on
               Neoverse-N1 (DIC=0, broadcast ic ivau) costs tens of suite
               seconds. */
            xMakeDataExecutable((BytePtr)xiv_dest,
                                (natural)((char *)dest - (char *)xiv_dest));
          }
          set_bitidx_vars(markbits,dnode,bitsp,bits,bitidx);
        } else {                                     /* ppc-gc.c:1588-1594 */
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
        /* ARM64-DEVIATION (perf, measured 16m62): the PPC-shaped
           whole-region flush that lived here moved INTO the compaction
           copy loop above -- each relocated code vector is flushed at its
           new extent as it lands, so by this point every relocated code
           vector is already synced and flushing the data between them
           buys nothing.  (nbytes computation kept: it documents the
           region the PPC form covered.) */
      }
    }
  }
  return ptr_to_lispobj(dest);
}




/*
  Total the (physical) byte sizes of all ivectors in the indicated memory range
*/

natural
unboxed_bytes_in_range(LispObj *start, LispObj *end) /* ppc-gc.c:1618-1682 */
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

        /* ppc-gc.c:1637-1654 (PPC64 class switch), arm64 classes */
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
          } else if (subtag >= subtag_s8_vector) {
            bytes = 8 + elements;
          } else if (subtag == subtag_complex_double_float_vector) {
            bytes = 8 + (elements<<4);  /* 16m41; see mark_root */
          } else {              /* 16-bit: s16, u16 */
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
     This assumes that it's getting called with an ivector
     argument and that there's room for the object in the
     destination area.
  */


LispObj
purify_displaced_object(LispObj obj, area *dest, natural disp)
{                                                    /* ppc-gc.c:1692-1727 */
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
    *old++ = (BytePtr) forward_marker;   /* = fulltag_nil under -DARM64, see shim note */
    *old++ = (BytePtr) free;
    free += dnode_size;
    physbytes -= dnode_size;
  }
  return new;
}

LispObj
purify_object(LispObj obj, area *dest)               /* ppc-gc.c:1729-1733 */
{
  return purify_displaced_object(obj, dest, fulltag_of(obj));
}



void
copy_ivector_reference(LispObj *ref, BytePtr low, BytePtr high, area *dest)
{                                                    /* ppc-gc.c:1737-1758 */
  LispObj obj = *ref, header;
  natural tag = fulltag_of(obj), header_tag;

  /* Only fulltag_misc references can name ivectors on arm64 (symbols and
     functions are gvectors; a function's codevector slot is misc-tagged),
     so PPC's misc-only test ports unchanged. */
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
{                                                    /* ppc-gc.c:1760-1814 (PPC64 branch) */
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
    /* ARM64-DEVIATION: `(loc & 3) == 0' replaces PPC64's four-case
       switch (even/odd fixnum, cons, misc â€” its 4-aligned residues);
       arm64's are even_fixnum/misc/odd_fixnum/immheader_1, cf.
       locative_forwarding_address. */
    if ((loc & 3) == 0) {
      if (*headerP == forward_marker) {
        *locaddr = (headerP[1]+tag);                 /* ppc-gc.c:1784-1785 */
      } else {
        /* Grovel backwards until the code vector's udf#0 sentinel is
           found; copy the code vector to to-space, then treat it as if
           it hadn't already been copied.  (ppc-gc.c:1786-1798, 'CODE'
           prefix â†’ sentinel.) */
        p = (opcode *)headerP;
        do {
          p -= 2;
          tag += 8;
          insn = *p;
        } while (insn != ARM64_CODE_VECTOR_SENTINEL);
        /* VENDOR BUG FIXED (see file header note 6 / report Â§5.11):
           the header word sits node_size below the sentinel, so the
           displacement from the header is (loc - sentinel) + node_size;
           vendor ppc-gc.c:1795-1798 passes tag = loc - sentinel,
           relocating the locative one node low (the PPC32 branch at
           ppc-gc.c:1800-1801, which grovels for the header itself, gets
           it right). */
        tag += node_size;
        headerP = ((LispObj*)p)-1;
        *locaddr = purify_displaced_object(((LispObj)headerP), to, tag);
      }
    }
  }
}

void
purify_range(LispObj *start, LispObj *end, BytePtr low, BytePtr high, area *to)
{                                                    /* ppc-gc.c:1816-1840 */
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
        /* OPEN-ENTRYPOINT (ARM64-DEVIATION, arm-gc.c:1630-1645): a
           function (pseudofunction removed upstream @ a6314ba)'s first slot is its entrypoint; if it
           is an odd_fixnum-tagged locative into a purifiable code
           vector, retag as misc, purify, retag back. */
        subtag = header_subtag(header);
        if (nodeheader_tag_p(tag) &&
            (subtag == subtag_function)) {
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

/* Purify references from tstack areas */
void
purify_tstack_area(area *a, BytePtr low, BytePtr high, area *to)
{                                                    /* ppc-gc.c:1843-1862 */
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
{                                                    /* ppc-gc.c:1865-1877 */
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
{                                                    /* arm-gc.c:1670-1716 */
  /* ARM64-DEVIATION (whole function; ppc-gc.c:1880-1900 walks backlink
     frames): marker-frame walk, case order corrected as in
     mark_cstack_area. */
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
    } else if (nodeheader_tag_p(fulltag_of(header))) { /* REORDERED, see mark_cstack_area */
      natural elements = header_element_count(header);

      current++;
      /* OPEN-ENTRYPOINT (arm-gc.c:1696-1702) */
      subtag = header_subtag(header);
      if (subtag == subtag_function) {
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
    } else if (immheader_tag_p(fulltag_of(header))) {  /* REORDERED */
      current=(LispObj *)skip_over_ivector((natural)current,header);
    } else if ((header & fixnummask) == 0) {
      current = (LispObj *)header;
    } else {
      Bug(NULL, "Unknown stack word at 0x" LISP ":\n", current);
    }
    /* Match mark_cstack_area: a bad stride must not silently skip frames. */
    if (current > limit) {
      Bug(NULL, "Ran off the end of cstack area\n");
      current = limit;
    }
  }
  if (current != limit) {
    Bug(NULL, "Ran off the end of cstack area\n");
  }
}

void
purify_xp(ExceptionInformation *xp, BytePtr low, BytePtr high, area *to)
{                                                    /* ppc-gc.c:1902-1922 */
  natural *regs = (natural *) xpGPRvector(xp);

  int r;

  /* Same register SET as mark_xp (see file header); PC and LR are
     treated as "locatives". */

  for (r = fn; r <= rnil; r++) {
    if (r != 18) {
      copy_ivector_reference((LispObj*) (&(regs[r])), low, high, to);
    }
  }

  purify_locref((LispObj*) (&(xpPC(xp))), low, high, to);
  purify_locref((LispObj*) (&(xpLR(xp))), low, high, to);
}

void
purify_tcr_tlb(TCR *tcr, BytePtr low, BytePtr high, area *to)
{                                                    /* ppc-gc.c:1924-1931 */
  natural n = tcr->tlb_limit;
  LispObj *start = tcr->tlb_pointer, *end = (LispObj *) ((BytePtr)start+n);

  purify_range(start, end, low, high, to);
}

void
purify_tcr_xframes(TCR *tcr, BytePtr low, BytePtr high, area *to)
{                                                    /* ppc-gc.c:1933-1947 */
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
{                                                    /* ppc-gc.c:1949-1959 */
  LispObj *prev = &(lisp_global(GCABLE_POINTERS)), next;

  while ((*prev) != (LispObj)NULL) {
    copy_ivector_reference(prev, low, high, to);
    next = *prev;
    prev = &(((xmacptr *)ptr_from_lispobj(untag(next)))->link);
  }
}


void
purify_areas(BytePtr low, BytePtr high, area *target)
{                                                    /* ppc-gc.c:1962-1991 */
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

    /* AREA_MANAGED_STATIC remains gated by the flash_freeze stub.  Add
       traversal before implementing that gate. */
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
purify(TCR *tcr, signed_natural param)               /* ppc-gc.c:2001-2048 */
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
impurify_locref(LispObj *p, LispObj low, LispObj high, signed_natural delta)
{                                                    /* ppc-gc.c:2050-2066 */
  LispObj q = *p;

  /* ARM64-DEVIATION: `(q & 3) == 0' replaces PPC64's switch over
     {cons, misc, even_fixnum, odd_fixnum} â€” see locative_forwarding_address
     for the residue analysis; conses are never purified (only ivectors
     reach the readonly area), so excluding fulltag_cons(3) loses nothing. */
  if (((q & 3) == 0) &&
      (q >= low) && (q < high)) {
    *p = (q+delta);
  }
}


void
impurify_noderef(LispObj *p, LispObj low, LispObj high, signed_natural delta)
{                                                    /* ppc-gc.c:2069-2079 */
  LispObj q = *p;

  if ((fulltag_of(q) == fulltag_misc) &&
      (q >= low) &&
      (q < high)) {
    *p = (q+delta);
  }
}


void
impurify_cstack_area(area *a, LispObj low, LispObj high, signed_natural delta)
{                                                    /* arm-gc.c:1889-1934 */
  /* ARM64-DEVIATION (whole function; ppc-gc.c:2082-2104 walks backlink
     frames): marker-frame walk, case order corrected as in
     mark_cstack_area. */
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
    } else if (nodeheader_tag_p(fulltag_of(header))) { /* REORDERED, see mark_cstack_area */
      natural elements = header_element_count(header);

      current++;
      /* OPEN-ENTRYPOINT (arm-gc.c:1914-1920) */
      subtag = header_subtag(header);
      if (subtag == subtag_function) {
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
    } else if (immheader_tag_p(fulltag_of(header))) {  /* REORDERED */
      current=(LispObj *)skip_over_ivector((natural)current,header);
    } else if ((header & fixnummask) == 0) {
      current = (LispObj *) header;
    } else {
      Bug(NULL, "Unknown stack word at 0x" LISP ":\n", current);
    }
    /* Match mark_cstack_area: a bad stride must not silently skip frames. */
    if (current > limit) {
      Bug(NULL, "Ran off the end of cstack area\n");
      current = limit;
    }
  }
  if (current != limit) {
    Bug(NULL, "Ran off the end of cstack area\n");
  }
}


void
impurify_xp(ExceptionInformation *xp, LispObj low, LispObj high, signed_natural delta)
{                                                    /* ppc-gc.c:2106-2128 */
  natural *regs = (natural *) xpGPRvector(xp);
  int r;

  /* Same register SET as mark_xp (see file header); PC and LR are
     treated as "locatives". */

  for (r = fn; r <= rnil; r++) {
    if (r != 18) {
      impurify_noderef((LispObj*) (&(regs[r])), low, high, delta);
    }
  }

  impurify_locref((LispObj*) (&(xpPC(xp))), low, high, delta);
  impurify_locref((LispObj*) (&(xpLR(xp))), low, high, delta);
}


void
impurify_range(LispObj *start, LispObj *end, LispObj low, LispObj high, signed_natural delta)
{                                                    /* ppc-gc.c:2131-2151 */
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
      /* OPEN-ENTRYPOINT (ARM64-DEVIATION, arm-gc.c:1974-1988): mirror
         of purify_range's entrypoint handling. */
      subtag = header_subtag(header);
      if (nodeheader_tag_p(tag) &&
          (subtag == subtag_function)) {
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
      } else {
        impurify_noderef(start, low, high, delta);
      }
      start++;
    }
  }
}




void
impurify_tcr_tlb(TCR *tcr,  LispObj low, LispObj high, signed_natural delta)
{                                                    /* ppc-gc.c:2156-2163 */
  unsigned n = tcr->tlb_limit;
  LispObj *start = tcr->tlb_pointer, *end = (LispObj *) ((BytePtr)start+n);

  impurify_range(start, end, low, high, delta);
}

void
impurify_tcr_xframes(TCR *tcr, LispObj low, LispObj high, signed_natural delta)
{                                                    /* ppc-gc.c:2165-2179 */
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
impurify_tstack_area(area *a, LispObj low, LispObj high, signed_natural delta)
{                                                    /* ppc-gc.c:2181-2200 */
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
impurify_vstack_area(area *a, LispObj low, LispObj high, signed_natural delta)
{                                                    /* ppc-gc.c:2201-2213 */
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
impurify_areas(LispObj low, LispObj high, signed_natural delta)
{                                                    /* ppc-gc.c:2216-2247 */
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
      impurify_cstack_area(next_area, low, high, delta);
      break;

    case AREA_STATIC:
    case AREA_DYNAMIC:
      impurify_range((LispObj *) next_area->low, (LispObj *) next_area->active, low, high, delta);
      break;

    /* AREA_MANAGED_STATIC remains gated by the flash_freeze stub.  Add
       traversal before implementing that gate. */
    default:
      break;
    }
  }
}

void
impurify_gcable_ptrs(LispObj low, LispObj high, signed_natural delta)
{                                                    /* ppc-gc.c:2249-2259 */
  LispObj *prev = &(lisp_global(GCABLE_POINTERS)), next;

  while ((*prev) != (LispObj)NULL) {
    impurify_noderef(prev, low, high, delta);
    next = *prev;
    prev = &(((xmacptr *)ptr_from_lispobj(untag(next)))->link);
  }
}

signed_natural
impurify(TCR *tcr, signed_natural param)             /* ppc-gc.c:2261-2303 */
{
  area *r = readonly_area;

  if (r) {
    area *a = active_dynamic_area;
    BytePtr ro_base = r->low, ro_limit = r->active, oldfree = a->active,
      oldhigh = a->high, newhigh;
    /* Match upstream x86-gc.c:3173-3174 at pin 9fb47830: the
       PURESPACE_RESERVE relocation cannot be represented by int. */
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
      xMakeDataExecutable(oldfree, n);
      munmap(ro_base, n);                            /* ppc-gc.c:2283 */
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
