/* SPDX-License-Identifier: Apache-2.0 */

#include "lisp.h"
#include "lisp_globals.h"
#include "bits.h"
#include "gc.h"
#include "area.h"
#include "lisp-exceptions.h"
#include "threads.h"
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

/*
 *  Return the address just past an ivector whose header word is HEADER
 *  and whose header sits at START.
 */
LispObj *
skip_over_ivector(natural start, LispObj header)
{
  natural element_count = header_element_count(header);
  natural subtag = header_subtag(header);
  natural nbytes;

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
      nbytes = (element_count + 7) >> 3;
    } else if (subtag == subtag_complex_double_float_vector) {
      nbytes = element_count << 4;
    } else if (subtag >= subtag_s8_vector) {
      nbytes = element_count;           /* 8-bit elements */
    } else {
      nbytes = element_count << 1;      /* 16-bit elements */
    }
  }
  /* one node for the header, then the elements, rounded up to a dnode. */
  return ptr_from_lispobj(start + ((nbytes + node_size + (dnode_size - 1))
                                   & ~(dnode_size - 1)));
}

/*
  Mark Lisp roots on the control stack, from a->active up to a->high.

  When the thread has Lisp valence, a->active is its live SP.  When it
  was suspended in foreign code, the suspend path has already saved
  a->active to tcr->last_lisp_frame so the raw C frames below the
  boundary frame are simply not in range here.

  Every word this loop lands on is a frame-leading word, and on arm64 that is
  exactly one of two things:

    a lisp_frame_marker: a {marker,savevsp,savefn,savelr} Lisp frame,
        built by the save-lisp-context vinsns.  It contains two roots,
        namely savefn and savelr.

    an immheader: a u64-vector that encapsulates or covers an unboxed region.
        Two producers make these on the control stack:
        
        1. A C frame, whose element could is ultimately shrunk to expose
           a boundary lisp frame at its high end.
        2. In the callback case, a u64-vector set up to cover an entire
           lisp->C->...->C stretch.

        Either way, skip_over_ivector steps over the whole covered
        region in one hop and lands us on the next real frame.

  Unlike on other ports, a fixnum-tagged, zero, or otherwise unrecognized
  word is a bug (not a backlink or anything else).  Stack-consed objects
  live on the temp stack, never here, so there's no node-header case, and
  no stack_alloc_marker case either.
*/
void
mark_cstack_area(area *a)
{
  LispObj *current = (LispObj *)(a->active),
          *limit = (LispObj *)(a->high),
          header;

  while (current < limit) {
    header = *current;

    if (header == lisp_frame_marker) {
      lisp_frame *frame = (lisp_frame *)current;
      mark_root(frame->savefn);         /* the running function */
      mark_pc_root(frame->savelr);      /* return address -> code vector */
      current += sizeof(lisp_frame) / sizeof(LispObj);  /* 4 nodes */
    } else if (immheader_tag_p(fulltag_of(header))) {
      current = skip_over_ivector((natural)current, header);
    } else {
      Bug(NULL, "Unrecognized control stack word 0x" LISP " at 0x" LISP "\n",
          header, (natural)current);
    }
  }
  if (current != limit) {
    Bug(NULL, "Ran off the end of the control stack area\n");
  }
}
