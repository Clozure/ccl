/* SPDX-License-Identifier: Apache-2.0 */

/*
 * Copyright 1994-2009 Clozure Associates
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/* ARM64-SPECIFIC — justification: ARM-family kernel printer ported from
 * arm_print.c (no PPC64 analog file): the kernel debugger's lisp-object
 * printer; its export print_lisp_object is the last undefined symbol in
 * the arm64 kernel link (lisp.h:340, lispdcmd users).
 *
 * Source: arm_print.c (ARM32, 507 lines) @ upstream tip 115b7aa, remapped
 * onto Matt Emerson's ARM64 low-tag design.  Layout reference for the
 * 64-bit/low-tag deviations: x86_print.c (same tree; the x8664 sibling has
 * the same 64-bit low-tag shape), cited x86_print.c:NNN.
 * Tag authority: arm64-constants.h @ 115b7aa (fulltags :120-135, misc
 * offsets :137-141, subtags :143-215); generic object accessors from
 * macros.h:28-45 (mask-based, correct for this scheme as-is).
 *
 * PORT-NOTE — deviations from arm_print.c (each tagged inline):
 *  1. Dispatch restructured for the 16-fulltag space: symbols have their
 *     OWN pointer fulltag here (fulltag_symbol=7), not a misc subtag;
 *     functions are ordinary miscobjs (fulltag_function removed, patch
 *     0055); NIL has fulltag_nil=11; single-floats are immediates with
 *     fulltag_single_float=1.
 *  2. Single-float bits live in the top 32 bits of the immediate
 *     (compiler/ARM64/arm64-lapmacros.lisp:49-51); extraction idiom from
 *     x86_print.c:604-608.
 *  3. KNOWN UPSTREAM BUG: subtag_s16_vector ==
 *     subtag_complex_double_float_vector == 0x95 (arm64-constants.h:
 *     164-166).  The one dispatch on that value here (vector_subtag_name)
 *     treats it as s16 per lane ruling.
 * See drafts/arm64-print-report.md for the full inventory and findings.
 */

#include <stdio.h>              /* arm_print.c:17 */
#include <stdarg.h>             /* arm_print.c:18 */
#include <setjmp.h>             /* arm_print.c:19 */
#include <string.h>             /* ARM64-DEVIATION: arm_print.c calls strlen
                                   (:68) with no declaration in scope (lisp.h
                                   pulls no string.h); implicit decls are
                                   errors on modern GCC and int-returning on
                                   LP64.  arm64-exceptions.c:47 precedent. */

#include "lisp.h"               /* arm_print.c:21 */
#include "area.h"               /* arm_print.c:22 */
#include "lisp-exceptions.h"    /* arm_print.c:23 */
#include "lisp_globals.h"       /* arm_print.c:24 */

/* lisp_global/nrs_symbol/unbound/fixnum_bitmask/immheader_tag_p are
 * supplied (guarded) by platform-linuxarm64.h, which every kernel .c gets
 * via `-include' (linuxarm64/Makefile:51); linuxarm64 builds define
 * -DARM64, not -DARM (Makefile:26-27), so lisp_globals.h's #ifdef ARM
 * ladder stays quiet.  Nothing is redefined here. */

void
sprint_lisp_object(LispObj, int);       /* arm_print.c:26-27 */

#define PBUFLEN 252             /* arm_print.c:29 */

char printbuf[PBUFLEN + 4];     /* arm_print.c:31 */
int bufpos = 0;                 /* arm_print.c:32 */

jmp_buf escape;                 /* arm_print.c:34 */

void
add_char(char c)                /* arm_print.c:36-44 */
{
  if (bufpos >= PBUFLEN) {
    longjmp(escape, 1);
  } else {
    printbuf[bufpos++] = c;
  }
}

void
add_string(char *s, int len)    /* arm_print.c:46-52 */
{
  while(len--) {
    add_char(*s++);
  }
}

void
add_lisp_base_string(LispObj str)  /* arm_print.c:54-63 */
{
  /* lisp_char_code is int32_t (lisptypes.h:32); simple_base_string is a
   * 32-bit ivector on this target too (arm64-constants.h:159).
   * misc_data_offset = +4 from the tagged pointer = untag+8
   * (arm64-constants.h:139-140). */
  lisp_char_code *src = (lisp_char_code *)  (ptr_from_lispobj(str + misc_data_offset));
  natural i, n = header_element_count(header_of(str));

  for (i=0; i < n; i++) {
    add_char((char)(*src++));
  }
}

void
add_c_string(char *s)           /* arm_print.c:65-69 */
{
  add_string(s, strlen(s));
}

char numbuf[64];                /* arm_print.c:71 */

void
sprint_signed_decimal(signed_natural n)  /* arm_print.c:73-78 */
{
  sprintf(numbuf, "%ld", (long)n);      /* signed_natural = int64_t (lisptypes.h:25) */
  add_c_string(numbuf);
}

void
sprint_unsigned_decimal(natural n)  /* arm_print.c:80-85 */
{
  sprintf(numbuf, "%lu", (unsigned long)n);
  add_c_string(numbuf);
}

void
sprint_unsigned_hex(natural n)  /* arm_print.c:87-96; 64-bit (PPC64) branch
                                   of the source's #ifdef selected —
                                   WORD_SIZE == 64 (x86_print.c:112-113) */
{
  sprintf(numbuf, "#x%016lx", (unsigned long)n);  /* arm_print.c:91 */
  add_c_string(numbuf);
}

void
sprint_list(LispObj o, int depth)  /* arm_print.c:98-122 */
{
  /* car/cdr from macros.h:44-45 over constants.h:41-44 cons {cdr,car}:
   * cdr@untag+0, car@untag+8 — correct for fulltag_cons=3 as-is. */
  LispObj the_cdr;

  add_char('(');
  while(1) {
    if (o != lisp_nil) {
      sprint_lisp_object(ptr_to_lispobj(car(o)), depth);
      the_cdr = ptr_to_lispobj(cdr(o));
      if (the_cdr != lisp_nil) {
        add_char(' ');
        if (fulltag_of(the_cdr) == fulltag_cons) {
          o = the_cdr;
          continue;
        }
        add_c_string(". ");
        sprint_lisp_object(the_cdr, depth);
        break;
      }
    }
    break;
  }
  add_char(')');
}

/*
  Print a list of method specializers, using the class name instead of the class object.
*/

void
sprint_specializers_list(LispObj o, int depth)  /* arm_print.c:128-171 */
{
  /* x86_print.c:183-234 adds a DARWIN-only foreign_class_name branch;
   * arm_print.c (and LINUX) have none — the ARM32 shape is kept. */
  LispObj the_cdr, the_car;

  add_char('(');
  while(1) {
    if (o != lisp_nil) {
      the_car = car(o);
      if (fulltag_of(the_car) == fulltag_misc) {
	LispObj header = header_of(the_car);
	unsigned subtag = header_subtag(header);

	if (subtag == subtag_instance) {
          if (unbox_fixnum(deref(the_car,1)) < (1<<20)) {
            sprint_lisp_object(deref(deref(the_car,3), 4), depth);
          } else {
            /* An EQL specializer */
            add_c_string("(EQL ");
            sprint_lisp_object(deref(deref(the_car,3), 3), depth);
            add_char(')');
          }
	} else {
	  sprint_lisp_object(the_car, depth);
	}
      } else {
        sprint_lisp_object(the_car, depth);
      }
      the_cdr = cdr(o);
      if (the_cdr != lisp_nil) {
        add_char(' ');
        if (fulltag_of(the_cdr) == fulltag_cons) {
          o = the_cdr;
          continue;
        }
        add_c_string(". ");
        sprint_lisp_object(the_cdr, depth);
        break;
      }
    }
    break;
  }
  add_char(')');
}

char *
vector_subtag_name(unsigned subtag)  /* arm_print.c:173-225 */
{
  switch (subtag) {
  case subtag_bit_vector:
    return "BIT-VECTOR";
    break;
  case subtag_instance:
    return "INSTANCE";
    break;
  case subtag_bignum:
    return "BIGNUM";
    break;
  case subtag_u8_vector:
    return "(UNSIGNED-BYTE 8)";
    break;
  case subtag_s8_vector:
    return "(SIGNED-BYTE 8)";
    break;
  case subtag_u16_vector:
    return "(UNSIGNED-BYTE 16)";
    break;
  case subtag_s16_vector:
    /* KNOWN UPSTREAM BUG: subtag_s16_vector ==
     * subtag_complex_double_float_vector == 0x95 (arm64-constants.h:
     * 164-166, both SUBTAG(ivector_class_other_bit,9)).  Per lane ruling
     * this value dispatches as s16 here. */
    return "(SIGNED-BYTE 16)";
    break;
  case subtag_u32_vector:
    return "(UNSIGNED-BYTE 32)";
    break;
  case subtag_s32_vector:
    return "(SIGNED-BYTE 32)";
    break;
  case subtag_u64_vector:      /* 64-bit target: x86_print.c:267-274
                                  (X8664 branch); arm64-constants.h:155-156 */
    return "(UNSIGNED-BYTE 64)";
    break;
  case subtag_s64_vector:
    return "(SIGNED-BYTE 64)";
    break;
  case subtag_package:
    return "PACKAGE";
    break;
  case subtag_code_vector:     /* arm_print.c:215-217; exists here too
                                  (arm64-constants.h:181) */
    return "CODE-VECTOR";
    break;
  case subtag_slot_vector:
    return "SLOT-VECTOR";
    break;
  default:
    return "";
    break;
  }
}


void
sprint_random_vector(LispObj o, unsigned subtag, natural elements)  /* arm_print.c:228-240 */
{
  add_c_string("#<");
  sprint_unsigned_decimal(elements);
  add_c_string("-element vector subtag = ");
  sprintf(numbuf, "%02X @", subtag);
  add_c_string(numbuf);
  sprint_unsigned_hex(o);
  add_c_string(" (");
  add_c_string(vector_subtag_name(subtag));
  add_c_string(")>");
}

void
sprint_symbol(LispObj o)        /* arm_print.c:242-264 */
{
  /* untag (macros.h:30) strips any fulltag, so this works both for
   * fulltag_symbol pointers and for symbols reached via other tags;
   * constants.h:48-57 lispsymbol field order matches the asm symbol
   * struct (arm64-constants.h:332-340).  The source's #ifdef PPC64
   * nil-check (arm_print.c:250-255) is dropped: fulltag_nil is dispatched
   * before sprint_symbol can be reached (x86_print.c:303-319 precedent). */
  lispsymbol *rawsym = (lispsymbol *) ptr_from_lispobj(untag(o));
  LispObj
    pname = rawsym->pname,
    package = rawsym->package_predicate;

  if (fulltag_of(package) == fulltag_cons) {
    package = car(package);
  }

  if (package == nrs_KEYWORD_PACKAGE.vcell) {  /* lisp_globals.h:166 */
    add_char(':');
  }
  add_lisp_base_string(pname);
}

void
sprint_function(LispObj o, int depth)  /* arm_print.c:266-315 */
{
  /* deref/header_of are fulltag-agnostic; functions arrive misc-tagged
   * (fulltag_function removed, patch 0055) via the sprint_gvector
   * subtag dispatch.  lfbits is the last element, name the one before
   * (macros.h:91-93 convention; lfbits masks constants.h:162-165). */
  LispObj lfbits, header, name = lisp_nil;
  natural elements;

  header = header_of(o);
  elements = header_element_count(header);
  lfbits = deref(o, elements);

  if ((lfbits & lfbits_noname_mask) == 0) {
    name = deref(o, elements-1);
  }

  add_c_string("#<");
  if (name == lisp_nil) {
    add_c_string("Anonymous Function ");
  } else {
    if (lfbits & lfbits_method_mask) {
      if (header_subtag(header_of(name)) == subtag_instance) {
        LispObj
          slot_vector = deref(name,3),
          method_name = deref(slot_vector, 6),
          method_qualifiers = deref(slot_vector, 2),
          method_specializers = deref(slot_vector, 3);
        add_c_string("Method-Function ");
        sprint_lisp_object(method_name, depth);
        add_char(' ');
        if (method_qualifiers != lisp_nil) {
          if (cdr(method_qualifiers) == lisp_nil) {
            sprint_lisp_object(car(method_qualifiers), depth);
          } else {
            sprint_lisp_object(method_qualifiers, depth);
          }
          add_char(' ');
        }
        sprint_specializers_list(method_specializers, depth);
      } else {
        sprint_lisp_object(name, depth);
      }
      add_char(' ');
    } else if (lfbits & lfbits_gfn_mask) {
      /* x86_print.c:375-386 (64-bit shape: label only; the name-digging
       * block there is X8632-specific and is omitted, as x8664 omits it) */
      add_c_string("Generic Function ");
    } else {
      add_c_string("Function ");
      sprint_lisp_object(name, depth);
      add_char(' ');
    }
  }
  sprint_unsigned_hex(o);
  add_char('>');
}

void
sprint_gvector(LispObj o, int depth)  /* arm_print.c:317-361 */
{
  LispObj header = header_of(o);
  unsigned
    elements = header_element_count(header),
    subtag = header_subtag(header);

  switch(subtag) {
  /* subtag_function/subtag_symbol are retained although such objects
   * normally carry their own fulltags here — header subtags still exist
   * (arm64-constants.h:183,191) and x86_print.c:451-457 retains both on
   * x8664 likewise. */
  case subtag_function:
    sprint_function(o, depth);
    break;

  case subtag_symbol:
    sprint_symbol(o);
    break;

  case subtag_struct:
  case subtag_istruct:
    add_c_string("#<");
    sprint_lisp_object(deref(o,1), depth);
    add_c_string(" @");
    sprint_unsigned_hex(o);
    add_c_string(">");
    break;

  case subtag_simple_vector:
    {
      int i;
      add_c_string("#(");
      for(i = 1; i <= elements; i++) {
        if (i > 1) {
          add_char(' ');
        }
        sprint_lisp_object(deref(o, i), depth);
      }
      add_char(')');
      break;
    }

  case subtag_instance:        /* x86_print.c:482-496 (64-bit sibling
                                  addition; absent from arm_print.c) */
    {
      LispObj class_or_hash = deref(o,1);

      if (tag_of(class_or_hash) == tag_fixnum) {
	sprint_random_vector(o, subtag, elements);
      } else {
	add_c_string("#<CLASS ");
	sprint_lisp_object(class_or_hash, depth);
	add_c_string(" @");
	sprint_unsigned_hex(o);
	add_c_string(">");
      }
      break;
    }

  default:
    sprint_random_vector(o, subtag, elements);
    break;
  }
}

void
sprint_ivector(LispObj o)       /* arm_print.c:363-401 */
{
  LispObj header = header_of(o);
  unsigned
    elements = header_element_count(header),
    subtag = header_subtag(header);

  switch(subtag) {
  case subtag_simple_base_string:
    add_char('"');
    add_lisp_base_string(o);
    add_char('"');
    return;

  case subtag_bignum:
    /* NOTE (inherited quirk, see report): bigits are 32-bit here
     * (arm64-constants.h:377-378) but this reads 64-bit words —
     * x86_print.c:521-529 ships exactly this shape on x8664 (same 32-bit
     * bigits) and is followed verbatim; on 64-bit targets elements==1
     * cannot occur and the elements==2 read yields the right LE value. */
    if (elements == 1) {
      sprint_signed_decimal((signed_natural)(deref(o, 1)));
      return;
    }
    if ((elements == 2) && (deref(o, 2) == 0)) {
      sprint_unsigned_decimal(deref(o, 1));
      return;
    }
    break;

  case subtag_double_float:
    /* prints nothing — inherited from arm_print.c:389-390 ==
     * x86_print.c:532-533 (both fall out of the switch with no output) */
    break;

  case subtag_macptr:
    add_c_string("#<MACPTR ");
    sprint_unsigned_hex(deref(o,1));
    add_c_string(">");
    break;

  default:
    sprint_random_vector(o, subtag, elements);
  }
}

void
sprint_vector(LispObj o, int depth)  /* arm_print.c:403-413 */
{
  /* immheader_tag_p over {immheader_0,immheader_1,immheader_2} comes from
   * platform-linuxarm64.h:134-139 */
  LispObj header = header_of(o);

  if (immheader_tag_p(fulltag_of(header))) {
    sprint_ivector(o);
  } else {
    sprint_gvector(o, depth);
  }
}

void
sprint_lisp_object(LispObj o, int depth)  /* arm_print.c:415-491 */
{
  if (--depth < 0) {
    add_char('#');
  } else {
    /* ARM64-DEVIATION: dispatch restructured from ARM32's 8-fulltag
     * switch (arm_print.c:421-489) onto this design's 16-fulltag space
     * (arm64-constants.h:120-135), following x86_print.c:564-648's
     * 64-bit shape.  The ARM32 switch was total over its tag space; this
     * one is kept total too. */
    switch (fulltag_of(o)) {
    case fulltag_even_fixnum:           /* arm_print.c:422-424 */
    case fulltag_odd_fixnum:
      sprint_signed_decimal(unbox_fixnum(o));
      break;

    case fulltag_single_float:
      /* ARM64-DEVIATION: single-floats are immediates with their own
       * fulltag (arm64-arch.lisp:47,57,78 "single-float (and nothing
       * but)"); no ARM32 analog.  Bits in the TOP 32 BITS
       * (arm64-lapmacros.lisp:49-51); LE extraction idiom from
       * x86_print.c:604-608. */
      {
        LispObj xx = o;
        float f = ((float *)&xx)[1];
        sprintf(numbuf, "%f", f);
        add_c_string(numbuf);
      }
      break;

    case fulltag_immheader_0:           /* arm_print.c:437-443; three imm-
                                           header + two node-header fulltags
                                           here (arm64-constants.h:125-126,
                                           132-134) vs ARM32's one of each */
    case fulltag_immheader_1:
    case fulltag_immheader_2:
    case fulltag_nodeheader_0:
    case fulltag_nodeheader_1:
      add_c_string("#<header ? ");
      sprint_unsigned_hex(o);
      add_c_string(">");
      break;

    case fulltag_imm_0:                 /* characters (arm64-constants.h:203) */
    case fulltag_imm_1:                 /* markers (arm64-constants.h:205-215) */
      /* arm_print.c:446-477 fulltag_imm case; both imm fulltags route
       * here like x86_print.c:586-590's imm_0/imm_1.  The source's PPC64
       * single-float sub-branch is not needed — singles have their own
       * fulltag (case above). */
      if (o == unbound) {               /* `unbound' alias: platform-linuxarm64.h:82-84 */
        add_c_string("#<Unbound>");
      } else {
        if (header_subtag(o) == subtag_character) {
          unsigned c = (o >> charcode_shift);
          add_c_string("#\\");
          if ((c >= ' ') && (c < 0x7f)) {
            add_char(c);
          } else {
            sprintf(numbuf, "%o", c);
            add_c_string(numbuf);
          }
        } else {
          add_c_string("#<imm ");
          sprint_unsigned_hex(o);
          add_c_string(">");
        }
      }
      break;

    case fulltag_nil:
      /* ARM64-DEVIATION (lane directive): NIL prints as NIL.  ARM32
       * folded fulltag_nil into the cons case (arm_print.c:480-484) and
       * x86_print.c:619-624 does the same (yielding "()"); flagged in the
       * report as a reviewable choice. */
      add_c_string("NIL");
      break;

    case fulltag_cons:                  /* arm_print.c:482-484 */
      sprint_list(o, depth);
      break;

    case fulltag_misc:                  /* arm_print.c:486-488 */
      sprint_vector(o, depth);
      break;

    case fulltag_symbol:
      /* ARM64-DEVIATION: symbols have their own pointer fulltag here
       * (arm64-constants.h:127), not a misc subtag as on ARM32;
       * x86_print.c:631-633. */
      sprint_symbol(o);
      break;

    /* fulltag_function removed (patch 0055): functions are ordinary
     * miscobjs and reach sprint_function via the fulltag_misc case's
     * subtag dispatch; tag 15 joins fulltag_reserved below so the
     * dispatch stays total.  (No TRA case: TRA is x86-only; ARM64 is
     * LR-based like ARM32, whose printer has none.) */

    case 15:                            /* was fulltag_function */
    case fulltag_reserved:
      /* ARM64-DEVIATION: no analog in either source; fulltag 0b1001 is
       * reserved (arm64-arch.lisp:65).  Case added so the dispatch stays
       * total over the tag space, as ARM32's was. */
      add_c_string("#<reserved-tag ");
      sprint_unsigned_hex(o);
      add_c_string(">");
      break;
    }
  }
}

char *
print_lisp_object(LispObj o)    /* arm_print.c:493-507 */
{
  bufpos = 0;
  if (setjmp(escape) == 0) {
    sprint_lisp_object(o, 5);
    printbuf[bufpos] = 0;
  } else {
    printbuf[PBUFLEN+0] = '.';
    printbuf[PBUFLEN+1] = '.';
    printbuf[PBUFLEN+2] = '.';
    printbuf[PBUFLEN+3] = 0;
  }
  return printbuf;
}
