/*
   Copyright (C) 2005-2009, Clozure Associates
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

#include <stdio.h>
#include <stdarg.h>
#include <setjmp.h>

#include "lisp.h"
#include "area.h"
#include "lisp-exceptions.h"
#include "lisp_globals.h"

void
sprint_lisp_object(LispObj, int);

#define PBUFLEN 252

char printbuf[PBUFLEN + 4];
int bufpos = 0;

jmp_buf escape;

void
add_char(char c)
{
  if (bufpos >= PBUFLEN) {
    longjmp(escape, 1);
  } else {
    printbuf[bufpos++] = c;
  }
}

void
add_string(char *s, int len) 
{
  while(len--) {
    add_char(*s++);
  }
}

void
add_lisp_base_string(LispObj str)
{
  lisp_char_code *src = (lisp_char_code *)  (ptr_from_lispobj(str + misc_data_offset));
  natural i, n = header_element_count(header_of(str));

  for (i=0; i < n; i++) {
    add_char((char)(*src++));
  }
}

void
add_c_string(char *s)
{
  add_string(s, strlen(s));
}

char numbuf[64], *digits = "0123456789ABCDEF";


void
sprint_unsigned_decimal_aux(natural n, Boolean first)
{
  if (n == 0) {
    if (first) {
      add_char('0');
    }
  } else {
    sprint_unsigned_decimal_aux(n/10, false);
    add_char(digits[n%10]);
  }
}

void
sprint_unsigned_decimal(natural n)
{
  sprint_unsigned_decimal_aux(n, true);
}

void
sprint_signed_decimal(signed_natural n)
{
  if (n < 0) {
    add_char('-');
    n = -n;
  }
  sprint_unsigned_decimal(n);
}


void
sprint_unsigned_hex(natural n)
{
  int i, 
    ndigits =
#if WORD_SIZE == 64
    16
#else
    8
#endif
    ;

  add_c_string("#x");
  for (i = 0; i < ndigits; i++) {
    add_char(digits[(n>>(4*(ndigits-(i+1))))&15]);
  }
}

void
sprint_list(LispObj o, int depth)
{
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
sprint_specializers_list(LispObj o, int depth)
{
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
vector_subtag_name(unsigned subtag)
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
    return "(SIGNED-BYTE 16)";
    break;
  case subtag_u32_vector:
    return "(UNSIGNED-BYTE 32)";
    break;
  case subtag_s32_vector:
    return "(SIGNED-BYTE 32)";
    break;
#ifdef X8664
  case subtag_u64_vector:
    return "(UNSIGNED-BYTE 64)";
    break;
  case subtag_s64_vector:
    return "(SIGNED-BYTE 64)";
    break;
#endif
  case subtag_package:
    return "PACKAGE";
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
sprint_random_vector(LispObj o, unsigned subtag, natural elements)
{
  add_c_string("#<");
  sprint_unsigned_decimal(elements);
  add_c_string("-element vector subtag = #x");
  add_char(digits[subtag>>4]);
  add_char(digits[subtag&15]);
  add_c_string(" @");
  sprint_unsigned_hex(o);
  add_c_string(" (");
  add_c_string(vector_subtag_name(subtag));
  add_c_string(")>");
}

void
sprint_symbol(LispObj o)
{
  lispsymbol *rawsym = (lispsymbol *) ptr_from_lispobj(untag(o));
  LispObj 
    pname = rawsym->pname,
    package = rawsym->package_predicate;

  if (fulltag_of(package) == fulltag_cons) {
    package = car(package);
  }

  if (package == nrs_KEYWORD_PACKAGE.vcell) {
    add_char(':');
  }
  add_lisp_base_string(pname);
}

#ifdef X8632
LispObj
nth_immediate(LispObj o, unsigned n)
{
  u16_t imm_word_count = *(u16_t *)(o + misc_data_offset);
  natural *constants = (natural *)((char *)o + misc_data_offset + (imm_word_count << 2));
  LispObj result = (LispObj)(constants[n-1]);

  return result;
}
#endif

void
sprint_function(LispObj o, int depth)
{
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
      }
      else {
        sprint_lisp_object(name,depth);
      }
      add_char(' ');
    } else if (lfbits & lfbits_gfn_mask) {
      add_c_string("Generic Function ");

#ifdef X8632
      {
	LispObj gf_slots = nth_immediate(o, 2);
	LispObj gf_name = deref(gf_slots, 2);

	sprint_lisp_object(gf_name, depth);
	add_char(' ');
      }
#endif
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
sprint_tra(LispObj o, int depth)
{
#ifdef X8664
  signed sdisp;
  unsigned disp = 0;
  LispObj f = 0;

  if ((*((unsigned short *)o) == RECOVER_FN_FROM_RIP_WORD0) &&
      (*((unsigned char *)(o+2)) == RECOVER_FN_FROM_RIP_BYTE2)) {
    sdisp = (*(int *) (o+3));
    f = RECOVER_FN_FROM_RIP_LENGTH+o+sdisp;
    disp = o-f;
  }

  if (fulltag_of(f) == fulltag_function) {
    add_c_string("tagged return address: ");
    sprint_function(f, depth);
    add_c_string(" + ");
    sprint_unsigned_decimal(disp);
  } else {
    add_c_string("(tra ?) : ");
    sprint_unsigned_hex(o);
  }
#else
  LispObj f = 0;
  unsigned disp = 0;

  if (*(unsigned char *)o == RECOVER_FN_OPCODE) {
    f = (LispObj)(*((natural *)(o + 1)));
    disp = o - f;
  }

  if (f && header_subtag(header_of(f)) == subtag_function) {
    add_c_string("tagged return address: ");
    sprint_function(f, depth);
    add_c_string(" + ");
    sprint_unsigned_decimal(disp);
  } else {
    add_c_string("(tra ?) : ");
    sprint_unsigned_hex(o);
  }
#endif
}
	       
void
sprint_gvector(LispObj o, int depth)
{
  LispObj header = header_of(o);
  unsigned 
    elements = header_element_count(header),
    subtag = header_subtag(header);
    
  switch(subtag) {
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

  case subtag_instance:
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
sprint_ivector(LispObj o)
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
sprint_vector(LispObj o, int depth)
{
  LispObj header = header_of(o);
  
  if (immheader_tag_p(fulltag_of(header))) {
    sprint_ivector(o);
  } else {
    sprint_gvector(o, depth);
  }
}

void
sprint_lisp_object(LispObj o, int depth) 
{
  if (--depth < 0) {
    add_char('#');
  } else {
    switch (fulltag_of(o)) {
    case fulltag_even_fixnum:
    case fulltag_odd_fixnum:
      sprint_signed_decimal(unbox_fixnum(o));
      break;
    
#ifdef X8664
    case fulltag_immheader_0:
    case fulltag_immheader_1:
    case fulltag_immheader_2:
    case fulltag_nodeheader_0:
    case fulltag_nodeheader_1:
#else
    case fulltag_immheader:
    case fulltag_nodeheader:
#endif      
      add_c_string("#<header ? ");
      sprint_unsigned_hex(o);
      add_c_string(">");
      break;

#ifdef X8664
    case fulltag_imm_0:
    case fulltag_imm_1:
#else
    case fulltag_imm:
#endif
      if (o == unbound) {
        add_c_string("#<Unbound>");
      } else {
        if (header_subtag(o) == subtag_character) {
          unsigned c = (o >> charcode_shift);
          add_c_string("#\\");
          if ((c >= ' ') && (c < 0x7f)) {
            add_char(c);
          } else {
            sprintf(numbuf, "%#o", c);
            add_c_string(numbuf);
          }
#ifdef X8664
        } else if (header_subtag(o) == subtag_single_float) {
          LispObj xx = o;
          float f = ((float *)&xx)[1];
          sprintf(numbuf, "%f", f);
          add_c_string(numbuf);
#endif
        } else {

          add_c_string("#<imm ");
          sprint_unsigned_hex(o);
          add_c_string(">");
        }
      }
      break;

#ifdef X8664
    case fulltag_nil:
#endif
    case fulltag_cons:
      sprint_list(o, depth);
      break;
     
    case fulltag_misc:
      sprint_vector(o, depth);
      break;

#ifdef X8664
    case fulltag_symbol:
      sprint_symbol(o);
      break;

    case fulltag_function:
      sprint_function(o, depth);
      break;
#endif

#ifdef X8664
    case fulltag_tra_0:
    case fulltag_tra_1:
#else
    case fulltag_tra:
#endif
      sprint_tra(o,depth);
      break;
    }
  }
}

char *
print_lisp_object(LispObj o)
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
