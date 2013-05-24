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
#include "bits.h"
#include "lisp-exceptions.h"


/* This should be a lot faster than calling set_bit N times */

void
set_n_bits(bitvector bits, natural first, natural n)
{
  if (n) {
    natural
      lastbit = (first+n)-1,
      leftbit = first & bitmap_shift_count_mask,
      leftmask = ALL_ONES >> leftbit,
      rightmask = ALL_ONES << ((nbits_in_word-1) - (lastbit & bitmap_shift_count_mask)),
      *wstart = ((natural *) bits) + (first>>bitmap_shift),
      *wend = ((natural *) bits) + (lastbit>>bitmap_shift);

    if (wstart == wend) {
      *wstart |= (leftmask & rightmask);
    } else {
      *wstart++ |= leftmask;
      n -= (nbits_in_word - leftbit);
      
      while (n >= nbits_in_word) {
        *wstart++ = ALL_ONES;
        n-= nbits_in_word;
      }
      
      if (n) {
        *wstart |= rightmask;
      }
    }
  }
}

/* Note that this zeros natural-sized words */
void
zero_bits(bitvector bits, natural nbits)
{
  natural i, n = (((nbits+(nbits_in_word-1)))>>bitmap_shift);
  
  for(i=0; i < n; i++) {
    bits[i]= 0;
  }
}

void
ior_bits(bitvector dest, bitvector src, natural nbits)
{
  while (nbits > 0) {
    *dest++ |= *src++;
    nbits -= nbits_in_word;
  }
}
