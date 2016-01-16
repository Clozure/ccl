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
