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



#ifndef __bits_h__
#define __bits_h__ 1

#include <string.h>

typedef natural *bitvector;

#if WORD_SIZE == 64
#define bitmap_shift 6
#define BIT0_MASK 0x8000000000000000ULL
#define ALL_ONES  0xffffffffffffffffULL
#else
#define bitmap_shift 5
#define BIT0_MASK 0x80000000U 
#define ALL_ONES  0xFFFFFFFFU
#endif

#define bitmap_shift_count_mask ((1<<bitmap_shift)-1)

static inline int
set_bit(bitvector bits,natural bitnum)  __attribute__((always_inline));

static inline int
set_bit(bitvector bits,natural bitnum)
{
  natural
    windex = bitnum>>bitmap_shift, 
    old = bits[windex],
    new = old | (BIT0_MASK >> (bitnum & bitmap_shift_count_mask));
  if (new == old) {
    return 1;			/* Was set */
  } else {
    bits[windex] = new;
    return 0;			/* Was clear */
  }
}

static inline int 
atomic_set_bit(bitvector bits ,natural bitnum)
{
  extern natural atomic_ior(bitvector, natural);
  natural
    windex = bitnum>>bitmap_shift,
    mask = (BIT0_MASK >> (bitnum & bitmap_shift_count_mask));

  return atomic_ior(bits + windex, mask);
}

void set_n_bits(bitvector,natural,natural);

static inline int
clr_bit(bitvector bits, natural bitnum)
{
  unsigned 
    windex = bitnum>>bitmap_shift, 
    old = bits[windex],
    new = old & ~(BIT0_MASK >> (bitnum & bitmap_shift_count_mask));
  if (new == old) {
    return 0;	/* Was clear */
  } else {
    bits[windex] = new;
    return 1;	/* Was set */
  }
}


static inline unsigned
ref_bit(bitvector bits,natural bitnum) __attribute__((always_inline));

static inline unsigned
ref_bit(bitvector bits,natural bitnum)
{
  return ((bits[bitnum>>bitmap_shift] & (BIT0_MASK >> (bitnum & bitmap_shift_count_mask))) != 0);
}

void zero_bits(bitvector, natural);
void ior_bits(bitvector,bitvector,natural);

#define bits_word_index(bitnum) (((natural)(bitnum)) >> bitmap_shift)
#define bits_bit_index(bitnum) (((natural)(bitnum)) & bitmap_shift_count_mask)
#define bits_word_ptr(bits,bitnum) \
  ((natural*) (((natural*) bits) + ((natural) (bits_word_index(bitnum)))))
#define bits_word_mask(bitnum) ((BIT0_MASK) >> bits_bit_index(bitnum))
#define bits_indexed_word(bitv,indexw) ((((natural*)(bitv))[indexw]))
#define bits_word(bitv,bitnum) bits_indexed_word(bits,bits_word_index(bitnum))

/* Evaluates some arguments twice */

#define set_bits_vars(BITVvar,BITNUMvar,BITPvar,BITWvar,MASKvar) \
{ BITPvar = bits_word_ptr(BITVvar,BITNUMvar); BITWvar = *BITPvar; MASKvar = bits_word_mask(BITNUMvar); }

#define set_bitidx_vars(BITVvar,BITNUMvar,BITPvar,BITWvar,BITIDXvar) \
{ BITPvar = bits_word_ptr(BITVvar,BITNUMvar); BITIDXvar = bits_bit_index(BITNUMvar); \
    BITWvar = (*BITPvar << BITIDXvar) >> BITIDXvar; }

#ifdef __GNUC__
static __inline__ natural
current_stack_pointer(void) __attribute__((always_inline));

static __inline__ natural
current_stack_pointer(void)
{
#ifdef PPC
  register natural _sp __asm__("r1");
#endif
#ifdef X8664
  register natural _sp __asm__("%rsp");
#endif
  return _sp;
}
#else
natural
current_stack_pointer(void);
#endif

#ifdef __GNUC__
static __inline__ unsigned
count_leading_zeros(natural w) __attribute__((always_inline));


/* Beware: on some platforms, __builtin_clz[ll](0) returns an undefined
   result */

static __inline__ unsigned
count_leading_zeros(natural w)
{
#if __GNUC__ >= 4
#if WORD_SIZE == 64
  return __builtin_clzll(w);  
#else
  return __builtin_clz(w);  
#endif
#else /* __GNUC__ < 4 */
  natural lz;
#ifdef PPC
#ifdef PPC64
  __asm__  ("cntlzd %0,%1" : "=r" (lz) : "r" (w));
#else
  __asm__  ("cntlzw %0,%1" : "=r" (lz) : "r" (w));
#endif
#endif /* PPC */
#ifdef X86
#ifdef X8664
  __asm__ ("bsr %1,%0" : "=r" (lz) : "r" (w));
  __asm__ ("xor $63,%0" : "=r" (lz));
#else
  __asm__ ("bsr %1,%0" : "=r" (lz) : "r" (w));
  __asm__ ("xor $31,%0" : "=r" (lz));
#endif 
#endif
  return lz;
#endif
}
#else /* not __GNUC__ */
unsigned
count_leading_zeros(natural);
#endif
                                        
#endif /* __bits_h__ */
