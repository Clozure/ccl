/*
   Copyright (C) 2002-2009 Clozure Associates
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


#define IMAGE_SIG0 (('O'<<24) | ('p'<<16) | ('e'<<8) | 'n')
#define IMAGE_SIG1 (('M'<<24) | ('C'<<16) | ('L'<<8) | 'I')
#define IMAGE_SIG2 (('m'<<24) | ('a'<<16) | ('g'<<8) | 'e')
#define IMAGE_SIG3 (('F'<<24) | ('i'<<16) | ('l'<<8) | 'e')

/* 
   An image file contains a header (which describes the type, size,
   and nominal memory address of one or more sections) and data for
   each section; each section's data is page-aligned within the image
   file, so its disk address is implicit.  The header must reside
   entirely within a page; the first section's data starts on the page
   after the image header, and subsequent sections start on the pages
   after the page which contains the last byte of their predecessor's
   data.

   The image header's position relative to the start of the file is
   arbitrary.  The image header's position relative to the end of the
   file is indicated by the last word in the file (which is preceded
   by the first three signature words above.)  The last word contains
   the distance from the end-of-file to the start of the header.

   As long as these alignment constraints are met, the image file can
   have arbitrary data (or executable programs, or shell scripts)
   prepended to it.  This is supposed to simplify distribution.
*/

typedef struct {
  natural code;
  area *area;
  natural memory_size;
  natural static_dnodes;
} openmcl_image_section_header;

typedef struct {
  unsigned sig0, sig1, sig2, sig3;
  unsigned timestamp;
  unsigned canonical_image_base_32; /* IMAGE_BASE_ADDRESS */
  unsigned actual_image_base_32;	/* Hopefully the same */
  unsigned nsections;
  unsigned abi_version;
#if WORD_SIZE == 64
  int section_data_offset_high; /* signed offset from end of
                                         section headers to first
                                         section's data.  May be zero. */
  unsigned section_data_offset_low;
  unsigned flags; 
  natural canonical_image_base_64;
  natural actual_image_base_64;
#else 
  unsigned pad0[2]; 
  unsigned flags;
  unsigned pad1[4];
#endif
} openmcl_image_file_header;

#if WORD_SIZE == 64
#define ACTUAL_IMAGE_BASE(header) ((header)->actual_image_base_64)
#define CANONICAL_IMAGE_BASE(header) ((header)->canonical_image_base_64)
#else
#define ACTUAL_IMAGE_BASE(header) ((header)->actual_image_base_32)
#define CANONICAL_IMAGE_BASE(header) ((header)->canonical_image_base_32)
#endif

typedef struct {
  unsigned sig0, sig1, sig2;
  int delta;
} openmcl_image_file_trailer;

LispObj
load_openmcl_image(int, openmcl_image_file_header*);

Boolean find_openmcl_image_file_header(int fd, openmcl_image_file_header *h);



#define ABI_VERSION_MIN 1036
#define ABI_VERSION_CURRENT 1037
#define ABI_VERSION_MAX 1037

#define NUM_IMAGE_SECTIONS 5    /* used to be 3 */
