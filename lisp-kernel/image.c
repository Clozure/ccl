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

#include "lisp.h"
#include "lisp_globals.h"
#include "area.h"
#include "image.h"
#include "gc.h"
#include <errno.h>
#include <unistd.h>
#ifndef WINDOWS
#include <sys/mman.h>
#endif
#include <stdio.h>
#include <limits.h>
#include <time.h>


#if defined(PPC64) || defined(X8632)
#define RELOCATABLE_FULLTAG_MASK \
  ((1<<fulltag_cons)|(1<<fulltag_misc))
#else
#ifdef X8664
#define RELOCATABLE_FULLTAG_MASK \
  ((1<<fulltag_cons)|(1<<fulltag_misc)|(1<<fulltag_symbol)|(1<<fulltag_function))
#else
#define RELOCATABLE_FULLTAG_MASK \
  ((1<<fulltag_cons)|(1<<fulltag_nil)|(1<<fulltag_misc))
#endif
#endif

void
relocate_area_contents(area *a, LispObj bias)
{
  LispObj 
    *start = (LispObj *)(a->low), 
    *end = (LispObj *)(a->active),
    low = (LispObj)image_base - bias,
    high = ptr_to_lispobj(active_dynamic_area->active) - bias,
    w0, w1;
  int fulltag;
  Boolean fixnum_after_header_is_link = false;

  while (start < end) {
    w0 = *start;
    fulltag = fulltag_of(w0);
    if (immheader_tag_p(fulltag)) {
      start = (LispObj *)skip_over_ivector((natural)start, w0);
    } else {
#ifdef X86
      if (header_subtag(w0) == subtag_function) {
#ifdef X8664
        int skip = ((int) start[1])+1;
#else
        extern void update_self_references(LispObj *);
        extern natural imm_word_count(LispObj);

        natural skip = (natural)imm_word_count(((LispObj)start)+fulltag_misc)+1;
        update_self_references(start);
#endif
     
        start += skip;
        if (((LispObj) start) & node_size) {
          --start;
        }
        w0 = *start;
        fulltag = fulltag_of(w0);
      }
#endif
#ifdef ARM
      if (header_subtag(w0) == subtag_function) {
        w1 = start[1];
        if ((w1 >= low) && (w1 < high)) {
          start[1]=(w1+bias);
        }
        start+=2;
        w0 = *start;
        fulltag = fulltag_of(w0);
      }
#endif
      if (header_subtag(w0) == subtag_weak) {
        fixnum_after_header_is_link = true;
      }
      if (header_subtag(w0) == subtag_hash_vector) {
        hash_table_vector_header *hashp = (hash_table_vector_header *)start;
        
        if (hashp->flags & nhash_track_keys_mask) {
          hashp->flags |= nhash_key_moved_mask;
        }
        fixnum_after_header_is_link = true;
      }

      if ((w0 >= low) && (w0 < high) &&
	  ((1<<fulltag) & RELOCATABLE_FULLTAG_MASK)) {
	*start = (w0+bias);
      }
      w1 = *++start;
      fulltag = fulltag_of(w1);
      if ((w1 >= low) && (w1 < high) &&
	  (fixnum_after_header_is_link ||
           ((1<<fulltag) & RELOCATABLE_FULLTAG_MASK))) {
	*start = (w1+bias);
      }
      fixnum_after_header_is_link = false;
      ++start;
    }
  }
  if (start > end) {
    Bug(NULL, "Overran area bounds in relocate_area_contents");
  }
}
      



off_t
seek_to_next_page(int fd)
{
  off_t pos = LSEEK(fd, 0, SEEK_CUR);
  pos = align_to_power_of_2(pos, log2_page_size);
  return LSEEK(fd, pos, SEEK_SET);
}
  
/*
  fd is positioned to EOF; header has been allocated by caller.
  If we find a trailer (and that leads us to the header), read
  the header & return true else return false.
*/
Boolean
find_openmcl_image_file_header(int fd, openmcl_image_file_header *header)
{
  openmcl_image_file_trailer trailer;
  int disp;
  off_t pos;
  unsigned version, flags;

  pos = LSEEK(fd, 0, SEEK_END);
  if (pos < 0) {
    return false;
  }
  pos -= sizeof(trailer);

  if (LSEEK(fd, pos, SEEK_SET) < 0) {
    return false;
  }
  if (read(fd, &trailer, sizeof(trailer)) != sizeof(trailer)) {
    return false;
  }
  if ((trailer.sig0 != IMAGE_SIG0) ||
      (trailer.sig1 != IMAGE_SIG1) ||
      (trailer.sig2 != IMAGE_SIG2)) {
    return false;
  }
  disp = trailer.delta;
  
  if (disp >= 0) {
    return false;
  }
  if (LSEEK(fd, disp, SEEK_CUR) < 0) {
    return false;
  }
  if (read(fd, header, sizeof(openmcl_image_file_header)) !=
      sizeof(openmcl_image_file_header)) {
    return false;
  }
  if ((header->sig0 != IMAGE_SIG0) ||
      (header->sig1 != IMAGE_SIG1) ||
      (header->sig2 != IMAGE_SIG2) ||
      (header->sig3 != IMAGE_SIG3)) {
    return false;
  }
  version = (header->abi_version) & 0xffff;
  if (version < ABI_VERSION_MIN) {
    fprintf(dbgout, "Heap image is too old for this kernel.\n");
    return false;
  }
  if (version > ABI_VERSION_MAX) {
    fprintf(dbgout, "Heap image is too new for this kernel.\n");
    return false;
  }
  flags = header->flags;
  if (flags != PLATFORM) {
    fprintf(dbgout, "Heap image was saved for another platform.\n");
    return false;
  }
  return true;
}

void
load_image_section(int fd, openmcl_image_section_header *sect)
{
  extern area* allocate_dynamic_area(unsigned);
  off_t
    pos = seek_to_next_page(fd), advance;
  natural
    mem_size = sect->memory_size;
  char *addr;
  area *a;

  advance = mem_size;
  switch(sect->code) {
  case AREA_READONLY:
    if (!MapFile(pure_space_active,
		 pos,
		 align_to_power_of_2(mem_size,log2_page_size),
		 MEMPROTECT_RX,
		 fd)) {
      return;
    }
    a = new_area(pure_space_active, pure_space_limit, AREA_READONLY);
    pure_space_active += mem_size;
    a->active = pure_space_active;
    sect->area = a;      
    break;

  case AREA_STATIC:
    if (!MapFile(static_space_active,
		 pos,
		 align_to_power_of_2(mem_size,log2_page_size),
		 MEMPROTECT_RWX,
		 fd)) {
      return;
    }
    a = new_area(static_space_active, static_space_limit, AREA_STATIC);
    static_space_active += mem_size;
    a->active = static_space_active;
    sect->area = a;
    break;

  case AREA_DYNAMIC:
    a = allocate_dynamic_area(mem_size);
    if (!MapFile(a->low,
		 pos,
		 align_to_power_of_2(mem_size,log2_page_size),
		 MEMPROTECT_RWX,
		 fd)) {
      return;
    }

    a->static_dnodes = sect->static_dnodes;
    sect->area = a;
    break;

  case AREA_MANAGED_STATIC:
    a = new_area(pure_space_limit, pure_space_limit+align_to_power_of_2(mem_size,log2_page_size), AREA_MANAGED_STATIC);
    a->active = a->low+mem_size;
    if (mem_size) {
      natural
        refbits_size = align_to_power_of_2((((mem_size>>dnode_shift)+7)>>3),
                                           log2_page_size);
      if (!MapFile(a->low,
                   pos,
                   align_to_power_of_2(mem_size,log2_page_size),
                   MEMPROTECT_RWX,
                   fd)) {
        return;
      }
      /* Need to save/restore persistent refbits. */
      if (!MapFile(global_mark_ref_bits,
                   align_to_power_of_2(pos+mem_size,log2_page_size),
                   refbits_size,
                   MEMPROTECT_RW,
                   fd)) {
        return;
      }
      advance += refbits_size;
    }
    sect->area = a;
    a->ndnodes = area_dnode(a->active, a->low);
    managed_static_area = a;
    lisp_global(REF_BASE) = (LispObj) a->low;
    break;

    /* In many respects, the static_cons_area is part of the dynamic
       area; it's physically adjacent to it (immediately precedes the
       dynamic area in memory) and its contents are subject to full
       GC (but not compaction.)  It's maintained as a seperate section
       in the image file, at least for now. */


  case AREA_STATIC_CONS:
    addr = (char *) lisp_global(HEAP_START);
    a = new_area(addr-align_to_power_of_2(mem_size,log2_page_size), addr, AREA_STATIC_CONS);
    if (mem_size) {      
      if (!MapFile(a->low,
                   pos,
                   align_to_power_of_2(mem_size,log2_page_size),
                   MEMPROTECT_RWX,
                   fd)) {
        return;
      }
    }
    a->ndnodes = area_dnode(a->active, a->low);
    sect->area = a;
    static_cons_area = a;
    break;

  default:
    return;
    
  }
  LSEEK(fd, pos+advance, SEEK_SET);
}


LispObj
load_openmcl_image(int fd, openmcl_image_file_header *h)
{
  LispObj image_nil = 0;
  area *a;
  if (find_openmcl_image_file_header(fd, h)) {
    int i, nsections = h->nsections;
    openmcl_image_section_header sections[nsections], *sect=sections;
    LispObj bias = image_base - ACTUAL_IMAGE_BASE(h);
#if (WORD_SIZE== 64)
    signed_natural section_data_delta = 
      ((signed_natural)(h->section_data_offset_high) << 32L) | h->section_data_offset_low;
#endif

    if (read (fd, sections, nsections*sizeof(openmcl_image_section_header)) !=
	nsections * sizeof(openmcl_image_section_header)) {
      return 0;
    }
#if WORD_SIZE == 64
    LSEEK(fd, section_data_delta, SEEK_CUR);
#endif
    for (i = 0; i < nsections; i++, sect++) {
      load_image_section(fd, sect);
      a = sect->area;
      if (a == NULL) {
	return 0;
      }
    }

    for (i = 0, sect = sections; i < nsections; i++, sect++) {
      a = sect->area;
      switch(sect->code) {
      case AREA_STATIC:
	nilreg_area = a;
#ifdef PPC
#ifdef PPC64
        image_nil = ptr_to_lispobj(a->low + (1024*4) + sizeof(lispsymbol) + fulltag_misc);
#else
	image_nil = (LispObj)(a->low + 8 + 8 + (1024*4) + fulltag_nil);
#endif
#endif
#ifdef X86
#ifdef X8664
	image_nil = (LispObj)(a->low) + (1024*4) + fulltag_nil;
#else
	image_nil = (LispObj)(a->low) + (1024*4) + fulltag_cons;
#endif
#endif
#ifdef ARM
	image_nil = (LispObj)(a->low) + (1024*4) + fulltag_nil;
#endif
	set_nil(image_nil);
	if (bias) {
          LispObj weakvll = lisp_global(WEAKVLL);

          if ((weakvll >= ((LispObj)image_base-bias)) &&
              (weakvll < (ptr_to_lispobj(active_dynamic_area->active)-bias))) {
            lisp_global(WEAKVLL) = weakvll+bias;
          }
	  relocate_area_contents(a, bias);
	}
	make_dynamic_heap_executable(a->low, a->active);
        add_area_holding_area_lock(a);
        break;
        
      case AREA_READONLY:
        if (bias && 
            (managed_static_area->active != managed_static_area->low)) {
          UnProtectMemory(a->low, a->active-a->low);
          relocate_area_contents(a, bias);
          ProtectMemory(a->low, a->active-a->low);
        }
        readonly_area = a;
	add_area_holding_area_lock(a);
	break;
      }
    }
    for (i = 0, sect = sections; i < nsections; i++, sect++) {
      a = sect->area;
      switch(sect->code) {
      case AREA_MANAGED_STATIC:
        if (bias) {
          relocate_area_contents(a, bias);
        }
        add_area_holding_area_lock(a);
        break;
      case AREA_STATIC_CONS:
        break;
      case AREA_DYNAMIC:
        lower_heap_start(static_cons_area->low,a);
        if (bias) {
          relocate_area_contents(a, bias);
        }
	resize_dynamic_heap(a->active, lisp_heap_gc_threshold);
	xMakeDataExecutable(a->low, a->active - a->low);
	break;
      }
    }
  }
  return image_nil;
}
 
void
prepare_to_write_dynamic_space(area *a)
{
  LispObj 
    *start = (LispObj *)(a->low),
    *end = (LispObj *) (a->active),
    x1;
  int tag, subtag, element_count;

  while (start < end) {
    x1 = *start;
    tag = fulltag_of(x1);
    if (immheader_tag_p(tag)) {
      subtag = header_subtag(x1);
      if (subtag == subtag_macptr) {
        if ((start[1] >= (natural)0x10000) && (start[1] < (natural)-0x10000)) {
          /* Leave small pointers alone */
          *start = make_header(subtag_dead_macptr,header_element_count(x1));
        }
      }
      start = (LispObj *)skip_over_ivector((natural)start, x1);
    } else if (nodeheader_tag_p(tag)) {
      element_count = header_element_count(x1) | 1;
      start += (element_count+1);
    } else {
      start += 2;
    }
  }
}

  

int
write_file_and_section_headers(int fd, 
                               openmcl_image_file_header *file_header,
                               openmcl_image_section_header* section_headers,
                               int nsections,
                               off_t *header_pos)
{
  *header_pos = seek_to_next_page(fd);

  if (LSEEK (fd, *header_pos, SEEK_SET) < 0) {
    return errno;
  }
  if (write(fd, file_header, sizeof(*file_header)) != sizeof(*file_header)) {
    return errno;
  }
  if (write(fd, section_headers, sizeof(section_headers[0])*nsections)
      != (sizeof(section_headers[0])*nsections)) {
    return errno;
  }
  return 0;
}
  
natural
writebuf(int fd, char *bytes, natural n)
{
  natural remain = n, this_size;
  signed_natural result;

  while (remain) {
    this_size = remain;
    if (this_size > INT_MAX) {
      this_size = INT_MAX;
    }
    result = write(fd, bytes, this_size);
    if (result < 0) {
      return errno;
    }
    bytes += result;

    remain -= result;
  }
  return 0;
}

OSErr
save_application(unsigned fd, Boolean egc_was_enabled)
{
  openmcl_image_file_header fh;
  openmcl_image_section_header sections[NUM_IMAGE_SECTIONS];
  openmcl_image_file_trailer trailer;
  area *areas[NUM_IMAGE_SECTIONS], *a;
  int i, err;
  off_t header_pos, eof_pos;
#if WORD_SIZE == 64
  off_t image_data_pos;
  signed_natural section_data_delta;
#endif

  /*
    Coerce macptrs to dead_macptrs.
  */
  
  prepare_to_write_dynamic_space(active_dynamic_area);
  prepare_to_write_dynamic_space(managed_static_area);

  /* 
     If we ever support continuing after saving an image,
     undo this .. */

  if (static_cons_area->high > static_cons_area->low) {
    active_dynamic_area->low = static_cons_area->high;
    tenured_area->static_dnodes -= area_dnode(static_cons_area->high, static_cons_area->low);
  }

  areas[0] = nilreg_area; 
  areas[1] = readonly_area;
  areas[2] = active_dynamic_area;
  areas[3] = managed_static_area;
  areas[4] = static_cons_area;
  for (i = 0; i < NUM_IMAGE_SECTIONS; i++) {
    a = areas[i];
    sections[i].code = a->code;
    sections[i].area = NULL;
    sections[i].memory_size  = a->active - a->low;
    if (a == active_dynamic_area) {
      sections[i].static_dnodes = tenured_area->static_dnodes;
    } else {
      sections[i].static_dnodes = 0;
    }
  }
  fh.sig0 = IMAGE_SIG0;
  fh.sig1 = IMAGE_SIG1;
  fh.sig2 = IMAGE_SIG2;
  fh.sig3 = IMAGE_SIG3;
  fh.timestamp = time(NULL);
  CANONICAL_IMAGE_BASE(&fh) = IMAGE_BASE_ADDRESS;
  ACTUAL_IMAGE_BASE(&fh) = image_base;
  fh.nsections = NUM_IMAGE_SECTIONS;
  fh.abi_version=ABI_VERSION_CURRENT;
#if WORD_SIZE == 64
  fh.section_data_offset_high = 0;
  fh.section_data_offset_low = 0;
#else
  fh.pad0[0] = fh.pad0[1] = 0;
  fh.pad1[0] = fh.pad1[1] = fh.pad1[2] = fh.pad1[3] = 0;
#endif
  fh.flags = PLATFORM;

#if WORD_SIZE == 64
  image_data_pos = seek_to_next_page(fd);
#else
  err = write_file_and_section_headers(fd, &fh, sections, NUM_IMAGE_SECTIONS, &header_pos);
  if (err) {
    return err;
  }
#endif


  {
    area *g0_area = g1_area->younger;

    /* Save GC config */
    lisp_global(LISP_HEAP_THRESHOLD) = lisp_heap_gc_threshold;
    lisp_global(G0_THRESHOLD) = g0_area->threshold;
    lisp_global(G1_THRESHOLD) = g1_area->threshold;
    lisp_global(G2_THRESHOLD) = g2_area->threshold;
    lisp_global(EGC_ENABLED) = (LispObj)egc_was_enabled;
    lisp_global(GC_NOTIFY_THRESHOLD) = lisp_heap_notify_threshold;
  }
  /*
    lisp_global(GC_NUM) and lisp_global(FWDNUM) are persistent,
    as is DELETED_STATIC_PAIRS.
    Nothing else is even meaningful at this point.
    Except for those things that've become meaningful since that
    comment was written.
  */
  for (i = MIN_KERNEL_GLOBAL; i < 0; i++) {
    switch (i) {
    case FREE_STATIC_CONSES:
    case FWDNUM:
    case GC_NUM:
    case STATIC_CONSES:
    case WEAK_GC_METHOD:
    case LISP_HEAP_THRESHOLD:
    case EGC_ENABLED:
    case G0_THRESHOLD:
    case G1_THRESHOLD:
    case G2_THRESHOLD:
    case GC_NOTIFY_THRESHOLD:
      break;
    case WEAKVLL:
      break;
    default:
      lisp_global(i) = 0;
    }
  }

  for (i = 0; i < NUM_IMAGE_SECTIONS; i++) {
    natural n;
    a = areas[i];
    seek_to_next_page(fd);
    n = sections[i].memory_size;
    if (writebuf(fd, a->low, n)) {
	return errno;
    }
    if (n &&  ((sections[i].code) == AREA_MANAGED_STATIC)) {
      natural ndnodes = area_dnode(a->active, a->low);
      natural nrefbytes = align_to_power_of_2((ndnodes+7)>>3,log2_page_size);

      seek_to_next_page(fd);
      if (writebuf(fd,(char*)a->refbits,nrefbytes)) {
        return errno;
      }
    }
  }

#if WORD_SIZE == 64
  seek_to_next_page(fd);
  section_data_delta = -((LSEEK(fd,0,SEEK_CUR)+sizeof(fh)+sizeof(sections)) -
                         image_data_pos);
  fh.section_data_offset_high = (int)(section_data_delta>>32L);
  fh.section_data_offset_low = (unsigned)section_data_delta;
  err =  write_file_and_section_headers(fd, &fh, sections, NUM_IMAGE_SECTIONS, &header_pos);
  if (err) {
    return err;
  }  
#endif

  trailer.sig0 = IMAGE_SIG0;
  trailer.sig1 = IMAGE_SIG1;
  trailer.sig2 = IMAGE_SIG2;
  eof_pos = LSEEK(fd, 0, SEEK_CUR) + sizeof(trailer);
  trailer.delta = (int) (header_pos-eof_pos);
  if (write(fd, &trailer, sizeof(trailer)) == sizeof(trailer)) {
#ifndef WINDOWS
    fsync(fd);
#endif
    close(fd);
    return 0;
  } 
  i = errno;
  close(fd);
  return i;
}
      



