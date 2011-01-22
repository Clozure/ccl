/*
   Copyright (C) 2011 Clozure Associates
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

#include <mach-o/loader.h>
#include <mach-o/nlist.h>
#include <mach-o/stab.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include "lisp.h"

#if WORD_SIZE==64
typedef struct mach_header_64 macho_header;
#define MACHO_MAGIC MH_MAGIC_64
#define MACHO_LC_SEGMENT LC_SEGMENT_64
typedef struct segment_command_64 macho_segment_command;
typedef struct section_64 macho_section;
typedef struct nlist_64 macho_nlist;
#else
typedef struct mach_header_64 macho_header;
#define MACHO_MAGIC MH_MAGIC
#define MACHO_LC_SEGMENT LC_SEGMENT
typedef struct segment_command macho_segment_command;
typedef struct section macho_section;
typedef struct nlist macho_nlist;
#endif

typedef struct {
  natural used;
  natural allocated;
  char *data;
} macho_string_table;

macho_string_table *global_string_table, *initial_string_table;

typedef struct {
  natural used;
  natural allocated;
  macho_nlist *symbols;
} macho_symbol_table;

macho_symbol_table *all_symbols, *local_symbols, *defined_external_symbols, *undefined_external_symbols;

macho_section *
nth_section_in_segment(macho_segment_command *segment, int sectno)
{
  return (macho_section *)(((char *)segment)+sizeof(macho_segment_command)+(sizeof(macho_section) *sectno));
}

ssize_t
safe_read(int fd, char *buf, size_t nbytes)
{
  size_t total = 0;
  ssize_t n;
  
  while (total <  nbytes) {
    n = nbytes-total;
    if (n > INT_MAX) {
      n = INT_MAX;
    }
    n = read(fd, buf, n);
    if (n < 0) {
      perror("reading from image");
      exit(1);
    }
    if (n == 0) {
      fprintf(stderr, "unexpected end of file reading image\n");
      exit(1);
    }
    total += n;
    buf += n;
  }
  return total;
}

ssize_t
safe_write(int fd, char *buf, size_t nbytes)
{
  size_t total = 0;
  ssize_t n;
  while (total <  nbytes) {
    n = nbytes-total;
    if (n > INT_MAX) {
      n = INT_MAX;
    }
    n = write(fd, buf, n);
    if (n < 0) {
      perror("writing to image");
      exit(1);
    }
    total += n;
    buf += n;
  }
  return total;
}

natural
save_string(char *s, macho_string_table *t)
{
  natural 
    used = t->used, 
    allocated = t->allocated, 
    len = strlen(s),
    need = len+1;

  if ((used+need) > allocated) {
    allocated = t->allocated = allocated+need+1000;
    t->data = realloc(t->data,allocated);
  }
  memcpy(t->data+used,s,need);  /* copy trailing nul */
  t->used += need;
  return used;
}

macho_string_table *
create_string_table()
{
  natural allocated = 1000;
  macho_string_table *t = malloc(sizeof(macho_string_table));
  char *data = malloc(allocated);

  *data = 0;
  t->allocated = allocated;
  t->data = data;
  t->used = 1;
  return t;
}

macho_string_table *
read_string_table(int fd, off_t pos, uint32_t size)
{
  macho_string_table *t = malloc(sizeof(macho_string_table));
  natural allocated = size+1000;
  char *data = malloc(allocated);
  off_t curpos = lseek(fd,0,SEEK_CUR);

  lseek(fd, pos, SEEK_SET);
  safe_read(fd, data, size);
  t->used = size;
  t->allocated = allocated;
  t->data = data;
  lseek(fd, curpos, SEEK_SET);
  return t;
}
  

natural
add_symbol(macho_nlist *n, macho_symbol_table *t)
{
  natural used = t->used;
  macho_nlist *symbols;
  
  if (used == t->allocated) {
    t->allocated += 32;
    t->symbols = realloc(t->symbols,t->allocated*sizeof(macho_nlist));
  }
  symbols = t->symbols;
  symbols[used]=*n;
  t->used++;
  return used;
}

macho_symbol_table *
new_macho_symbol_table(natural capacity)
{
  macho_symbol_table * t = malloc(sizeof(macho_symbol_table));
  macho_nlist *syms = malloc(sizeof(macho_nlist)*capacity);
  t->symbols = syms;
  t->allocated = capacity;
  t->used = 0;
  return t;
}

macho_symbol_table *
read_symbols(int fd, off_t sympos, uint32_t startidx, uint32_t nsyms)
{
  off_t curpos = lseek(fd, 0, SEEK_CUR);
  macho_symbol_table *t = new_macho_symbol_table(nsyms+32);
  
  lseek(fd, sympos+(startidx*sizeof(macho_nlist)), SEEK_SET);
  safe_read(fd, (char *)(t->symbols), nsyms*sizeof(macho_nlist));
  t->used = nsyms;
  return t;
}

int
compare_macho_symbols(const void *a, const void *b)
{
  char *strings = global_string_table->data;

  return strcmp(strings+((macho_nlist *)a)->n_un.n_strx,
                strings+((macho_nlist *)b)->n_un.n_strx);
}

void
sort_macho_symbol_table(macho_symbol_table *t)
{
  qsort(t->symbols,t->used,sizeof(macho_nlist),compare_macho_symbols);
}

macho_symbol_table *
filter_macho_symbols(macho_symbol_table *in,
                     macho_string_table *in_strings,
                     macho_string_table **pout_strings,
                     int max_section)
{
  natural i, n = in->used;
  macho_symbol_table *out = new_macho_symbol_table(n);
  macho_string_table *out_strings = *pout_strings;
  macho_nlist *syms = in->symbols, outsym;
  char *inchars = in_strings->data;
  Boolean skip = false;
  
  for (i = 0; i < n; i++, syms++) {
    if (!skip) {
      if (((syms->n_type)==N_BNSYM) &&
          ((syms->n_sect)>max_section)) {
        skip = true;
      }
    }
    if (!skip) {
      outsym = *syms;
      outsym.n_un.n_strx = save_string(inchars+syms->n_un.n_strx,out_strings);
      add_symbol(&outsym,out);
    } else {
      if (syms->n_type == N_ENSYM) {
        skip = false;
      }
    }
  }
  return out;
}

void
add_lisp_function_stab(LispObj f, natural size_in_bytes, macho_string_table *strings, macho_symbol_table *syms, int section_ordinal)
{
  macho_nlist symbol;
  
  symbol.n_type = N_BNSYM;
  symbol.n_un.n_strx = 1;
  symbol.n_sect = section_ordinal;
  symbol.n_desc = 0;
  symbol.n_value = f;
  add_symbol(&symbol, syms);

  symbol.n_type = N_FUN;
  symbol.n_un.n_strx = save_string(print_lisp_object(f),strings);
  symbol.n_sect = section_ordinal;
  symbol.n_desc = 0;
  symbol.n_value = f;
  add_symbol(&symbol, syms);

  symbol.n_type = N_FUN;
  symbol.n_un.n_strx = 1;
  symbol.n_sect = NO_SECT;
  symbol.n_desc = 0;
  symbol.n_value = size_in_bytes;
  add_symbol(&symbol, syms);

  symbol.n_type = N_ENSYM;
  symbol.n_un.n_strx = 1;
  symbol.n_sect = section_ordinal;
  symbol.n_desc = 0;
  symbol.n_value = size_in_bytes;
  add_symbol(&symbol, syms);
}

#ifdef X86
void
add_lisp_function_stabs(macho_symbol_table *symbols, macho_string_table *strings, int section_ordinal)
{
  LispObj 
    *start = (LispObj *) readonly_area->low,
    *end = (LispObj *) readonly_area->active,
    header,
    f;
  int tag;
  natural size_in_bytes;

  while (start < end) {
    header = *start;
    tag = header_subtag(header);
    if (tag == subtag_function) {
#ifdef X8632
      f = ((LispObj)start)+fulltag_misc;
      size_in_bytes = (header_element_count(header)<<node_shift)-tag_misc;
#endif
#ifdef X8664
      f = ((LispObj)start)+fulltag_function;
      size_in_bytes = (header_element_count(header)<<node_shift)-tag_function;
#endif

      add_lisp_function_stab(f,size_in_bytes,strings,symbols,section_ordinal);
      start += ((header_element_count(header)+2)&~1);
    } else {
      start = (LispObj *)skip_over_ivector((LispObj)start,header);
    }
  }
}
#endif

typedef struct {
  char page[4096];
  int used;
  int load_command_offsets[16];
} macho_prefix;

macho_prefix *
init_macho_prefix(uint32_t magic, cpu_type_t cputype, cpu_subtype_t cpusubtype, uint32_t filetype, uint32_t flags) {
  macho_prefix *p = calloc(1,sizeof(macho_prefix));
  macho_header *h = (macho_header *) p;

  h->magic = magic;
  h->cputype = cputype;
  h->cpusubtype = cpusubtype;
  h->filetype = filetype;
  h->flags = flags;
  p->used = sizeof(macho_header);
  return p;
}

struct load_command *
add_load_command(macho_prefix *p, uint32_t cmd, uint32_t cmdsize)
{
  struct load_command *l = (struct load_command *)&(p->page[p->used]);
  macho_header *h = (macho_header *)p;

  cmdsize = align_to_power_of_2(cmdsize,node_shift);
  p->load_command_offsets[h->ncmds] = p->used;
  p->used += cmdsize;
  l->cmd = cmd;
  l->cmdsize += cmdsize;
  h->ncmds++;
  h->sizeofcmds += cmdsize;
  return l;
}

macho_segment_command *
add_segment(macho_prefix *p,char *segname, int nsections, ...) /* sectnames */
{
  macho_segment_command *seg = (macho_segment_command *) add_load_command(p, MACHO_LC_SEGMENT, sizeof(macho_segment_command)+(nsections * sizeof(macho_section)));
  macho_section *sect = nth_section_in_segment(seg, 0);
  va_list sectnames;
  char *sectname;
  
  seg->nsects = nsections;
  strncpy(seg->segname,segname,sizeof(seg->segname));
  va_start(sectnames,nsections);
  while(nsections--) {
    sectname = va_arg(sectnames,char *);
    strncpy(sect->sectname,sectname,sizeof(sect->sectname));
    strncpy(sect->segname,segname,sizeof(sect->segname));
    sect++;
  }
  return seg;
}

    
void
save_native_library(int fd)
{
  macho_prefix *p = init_macho_prefix(MACHO_MAGIC,
#ifdef X8632
                                      CPU_TYPE_I386,
                                      CPU_SUBTYPE_X86_ALL,
#endif
#ifdef X8664
                                      CPU_TYPE_X86_64,
                                      CPU_SUBTYPE_X86_64_ALL,
#endif
#ifdef PPC32
                                      CPU_TYPE_POWERPC,
                                      CPU_SUBTYPE_POWERPC_ALL,
#endif
#ifdef PPC64
                                      CPU_TYPE_POWERPC64,
                                      CPU_TYPE_POWERPC_ALL,
#endif
#ifdef ARM
                                      CPU_TYPE_ARM,
                                      CPU_SUBTYPE_ARM_ALL,
#endif
                                      MH_DYLIB,
                                      MH_NOUNDEFS);
  macho_segment_command *seg;
  macho_section *sect;
  off_t curpos = 4096;


  seg = add_segment(p, "innocent", 1, "bystander");
  seg->vmaddr = (natural)(readonly_area->low-4096);
  seg->vmsize = 4096;
  seg->fileoff = 0;
  seg->filesize = 4096;
  seg->maxprot = VM_PROT_READ|VM_PROT_WRITE|VM_PROT_EXECUTE;
  seg->initprot = VM_PROT_READ|VM_PROT_EXECUTE;
  sect = nth_section_in_segment(seg,0);
  sect->addr = (natural)(readonly_area->low-1);
  sect->size = 1;
  sect->offset = 4095;
  sect->flags = S_ATTR_SOME_INSTRUCTIONS | S_ATTR_PURE_INSTRUCTIONS;
  p->page[4095] = 0xcc;

  seg = add_segment(p,"READONLY",1,"readonly");
  seg->vmaddr = (natural)(readonly_area->low);
  seg->vmsize = align_to_power_of_2(readonly_area->active-readonly_area->low,12);
  seg->fileoff = curpos;
  seg->filesize = seg->vmsize;
  seg->maxprot = VM_PROT_READ|VM_PROT_WRITE|VM_PROT_EXECUTE;
  seg->initprot = VM_PROT_READ|VM_PROT_EXECUTE;
  sect = nth_section_in_segment(seg,0);
  sect->addr = (natural)(readonly_area->low);
  sect->size = readonly_area->active-readonly_area->low;
  sect->offset = seg->fileoff;
  sect->flags = S_ATTR_SOME_INSTRUCTIONS;
  lseek(fd,curpos,SEEK_SET);
  safe_write(fd,readonly_area->low,seg->filesize);



}
