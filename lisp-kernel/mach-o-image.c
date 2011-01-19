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
typedef struct segment_command_64 macho_segment_command;
typedef struct section_64 macho_section;
typedef struct nlist_64 macho_nlist;
#else
typedef struct mach_header_64 macho_header;
#define MACHO_MAGIC MH_MAGIC
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

