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
#include <unistd.h>
#include <sys/fcntl.h>
#include <sys/mman.h>
#include <dlfcn.h>
#include "lisp.h"
#include "gc.h"
#include "lisp_globals.h"

#if WORD_SIZE==64
typedef struct mach_header_64 macho_header;
#define MACHO_MAGIC MH_MAGIC_64
#define MACHO_LC_SEGMENT LC_SEGMENT_64
typedef struct segment_command_64 macho_segment_command;
typedef struct section_64 macho_section;
typedef struct nlist_64 macho_nlist;
typedef struct dylib_module_64 macho_module;
#else
typedef struct mach_header macho_header;
#define MACHO_MAGIC MH_MAGIC
#define MACHO_LC_SEGMENT LC_SEGMENT
typedef struct segment_command macho_segment_command;
typedef struct section macho_section;
typedef struct nlist macho_nlist;
typedef struct dylib_module macho_module;
#endif

void map_initial_reloctab(BytePtr, BytePtr);
void map_initial_markbits(BytePtr, BytePtr);

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
      _exit(1);
    }
    if (n == 0) {
      fprintf(stderr, "unexpected end of file reading image\n");
      _exit(1);
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
      _exit(1);
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

  data[0] = ' ';
  data[1] = 0;
  t->allocated = allocated;
  t->data = data;
  t->used = 2;
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
sort_macho_symbol_table(macho_symbol_table *t, int first, int n)
{
  qsort(t->symbols+first,n,sizeof(macho_nlist),compare_macho_symbols);
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
  natural strx = save_string(print_lisp_object(f),strings);
  
  symbol.n_type = N_BNSYM;
  symbol.n_un.n_strx = 1;
  symbol.n_sect = section_ordinal;
  symbol.n_desc = 0;
  symbol.n_value = f;
  add_symbol(&symbol, syms);

  symbol.n_type = N_FUN;
  symbol.n_un.n_strx = strx;
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

  symbol.n_type = N_SECT;
  symbol.n_un.n_strx = strx;
  symbol.n_sect = section_ordinal;
  symbol.n_desc = 0;
  symbol.n_value = f;
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
  natural size_in_bytes, code_words;
  macho_nlist symbol;

  symbol.n_type = N_SO;
  symbol.n_un.n_strx = save_string("/pretend/", strings);
  symbol.n_sect = NO_SECT;
  symbol.n_desc = 0;
  symbol.n_value = 0;
  add_symbol(&symbol, symbols);
  
  symbol.n_type = N_SO;
  symbol.n_un.n_strx = save_string("pretend.lisp", strings);
  symbol.n_sect = NO_SECT;
  symbol.n_desc = 0;
  symbol.n_value = 0;
  add_symbol(&symbol, symbols);

  while (start < end) {
    header = *start;
    tag = header_subtag(header);
    if (tag == subtag_function) {
#ifdef X8632
      f = ((LispObj)start)+fulltag_misc;
      code_words = (unsigned short)deref(f,1);
      if (code_words & 0x8000) {
        code_words = header_element_count(header) - (code_words & 0x7fff);
      }
      size_in_bytes = (code_words<<node_shift)-tag_misc;
#endif
#ifdef X8664
      f = ((LispObj)start)+fulltag_function;
      code_words = (int)deref(f,1);
      size_in_bytes = (code_words<<node_shift)-tag_function;
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
add_macho_segment(macho_prefix *p,
                  char *segname,
                  natural vmaddr,
                  natural vmsize,
                  natural fileoff,
                  natural filesize,
                  vm_prot_t maxprot,
                  vm_prot_t initprot,
                  int nsections, ...) /* sectnames */
{
  macho_segment_command *seg = (macho_segment_command *) add_load_command(p, MACHO_LC_SEGMENT, sizeof(macho_segment_command)+(nsections * sizeof(macho_section)));
  macho_section *sect = nth_section_in_segment(seg, 0);
  va_list sectnames;
  char *sectname;

  seg->vmaddr = vmaddr;
  seg->vmsize = vmsize;
  seg->fileoff = fileoff;
  seg->filesize = filesize;
  seg->maxprot = maxprot;
  seg->initprot = initprot;  
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


macho_section *
init_macho_section(macho_segment_command *seg,
                   int sectno,
                   natural addr,
                   natural size,
                   natural offset,
                   uint32_t flags)
{
  macho_section *sect = nth_section_in_segment(seg,sectno);
  sect->addr = addr;
  sect->size = size;
  sect->offset = offset;
  sect->flags = flags;
  return sect;
}
             
void
save_native_library(int fd, Boolean egc_was_enabled)
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
  struct dylib_command *dylib;
  struct symtab_command *symtab;
  struct dysymtab_command *dysymtab;
  char *dylib_name = "CCL Heap Image.dylib";
  macho_nlist symbol;
  macho_module m;
  struct dylib_table_of_contents *toc;
  struct dylib_reference *refs;
  int 
    readonly_section_ordinal = 0,
    managed_static_section_ordinal = 0,
    managed_static_refbits_section_ordinal = 0,
    dynamic_section_ordinal = 0,
    static_section_ordinal = 0,
    next_section_ordinal = 0;
  natural nrefbytes, first_external_symbol, num_external_symbols, i, j;
    
  all_symbols = new_macho_symbol_table(100000);
  global_string_table = create_string_table();

  seg = add_macho_segment(p, 
                          "__TEXT",
                          (natural)(readonly_area->low-4096),
                          4096+align_to_power_of_2(readonly_area->active-readonly_area->low,12),
                          0,
                          4096+align_to_power_of_2(readonly_area->active-readonly_area->low,12),
                          VM_PROT_READ|VM_PROT_WRITE|VM_PROT_EXECUTE,
                          VM_PROT_READ|VM_PROT_EXECUTE,
                          1, 
                          "text");
  init_macho_section(seg,
                     0,
                     (natural)(readonly_area->low),
                     readonly_area->active-readonly_area->low,
                     curpos,
                     S_ATTR_SOME_INSTRUCTIONS);
  readonly_section_ordinal = ++next_section_ordinal;
  add_lisp_function_stabs(all_symbols,global_string_table,readonly_section_ordinal);
  lseek(fd,curpos,SEEK_SET);
  safe_write(fd,readonly_area->low,seg->filesize);
  curpos = align_to_power_of_2(lseek(fd,0,SEEK_CUR),12);
  
  if (managed_static_area->active != managed_static_area->low) {
    nrefbytes = ((area_dnode(managed_static_area->active,managed_static_area->low)+7)>>3);

    prepare_to_write_dynamic_space(managed_static_area);
    seg = add_macho_segment(p,
                            "MANAGED-STATIC",
                            (natural)(managed_static_area->low),
                            align_to_power_of_2((managed_static_area->active-managed_static_area->low)+nrefbytes,12),
                            curpos,
                            align_to_power_of_2((managed_static_area->active-managed_static_area->low)+nrefbytes,12),
                            VM_PROT_READ|VM_PROT_WRITE|VM_PROT_EXECUTE,
                            VM_PROT_READ|VM_PROT_WRITE|VM_PROT_EXECUTE,
                            2,
                            "contents",
                            "refbits");
    init_macho_section(seg,
                       0,
                       seg->vmaddr,
                       managed_static_area->active-managed_static_area->low,
                       curpos,
                       S_ATTR_SOME_INSTRUCTIONS);
    managed_static_section_ordinal=++next_section_ordinal;
    lseek(fd,curpos,SEEK_SET);
    safe_write(fd,managed_static_area->low,managed_static_area->active-managed_static_area->low);
    curpos = lseek(fd,0,SEEK_CUR);
    init_macho_section(seg,
                       1,
                       seg->vmaddr+(managed_static_area->active-managed_static_area->low),
                       nrefbytes,
                       curpos,
                       S_REGULAR);
    managed_static_refbits_section_ordinal=++next_section_ordinal;
    safe_write(fd,(char *)managed_static_area->refbits,nrefbytes);
    curpos = align_to_power_of_2(lseek(fd,0,SEEK_CUR),12);
  }
  prepare_to_write_dynamic_space(active_dynamic_area);
  seg = add_macho_segment(p,
                          "DYNAMIC",
                          truncate_to_power_of_2((natural)static_cons_area->low,12),
                          align_to_power_of_2(active_dynamic_area->active,12)-truncate_to_power_of_2((natural)static_cons_area->low,12),
                          curpos,
                          align_to_power_of_2(active_dynamic_area->active,12)-truncate_to_power_of_2((natural)static_cons_area->low,12),

                          VM_PROT_READ|VM_PROT_WRITE|VM_PROT_EXECUTE,
                          VM_PROT_READ|VM_PROT_WRITE|VM_PROT_EXECUTE,
                          1,
                          "heap");
  init_macho_section(seg,
                     0,
                     truncate_to_power_of_2((natural)static_cons_area->low,12),
                     align_to_power_of_2(active_dynamic_area->active,12)-truncate_to_power_of_2((natural)static_cons_area->low,12),
                     curpos,
                     S_ATTR_SOME_INSTRUCTIONS);
  dynamic_section_ordinal=++next_section_ordinal;
  lseek(fd,curpos,SEEK_SET);
  safe_write(fd,(char *)truncate_to_power_of_2((natural)static_cons_area->low,12), align_to_power_of_2(active_dynamic_area->active,12)-truncate_to_power_of_2((natural)static_cons_area->low,12));
  curpos = align_to_power_of_2(lseek(fd,0,SEEK_CUR),12);

  prepare_to_write_static_space(egc_was_enabled);
  seg = add_macho_segment(p,
                          "STATIC",
                          align_to_power_of_2(active_dynamic_area->active,12),
                          8192,
                          curpos,
                          8192,
                          VM_PROT_READ|VM_PROT_WRITE|VM_PROT_EXECUTE,
                          VM_PROT_READ|VM_PROT_WRITE|VM_PROT_EXECUTE,
                          1,
                          "copy");
  init_macho_section(seg,
                     0,
                     align_to_power_of_2(active_dynamic_area->active,12),
                     8192,
                     curpos,
                     S_ATTR_SOME_INSTRUCTIONS);
  static_section_ordinal=++next_section_ordinal;
  lseek(fd,curpos,SEEK_SET);
  safe_write(fd,nilreg_area->low,8192);
  curpos = align_to_power_of_2(lseek(fd,0,SEEK_CUR),12);
  seg = add_macho_segment(p,
                          "__LINKEDIT",
                          align_to_power_of_2(reserved_region_end,12),
                          0,    /* tbd */
                          curpos,
                          0,    /* tbd */
                          VM_PROT_READ|VM_PROT_WRITE|VM_PROT_EXECUTE,
                          VM_PROT_READ,
                          0);
  dylib = (struct dylib_command *)add_load_command(p,LC_ID_DYLIB,sizeof(struct dylib_command)+strlen(dylib_name)+1);
  dylib->dylib.name.offset = sizeof(struct dylib_command);
  strcpy((char *)dylib+sizeof(struct dylib_command),dylib_name);
  symtab = (struct symtab_command *)add_load_command(p,LC_SYMTAB,sizeof(struct symtab_command));
  dysymtab = (struct dysymtab_command *)add_load_command(p,LC_DYSYMTAB,sizeof(struct dysymtab_command));
  dysymtab->nlocalsym=all_symbols->used;
  first_external_symbol = all_symbols->used;

  /* Add external symbols describing section boundaries. */
  symbol.n_un.n_strx = save_string("_READONLY_START",global_string_table);
  symbol.n_type = N_SECT|N_EXT;
  symbol.n_sect = readonly_section_ordinal;
  symbol.n_desc = 0;
  symbol.n_value = (natural)readonly_area->low;
  add_symbol(&symbol,all_symbols);

  symbol.n_un.n_strx = save_string("_READONLY_END",global_string_table);
  symbol.n_value = (natural)readonly_area->active;
  add_symbol(&symbol,all_symbols);
  
  if (managed_static_section_ordinal) {
    symbol.n_un.n_strx = save_string("_MANAGED_STATIC_START",global_string_table);
    symbol.n_sect = managed_static_section_ordinal;
    symbol.n_value = (natural)managed_static_area->low;
    add_symbol(&symbol,all_symbols);

    symbol.n_un.n_strx = save_string("_MANAGED_STATIC_END",global_string_table);
    symbol.n_value = (natural)managed_static_area->active;
    add_symbol(&symbol,all_symbols);

    symbol.n_un.n_strx = save_string("_MANAGED_STATIC_REFMAP_END",global_string_table);
    symbol.n_sect = managed_static_refbits_section_ordinal;
    symbol.n_value = (natural)managed_static_area->active+nrefbytes;
    add_symbol(&symbol,all_symbols);
  }
  symbol.n_un.n_strx = save_string("_STATIC_CONS_START",global_string_table);
  symbol.n_sect = dynamic_section_ordinal;
  symbol.n_value = (natural)static_cons_area->low;
  add_symbol(&symbol,all_symbols);

  symbol.n_un.n_strx = save_string("_STATIC_CONS_END",global_string_table);
  symbol.n_value = (natural)static_cons_area->high;
  add_symbol(&symbol,all_symbols);

  symbol.n_un.n_strx = save_string("_DYNAMIC_HEAP_END",global_string_table);
  symbol.n_value = (natural)active_dynamic_area->active;
  add_symbol(&symbol,all_symbols);

  num_external_symbols = all_symbols->used - first_external_symbol;
  dysymtab->iextdefsym = first_external_symbol;
  dysymtab->nextdefsym = num_external_symbols;
  sort_macho_symbol_table(all_symbols,first_external_symbol,num_external_symbols);
  symtab->symoff = curpos;
  symtab->nsyms = all_symbols->used;
  safe_write(fd,(char *)all_symbols->symbols,all_symbols->used*sizeof(macho_nlist));
  curpos = lseek(fd, 0, SEEK_CUR);
  dysymtab->tocoff = curpos;
  dysymtab->ntoc = num_external_symbols;
  toc = (struct dylib_table_of_contents *)malloc(num_external_symbols*sizeof(struct dylib_table_of_contents));
  
  for (i=0,j=first_external_symbol;
       i<num_external_symbols;
       i++,j++) {
    toc[i].symbol_index = j;
    toc[i].module_index = 0;
  }
  safe_write(fd,(char *)toc,num_external_symbols*sizeof(struct dylib_table_of_contents));
  curpos = lseek(fd, 0, SEEK_CUR);
  dysymtab->modtaboff = curpos;
  dysymtab->nmodtab = 1;
  memset(&m,0,sizeof(macho_module));
  m.module_name = save_string("single_module",global_string_table);
  m.iextdefsym = first_external_symbol;
  m.nextdefsym = num_external_symbols;
  m.irefsym = 0;
  m.nrefsym = num_external_symbols;
  m.ilocalsym = 0;
  m.nlocalsym = first_external_symbol;
  safe_write(fd,(char *)&m,sizeof(macho_module));
  curpos = lseek(fd, 0, SEEK_CUR);
  dysymtab->extrefsymoff = curpos;
  dysymtab->nextrefsyms = num_external_symbols;
  refs = (struct dylib_reference *)malloc(sizeof(struct dylib_reference)*num_external_symbols);
  for (i = 0, j = first_external_symbol;
       i < num_external_symbols;
       i++, j++) {
    refs[i].isym = j;
    refs[i].flags = REFERENCE_FLAG_DEFINED;
  }
  safe_write(fd,(char *)refs,sizeof(struct dylib_reference)*num_external_symbols);
  curpos = lseek(fd, 0, SEEK_CUR);
  symtab->stroff = curpos;
  symtab->strsize = global_string_table->used;
  safe_write(fd,global_string_table->data,global_string_table->used);
  curpos = lseek(fd, 0, SEEK_CUR);
  /* 'seg' still refers to the last segment, i.e., the __LINKEDIT segment */
  seg->filesize = curpos - seg->fileoff;
  seg->vmsize = align_to_power_of_2(seg->filesize,12);
  
  lseek(fd,0,SEEK_SET);
  safe_write(fd,p->page,4096);
}                

LispObj
load_native_library(char *path)
{
  extern BytePtr allocate_from_reserved_area(natural);
  void *lib;
  LispObj image_nil = 0;

  /* Because of the way that we've reserved memory, we can only
     load the image's segments at their preferred address if we
     make the pages at those addresses free. */
  {
    int fd = open(path,O_RDONLY);
    Boolean win = false;

    if (fd >= 0) {
      char * p = mmap(NULL, 4096, PROT_READ, MAP_PRIVATE, fd, 0);   
      
      if (p != MAP_FAILED) {
        macho_header *h = (macho_header *)p;
        
        if ((h->magic == MACHO_MAGIC) &&
            (h->cputype ==
#ifdef X8632
             CPU_TYPE_I386
#endif
#ifdef X8664
             CPU_TYPE_X86_64
#endif
             )) {
          struct load_command *lc;
          macho_segment_command *seg;
          
          for (lc = (struct load_command *)(p+sizeof(macho_header));
               lc->cmd == MACHO_LC_SEGMENT;
               lc = (struct load_command *)(((char *)lc)+lc->cmdsize)) {
            seg = (macho_segment_command *) lc;
            if (seg->vmaddr && seg->vmsize) {
              munmap((void *)seg->vmaddr, seg->vmsize);
            }
          }
          win = true;
        }
        munmap(p,4096);
      }
      close(fd);
    }
    if (! win) {
      return 0;
    }
  }
  lib = dlopen(path, RTLD_GLOBAL);
  if (lib == NULL) {
    return 0;
  } else {
    area *a;
    natural initsize,totalsize,nrefbytes;
    char 
      *ro_start = dlsym(lib,"READONLY_START"), 
      *ro_end   = dlsym(lib,"READONLY_END"), 
      *ms_start = dlsym(lib,"MANAGED_STATIC_START"), 
      *ms_end   = dlsym(lib,"MANAGED_STATIC_END"), 
      *msr_end  = dlsym(lib,"MANAGED_STATIC_REFMAP_END"), 
      *sc_start = dlsym(lib,"STATIC_CONS_START"),
      *sc_end   = dlsym(lib,"STATIC_CONS_START"), 
      *dh_end   = dlsym(lib,"DYNAMIC_HEAP_END"),
      *p,
      *q;

    if ((dh_end == NULL) ||
        (ro_start != pure_space_active)) {
      dlclose(lib);
      return 0;
    }
    p = (BytePtr)align_to_power_of_2(dh_end,12);
    q = static_space_active;
    mprotect(q,8192,PROT_READ|PROT_WRITE|PROT_EXEC);
    memcpy(q,p,8192);
    memset(p,0,8192);

    a = nilreg_area = new_area(q,q+8192,AREA_STATIC);
    nilreg_area->active = nilreg_area->high; /* a little wrong */
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
    add_area_holding_area_lock(a);
    
    a = new_area(pure_space_active,pure_space_limit,AREA_READONLY);
    readonly_area = a;
    add_area_holding_area_lock(a);
    pure_space_active = a->active = ro_end;
    
    initsize = dh_end - sc_end;
    totalsize = align_to_power_of_2(initsize, log2_heap_segment_size);
    
    p = allocate_from_reserved_area(totalsize);
    q = p+totalsize;
    a = new_area(p,q,AREA_DYNAMIC);
    a->active = dh_end;
    a->h = p;
    CommitMemory((char *)(align_to_power_of_2(dh_end,12)),
                 q-(char *)align_to_power_of_2(dh_end,12));
    map_initial_reloctab(p, q);
    map_initial_markbits(p, q);
    lisp_global(HEAP_START) = (LispObj)p;
    lisp_global(HEAP_END) = (LispObj)q;
    add_area_holding_area_lock(a);
    resize_dynamic_heap(dh_end, lisp_heap_gc_threshold);
    xMakeDataExecutable(a->low, a->active - a->low);

    static_cons_area = new_area(sc_start, sc_end, AREA_STATIC_CONS);
    static_cons_area->active = sc_start;
    lower_heap_start(sc_start,a);
    a->static_dnodes = area_dnode(sc_end,sc_start);
    
    managed_static_area = new_area(ms_start,ms_end,AREA_MANAGED_STATIC);
    managed_static_area->active = ms_end;
    add_area_holding_area_lock(managed_static_area);
    lisp_global(REF_BASE) = (LispObj) ms_start;
    
    nrefbytes = msr_end - ms_end;
    CommitMemory(global_mark_ref_bits,align_to_power_of_2(nrefbytes, 12));
    memcpy(global_mark_ref_bits,ms_end,nrefbytes);
    memset(ms_end,0,nrefbytes);
    
    return image_nil;
  }
}
