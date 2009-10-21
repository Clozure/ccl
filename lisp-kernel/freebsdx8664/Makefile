#
#   Copyright (C) 2005-2006 Clozure Associates
#   This file is part of Clozure CL.  
#
#   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public
#   License , known as the LLGPL and distributed with Clozure CL as the
#   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
#   which is distributed with Clozure CL as the file "LGPL".  Where these
#   conflict, the preamble takes precedence.  
#
#   Clozure CL is referenced in the preamble as the "LIBRARY."
#
#   The LLGPL is also available online at
#   http://opensource.franz.com/preamble.html


VPATH = ..
RM = /bin/rm
AS = as
M4 = m4
ASFLAGS = --64
M4FLAGS = -DFREEBSD -DX86 -DX8664 -DHAVE_TLS
CDEFINES = -DFREEBSD -D_REENTRANT -DX86 -DX8664 -D_GNU_SOURCE -DHAVE_TLS
CDEBUG = -g
COPT = #-O2




SPOBJ = pad.o x86-spjump64.o x86-spentry64.o x86-subprims64.o
ASMOBJ = x86-asmutils64.o imports.o

COBJ  = pmcl-kernel.o gc-common.o  x86-gc.o bits.o  x86-exceptions.o \
	image.o thread_manager.o lisp-debug.o memory.o unix-calls.o

DEBUGOBJ = lispdcmd.o plprint.o plsym.o xlbt.o x86_print.o
KERNELOBJ= $(COBJ) x86-asmutils64.o  imports.o

SPINC =	lisp.s m4macros.m4 x86-constants.s x86-macros.s errors.s x86-uuo.s \
	x86-constants64.s

CHEADERS = area.h bits.h x86-constants.h lisp-errors.h gc.h lisp.h \
	lisp-exceptions.h lisp_globals.h macros.h memprotect.h image.h \
	Threads.h x86-constants64.h x86-exceptions.h


KSPOBJ = $(SPOBJ)
all:	../../fx86cl64


OSLIBS = -lm -lthr

.s.o:
	$(M4) $(M4FLAGS) -I../ $< | $(AS)  $(ASFLAGS) -o $@
.c.o:
	$(CC) -c $< $(CDEFINES) $(CDEBUG) $(COPT) -m64 -o $@

LINKSCRIPTFILE = # ./elf_x86_64.x
LINKSCRIPT =  # -T $(LINKSCRIPTFILE)

../../fx86cl64:	$(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ) $(LINKSCRIPTFILE)
	$(CC) -m64 $(CDEBUG)  -Wl,--export-dynamic  $(LINKSCRIPT)  -o $@  $(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ) $(OSLIBS)


$(SPOBJ): $(SPINC)
$(ASMOBJ): $(SPINC)
$(COBJ): $(CHEADERS)
$(DEBUGOBJ): $(CHEADERS) lispdcmd.h


cclean:
	$(RM) -f $(KERNELOBJ) $(DEBUGOBJ) ../../fx86cl64

clean:	cclean
	$(RM) -f $(SPOBJ)

strip:	../../fx86cl64
	strip -g ../../fx86cl64
