#
#   Copyright (C) 2005-2006 Clozure Associates
#   This file is part of OpenMCL.  
#
#   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
#   License , known as the LLGPL and distributed with OpenMCL as the
#   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
#   which is distributed with OpenMCL as the file "LGPL".  Where these
#   conflict, the preamble takes precedence.  
#
#   OpenMCL is referenced in the preamble as the "LIBRARY."
#
#   The LLGPL is also available online at
#   http://opensource.franz.com/preamble.html


VPATH = ..
RM = /bin/rm
AS = as
M4 = m4
ASFLAGS = --32
M4FLAGS = -DFREEBSD -DX86 -DX8632 -DHAVE_TLS
CDEFINES = -DFREEBSD -D_REENTRANT -DX86 -DX8632 -D_GNU_SOURCE -DHAVE_TLS
CDEBUG = -g
COPT = #-O2




SPOBJ = pad.o x86-spjump32.o x86-spentry32.o x86-subprims32.o
ASMOBJ = x86-asmutils32.o imports.o

COBJ  = pmcl-kernel.o gc-common.o  x86-gc.o bits.o  x86-exceptions.o \
	image.o thread_manager.o lisp-debug.o memory.o unix-calls.o

DEBUGOBJ = lispdcmd.o plprint.o plsym.o xlbt.o x86_print.o
KERNELOBJ= $(COBJ) x86-asmutils32.o  imports.o

SPINC =	lisp.s m4macros.m4 x86-constants.s x86-macros.s errors.s x86-uuo.s \
	x86-constants32.s

CHEADERS = area.h bits.h x86-constants.h lisp-errors.h gc.h lisp.h \
	lisp-exceptions.h lisp_globals.h macros.h memprotect.h image.h \
	Threads.h x86-constants32.h x86-exceptions.h

.if $(MACHINE) == "amd64"
CROSS = -B/usr/lib32
.endif

KSPOBJ = $(SPOBJ)
all:	../../fx86cl


OSLIBS = -lm -lthr $(CROSS)

.s.o:
	$(M4) $(M4FLAGS) -I../ $< | $(AS)  $(ASFLAGS) -o $@
.c.o:
	$(CC) -c $< $(CDEFINES) $(CDEBUG) $(COPT) -m32 -o $@

LINKSCRIPTFILE = # 
LINKSCRIPT =  # -T $(LINKSCRIPTFILE)

../../fx86cl:	$(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ) $(LINKSCRIPTFILE)
	$(CC) -m32 $(CDEBUG)  -Wl,--export-dynamic  $(LINKSCRIPT)  -o $@  $(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ) $(OSLIBS)


$(SPOBJ): $(SPINC)
$(ASMOBJ): $(SPINC)
$(COBJ): $(CHEADERS)
$(DEBUGOBJ): $(CHEADERS) lispdcmd.h


cclean:
	$(RM) -f $(KERNELOBJ) $(DEBUGOBJ) ../../fx86cl

clean:	cclean
	$(RM) -f $(SPOBJ)

strip:	../../fx86cl
	strip -g ../../fx86cl
