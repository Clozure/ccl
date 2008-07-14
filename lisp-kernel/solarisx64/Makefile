#
#   Copyright (C) 2006 Clozure Associates and contributors
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
AS = /usr/sfw/bin/gas
# As of this writing, /usr/sfw/bin/gm4 is both more recent (1.4.2 vs 1.4)
# and more buggy than /opt/sfw/bin/gm4, which is available on the 
# "Solaris companion" disk.  Do you get the impression that the people
# who put this stuff together aren't paying much attention ?
# Marching forward: as of the OpenSolais 0805 (snv_86) release, there
# doesn't seem to be any way of obtaining a non-broken GNU m4 from Sun.
# I just downloaded the source to 1.4.11 and installed it in /usr/local/bin;
# I didn't try blastwave.org or sunfreeware.com; there might be working
# packages there
M4 = /usr/local/bin/m4
CC = /usr/sfw/bin/gcc
ASFLAGS = --64
M4FLAGS = -DSOLARIS -DX86 -DX8664
CDEFINES = -DSOLARIS -D_REENTRANT -DX86 -DX8664 -D__EXTENSIONS__ -DHAVE_TLS -DDISABLE_EGC
CDEBUG = -g
COPT = #-O2



.s.o:
	$(M4) $(M4FLAGS) -I../ $< | $(AS)  $(ASFLAGS) -o $@
.c.o:
	$(CC) -c $< $(CDEFINES) $(CDEBUG) $(COPT) -m64 -o $@

SPOBJ = pad.o x86-spjump64.o x86-spentry64.o x86-subprims64.o
ASMOBJ = x86-asmutils64.o imports.o

COBJ  = pmcl-kernel.o gc-common.o x86-gc.o bits.o  x86-exceptions.o \
	image.o thread_manager.o lisp-debug.o memory.o

DEBUGOBJ = lispdcmd.o plprint.o plsym.o xlbt.o x86_print.o
KERNELOBJ= $(COBJ) x86-asmutils64.o  imports.o

SPINC =	lisp.s m4macros.m4 x86-constants.s x86-macros.s errors.s x86-uuo.s \
	x86-constants64.s

CHEADERS = area.h bits.h x86-constants.h lisp-errors.h gc.h lisp.h \
	lisp-exceptions.h lisp_globals.h macros.h memprotect.h image.h \
	Threads.h x86-constants64.h x86-exceptions.h lisptypes.h


KSPOBJ = $(SPOBJ)
all:	../../sx86cl64


OSLIBS = -ldl -lm -lpthread -lsocket -lnsl -lrt


../../sx86cl64:	$(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ) Makefile
	$(CC)  -m64 $(CDEBUG) -o $@  $(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ) $(OSLIBS)


$(SPOBJ): $(SPINC)
$(ASMOBJ): $(SPINC)
$(COBJ): $(CHEADERS)
$(DEBUGOBJ): $(CHEADERS) lispdcmd.h


cclean:
	$(RM) -f $(KERNELOBJ) $(DEBUGOBJ) ../../sx86cl64

clean:	cclean
	$(RM) -f $(SPOBJ)

strip:	../../sx86cl64
	strip -g ../../sx86cl64
