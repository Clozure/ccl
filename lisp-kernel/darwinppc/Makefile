#
#   Copyright (C) 1994-2001 Digitool, Inc
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

MDYNAMIC_NO_PIC = $(shell ($(CC) --help -v 2>&1 | grep -q -e "-mdynamic-no-pic") && /bin/echo "-mdynamic-no-pic")

VPATH = ..
RM = /bin/rm
LD = ld
LDFLAGS = -arch ppc -dynamic  -o $@ -e start -pagezero_size 0x1000 -seg1addr 0x00001000 -sectalign __TEXT __text 0x1000 
AS = as
M4 = gm4
M4FLAGS = -DDARWIN -DPPC
ASFLAGS = -arch ppc -force_cpusubtype_ALL
CDEFINES = -DDARWIN -DPPC  #-DDEBUG -DGC_INTEGRITY_CHECKING
CDEBUG = -g
COPT = -O2
# Once in a while, -Wformat says something useful.  The odds are against that,
# however.
WFORMAT = -Wno-format
CC=gcc-4.0
PLATFORM_H = platform-darwinppc.h

.s.o:
	$(M4) $(M4FLAGS) -I../ $< | $(AS) $(ASFLAGS) -o $@
.c.o:
	$(CC) -include ../$(PLATFORM_H) -c -arch ppc $< $(CDEFINES) $(CDEBUG) $(COPT) -Wno-deprecated-declarations $(WFORMAT) -mmacosx-version-min=10.5 -isysroot /Developer/SDKs/MacOSX10.5.sdk  $(MDYNAMIC_NO_PIC) -o $@

SPOBJ = ppc-spjump.o ppc-spentry.o  ppc-subprims.o 
ASMOBJ = ppc-asmutils.o imports.o

COBJ  = pmcl-kernel.o gc-common.o ppc-gc.o bits.o  ppc-exceptions.o \
	thread_manager.o lisp-debug.o image.o memory.o unix-calls.o

DEBUGOBJ = lispdcmd.o plprint.o plsym.o plbt.o ppc_print.o
KERNELOBJ= imports.o $(COBJ) ppc-asmutils.o 

SPINC =	lisp.s m4macros.m4 ppc-constants.s ppc-macros.s errors.s ppc-uuo.s ppc-constants32.s

CHEADERS = area.h bits.h ppc-constants.h lisp-errors.h gc.h lisp.h \
	lisp-exceptions.h lisp_globals.h macros.h memprotect.h image.h \
	Threads.h lisptypes.h ppc-constants32.h ppc-exceptions.h \
	$(PLATFORM_H) constants.h os-darwin.h

# Subprims linked into the kernel ?
# Yes:

KSPOBJ= $(SPOBJ)
all:	../../dppccl


# No:

# KSPOBJ=

OSEARLYLIBS = -lcrt1.o
OSLATELIBS = -lSystem

OSLIBS = $(OSEARLYLIBS) $(OSMIDDLELIBS) $(OSLATELIBS)

../../dppccl:	 $(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ)
	$(LD)  $(LDFLAGS) $(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ)   $(OSLIBS)


$(SPOBJ): $(SPINC)
$(ASMOBJ): $(SPINC)
$(COBJ): $(CHEADERS)
$(DEBUGOBJ): $(CHEADERS) lispdcmd.h



thread_manager.o: thread_manager.c 

cclean:
	$(RM) -f $(KERNELOBJ) $(DEBUGOBJ) ../../dppccl 

# Some earlier versions of this Makefile built "subprims_r.o".  
# (That file is now defunct.)
clean:	cclean
	$(RM) -f $(SPOBJ) $(KSPOBJ) subprims_r.o

strip:	../../dppccl
	strip -s retain ../../dppccl
