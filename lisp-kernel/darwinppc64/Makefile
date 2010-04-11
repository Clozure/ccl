#
#   Copyright (C) 2005 Clozure Associates and contributors.
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
LD = ld64

### The -pagezero_size/-seg1addr args are an attempt to work around a
### bug (#4057702) in ld64.

### The -seg1addr and -pagezero_size arguments below are nonsense;
### early versions of ld64 were/are broken.
LDFLAGS = -macosx_version_min 10.5 -M -arch ppc64 -dynamic  -o $@ -e start -pagezero_size 0x1000 -seg1addr 0x1000 -sectalign __TEXT __text 0x1000
AS = as
M4 = gm4
M4FLAGS = -DDARWIN -DPPC -DPPC64
ASFLAGS = -arch ppc64
CDEFINES = -DDARWIN -DPPC -DPPC64
CDEBUG = -g
COPT = -O2
# Once in a while, -Wformat says something useful.  The odds are against that,
# however.
WFORMAT = -Wno-format
CC=gcc-4.0
PLATFORM_H = platform-darwinppc64.h

.s.o:
	$(M4) $(M4FLAGS) -I../ $< | $(AS) $(ASFLAGS) -o $@
.c.o:
	$(CC) -include ../$(PLATFORM_H) -c $< -arch ppc64 $(CDEFINES) $(CDEBUG) $(COPT) $(MDYNAMIC_NO_PIC) $(WFORMAT) -mmacosx-version-min=10.5 -isysroot /Developer/SDKs/MacOSX10.5.sdk -o $@

SPOBJ = ppc-spjump.o ppc-spentry.o ppc-subprims.o 
ASMOBJ = ppc-asmutils.o imports.o

COBJ  = pmcl-kernel.o gc-common.o ppc-gc.o bits.o  ppc-exceptions.o \
	thread_manager.o lisp-debug.o image.o memory.o unix-calls.o

DEBUGOBJ = lispdcmd.o plprint.o plsym.o plbt.o ppc_print.o
KERNELOBJ= imports.o $(COBJ) ppc-asmutils.o 

SPINC =	lisp.s m4macros.m4 ppc-constants.s ppc-macros.s errors.s ppc-uuo.s \
	ppc-constants64.s

CHEADERS = area.h bits.h ppc-constants.h lisp-errors.h gc.h lisp.h \
	lisp-exceptions.h lisp_globals.h macros.h memprotect.h image.h \
	Threads.h lisptypes.h ppc-constants64.h ppc-exceptions.h $(PLATFORM_H)

# Subprims linked into the kernel ?
# Yes:

KSPOBJ= $(SPOBJ)
all:	../../dppccl64


# No:

# KSPOBJ=

OSEARLYLIBS = -lcrt1.o
OSLATELIBS = -lSystem -lmx

OSLIBS = $(OSEARLYLIBS) $(OSMIDDLELIBS) $(OSLATELIBS)

../../dppccl64:	 $(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ) Makefile
	$(LD)  $(LDFLAGS) $(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ)   $(OSLIBS)


$(SPOBJ): $(SPINC)
$(ASMOBJ): $(SPINC)
$(COBJ): $(CHEADERS)
$(DEBUGOBJ): $(CHEADERS) lispdcmd.h


thread_manager.o: thread_manager.c 

cclean:
	$(RM) -f $(KERNELOBJ) $(DEBUGOBJ) ../../dppccl64 

# Some earlier versions of this Makefile built "subprims_r.o".  
# (That file is now defunct.)
clean:	cclean
	$(RM) -f $(SPOBJ) $(KSPOBJ) subprims_r.o

strip:	../../dppccl64
	strip -s retain ../../dppccl64
