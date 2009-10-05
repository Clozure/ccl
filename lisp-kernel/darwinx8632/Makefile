#
#   Copyright (C) 2005 Clozure Associates and contributors.
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


MDYNAMIC_NO_PIC = $(shell ($(CC) --help -v 2>&1 | grep -q -e "-mdynamic-no-pic") && /bin/echo "-mdynamic-no-pic")

VPATH = ..
RM = /bin/rm
LD = ld
LDFLAGS =  -macosx_version_min 10.4 -arch i386 -dynamic  -o $@ -e start -pagezero_size 0x11000 -seg1addr 0x00011000 -sectalign __TEXT __text 0x1000 -segprot __PAD - -
AS = as
M4 = gm4
M4FLAGS = -DDARWIN -DX86 -DX8632
ASFLAGS = -arch i386 -g
CDEFINES = -DDARWIN -DX86 -DX8632 #-DGC_INTEGRITY_CHECKING -DFORCE_DWS_MARK -DDISABLE_EGC -DDEBUG_MACH_EXCEPTIONS
CDEBUG = -g
COPT = #-O2
CC=gcc-4.0

.s.o:
	$(M4) $(M4FLAGS) -I../ $< | $(AS) $(ASFLAGS) -o $@
.c.o:
	$(CC) -c -arch i386 $< $(CDEFINES) $(CDEBUG) $(COPT) $(MDYNAMIC_NO_PIC) -mmacosx-version-min=10.4 -isysroot /Developer/SDKs/MacOSX10.4u.sdk -o $@

SPOBJ = x86-spjump32.o x86-spentry32.o x86-subprims32.o
ASMOBJ = x86-asmutils32.o imports.o

COBJ  = pmcl-kernel.o gc-common.o bits.o  \
	thread_manager.o lisp-debug.o image.o memory.o x86-gc.o \
	x86-exceptions.o unix-calls.o

DEBUGOBJ = lispdcmd.o plprint.o plsym.o x86_print.o xlbt.o
KERNELOBJ= imports.o $(COBJ) x86-asmutils32.o 
LATEOBJ = x86-pad32.o

SPINC =	lisp.s m4macros.m4 x86-constants.s x86-macros.s errors.s x86-uuo.s x86-constants32.s

CHEADERS = area.h bits.h x86-constants.h lisp-errors.h gc.h lisp.h \
	lisp-exceptions.h lisp_globals.h macros.h memprotect.h image.h \
	Threads.h lisptypes.h x86-constants32.h x86-exceptions.h

# Subprims linked into the kernel ?
# Yes:

KSPOBJ= $(SPOBJ)
all:	../../dx86cl


# No:

# KSPOBJ=

OSEARLYLIBS = -lcrt1.o
OSLATELIBS = -lSystem

# is this needed?
#OSMIDDLELIBS = -lSystemStubs

OSLIBS = $(OSEARLYLIBS) $(OSMIDDLELIBS) $(OSLATELIBS)

../../dx86cl:	 $(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ) $(LATEOBJ) Makefile
	$(LD) $(LDFLAGS) $(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ) $(LATEOBJ) $(OSLIBS)


$(SPOBJ): $(SPINC)
$(ASMOBJ): $(SPINC)
$(COBJ): $(CHEADERS)
$(DEBUGOBJ): $(CHEADERS) lispdcmd.h



thread_manager.o: thread_manager.c 

cclean:
	$(RM) -f $(KERNELOBJ) $(DEBUGOBJ) ../../dx86cl 

# Some earlier versions of this Makefile built "subprims_r.o".  
# (That file is now defunct.)
clean:	cclean
	$(RM) -f $(SPOBJ) $(KSPOBJ) subprims_r.o

# retain file not here at the moment
strip:	../../dx86cl
	strip -s retain ../../dx86cl

