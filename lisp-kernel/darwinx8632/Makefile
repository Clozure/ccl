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


SVN_REVISION=$(shell svnversion || echo "unknown")

SDKROOT=/
OSVERSION=10.5
#SDKROOT=/Developer/SDKs/MacOSX10.6.sdk
#OSVERSION=10.6

MDYNAMIC_NO_PIC = $(shell ($(CC) --help -v 2>&1 | grep -q -e "-mdynamic-no-pic") && /bin/echo "-mdynamic-no-pic")

VPATH = ..
RM = /bin/rm
LD = ld
LDFLAGS =  -macosx_version_min $(OSVERSION) -arch i386 -dynamic  -o $@ -e start -pagezero_size 0x11000 -seg1addr 0x00011000 -sectalign __TEXT __text 0x1000 \
 -exported_symbols_list exports
AS = as
M4 = gm4
M4FLAGS = -DDARWIN -DX86 -DX8632
ASFLAGS = -arch i386 -g
CDEFINES = -DDARWIN -DX86 -DX8632 -DSVN_REVISION=$(SVN_REVISION) -D_DARWIN_NO_64_BIT_INODE
CDEBUG = -g
COPT = -O2
# Once in a while, -Wformat says something useful.  The odds are against that,
# however.
WFORMAT = -Wno-format
CC=cc
PLATFORM_H = platform-darwinx8632.h

.s.o:
	$(M4) $(M4FLAGS) -I../ $< | $(AS) $(ASFLAGS) -o $@
.c.o:
	$(CC) -include ../$(PLATFORM_H) -c -arch i386 $< $(CDEFINES) $(CDEBUG) $(COPT) $(MDYNAMIC_NO_PIC) $(WFORMAT) -mmacosx-version-min=$(OSVERSION) -isysroot $(SDKROOT) -o $@

SPOBJ = x86-spjump32.o x86-spentry32.o x86-subprims32.o
ASMOBJ = x86-asmutils32.o imports.o

COBJ  = pmcl-kernel.o gc-common.o bits.o  \
	thread_manager.o lisp-debug.o image.o memory.o x86-gc.o \
	x86-exceptions.o unix-calls.o mach-o-image.o

DEBUGOBJ = lispdcmd.o plprint.o plsym.o x86_print.o xlbt.o
KERNELOBJ= imports.o $(COBJ) x86-asmutils32.o 

SPINC =	lisp.s m4macros.m4 x86-constants.s x86-macros.s errors.s x86-uuo.s x86-constants32.s

CHEADERS = area.h bits.h x86-constants.h lisp-errors.h gc.h lisp.h \
	lisp-exceptions.h lisp_globals.h macros.h memprotect.h image.h \
	threads.h lisptypes.h x86-constants32.h x86-exceptions.h \
	$(PLATFORM_H) constants.h os-darwin.h

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

../../dx86cl:	 $(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ) Makefile
	$(LD)  $(LDFLAGS) $(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ)   $(OSLIBS)


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

