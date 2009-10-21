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


# For versions of GCC prior to 3.3, the option "-traditional-cpp" meant
# "don't use precompiled headers", which was good advice since they didn't
# work too well.  Beginning with GCC 3.3, the "-traditional-cpp" means 
# "use a broken preprocessor", which is (in a sense) the opposite of what
# it used to mean.

# Try to determine the version of GCC in use.  Invoke gcc with the
# -v flag, and look for a line containing the phrase "specs from" in
# the output.  Use sed to extract the full pathname of ths specs file
# printed in that line, then strip off the trailing "/specs".
gccdir = $(shell $(CC) -v 2>&1 | grep "specs from" | sed -e 's/.*from //' -e 's|/specs||')
# $(gccdir) is set to the directory containing the specs file, without the
# trailing slash.  The make intrinsic 'notdir' will strip a leading directory
# prefix from that pathname, leaving us with a string that should match
# the gcc version number
ifneq ($(gccdir),)
gccversion:=$(notdir $(gccdir))
oldgcc:=$(shell expr $(gccversion) "<" "3.3")
pregcc4:=$(shell expr $(gccversion) "<" "4.0")
ifeq ($(oldgcc),1)
BROKEN_PREPROCESSOR_WORKAROUND = -traditional-cpp
endif
endif

MDYNAMIC_NO_PIC = $(shell ($(CC) --help -v 2>&1 | grep -q -e "-mdynamic-no-pic") && /bin/echo "-mdynamic-no-pic")

OPENMCL_MAJOR_VERSION=0
OPENMCL_MINOR_VERSION=14

VPATH = ..
RM = /bin/rm
LD = ld
LDFLAGS = -arch ppc -dynamic  -o $@ -e start -pagezero_size 0x1000 -seg1addr 0x00001000 -sectalign __TEXT __text 0x1000 
AS = as
M4 = gm4
M4FLAGS = -DDARWIN -DPPC
ASFLAGS = -arch ppc -force_cpusubtype_ALL
CDEFINES = -DDARWIN -DPPC  $(BROKEN_PREPROCESSOR_WORKAROUND) #-DDEBUG -DGC_INTEGRITY_CHECKING
CDEBUG = -g
COPT = -O2

.s.o:
	$(M4) $(M4FLAGS) -I../ $< | $(AS) $(ASFLAGS) -o $@
.c.o:
	$(CC) -c -arch ppc $< $(CDEFINES) $(CDEBUG) $(COPT) -Wno-deprecated-declarations -mmacosx-version-min=10.4 -isysroot /Developer/SDKs/MacOSX10.4u.sdk  $(MDYNAMIC_NO_PIC) -o $@

SPOBJ = ppc-spjump.o ppc-spentry.o  ppc-subprims.o 
ASMOBJ = ppc-asmutils.o imports.o

COBJ  = pmcl-kernel.o gc-common.o ppc-gc.o bits.o  ppc-exceptions.o \
	thread_manager.o lisp-debug.o image.o memory.o unix-calls.o

DEBUGOBJ = lispdcmd.o plprint.o plsym.o plbt.o ppc_print.o
KERNELOBJ= imports.o $(COBJ) ppc-asmutils.o 

SPINC =	lisp.s m4macros.m4 ppc-constants.s ppc-macros.s errors.s ppc-uuo.s ppc-constants32.s

CHEADERS = area.h bits.h ppc-constants.h lisp-errors.h gc.h lisp.h \
	lisp-exceptions.h lisp_globals.h macros.h memprotect.h image.h \
	Threads.h lisptypes.h ppc-constants32.h ppc-exceptions.h

# Subprims linked into the kernel ?
# Yes:

KSPOBJ= $(SPOBJ)
all:	../../dppccl


# No:

# KSPOBJ=

OSEARLYLIBS = -lcrt1.o
OSLATELIBS = -lSystem

# gcc 4.0 and later want to use -lSystemStubs for many of the
# runtime support functions that were in -lgcc in previous
# versions.  'pregcc4' may have been set above.
ifeq ($(pregcc4),1)
OSMIDDLELIBS = -lgcc
else
OSMIDDLELIBS = -lSystemStubs
endif

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
