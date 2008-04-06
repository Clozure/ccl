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
#gccversion:=$(notdir $(gccdir))
#oldgcc:=$(shell expr $(gccversion) "<" "3.3")
#ifeq ($(oldgcc),1)
#BROKEN_PREPROCESSOR_WORKAROUND = -traditional-cpp
#endif

MDYNAMIC_NO_PIC = $(shell ($(CC) --help -v 2>&1 | grep -q -e "-mdynamic-no-pic") && /bin/echo "-mdynamic-no-pic")

OPENMCL_MAJOR_VERSION=0
OPENMCL_MINOR_VERSION=14

VPATH = ..
RM = /bin/rm
LD = ld64

### The -pagezero_size/-seg1addr args are an attempt to work around a
### bug (#4057702) in ld64.

### The -seg1addr and -pagezero_size arguments below are nonsense;
### early versions of ld64 were/are broken.
LDFLAGS = -macosx_version_min 10.4 -M -arch ppc64 -dynamic  -o $@ -e start -pagezero_size 0x1000 -seg1addr 0x1000 -sectalign __TEXT __text 0x1000
AS = as
M4 = gm4
M4FLAGS = -DDARWIN -DPPC -DPPC64
ASFLAGS = -arch ppc64
CDEFINES = -DDARWIN -DPPC -DPPC64 $(BROKEN_PREPROCESSOR_WORKAROUND)
CDEBUG = -g
COPT = #-O2

.s.o:
	$(M4) $(M4FLAGS) -I../ $< | $(AS) $(ASFLAGS) -o $@
.c.o:
	$(CC) -c $< -arch ppc64 -m64 $(CDEFINES) $(CDEBUG) $(COPT) $(MDYNAMIC_NO_PIC) -mmacosx-version-min=10.4 -isysroot /Developer/SDKs/MacOSX10.4u.sdk -o $@

SPOBJ = ppc-spjump.o ppc-spentry.o ppc-subprims.o 
ASMOBJ = ppc-asmutils.o imports.o

COBJ  = pmcl-kernel.o gc-common.o ppc-gc.o bits.o  ppc-exceptions.o \
	thread_manager.o lisp-debug.o image.o memory.o

DEBUGOBJ = lispdcmd.o plprint.o plsym.o plbt.o ppc_print.o
KERNELOBJ= imports.o $(COBJ) ppc-asmutils.o 

SPINC =	lisp.s m4macros.m4 ppc-constants.s ppc-macros.s errors.s ppc-uuo.s \
	ppc-constants64.s

CHEADERS = area.h bits.h ppc-constants.h lisp-errors.h gc.h lisp.h \
	lisp-exceptions.h lisp_globals.h macros.h memprotect.h image.h \
	Threads.h lisptypes.h ppc-constants64.h ppc-exceptions.h

# Subprims linked into the kernel ?
# Yes:

KSPOBJ= $(SPOBJ)
all:	../../dppccl64


# No:

# KSPOBJ=

OSEARLYLIBS = -lcrt1.o
OSLATELIBS = -lSystem -lmx

# If the linker can find an absolute path to -lSystemStubs, use
# -lSystemStubs; otherwise, just use libgcc.a
SYSTEMSTUBSPATH = $(shell $(CC) --print-file-name=libSystemStubs.a)
SYSTEMSTUBSABSOLUTE = $(shell expr $(SYSTEMSTUBSPATH) : "^/*")
ifeq ($(SYSTEMSTUBSABSOLUTE),1)
OSMIDDLELIBS = -lSystemStubs
else
OSMIDDLELIBS = -lgcc
endif

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
