#
#   Copyright (C) 2010 Clozure Associates
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

NO_WARN_DEPRECATED = $(shell ($(AS) --help -v 2>&1 | grep -q -e "-mno-warn-deprecated") && /bin/echo "-mno-warn-deprecated")

VPATH = ../
RM = /bin/rm
# Versions of GNU as >= 2.9.1 all seem to work
# AS = gas-2.9.1
AS = as
M4 = m4
# we use the swp instruction in a couple of places;  tell the assembler
# to remain calm.
ASFLAGS = -mfpu=vfp -march=armv7-a $(NO_WARN_DEPRECATED)
M4FLAGS = -DLINUX -DARM
CDEFINES = -DLINUX -DARM -D_REENTRANT -D_GNU_SOURCE -DUSE_FUTEX -DSVN_REVISION=$(SVN_REVISION)
CDEBUG = -g
COPT = -O2
# Once in a while, -Wformat says something useful.  The odds are against that,
# however.
WFORMAT = -Wno-format
PLATFORM_H = platform-linuxarm.h

# If the linker supports a "--hash-style=" option, use traditional
# Sysv hash tables.  (If it doesn't support that option, assume
# that traditional hash tables will be used by default.)
ld_has_hash_style = $(shell $(LD) --help | grep "hash-style=")
ifeq ($(ld_has_hash_style),)
HASH_STYLE=
else
HASH_STYLE="-Wl,--hash-style=sysv"
endif


# Likewise, some versions of GAS may need a "-a32" flag, to force the

# output file to be 32-bit compatible.

A32 = $(shell ($(AS) --help -v 2>&1 | grep -q -e "-a32") && /bin/echo "-a32")

.s.o:
	$(M4) $(M4FLAGS) -I../ $< | $(AS) $(A32) $(ASFLAGS) -o $@
.c.o:
	$(CC) -include ../$(PLATFORM_H) -c $< $(CDEFINES) $(CDEBUG) $(COPT) -marm $(WFORMAT)  -o $@

SPOBJ = pad.o  arm-spentry.o
ASMOBJ = arm-asmutils.o imports.o

COBJ  = pmcl-kernel.o gc-common.o arm-gc.o bits.o  arm-exceptions.o \
	image.o thread_manager.o lisp-debug.o memory.o unix-calls.o

DEBUGOBJ = lispdcmd.o plprint.o plsym.o albt.o arm_print.o
KERNELOBJ= $(COBJ) arm-asmutils.o  imports.o

SPINC =	lisp.s m4macros.m4 arm-constants.s arm-macros.s errors.s arm-uuo.s

CHEADERS = area.h bits.h arm-constants.h lisp-errors.h gc.h lisp.h \
	lisp-exceptions.h lisp_globals.h macros.h memprotect.h image.h \
	threads.h arm-exceptions.h $(PLATFORM_H)

# Subprims linked into the kernel ?
# Yes:

KSPOBJ = $(SPOBJ)
all:	../../armcl


# No:

# KSPOBJ=
# all:	../../ppccl ../../subprims.so

OSLIBS = -ldl -lm -lpthread


../../armcl:	$(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ)
	$(CC)  $(CDEBUG)  -Wl,--export-dynamic $(HASH_STYLE) -o $@ -T ./armlinux.x $(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ) $(OSLIBS)


$(SPOBJ): $(SPINC)
$(ASMOBJ): $(SPINC)
$(COBJ): $(CHEADERS)
$(DEBUGOBJ): $(CHEADERS) lispdcmd.h


cclean:
	$(RM) -f $(KERNELOBJ) $(DEBUGOBJ) ../../ppccl

clean:	cclean
	$(RM) -f $(SPOBJ)

strip:	../../ppccl
	strip -g ../../ppccl
