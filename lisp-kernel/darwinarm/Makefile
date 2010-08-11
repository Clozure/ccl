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


VPATH = ../
RM = /bin/rm
AS = /Developer/Platforms/iPhoneOS.platform/Developer/usr/libexec/gcc/darwin/arm/as
M4 = m4
ASFLAGS = -arch armv6
M4FLAGS = -DDARWIN -DARM
CC = /Developer/Platforms/iPhoneOS.platform/usr/bin/llvm-gcc-4.2
CDEFINES = -isysroot /Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS3.2.sdk/ -DDARWIN -DARM -D_REENTRANT -D_GNU_SOURCE -DDISABLE_EGC -DGC_INTEGRITY_CHECKING -arch armv6 -mfpu=vfp -Wp,-dM
CDEBUG = -gstabs
COPT = #-O2
# Once in a while, -Wformat says something useful.  The odds are against that,
# however.
WFORMAT = -Wno-format
PLATFORM_H = platform-darwinarm.h



.s.o:
	$(M4) $(M4FLAGS) -I../ $< | $(AS) $(A32) $(ASFLAGS) -o $@
.c.o:
	$(CC) -arch arm -include ../$(PLATFORM_H) -c $< $(CDEFINES) $(CDEBUG) $(COPT) -marm $(WFORMAT)  -o $@

SPOBJ = arm-spentry.o arm-subprims.o
ASMOBJ = arm-asmutils.o imports.o

COBJ  = pmcl-kernel.o gc-common.o arm-gc.o bits.o  arm-exceptions.o \
	image.o thread_manager.o lisp-debug.o memory.o unix-calls.o

DEBUGOBJ = lispdcmd.o plprint.o plsym.o albt.o arm_print.o
KERNELOBJ= $(COBJ) arm-asmutils.o  imports.o

SPINC =	lisp.s m4macros.m4 arm-constants.s arm-macros.s errors.s arm-uuo.s

CHEADERS = area.h bits.h arm-constants.h lisp-errors.h gc.h lisp.h \
	lisp-exceptions.h lisp_globals.h macros.h memprotect.h image.h \
	Threads.h arm-exceptions.h $(PLATFORM_H)


KSPOBJ = $(SPOBJ)
all:	../../darmcl

OSLIBS = /Developer/Platforms/iPhoneOS.platform/Developer/usr/llvm-gcc-4.2/lib/gcc/arm-apple-darwin10/4.2.1/v6/libgcc.a -lSystem /Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS3.2.sdk/usr/lib/crt1.o /Developer/Platforms/iPhoneOS.platform/DeviceSupport/3.1.3/Symbols/usr/lib/libgcc_s.1.dylib


../../darmcl:	$(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ)
	$(LD) -pagezero_size 0x8000 -sectalign __TEXT __text 0x1000 $(KSPOBJ) -L/Developer/Platforms/iPhoneOS.platform/DeviceSupport/3.1.3/Symbols/usr/lib  -arch armv6 -o $@   $(KERNELOBJ) $(DEBUGOBJ) $(OSLIBS)


$(SPOBJ): $(SPINC)
$(ASMOBJ): $(SPINC)
$(COBJ): $(CHEADERS)
$(DEBUGOBJ): $(CHEADERS) lispdcmd.h


cclean:
	$(RM) -f $(KERNELOBJ) $(DEBUGOBJ) ../../darmcl

clean:	cclean
	$(RM) -f $(SPOBJ)

strip:	../../darmcl
	strip -g ../../darmcl
