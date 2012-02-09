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


SVN_REVISION := "$(shell svnversion || echo unknown)"

SDKROOT=/
OSVERSION=10.5

MDYNAMIC_NO_PIC = $(shell ($(CC) --help -v 2>&1 | grep -q -e "-mdynamic-no-pic") && /bin/echo "-mdynamic-no-pic")


VPATH = ..
RM = /bin/rm
CC=cc

### Current ld64 bugs include the claim that 0x1000 isn't a power of 2.
### Gosh.  I always thought that it was.  Go know, right ?
LDFLAGS = -macosx_version_min $(OSVERSION) -arch x86_64 -dynamic \
	  -o $@ -e start -pagezero_size 0x11000 -seg1addr 0x00011000 \
	  -exported_symbols_list exports

AS = as
M4 = gm4
###
### DARWIN_GS_HACK enables some awful, dangerous, and slow workarounds
### for the fact that early versions of x86-64 Darwin don't provide
### working mechanisms for threads to address thread-local-data
### relative to a spare segment register.  We instead use the
### undocumented mechanism which the pthreads library uses to
### keep pthread data in %gs, and switch %gs between pthread data
### when running foreign code and lisp tcr data when running lisp
### code.  Hopefully, we won't have to do this for very long.
###
### (Things like i386_set_ldt() are defined, but not implemented
### correctly on the libc side and not implemented at all on the
### Mach kernel side.)
###
### Apple never
M4FLAGS = -DDARWIN -DX86 -DX8664 -DTCR_IN_GPR
ASFLAGS = -arch x86_64 -g
CDEFINES = -DDARWIN -DX86 -DX8664 -DTCR_IN_GPR -DSVN_REVISION=$(SVN_REVISION) \
	   -D_DARWIN_NO_64_BIT_INODE -DUSE_DTRACE
CDEBUG = -g
COPT = -O2
# Once in a while, -Wformat says something useful.  The odds are against that,
# however.
WFORMAT = -Wno-format
PLATFORM_H = platform-darwinx8664.h

.s.o:
	$(M4) $(M4FLAGS) -I../ $< | $(AS) $(ASFLAGS) -o $@
.c.o:
	$(CC) -include ../$(PLATFORM_H) -c $< -arch x86_64 $(CDEFINES) \
	$(CDEBUG) $(COPT) $(WFORMAT) $(MDYNAMIC_NO_PIC) \
	 -I. -mmacosx-version-min=$(OSVERSION) -isysroot $(SDKROOT) -o $@

SPOBJ = x86-spjump64.o x86-spentry64.o x86-subprims64.o 
ASMOBJ = x86-asmutils64.o imports.o

COBJ  = pmcl-kernel.o gc-common.o x86-gc.o bits.o  x86-exceptions.o \
	x86-utils.o \
	thread_manager.o lisp-debug.o image.o memory.o unix-calls.o \
	mach-o-image.o

DEBUGOBJ = lispdcmd.o plprint.o plsym.o xlbt.o x86_print.o
KERNELOBJ= imports.o $(COBJ) x86-asmutils64.o 

SPINC =	lisp.s m4macros.m4 x86-constants.s x86-macros.s errors.s x86-uuo.s \
	x86-constants64.s lisp_globals.s

CHEADERS = area.h bits.h x86-constants.h lisp-errors.h gc.h lisp.h \
	lisp-exceptions.h lisp_globals.h macros.h memprotect.h image.h \
	threads.h lisptypes.h x86-constants64.h x86-exceptions.h \
	x86-utils.h \
	$(PLATFORM_H) constants.h os-darwin.h

KSPOBJ= $(SPOBJ)
all:	../../dx86cl64



OSEARLYLIBS = -lcrt1.o
OSLATELIBS = -lSystem

OSMIDDLELIBS = 


OSLIBS = $(OSEARLYLIBS) $(OSMIDDLELIBS) $(OSLATELIBS)

../../dx86cl64:	 $(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ) Makefile
	$(LD) $(LDFLAGS) $(KSPOBJ) $(KERNELOBJ)  $(DEBUGOBJ) $(OSLIBS)


$(SPOBJ): $(SPINC)
$(ASMOBJ): $(SPINC)
$(COBJ): $(CHEADERS) probes.h
$(DEBUGOBJ): $(CHEADERS) lispdcmd.h

probes.h: probes.d
	dtrace -h -s $<

thread_manager.o: thread_manager.c 

cclean:
	$(RM) -f $(KERNELOBJ) $(DEBUGOBJ) ../../dx86cl64 

# Some earlier versions of this Makefile built "subprims_r.o".  
# (That file is now defunct.)
clean:	cclean
	$(RM) -f $(SPOBJ) $(KSPOBJ) subprims_r.o

strip:	../../dx86cl64
	strip -s retain ../../dx86cl64

