#
#   Copyright (C) 2007 Clozure Associates
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


VPATH = ../
RM = /bin/rm
# gcc64, as64: until there's a real win64 gcc, assume that gcc and gas
# are installed under these names
CC = x86_64-pc-mingw32-gcc
AS = x86_64-pc-mingw32-as
M4 = m4
LD = x86_64-pc-mingw32-ld
ASFLAGS = -g --64
M4FLAGS = -DWIN_64 -DWINDOWS -DX86 -DX8664 -DHAVE_TLS -DEMUTLS
CDEFINES = -DWIN_64 -DWINDOWS -D_REENTRANT -DX86 -DX8664 -D_GNU_SOURCE -DHAVE_TLS -DEMUTLS
CDEBUG = -g
COPT = -O2

# If the linker supports a "--hash-style=" option, use traditional
# SysV hash tables.  (If it doesn't support that option, assume
# that traditional hash tables will be used by default.)
ld_has_hash_style = $(shell $(LD) --help | grep "hash-style=")
ifeq ($(ld_has_hash_style),)
HASH_STYLE=
else
HASH_STYLE="-Wl,--hash-style=sysv"
endif

# There may be some confusion about whether or not C symbols have
# leading underscores or not.  The assembler sources seem to
# expect them to and mingw import libs seem to use them, but
# it's not clear whether or not native win64 libraries use this
# convention (and I'm not sure whether the Cygwin-hosted win64
# toolchain behaves the same was as when hosted on Linux ...
# The compiler default seems to be to use them; if we want to
# suppress their use, uncomment the following:

SUPPRESS_UNDERSCORES=#-fno-leading-underscore

.s.o:
	$(M4) $(M4FLAGS) -I../ $< | $(AS)  $(ASFLAGS) -o $@
.c.o:
	$(CC) -c $< $(CDEFINES) $(CDEBUG) $(COPT) ${SUPPRESS_UNDERSCORES} -m64 -o $@

SPOBJ = pad.o x86-spjump64.o x86-spentry64.o x86-subprims64.o
ASMOBJ = x86-asmutils64.o imports.o

COBJ  = pmcl-kernel.o gc-common.o x86-gc.o bits.o  x86-exceptions.o \
	image.o thread_manager.o lisp-debug.o memory.o windows-calls.o

DEBUGOBJ = lispdcmd.o plprint.o plsym.o xlbt.o x86_print.o
KERNELOBJ= $(COBJ) x86-asmutils64.o  imports.o

SPINC =	lisp.s m4macros.m4 x86-constants.s x86-macros.s errors.s x86-uuo.s \
	x86-constants64.s

CHEADERS = area.h bits.h x86-constants.h lisp-errors.h gc.h lisp.h \
	lisp-exceptions.h lisp_globals.h macros.h memprotect.h image.h \
	Threads.h x86-constants64.h x86-exceptions.h lisptypes.h


KSPOBJ = $(SPOBJ)
all:	../../wx86cl64.exe


OSLIBS = -lpsapi -lws2_32


../../wx86cl64.exe:	$(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ) Makefile pei-x86-64.x
	$(CC) -Wl,--image-base=0x10000 -Wl,-script=pei-x86-64.x -m64 $(CDEBUG)  -Wl,--export-dynamic $(HASH_STYLE) -o $@ $(USE_LINK_MAP) $(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ) $(OSLIBS)


$(SPOBJ): $(SPINC)
$(ASMOBJ): $(SPINC)
$(COBJ): $(CHEADERS)
$(DEBUGOBJ): $(CHEADERS) lispdcmd.h


cclean:
	$(RM) -f $(KERNELOBJ) $(DEBUGOBJ) ../../wx86cl64.exe

clean:	cclean
	$(RM) -f $(SPOBJ)

strip:	../../wx86cl64.exe
	strip -g ../../wx86cl64.exe
