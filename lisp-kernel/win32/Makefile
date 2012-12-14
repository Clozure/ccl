#
#   Copyright (C) 2008-2011 Clozure Associates
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

# This makefile is written to be used with the mingw toolchain
# running in the Cygwin environment.
#
# Install the following Cygwin packages:
# * make
# * m4
# * mingw64-i686-binutils
# * mingw64-i686-gcc-core
# * mingw64-i686-headers
# * mingw64-i686-pthreads
# * mingw64-i686-runtime
#
# Accept the dependencies the installation tool may add.

SVN_REVISION := "$(shell svnversion || echo unknown)"

VPATH = ../
RM = rm
CC = i686-w64-mingw32-gcc
AS = i686-w64-mingw32-as
M4 = m4
LD = i686-w64-mingw32-ld
ASFLAGS = -g --32
M4FLAGS = -DWIN_32 -DWINDOWS -DX86 -DX8632
CDEFINES = -DWIN_32 -DWINDOWS -D_REENTRANT -DX86 -DX8632 -D_GNU_SOURCE  -D__MSVCRT__ -D__MSVCRT_VERSION__=0x700 -D_WIN32_WINNT=0x0502 -DSVN_REVISION=$(SVN_REVISION)
CDEBUG = -g
COPT = -O
# Once in a while, -Wformat says something useful.  The odds are against that,
# however.
WFORMAT = -Wno-format
PLATFORM_H = platform-win32.h

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
# toolchain behaves the same way as when hosted on Linux ...
# The compiler default seems to be to use them; if we want to
# suppress their use, uncomment the following:

SUPPRESS_UNDERSCORES=#-fno-leading-underscore

.s.o:
	$(M4) $(M4FLAGS) -I../ $< | $(AS)  $(ASFLAGS) -o $@
.c.o:
	$(CC) -m32 -include ../$(PLATFORM_H) -c $< $(CDEFINES) $(CDEBUG) $(COPT) $(WFORMAT) ${SUPPRESS_UNDERSCORES} -o $@

# order matters: x86-spjump32.o must be first.
SPOBJ = x86-spjump32.o x86-spentry32.o x86-subprims32.o
ASMOBJ = x86-asmutils32.o imports.o

COBJ  = pmcl-kernel.o gc-common.o x86-gc.o bits.o  x86-exceptions.o \
	x86-utils.o \
	image.o thread_manager.o lisp-debug.o memory.o windows-calls.o

DEBUGOBJ = lispdcmd.o plprint.o plsym.o xlbt.o x86_print.o
KERNELOBJ= $(COBJ) x86-asmutils32.o  imports.o

SPINC =	lisp.s m4macros.m4 x86-constants.s x86-macros.s errors.s x86-uuo.s \
	x86-constants32.s lisp_globals.s

CHEADERS = area.h bits.h x86-constants.h lisp-errors.h gc.h lisp.h \
	lisp-exceptions.h lisp_globals.h macros.h memprotect.h image.h \
	threads.h x86-constants32.h x86-exceptions.h lisptypes.h \
	x86-utils.h \
	$(PLATFORM_H) constants.h os-windows.h


KSPOBJ = $(SPOBJ)
all:	../../wx86cl.exe

OSLIBS = -lpsapi -lws2_32

../../wx86cl.exe: $(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ) Makefile
	$(CC) -m32 -o ../../wx86cl.exe  -Wl,--image-base=0x10000 \
	-Wl,--enable-auto-import \
	$(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ) $(OSLIBS)


$(SPOBJ): $(SPINC)
$(ASMOBJ): $(SPINC)
$(COBJ): $(CHEADERS)
$(DEBUGOBJ): $(CHEADERS) lispdcmd.h


cclean:
	$(RM) -f $(KERNELOBJ) $(DEBUGOBJ) ../../wx86cl.exe

clean:	cclean
	$(RM) -f $(SPOBJ)

strip:	../../wx86cl.exe
	strip -g ../../wx86cl.exe


win32-foreign-thread-support.o: win32-foreign-thread-support.c $(CHEADERS)
win32-foreign-thread-support.dll: win32-foreign-thread-support.o
	$(CC) --shared -g -o $@ $<
dll: win32-foreign-thread-support.dll
