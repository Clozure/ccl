#
# Copyright 2008-2011 Clozure Associates
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# This makefile written to be used with the MSYS2 software building
# platform, which is available from https://www.msys2.org.
#
# MSYS2 provides a number of "environments". For the lisp kernel on
# Windows/x8632, We want the MINGW32 environment (32-bit x86, MSVCRT C
# library).
#
# After installing MSYS2, open a MINGW32 shell and install these packages
# with pacman:
#
# pacman -S mingw-w64-i686-gcc
# pacman -S m4 make
#
# Accept any required dependencies. This should install all the
# necessary tools. (If m4 or make are already installed, there's no
# need to re-install them.)

# If git is available, use it to include revision information.
VC_REVISION := "$(shell git describe --dirty 2>/dev/null || echo unknown)"

VPATH = ../
RM = rm

# These are expected to be from "/mingw32/bin"
CC = cc
AS = as
LD = ld

M4 = m4

ASFLAGS = -g --32
M4FLAGS = -DWIN_32 -DWINDOWS -DX86 -DX8632
CDEFINES = -DWIN_32 -DWINDOWS -D_REENTRANT -DX86 -DX8632 -D_GNU_SOURCE  -D__MSVCRT__ -D__MSVCRT_VERSION__=0x700 -D_WIN32_WINNT=0x0502 -DVC_REVISION=$(VC_REVISION)
CDEBUG = -g
COPT = -O
# Once in a while, -Wformat says something useful.  The odds are against that,
# however.
WFORMAT = -Wno-format
PLATFORM_H = platform-win32.h

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

OSLIBS = -lpsapi -lws2_32 -ladvapi32

../../wx86cl.exe: $(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ) Makefile
	$(CC) -m32 -o ../../wx86cl.exe  -Wl,--image-base=0x10000 \
	-Wl,--enable-auto-import \
	-no-pie \
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
