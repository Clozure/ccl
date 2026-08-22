# SPDX-License-Identifier: Apache-2.0
# This Makefile needs GNU Make

VC_REVISION := "$(shell git describe --dirty 2>/dev/null || echo unknown)"

# See <http://people.debian.org/~wookey/bootstrap.html>
#CROSS=aarch64-linux-gnu-
VPATH = ../
RM = /bin/rm
AS = ${CROSS}as
CC = ${CROSS}gcc
ASFLAGS =
CDEFINES = -DLINUX -DARM64 -D_REENTRANT -D_GNU_SOURCE -DVC_REVISION=$(VC_REVISION)
CDEBUG = -g
# The xp accessor macros in platform-linuxarm64.h type-pun the signal
# context's __u64 fields through natural * and uint32_t **.  -O2 turns on
# -fstrict-aliasing, which licenses the compiler to assume that does not
# happen, so this is a correctness flag and not a warning suppression.
COPT = -O2 -fno-strict-aliasing
# Once in a while, -Wformat says something useful.  The odds are against that,
# however.
WFORMAT = -Wno-format
# -Wswitch reports an enum switch that has silently lost a case.  Kept
# separate from WFORMAT so that overriding WFORMAT on the command line to
# audit warnings does not also discard this.
WARN = -Wswitch -Wno-unused-parameter
PLATFORM_H = platform-linuxarm64.h


# If the linker supports a "--hash-style=" option, use traditional
# Sysv hash tables.  (If it doesn't support that option, assume
# that traditional hash tables will be used by default.)
ld_has_hash_style = $(shell $(LD) --help | grep "hash-style=")
ifeq ($(ld_has_hash_style),)
HASH_STYLE=
else
HASH_STYLE="-Wl,--hash-style=sysv"
endif


.s.o:
	$(CC) -c -x assembler-with-cpp -I../ $< $(CDEFINES) -o $@
.c.o:
	$(CC) -include ../$(PLATFORM_H) -c $< $(CDEFINES) $(CDEBUG) $(COPT)   $(WFORMAT) $(WARN)  -o $@

SPOBJ = pad.o  arm64-spentry.o
ASMOBJ = arm64-asmutils.o arm64-imports.o

COBJ  = pmcl-kernel.o gc-common.o arm64-gc.o bits.o  arm64-exceptions.o \
	image.o thread_manager.o lisp-debug.o memory.o unix-calls.o

DEBUGOBJ = lispdcmd.o plprint.o plsym.o albt.o arm64-print.o
KERNELOBJ= $(COBJ) arm64-asmutils.o  arm64-imports.o

SPINC =	lisp.s arm64-constants.h arm64-macros.s errors.s arm64-uuo.s \
	lisp_globals.s

CHEADERS = area.h bits.h arm64-constants.h lisp-errors.h gc.h lisp.h \
	lisp-exceptions.h lisp_globals.h macros.h memprotect.h image.h \
	threads.h arm64-exceptions.h $(PLATFORM_H)

KSPOBJ = $(SPOBJ)

all:	../../arm64cl

OSLIBS = -ldl -lm -lpthread -lrt


../../arm64cl:	$(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ)
	$(CC)  $(CDEBUG)  -Wl,--export-dynamic $(HASH_STYLE)  -o $@ $(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ) -Wl,--no-as-needed $(OSLIBS)


$(SPOBJ): $(SPINC)
$(ASMOBJ): $(SPINC)
$(COBJ): $(CHEADERS)
$(DEBUGOBJ): $(CHEADERS) lispdcmd.h


cclean:
	$(RM) -f $(KERNELOBJ) $(DEBUGOBJ) ../../arm64cl

clean:	cclean
	$(RM) -f $(SPOBJ)

arm64-imports.o: ../arm64-imports.s ../arm64-asm.h
