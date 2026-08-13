# SPDX-License-Identifier: Apache-2.0
# Darwin/arm64 (Apple Silicon) lisp kernel.
# Modeled on linuxarm64/Makefile + darwinx8664 Darwin link extras.
# No -pagezero_size / -no_pie: unsupported on arm64 Darwin (see doc/porting/darwin.md).

VC_REVISION := "$(shell git describe --dirty 2>/dev/null || echo unknown)"

OSVERSION ?= 11.0

VPATH = ../
RM = /bin/rm
CC = cc
ASFLAGS =
CDEFINES = -DDARWIN -DARM64 -D_REENTRANT -D_DARWIN_C_SOURCE \
	-DVC_REVISION=$(VC_REVISION) $(CDEFINES_EXTRA)
CDEBUG = -g
COPT = -O2
WFORMAT = -Wno-format
PLATFORM_H = platform-darwinarm64.h

.s.o:
	$(CC) -c -x assembler-with-cpp -I../ $< $(CDEFINES) \
		-mmacosx-version-min=$(OSVERSION) -o $@
.c.o:
	$(CC) -include ../$(PLATFORM_H) -c $< $(CDEFINES) $(CDEBUG) $(COPT) \
		$(WFORMAT) -mmacosx-version-min=$(OSVERSION) -o $@

SPOBJ = pad.o arm64-spentry.o
ASMOBJ = arm64-asmutils.o arm64-imports.o

COBJ = pmcl-kernel.o gc-common.o arm64-gc.o bits.o arm64-exceptions.o \
	arm64-darwin-mach.o image.o thread_manager.o lisp-debug.o \
	memory.o unix-calls.o

DEBUGOBJ = lispdcmd.o plprint.o plsym.o albt.o arm64-print.o
KERNELOBJ = $(COBJ) arm64-asmutils.o arm64-imports.o mach_exc_server.o

SPINC = lisp.s arm64-constants.h arm64-macros.s errors.s arm64-uuo.s \
	lisp_globals.s

CHEADERS = area.h bits.h arm64-constants.h lisp-errors.h gc.h lisp.h \
	lisp-exceptions.h lisp_globals.h macros.h memprotect.h image.h \
	threads.h arm64-exceptions.h $(PLATFORM_H) os-darwin.h

KSPOBJ = $(SPOBJ)

# Binary name matches lib/compile-ccl.lisp (:darwinarm64 "darm64cl")
all: ../../darm64cl

OSLIBS = -lSystem -ldl

../../darm64cl: $(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ)
	$(CC) $(CDEBUG) -mmacosx-version-min=$(OSVERSION) \
		-o $@ $(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ) $(OSLIBS)

$(SPOBJ): $(SPINC)
$(ASMOBJ): $(SPINC)
$(COBJ): $(CHEADERS)
$(DEBUGOBJ): $(CHEADERS) lispdcmd.h

mach_exc_server.o: mach_exc_server.c mach_exc.h
	$(CC) -include ../$(PLATFORM_H) -c mach_exc_server.c $(CDEFINES) \
		$(CDEBUG) $(COPT) $(WFORMAT) -mmacosx-version-min=$(OSVERSION) -o $@

mach_exc_server.c: ../mach_exc.defs
	mig -arch arm64 -header /dev/null -user /dev/null \
		-sheader mach_exc.h -server mach_exc_server.c \
		$<

mach_exc.h: mach_exc_server.c

cclean:
	$(RM) -f $(KERNELOBJ) $(DEBUGOBJ) ../../darm64cl \
	  mach_exc.h mach_exc_server.c

clean: cclean
	$(RM) -f $(SPOBJ)

arm64-imports.o: ../arm64-imports.s ../arm64-asm.h
