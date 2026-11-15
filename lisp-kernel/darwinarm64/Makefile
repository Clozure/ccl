# SPDX-License-Identifier: Apache-2.0
# This Makefile needs GNU Make

# capture absolute directory path of this Makefile...
MAKEFILE_DIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))

# ...and make sure we're in that directory
ifneq ($(CURDIR), $(MAKEFILE_DIR))
$(error You must run this Makefile from its own directory, not from $(CURDIR))
endif

VC_REVISION := "$(shell git describe --dirty 2>/dev/null || echo unknown)"

# arm64 macOS exists only on Big Sur and later.
OSVERSION = 11.0

VPATH = ../
RM = /bin/rm
CC = cc
ASFLAGS =
CDEFINES = -DDARWIN -DARM64 -D_REENTRANT -D_DARWIN_C_SOURCE \
	-DVC_REVISION=$(VC_REVISION) $(CDEFINES_EXTRA)
CDEBUG = -g
# Lisp passes the addresses of kernel imports around as fixnums, so
# they have to be 8-byte aligned; see check_kernel_imports().
COPT = -O2 -falign-functions=8
WFORMAT = -Wno-format
PLATFORM_H = platform-darwinarm64.h

.s.o:
	$(CC) -c -x assembler-with-cpp -I../ $< $(CDEFINES) \
		-mmacosx-version-min=$(OSVERSION) -o $@
.c.o:
	$(CC) -include ../$(PLATFORM_H) -c $< $(CDEFINES) $(CDEBUG) $(COPT) \
		$(WFORMAT) -mmacosx-version-min=$(OSVERSION) -o $@

SPOBJ = arm64-spentry.o
ASMOBJ = arm64-asmutils.o arm64-imports.o

COBJ = pmcl-kernel.o gc-common.o arm64-gc.o bits.o arm64-exceptions.o \
	arm64-darwin-mach.o image.o thread_manager.o lisp-debug.o \
	memory.o unix-calls.o

DEBUGOBJ = lispdcmd.o plprint.o plsym.o arm64-lbt.o arm64-print.o
KERNELOBJ = $(COBJ) arm64-asmutils.o arm64-imports.o mach_exc_server.o

SPINC = lisp.s arm64-constants.h arm64-macros.s errors.s arm64-uuo.s \
	lisp_globals.s

CHEADERS = area.h bits.h arm64-constants.h lisp-errors.h gc.h lisp.h \
	lisp-exceptions.h lisp_globals.h macros.h memprotect.h image.h \
	threads.h arm64-exceptions.h $(PLATFORM_H) os-darwin.h

KSPOBJ = $(SPOBJ)

# Binary name matches lib/compile-ccl.lisp (:darwinarm64 "darm64cl")
all: ../../darm64cl

../../darm64cl: $(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ)
	$(CC) $(CDEBUG) -mmacosx-version-min=$(OSVERSION) \
		-o $@ $(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ)

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

.PHONY: clean
clean: cclean
	$(RM) -f $(SPOBJ)

arm64-imports.o: ../arm64-imports.s ../arm64-asm.h

# GNU Make: delete any target file if its recipe fails after starting
# to write it.
.DELETE_ON_ERROR:
