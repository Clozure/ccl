#
# Copyright 2008 Clozure Associates
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

VC_REVISION := "$(shell git describe --dirty 2>/dev/null || echo unknown)"

VPATH = ../
RM = /bin/rm
AS = as
M4 = m4
ASFLAGS = --32
M4FLAGS = -DLINUX -DX86 -DX8632 -DHAVE_TLS
CDEFINES = -DLINUX -D_REENTRANT -DX86 -DX8632 -D_GNU_SOURCE -DHAVE_TLS -DVC_REVISION=$(VC_REVISION) # -DGC_INTEGRITY_CHECKING -DDISABLE_EGC
CDEBUG = -g
COPT = -O2
# Once in a while, -Wformat says something useful.  The odds are against that,
# however.
WFORMAT = -Wno-format
PLATFORM_H = platform-linuxx8632.h

# If the linker supports a "--hash-style=" option, use traditional
# SysV hash tables.  (If it doesn't support that option, assume
# that traditional hash tables will be used by default.)
ld_has_hash_style = $(shell $(LD) --help | grep "hash-style=")
ifeq ($(ld_has_hash_style),)
HASH_STYLE=
else
HASH_STYLE="-Wl,--hash-style=sysv"
endif


.s.o:
	$(M4) $(M4FLAGS) -I../ $< | $(AS)  $(ASFLAGS) -o $@
.c.o:
	$(CC) -include ../$(PLATFORM_H) -c $< $(CDEFINES) $(CDEBUG) $(COPT) $(WFORMAT) -m32 -o $@

SPOBJ = pad.o x86-spjump32.o x86-spentry32.o x86-subprims32.o
ASMOBJ = x86-asmutils32.o imports.o

COBJ  = pmcl-kernel.o gc-common.o x86-gc.o bits.o  x86-exceptions.o \
	x86-utils.o \
	image.o thread_manager.o lisp-debug.o memory.o unix-calls.o

DEBUGOBJ = lispdcmd.o plprint.o plsym.o xlbt.o x86_print.o
KERNELOBJ= $(COBJ) x86-asmutils32.o  imports.o

SPINC =	lisp.s m4macros.m4 x86-constants.s x86-macros.s errors.s x86-uuo.s \
	x86-constants32.s lisp_globals.s

CHEADERS = area.h bits.h x86-constants.h lisp-errors.h gc.h lisp.h \
	lisp-exceptions.h lisp_globals.h macros.h memprotect.h image.h \
	threads.h x86-constants32.h x86-exceptions.h lisptypes.h \
	x86-utils.h \
	$(PLATFORM_H) constants.h os-linux.h


KSPOBJ = $(SPOBJ)
all:	../../lx86cl


OSLIBS = -ldl -lm -lpthread -lrt
LINK_SCRIPT = # ./elf_x86_32.x
USE_LINK_SCRIPT = # -T $(LINK_SCRIPT)

# Disable PIE: we need to ensure the kernel functions are mapped to low
# addresses that fit in a fixnum. Otherwise ASLR could randomly load the
# text section at an address outside this range.
PIE = -no-pie

../../lx86cl:	$(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ) Makefile  $(LINK_SCRIPT)
	$(CC)  -m32 $(PIE) $(CDEBUG)  -Wl,--export-dynamic $(HASH_STYLE) -o $@ $(USE_LINK_SCRIPT) $(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ) -Wl,--no-as-needed $(OSLIBS)


$(SPOBJ): $(SPINC)
$(ASMOBJ): $(SPINC)
$(COBJ): $(CHEADERS)
$(DEBUGOBJ): $(CHEADERS) lispdcmd.h


cclean:
	$(RM) -f $(KERNELOBJ) $(DEBUGOBJ) ../../lx86cl

clean:	cclean
	$(RM) -f $(SPOBJ)

strip:	../../lx86cl
	strip -g ../../lx86cl
