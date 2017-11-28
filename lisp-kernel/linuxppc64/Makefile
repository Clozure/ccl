#
# Copyright 1994-2001 Clozure Associates
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
# Versions of GNU as >= 2.9.1 all seem to work
# AS = gas-2.9.1
AS = as
M4 = m4
ASFLAGS = -mregnames -mppc64 -a64 -maltivec
M4FLAGS = -DLINUX -DPPC -DPPC64
CDEFINES = -DLINUX -D_REENTRANT -DPPC -DPPC64 -D_GNU_SOURCE -DVC_REVISION=$(VC_REVISION)
CDEBUG = -g
COPT = -O2
# word size issues are a little more relevant on a 64-bit platform
# than elsewhere, but most gcc format warnings are still nonsense.
WFORMAT = -Wno-format
PLATFORM_H = platform-linuxppc64.h

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
	$(M4) $(M4FLAGS) -I../ $< | $(AS)  $(ASFLAGS) -o $@
.c.o:
	$(CC) -include ../$(PLATFORM_H) -c $< $(CDEFINES) $(CDEBUG) $(COPT) $(WFORMAT) -m64 -o $@

SPOBJ = pad.o ppc-spjump.o ppc-spentry.o ppc-subprims.o
ASMOBJ = ppc-asmutils.o imports.o

COBJ  = pmcl-kernel.o gc-common.o ppc-gc.o bits.o  ppc-exceptions.o \
	image.o thread_manager.o lisp-debug.o memory.o unix-calls.o

DEBUGOBJ = lispdcmd.o plprint.o plsym.o plbt.o ppc_print.o
KERNELOBJ= $(COBJ) ppc-asmutils.o  imports.o

SPINC =	lisp.s m4macros.m4 ppc-constants.s ppc-macros.s errors.s ppc-uuo.s ppc-constants64.s lisp_globals.s

CHEADERS = area.h bits.h ppc-constants.h lisp-errors.h gc.h lisp.h \
	lisp-exceptions.h lisp_globals.h macros.h memprotect.h image.h \
	threads.h ppc-constants64.h ppc-exceptions.h $(PLATFORM_H) \
	constants.h os-linux.h

# Subprims linked into the kernel ?
# Yes:

KSPOBJ = $(SPOBJ)
all:	../../ppccl64


# No:

# KSPOBJ=
# all:	../../ppccl64 ../../subprims.so

OSLIBS = -ldl -lm -lpthread -lrt


../../ppccl64:	$(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ)
	$(CC) -m64 $(CDEBUG)  -Wl,--export-dynamic $(HASH_STYLE)  -o $@ -T ./elf64ppc.x $(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ) -Wl,--no-as-needed $(OSLIBS)


$(SPOBJ): $(SPINC)
$(ASMOBJ): $(SPINC)
$(COBJ): $(CHEADERS)
$(DEBUGOBJ): $(CHEADERS) lispdcmd.h


cclean:
	$(RM) -f $(KERNELOBJ) $(DEBUGOBJ) ../../ppccl64

clean:	cclean
	$(RM) -f $(SPOBJ)

strip:	../../ppccl64
	strip -g ../../ppccl64
