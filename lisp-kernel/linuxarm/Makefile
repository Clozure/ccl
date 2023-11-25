#
# Copyright 2010 Clozure Associates
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

# default float-abi to whatever the toolchain defaults to.
FLOAT_ABI_OPTION =
VPATH = ../
# default float-abi to whatever the toolchain defaults to.
# allow float-abi.mk to override
FLOAT_ABI_OPTION =
VPATH = ../
RM = /bin/rm
# compile natively,  on a 32-bit ARM v7 running Linux
CROSS=
# a plausible cross-compilation prefix, useful on
# at least an odroid64 running Ubuntu 16.04
#CROSS=/usr/bin/arm-linux-gnueabihf-
AS = $(CROSS)as
CC = $(CROSS)gcc
M4 = m4
ASFLAGS = -mfpu=vfp -march=armv7-a $(FLOAT_ABI_OPTION)
M4FLAGS = -DLINUX -DARM
CDEFINES = -DLINUX -DARM -D_REENTRANT -D_GNU_SOURCE -DVC_REVISION=$(VC_REVISION)
CDEBUG = -g
COPT = -O2
# Once in a while, -Wformat says something useful.  The odds are against that,
# however.
WFORMAT = -Wno-format
PLATFORM_H = platform-linuxarm.h
include float_abi.mk

# If the linker supports a "--hash-style=" option, use traditional
# Sysv hash tables.  (If it doesn't support that option, assume
# that traditional hash tables will be used by default.)
ld_has_hash_style = $(shell $(LD) --help | grep "hash-style=")
ifeq ($(ld_has_hash_style),)
HASH_STYLE=
else
HASH_STYLE="-Wl,--hash-style=sysv"
endif


# Likewise, some versions of GAS may need a "-a32" flag, to force the

# output file to be 32-bit compatible.

A32 = $(shell ($(AS) --help -v 2>&1 | grep -q -e "-a32") && /bin/echo "-a32")

.s.o:
	$(M4) $(M4FLAGS) -I../ $< | $(AS) $(A32) $(ASFLAGS) -o $@
.c.o:
	$(CC) -include ../$(PLATFORM_H) -c $< $(CDEFINES) $(CDEBUG) $(COPT) -marm -march=armv7-a -mfpu=vfp $(FLOAT_ABI_OPTION) $(WFORMAT)  -o $@

SPOBJ = pad.o  arm-spentry.o
ASMOBJ = arm-asmutils.o imports.o

COBJ  = pmcl-kernel.o gc-common.o arm-gc.o bits.o  arm-exceptions.o \
	image.o thread_manager.o lisp-debug.o memory.o unix-calls.o

DEBUGOBJ = lispdcmd.o plprint.o plsym.o albt.o arm_print.o
KERNELOBJ= $(COBJ) arm-asmutils.o  imports.o

SPINC =	lisp.s m4macros.m4 arm-constants.s arm-macros.s errors.s arm-uuo.s \
	lisp_globals.s

CHEADERS = area.h bits.h arm-constants.h lisp-errors.h gc.h lisp.h \
	lisp-exceptions.h lisp_globals.h macros.h memprotect.h image.h \
	threads.h arm-exceptions.h $(PLATFORM_H)

# Subprims linked into the kernel ?
# Yes:

KSPOBJ = $(SPOBJ)
all:	../../armcl


# No:

# KSPOBJ=
# all:	../../armcl ../../subprims.so

OSLIBS = -ldl -lm -lpthread -lrt


../../armcl:	$(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ)
	$(CC)  $(CDEBUG)  -Wl,--export-dynamic $(HASH_STYLE) $(FLOAT_ABI_OPTION) -o $@  $(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ) -Wl,--no-as-needed $(OSLIBS)


$(SPOBJ): $(SPINC)
$(ASMOBJ): $(SPINC)
$(COBJ): $(CHEADERS)
$(DEBUGOBJ): $(CHEADERS) lispdcmd.h


cclean:
	$(RM) -f $(KERNELOBJ) $(DEBUGOBJ) ../../armcl

clean:	cclean
	$(RM) -f $(SPOBJ)

strip:	../../armcl
	strip -g ../../armcl
