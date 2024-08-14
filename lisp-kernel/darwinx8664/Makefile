#
# Copyright 2005 Clozure Associates
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

OSVERSION=10.13

VPATH = ..
RM = /bin/rm
CC=cc
AS = as
M4 = gm4

M4FLAGS = -DDARWIN -DX86 -DX8664 -DTCR_IN_GPR
ASFLAGS = -arch x86_64 -g

CDEFINES = -DDARWIN -DX86 -DX8664 -DTCR_IN_GPR -DVC_REVISION=$(VC_REVISION) \
	   -DUSE_DTRACE
CDEBUG = -g
COPT = -O
# Once in a while, -Wformat says something useful.  The odds are against that,
# however.
WFORMAT = -Wno-format
PLATFORM_H = platform-darwinx8664.h

.s.o:
	$(M4) $(M4FLAGS) -I../ $< | $(AS) $(ASFLAGS) -mmacosx-version-min=$(OSVERSION) -o $@ -
.c.o:
	$(CC) -include ../$(PLATFORM_H) -c $< -arch x86_64 $(CDEFINES) \
	$(CDEBUG) $(COPT) $(WFORMAT) \
	 -I. -mmacosx-version-min=$(OSVERSION) -o $@

SPOBJ = x86-spjump64.o x86-spentry64.o x86-subprims64.o 
ASMOBJ = x86-asmutils64.o imports.o

COBJ  = pmcl-kernel.o gc-common.o x86-gc.o bits.o  x86-exceptions.o \
	x86-utils.o \
	thread_manager.o lisp-debug.o image.o memory.o unix-calls.o \
	mach-o-image.o

DEBUGOBJ = lispdcmd.o plprint.o plsym.o xlbt.o x86_print.o
KERNELOBJ= imports.o $(COBJ) x86-asmutils64.o mach_exc_server.o

SPINC =	lisp.s m4macros.m4 x86-constants.s x86-macros.s errors.s x86-uuo.s \
	x86-constants64.s lisp_globals.s

CHEADERS = area.h bits.h x86-constants.h lisp-errors.h gc.h lisp.h \
	lisp-exceptions.h lisp_globals.h macros.h memprotect.h image.h \
	threads.h lisptypes.h x86-constants64.h x86-exceptions.h \
	x86-utils.h \
	$(PLATFORM_H) constants.h os-darwin.h

KSPOBJ= $(SPOBJ)
all:	../../dx86cl64

../../dx86cl64:	 $(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ) Makefile
	cc -mmacosx-version-min=$(OSVERSION) -arch x86_64 \
	 -Wl,-no_pie,-pagezero_size,0x10000,-seg1addr,0x10000 \
	 -o $@ $(SPOBJ) $(KERNELOBJ) $(DEBUGOBJ)

$(SPOBJ): $(SPINC)
$(ASMOBJ): $(SPINC)
$(COBJ): $(CHEADERS) probes.h
$(DEBUGOBJ): $(CHEADERS) lispdcmd.h probes.h

probes.h: probes.d
	dtrace -h -s $<

thread_manager.o: thread_manager.c 

mach_exc_server.o: mach_exc_server.c mach_exc.h

# What we want here is a grouped target, but we need
# GNU make version 4.3 for that.
# mach_exc.h mach_exc_server.c &: mach_exc.defs

mach_exc_server.c: mach_exc.defs
	mig -arch x86_64 -header /dev/null -user /dev/null \
		-sheader mach_exc.h -server mach_exc_server.c \
		 $<

mach_exc.h: mach_exc_server.c

cclean:
	$(RM) -f $(KERNELOBJ) $(DEBUGOBJ) ../../dx86cl64 \
	  probes.h \
	  mach_exc.h mach_exc_server.c

clean:	cclean
	$(RM) -f $(SPOBJ) $(KSPOBJ)

strip:	../../dx86cl64
	strip -s retain ../../dx86cl64

