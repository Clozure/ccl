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

VC_REVISION != sh -c "git describe --dirty 2>/dev/null || echo unknown"

VPATH = ..
RM = /bin/rm
AS = as
M4 = m4
ASFLAGS = -mfpu=vfp -march=armv7-a
M4FLAGS = -DNETBSD -DARM
CDEFINES = -DNETBSD -DARM -D_REENTRANT -D_NETBSD_SOURCE -DVC_REVISION="$(VC_REVISION)"
CDEBUG = -g
COPT = -O2
# Once in a while, -Wformat says something useful.  The odds are against that,
# however.
WFORMAT = -Wno-format
PLATFORM_H = platform-netbsdarm.h

.s.o:
	$(M4) $(M4FLAGS) -I../ $< | $(AS) $(ASFLAGS) -o $@
.c.o:
	$(CC) -include ../$(PLATFORM_H) -c $< $(CDEFINES) $(CDEBUG) $(COPT) -marm -march=armv7-a -mfpu=vfp $(WFORMAT) -o $@

SPOBJ = pad.o arm-spentry.o
ASMOBJ = arm-asmutils.o imports.o

COBJ = pmcl-kernel.o gc-common.o arm-gc.o bits.o arm-exceptions.o \
	image.o thread_manager.o lisp-debug.o memory.o unix-calls.o

DEBUGOBJ = lispdcmd.o plprint.o plsym.o albt.o arm_print.o
KERNELOBJ = $(COBJ) $(ASMOBJ)

SPINC = lisp.s m4macros.m4 arm-constants.s arm-macros.s errors.s arm-uuo.s \
	lisp_globals.s

CHEADERS = area.h bits.h arm-constants.h lisp-errors.h gc.h lisp.h \
	lisp-exceptions.h lisp_globals.h macros.h memprotect.h image.h \
	threads.h arm-exceptions.h $(PLATFORM_H) os-netbsd.h

KSPOBJ = $(SPOBJ)
all:	../../narmcl

OSLIBS = -lm -lpthread

../../narmcl:	$(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ)
	$(CC) $(CDEBUG) -Wl,--export-dynamic -o $@ $(KSPOBJ) $(KERNELOBJ) \
		$(DEBUGOBJ) $(OSLIBS)
	/usr/sbin/paxctl +m +a $@

$(SPOBJ): $(SPINC)
$(ASMOBJ): $(SPINC)
$(COBJ): $(CHEADERS)
$(DEBUGOBJ): $(CHEADERS) lispdcmd.h

cclean:
	$(RM) -f $(KERNELOBJ) $(DEBUGOBJ) ../../narmcl

clean:	cclean
	$(RM) -f $(SPOBJ)

strip:	../../narmcl
	strip -g ../../narmcl
