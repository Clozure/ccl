#
# Copyright 2006 Clozure Associates
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

VPATH = ..
RM = /bin/rm
AS = gas
# In earlier OpenSolaris releases, the bundled GNU m4 didn't work to
# build the lisp kernel.  In the 2009.06 release, /usr/bin/gm4 seems 
# to be adequate, but if m4 gives you any trouble, just download
# the GNU m4 source from somewhere and use that.
M4 = gm4
CC = gcc
ASFLAGS = --32 --divide
M4FLAGS = -DSOLARIS -DX86 -DX8632
CDEFINES = -DSOLARIS -D_REENTRANT -DX86 -DX8632 -D__EXTENSIONS__ -DHAVE_TLS -DVC_REVISION=$(VC_REVISION) #-DDISABLE_EGC
CDEBUG = -g
COPT = -O2
# Once in a while, -Wformat says something useful.  The odds are against that,
# however.
WFORMAT = -Wno-format
PLATFORM_H = platform-solarisx86.h


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
	$(PLATFORM_H) constants.h os-solaris.h


KSPOBJ = $(SPOBJ)
all:	../../sx86cl


OSLIBS = -ldl -lm -lpthread -lsocket -lnsl -lrt


../../sx86cl:	$(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ) Makefile
	$(CC)  -m32 $(CDEBUG) -o $@  $(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ) $(OSLIBS)


$(SPOBJ): $(SPINC)
$(ASMOBJ): $(SPINC)
$(COBJ): $(CHEADERS)
$(DEBUGOBJ): $(CHEADERS) lispdcmd.h


cclean:
	$(RM) -f $(KERNELOBJ) $(DEBUGOBJ) ../../sx86cl

clean:	cclean
	$(RM) -f $(SPOBJ)

strip:	../../sx86cl
	strip -g ../../sx86cl
