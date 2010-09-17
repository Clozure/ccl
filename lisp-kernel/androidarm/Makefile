#
#   Copyright (C) 2010 Clozure Associates
#   This file is part of Clozure CL.  
#
#   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public
#   License , known as the LLGPL and distributed with Clozure CL as the
#   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
#   which is distributed with Clozure CL as the file "LGPL".  Where these
#   conflict, the preamble takes precedence.  
#
#   Clozure CL is referenced in the preamble as the "LIBRARY."
#
#   The LLGPL is also available online at
#   http://opensource.franz.com/preamble.html


VPATH = ../
RM = /bin/rm
# Versions of GNU as >= 2.9.1 all seem to work
# AS = gas-2.9.1
AS = as
M4 = m4
ASFLAGS = -mfpu=vfpv2
M4FLAGS = -DLINUX -DARM -DANDROID
CDEFINES = -DLINUX -DARM -DANDROID -D_REENTRANT -D_GNU_SOURCE -DUSE_FUTEX
CDEBUG = -g
COPT = -O2
# Once in a while, -Wformat says something useful.  The odds are against that,
# however.
WFORMAT = -Wno-format
PLATFORM_H = platform-androidarm.h
NDK=/usr/local/src/ndk

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
	$(CC) -include ../$(PLATFORM_H) -nostdinc -isystem ${NDK}/usr/gcc-include -isystem $(NDK)/usr/include -isystem . -c $<  $(CDEFINES) $(CDEBUG) $(COPT)  $(WFORMAT)  -o $@

# -nostdinc -isystem ${NDK}/usr/gcc-include -isystem $(NDK)/usr/include
SPOBJ = pad.o  arm-spentry.o arm-subprims.o
ASMOBJ = arm-asmutils.o imports.o

COBJ  = pmcl-kernel.o gc-common.o arm-gc.o bits.o  arm-exceptions.o \
	image.o thread_manager.o lisp-debug.o memory.o unix-calls.o

DEBUGOBJ = lispdcmd.o plprint.o plsym.o albt.o arm_print.o
KERNELOBJ= $(COBJ) arm-asmutils.o  imports.o

SPINC =	lisp.s m4macros.m4 arm-constants.s arm-macros.s errors.s arm-uuo.s

CHEADERS = area.h bits.h arm-constants.h lisp-errors.h gc.h lisp.h \
	lisp-exceptions.h lisp_globals.h macros.h memprotect.h image.h \
	threads.h arm-exceptions.h $(PLATFORM_H)

# Subprims linked into the kernel ?
# Yes:

KSPOBJ = $(SPOBJ)
all:	../../aarmcl


# No:

# KSPOBJ=
# all:	../../armcl ../../subprims.so

OSLIBS = -ldl -lm $(NDK)/usr/lib/crtbegin_dynamic.o $(NDK)/usr/lib/crtend_android.o


../../aarmcl:	$(KSPOBJ) $(KERNELOBJ) $(DEBUGOBJ)
	$(LD) --build-id -e _start --eh-frame-hdr -dynamic-linker /system/bin/linker -X -m armelf_linux_eabi -nostdlib -o ../../aarmcl -z relro  --hash-style=sysv -L$(NDK)/usr/lib pad.o arm-spentry.o arm-subprims.o pmcl-kernel.o gc-common.o arm-gc.o bits.o arm-exceptions.o image.o thread_manager.o lisp-debug.o memory.o unix-calls.o arm-asmutils.o imports.o lispdcmd.o plprint.o plsym.o albt.o arm_print.o -ldl -lm /usr/local/src/ndk/usr/lib/crtbegin_dynamic.o /usr/local/src/ndk/usr/lib/crtend_android.o -lgcc -lc -lgcc  --no-as-needed -T ./armlinux.x



$(SPOBJ): $(SPINC)
$(ASMOBJ): $(SPINC)
$(COBJ): $(CHEADERS)
$(DEBUGOBJ): $(CHEADERS) lispdcmd.h


cclean:
	$(RM) -f $(KERNELOBJ) $(DEBUGOBJ) ../../aarmcl

clean:	cclean
	$(RM) -f $(SPOBJ)

strip:	../../ppccl
	strip -g ../../ppccl
