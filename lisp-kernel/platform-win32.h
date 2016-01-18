/*
 * Copyright 1994-2010 Clozure Associates
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#define WORD_SIZE 32
#define PLATFORM_OS PLATFORM_OS_WINDOWS
#define PLATFORM_CPU PLATFORM_CPU_X86
#define PLATFORM_WORD_SIZE PLATFORM_WORD_SIZE_32

#include <windows.h>
typedef CONTEXT ExceptionInformation;

#define MAXIMUM_MAPPABLE_MEMORY (1U<<30)
#define IMAGE_BASE_ADDRESS 0x04000000

#include "lisptypes.h"
#include "x86-constants32.h"

/* Offsets relative to _CONTEXT.Edi */
#define REG_EDI 0
#define REG_ESI 1
#define REG_EBX 2
#define REG_EDX 3
#define REG_ECX 4
#define REG_EAX 5
#define REG_EBP 6
#define REG_EIP 7
#define REG_EFL 9
#define REG_ESP 10

#define xpGPRvector(x) ((DWORD *)(&(x)->Edi))
#define xpGPR(x,gprno) (xpGPRvector(x)[gprno])
#define xpPC(x) xpGPR(x,Iip)
#define eflags_register(xp) xp->EFlags
#define xpFPRvector(x) ((natural *)(&(x->ExtendedRegisters[10*16])))
#define xpMMXreg(x,n)  (*((uint64_t *)(&(x->FloatSave.RegisterArea[10*(n)]))))
#define xpMXCSRptr(x) ((DWORD *)(&(x->ExtendedRegisters[24])))
#define xpMXCSR(x) (*xpMXCSRptr(x))

#define SIGNUM_FOR_INTN_TRAP SIGSEGV /* Also fake */
#define IS_MAYBE_INT_TRAP(info,xp) \
  ((info->ExceptionCode == EXCEPTION_ACCESS_VIOLATION) &&       \
   (info->ExceptionInformation[0]==0) &&                       \
   (info->ExceptionInformation[1]==(ULONG_PTR)(-1L)))
#define IS_PAGE_FAULT(info,xp) (1)
#define SIGRETURN(context)      /* for now */

#include "os-windows.h"
