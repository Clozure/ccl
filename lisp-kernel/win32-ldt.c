/*
   Copyright (C) 2008 Clozure Associates
   This file is part of OpenMCL.  

   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
   License , known as the LLGPL and distributed with OpenMCL as the
   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
   which is distributed with OpenMCL as the file "LGPL".  Where these
   conflict, the preamble takes precedence.  

   OpenMCL is referenced in the preamble as the "LIBRARY."

   The LLGPL is also available online at
   http://opensource.franz.com/preamble.html
*/

/* experimental code to get and set LDT entries on Win32.  */

#include <stdio.h>
#include <windows.h>
#include <winnt.h>

int (*NtQueryInformationProcess)(HANDLE,DWORD,VOID*,DWORD,DWORD*);
int (*NtSetLdtEntries)(DWORD, DWORD, DWORD, DWORD, DWORD, DWORD);
int (*NtSetInformationProcess)(HANDLE,DWORD,VOID*,DWORD);
HMODULE hNtdll;

int GetLDTSelectorEntry1(HANDLE hProcess,
		        DWORD dwSelector,
		        LPLDT_ENTRY lpSelectorEntry)
{
  DWORD buf[4];
  DWORD len;
  int res;

  buf[0] = dwSelector & 0xFFFFFFF8;  // selector --> offset
  buf[1] = 8;                    // size (multiple selectors may be added)
  res = NtQueryInformationProcess(hProcess,10,buf,16,&len);
  memcpy(lpSelectorEntry, &buf[2], 8);
  return res;
}

int GetLDTSelectorEntry2(HANDLE hProcess,
		        DWORD dwSelector,
		        LPLDT_ENTRY lpSelectorEntry)
{
  return GetThreadSelectorEntry(hProcess, dwSelector, lpSelectorEntry);
}

int SetLDTSelectorEntry1(HANDLE hProcess,
		         DWORD dwSelector,
		         LPLDT_ENTRY lpSelectorEntry)
{
  DWORD buf[4];
  DWORD len;
  int res;

  buf[0] = dwSelector & 0xFFFFFFF8;  // selector --> offset
  buf[1] = 8;                    // size (multiple selectors may be added)
  memcpy(&buf[2], lpSelectorEntry, 8);
  res = NtSetInformationProcess(hProcess,10,buf,16);
  return res;
}

int SetLDTSelectorEntry2(DWORD dwSelector,
		         LPLDT_ENTRY lpSelectorEntry)
{
  return NtSetLdtEntries(dwSelector,
			 *(DWORD*)lpSelectorEntry,
			 *(((DWORD*)lpSelectorEntry)+1),
			 0,0,0);
}

DWORD GetLdtBase (LDT_ENTRY ldtEntry) {
  return ldtEntry.BaseLow 
    + (ldtEntry.HighWord.Bytes.BaseMid << 16)
    + (ldtEntry.HighWord.Bytes.BaseHi << 24);
}

void SetLdtBase (LDT_ENTRY *ldtEntry, DWORD base) {
  ldtEntry->BaseLow = base & 0xffff;
  ldtEntry->HighWord.Bytes.BaseMid = base >> 16;
  ldtEntry->HighWord.Bytes.BaseHi = base >> 24;
}

extern int get_gs(void);
extern void set_gs(int);

char some_buffer[1024];

int main (int argc, char** argv) {
  HANDLE hProcess = GetCurrentProcess();
  LDT_ENTRY ldt_entry;
  int res;  

  hNtdll = LoadLibrary("ntdll.dll");

  (void*)NtQueryInformationProcess = (void*)GetProcAddress(hNtdll, "NtQueryInformationProcess");
  (void*)NtSetInformationProcess = (void*)GetProcAddress(hNtdll, "NtSetInformationProcess");
  (void*)NtSetLdtEntries = (void*)GetProcAddress(hNtdll, "NtSetLdtEntries");

  
  if (!GetLDTSelectorEntry2(hProcess, get_gs(), &ldt_entry)) {
    printf("Error getting LDT entry for 0x%x: 0x%x\n", get_gs(), GetLastError());
    exit(1);
  }

  printf("0x%x is based at 0x%x\n", get_gs(), GetLdtBase(ldt_entry));
  

  /*
  memset(&ldt_entry, 0, sizeof(LDT_ENTRY));
  SetLdtBase(&ldt_entry, &some_buffer);
  if (res = SetLDTSelectorEntry2(0x100, &ldt_entry)) {
    printf("Error setting LDT entry for 0x%x: 0x%x\n", 0x100, res);
    exit(1);
  }
  set_gs(0x100);
  */
  return 0;
}
