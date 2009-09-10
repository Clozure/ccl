#include <windows.h>
#include <psapi.h>
#include <stdio.h>
#include <string.h>


struct ProcessBasicInformation {
  DWORD ExitStatus;
  PVOID PebBaseAddress;
  DWORD AffinityMask;
  DWORD BasePriority;
  ULONG UniqueProcessId;
  ULONG InheritedFromUniqueProcessId;
};

/* Try to get the (Windows) pid of the parent process; return it, or
   return 0 on failure.  There doesn't seem to be a higher-level way
   to do this.
*/

typedef LONG (__stdcall *FPTR_NtQueryInformationProcess ) (HANDLE, INT, PVOID, ULONG, PULONG);

ULONG
getppid()
{
  ULONG ppid = 0, pid = GetCurrentProcessId();
  HANDLE hcurrent = OpenProcess(PROCESS_QUERY_INFORMATION,FALSE,pid);
  if (hcurrent) {
    FPTR_NtQueryInformationProcess NtQueryInformationProcess = (FPTR_NtQueryInformationProcess) GetProcAddress(GetModuleHandleA("ntdll"), "NtQueryInformationProcess");
    if (NtQueryInformationProcess != NULL) {
      struct ProcessBasicInformation pbi;
      if (NtQueryInformationProcess(hcurrent, 0, (void *)&pbi, sizeof(pbi), NULL) == 0) {
        ppid = pbi.InheritedFromUniqueProcessId;
      }
    }
    CloseHandle(hcurrent);
  }
  return ppid;
}

char *
parent_process_name(ULONG ppid)
{
  char procname[1024], *result = NULL;
  DWORD namelen;
  HANDLE hprocess = OpenProcess(PROCESS_QUERY_INFORMATION|PROCESS_VM_READ, FALSE, ppid);

  if (hprocess) {
    namelen = GetModuleBaseNameA(hprocess, NULL, procname, sizeof(procname)-1);
    if (namelen && (namelen <= sizeof(procname))) {
      result = strdup(procname);
    }
    CloseHandle(hprocess);
  }
  return result;
}


int
write_pending_console_input(HANDLE input, HANDLE output) 
{
  DWORD ninput_events, i, n, nread, nwritten;
  char buf[1024];
  INPUT_RECORD ir;

  switch (WaitForSingleObject(input, 100)) {
  case WAIT_TIMEOUT:
    return 0;
    break;
  case WAIT_OBJECT_0:
    ninput_events = 0;
    if (GetNumberOfConsoleInputEvents(input, &ninput_events)) {
      for (i = 0; i < ninput_events; i++) {
        PeekConsoleInput(input, &ir, 1, &n);
        if ((ir.EventType == KEY_EVENT)) {
          if (!ReadFile(input, buf, sizeof(buf), &nread, NULL)) {
            return -1;
          }
          if (!WriteFile(output, buf, nread, &nwritten, NULL)) {
            return -1;
          }
          return 0;
        } else {
          ReadConsoleInput(input, &ir, 1, &n);
        }
      }
    }
    return 0;
    break;

  default:
    return -1;
    break;
  }
}
  

int
write_pipe_input_to_console(HANDLE hpipe, HANDLE conout) 
{
  unsigned char buf[1024];
  DWORD navail, n, m;

  while (PeekNamedPipe(hpipe, NULL, 0, NULL, &navail, NULL) != 0) {
    if (navail == 0) {
      return 0;
    }
    if (navail > sizeof(buf)) {
      n = sizeof(buf);
    } else {
      n = navail;
    }
    if (!ReadFile(hpipe, buf, n, &m, NULL)) {
      return -1;
    }
    if (!WriteFile(conout, buf, m, &n, NULL)) {
      return -1;
    }
  }
  return -1;
}

int APIENTRY WinMain(HINSTANCE a,HINSTANCE b,LPSTR c,int d)
{
  HANDLE in, out, conin, conout;
  DWORD navail, err;
  ULONG ppid;
  HWND window;
  HMENU menu;
  char title[2048], *procname, buf[1024];

  in = GetStdHandle(STD_INPUT_HANDLE);
  out = GetStdHandle(STD_OUTPUT_HANDLE);

  while (1) {
    navail = 0;
    if (PeekNamedPipe(in, NULL, 0, NULL, &navail, NULL) == 0) {
      exit(0);                  /* error or EOF */
    }
    if (navail != 0) {
      break;
    }
    Sleep(100);
  }

  AllocConsole();

  conin = CreateFileA("CONIN$",GENERIC_READ|GENERIC_WRITE,FILE_SHARE_READ,NULL,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);
  conout = CreateFileA("CONOUT$",GENERIC_WRITE,FILE_SHARE_WRITE,NULL,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);

  ppid = getppid();
  procname = parent_process_name(ppid);
  if (!procname) {
    procname = "<Unknown>";
  }
  sprintf(title, "WaltConsole for %s (pid 0x%x)", procname, ppid);
  SetConsoleTitleA(
title);

  window = GetConsoleWindow();
  menu = GetSystemMenu(window, FALSE);
  EnableMenuItem(menu, SC_CLOSE, MF_BYCOMMAND|MF_GRAYED);

  ShowWindow(window, SW_SHOW);
  ShowWindow(window, SW_SHOW);

  do {
    if (write_pipe_input_to_console(in, conout) < 0) {
      break;
    }
    write_pending_console_input(conin, out);
  } while (1);

  EnableMenuItem(menu, SC_CLOSE, MF_BYCOMMAND|MF_ENABLED);
  sprintf(buf,"\n\nProcess %s (pid 0x%x) has exited; press any key to close this window.", procname, ppid);
  WriteFile(conout, buf, strlen(buf), &navail, NULL);
  FlushConsoleInputBuffer(conin);
  ReadConsoleA(conin,buf,1,&navail,NULL);

}
