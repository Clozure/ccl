#include "../threads.h"

typedef void(*shutdownfunc)(void *);

shutdownfunc shutdown_thread_tcr = NULL;

void *__declspec(dllexport) 
prepare_foreign_threads() 
{
  void *addr = GetProcAddress(NULL, "shutdown_thread_tcr");
  shutdown_thread_tcr = (shutdownfunc)addr;
  return addr;
}

BOOL WINAPI 
DllMain(HINSTANCE hinstDLL,
        DWORD     fdwReason,
        LPVOID    lpvReserved)
{
  TCR *tcr;

  switch (fdwReason) {
  case DLL_THREAD_DETACH:
    if (shutdown_thread_tcr) {
      tcr = ((TCR *)((char *)NtCurrentTeb() + TCR_BIAS))->linear;
      if (tcr && (tcr->flags & (1<<TCR_FLAG_BIT_FOREIGN))) {
        shutdown_thread_tcr((void *)tcr);
      }
    }
    break;
  default:
    break;
  }
  return true;
}
