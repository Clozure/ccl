/*
   Copyright (C) 2008, Clozure Associates and contributors,
   This file is part of Clozure CL.  

   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public
   License , known as the LLGPL and distributed with Clozure CL as the
   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
   which is distributed with Clozure CL as the file "LGPL".  Where these
   conflict, the preamble takes precedence.  

   Clozure CL is referenced in the preamble as the "LIBRARY."

   The LLGPL is also available online at
   http://opensource.franz.com/preamble.html
*/

#include "lisp.h"
#include "x86-exceptions.h"
#include <io.h>
#include <unistd.h>
#include <sys/fcntl.h>
#include <errno.h>
#include <sys/stat.h>
#include <windows.h>
#include <psapi.h>
#include <dirent.h>
#undef __argv
#include <stdio.h>
#include <math.h>

#ifndef WIN_32
#define _dosmaperr mingw_dosmaperr
#else
void
_dosmaperr(unsigned long oserrno)
{
  switch(oserrno) {
  case  ERROR_INVALID_FUNCTION:
    errno = EINVAL;
    break;
  case ERROR_FILE_NOT_FOUND:
    errno = ENOENT;
    break;
  case ERROR_PATH_NOT_FOUND:
    errno = ENOENT;
    break;
  case  ERROR_TOO_MANY_OPEN_FILES:
    errno = EMFILE;
    break;
  case  ERROR_ACCESS_DENIED:
    errno = EACCES;
    break;
  case  ERROR_ARENA_TRASHED:
    errno = ENOMEM;
    break;
  case  ERROR_NOT_ENOUGH_MEMORY:
    errno = ENOMEM;
    break;
  case  ERROR_INVALID_BLOCK:
    errno = ENOMEM;
    break;
  case  ERROR_BAD_ENVIRONMENT:
    errno = E2BIG;
    break;
  case  ERROR_BAD_FORMAT:
    errno = ENOEXEC;
    break;
  case  ERROR_INVALID_ACCESS:
    errno = EINVAL;
    break;
  case  ERROR_INVALID_DATA:
    errno = EINVAL;
    break;
  case  ERROR_INVALID_DRIVE:
    errno = ENOENT;
    break;
  case  ERROR_CURRENT_DIRECTORY:
    errno = EACCES;
    break;
  case  ERROR_NOT_SAME_DEVICE:
    errno = EXDEV;
    break;
  case  ERROR_NO_MORE_FILES:
    errno = ENOENT;
    break;
  case  ERROR_LOCK_VIOLATION:
    errno = EACCES;
    break;
  case  ERROR_BAD_NETPATH:
    errno = ENOENT;
    break;
  case  ERROR_NETWORK_ACCESS_DENIED:
    errno = EACCES;
    break;
  case  ERROR_BAD_NET_NAME:
    errno = ENOENT;
    break;
  case  ERROR_FILE_EXISTS:
    errno = EEXIST;
    break;
  case  ERROR_CANNOT_MAKE:
    errno = EACCES;
    break;
  case  ERROR_FAIL_I24:
    errno = EACCES;
    break;
  case  ERROR_INVALID_PARAMETER:
    errno = EINVAL;
    break;
  case  ERROR_NO_PROC_SLOTS:
    errno = EAGAIN;
    break;
  case  ERROR_DRIVE_LOCKED:
    errno = EACCES;
    break;
  case  ERROR_BROKEN_PIPE:
    errno = EPIPE;
    break;
  case  ERROR_DISK_FULL:
    errno = ENOSPC;
    break;
  case  ERROR_INVALID_TARGET_HANDLE:
    errno = EBADF;
    break;
  case  ERROR_INVALID_HANDLE:
    errno = EINVAL;
    break;
  case  ERROR_WAIT_NO_CHILDREN:
    errno = ECHILD;
    break;
  case  ERROR_CHILD_NOT_COMPLETE:
    errno = ECHILD;
    break;
  case  ERROR_DIRECT_ACCESS_HANDLE:
    errno = EBADF;
    break;
  case  ERROR_NEGATIVE_SEEK:
    errno = EINVAL;
    break;
  case  ERROR_SEEK_ON_DEVICE:   
    errno = EACCES;
    break;
  case  ERROR_DIR_NOT_EMPTY:
    errno = ENOTEMPTY;
    break;
  case  ERROR_NOT_LOCKED:
    errno = EACCES;
    break;
  case  ERROR_BAD_PATHNAME:
    errno = ENOENT;
    break;
  case  ERROR_MAX_THRDS_REACHED:
    errno = EAGAIN;
    break;
  case  ERROR_LOCK_FAILED:
    errno = EACCES;
    break;
  case  ERROR_ALREADY_EXISTS:
    errno = EEXIST;
    break;
  case  ERROR_FILENAME_EXCED_RANGE:
    errno = ENOENT;
    break;
  case  ERROR_NESTING_NOT_ALLOWED:
    errno = EAGAIN;
    break;
  case  ERROR_NOT_ENOUGH_QUOTA:
    errno = ENOMEM;
    break;
  default:
    errno = EINVAL;
    break;
  }
}
    
#endif



int
lisp_open(wchar_t *path, int flag, int mode)
{
  int fd;
  HANDLE hfile;
  DWORD dwDesiredAccess = 0;
  DWORD dwShareMode = 0;
  DWORD dwCreationDistribution = 0;
  DWORD dwFlagsAndAttributes = 0;
  SECURITY_ATTRIBUTES sa = {sizeof(SECURITY_ATTRIBUTES), NULL, TRUE};

  if ((flag & S_IREAD) == S_IREAD) {
    dwShareMode = FILE_SHARE_READ;
  } else {
    if ((flag & S_IWRITE) == S_IWRITE) {
      dwShareMode = FILE_SHARE_READ | FILE_SHARE_WRITE;
    }
  }

  if ((flag & _O_WRONLY) == _O_WRONLY) {
    dwDesiredAccess |= GENERIC_WRITE | FILE_WRITE_DATA |
      FILE_READ_ATTRIBUTES | FILE_WRITE_ATTRIBUTES;
  } else if ((flag & _O_RDWR) == _O_RDWR) {
    dwDesiredAccess |= GENERIC_WRITE|GENERIC_READ | FILE_READ_DATA |
      FILE_WRITE_DATA | FILE_READ_ATTRIBUTES | FILE_WRITE_ATTRIBUTES;
  } else {
    dwDesiredAccess |= GENERIC_READ | FILE_READ_DATA | FILE_READ_ATTRIBUTES |
      FILE_WRITE_ATTRIBUTES;
  }
    
  if ((flag & S_IREAD) == S_IREAD) {
    dwShareMode |= FILE_SHARE_READ;
  }
  if ((flag & S_IWRITE) == S_IWRITE) {
    dwShareMode |= FILE_SHARE_WRITE;
  }

  if ((flag & (_O_CREAT | _O_EXCL)) == (_O_CREAT | _O_EXCL)) {
    dwCreationDistribution |= CREATE_NEW;
  } else if ((flag &  O_TRUNC) == O_TRUNC) {
    if ((flag &  O_CREAT) ==  O_CREAT) {
      dwCreationDistribution |= CREATE_ALWAYS;
    } else if ((flag & O_RDONLY) != O_RDONLY) {
      dwCreationDistribution |= TRUNCATE_EXISTING;
    }
  } else if ((flag & _O_APPEND) == _O_APPEND) {
    dwCreationDistribution |= OPEN_EXISTING;
  } else if ((flag &  _O_CREAT) == _O_CREAT) {
    dwCreationDistribution |= OPEN_ALWAYS;
  } else {
    dwCreationDistribution |= OPEN_EXISTING;
  }
  if ((flag &  _O_RANDOM) == _O_RANDOM) {
    dwFlagsAndAttributes |= FILE_FLAG_RANDOM_ACCESS;
  }
  if ((flag &  _O_SEQUENTIAL) == _O_SEQUENTIAL) {
    dwFlagsAndAttributes |= FILE_FLAG_SEQUENTIAL_SCAN;
  }

  if ((flag &  _O_TEMPORARY) == _O_TEMPORARY) {
    dwFlagsAndAttributes |= FILE_FLAG_DELETE_ON_CLOSE;
  }

  if ((flag &  _O_SHORT_LIVED) == _O_SHORT_LIVED) {
    dwFlagsAndAttributes |= FILE_FLAG_DELETE_ON_CLOSE;
  }

  if (flag & _O_NOINHERIT) {
    sa.bInheritHandle = FALSE;
  }

#if 0
  dwFlagsAndAttributes |= FILE_FLAG_OVERLAPPED;
#endif
    

  hfile = CreateFileW(path,
                      dwDesiredAccess,
                      dwShareMode,
                      &sa,
                      dwCreationDistribution,
                      dwFlagsAndAttributes,
                      NULL);
  if (hfile == ((HANDLE)-1)) {
    _dosmaperr(GetLastError());
    return -errno;
  }
  fd = _open_osfhandle((intptr_t)hfile, flag);

  if (fd < 0) {
    CloseHandle(hfile);
  }
  return fd;
}

int
lisp_close(int fd)
{
  return close(fd);
}

ssize_t
lisp_read(int fd, void *buf, unsigned int count)
{
  HANDLE hfile;
  OVERLAPPED overlapped;
  DWORD err, nread;
  pending_io pending;
  TCR *tcr;
  extern TCR *get_tcr(int);

  hfile = (HANDLE) _get_osfhandle(fd);

  if (hfile == ((HANDLE)-1)) {
    errno = EBADF;
    return -1;
  }
  
  memset(&overlapped,0,sizeof(overlapped));

  if (GetFileType(hfile) == FILE_TYPE_DISK) {
    overlapped.Offset = SetFilePointer(hfile, 0, &(overlapped.OffsetHigh), FILE_CURRENT);
  }

  tcr = (TCR *)get_tcr(1);
  pending.h = hfile;
  pending.o = &overlapped;
  tcr->foreign_exception_status = (signed_natural)&pending;

  if (ReadFile(hfile, buf, count, &nread, &overlapped)) {
    tcr->foreign_exception_status = 0;
    return nread;
  }
  err = GetLastError();

  if (err == ERROR_HANDLE_EOF) {
    tcr->foreign_exception_status = 0;
    return 0;
  }

  if (err != ERROR_IO_PENDING) {
    _dosmaperr(err);
    tcr->foreign_exception_status = 0;
    return -1;
  }
  
  /* We block here */
  if (GetOverlappedResult(hfile, &overlapped, &nread, TRUE)) {
    tcr->foreign_exception_status = 0;
    return nread;
  }
  err = GetLastError();
  tcr->foreign_exception_status = 0;

  switch (err) {
  case ERROR_HANDLE_EOF: 
    return 0;
  case ERROR_OPERATION_ABORTED:
    errno = EINTR;
    return -1;
  default:
    _dosmaperr(err);
    return -1;
  }
}

ssize_t
lisp_write(int fd, void *buf, ssize_t count)
{
  HANDLE hfile;
  OVERLAPPED overlapped;
  DWORD err, nwritten;

  hfile = (HANDLE) _get_osfhandle(fd);

  if (hfile == ((HANDLE)-1)) {
    errno = EBADF;
    return -1;
  }

  memset(&overlapped,0,sizeof(overlapped));

  if (GetFileType(hfile) == FILE_TYPE_DISK) {
    overlapped.Offset = SetFilePointer(hfile, 0, &(overlapped.OffsetHigh), FILE_CURRENT);
  }

  if (WriteFile(hfile, buf, count, &nwritten, &overlapped)) {
    return nwritten;
  }
  
  err = GetLastError();
  _dosmaperr(err);
  return -1;
}

int
lisp_fchmod(int fd, int mode)
{
  errno = ENOSYS;
  return -1;
}

off_t
lisp_lseek(int fd, __int64 offset, int whence)
{
  return lseek64(fd, offset, whence);
}

int
lisp_stat(wchar_t *path, struct __stat64 *buf)
{
  return _wstat64(path,buf);
}

int
lisp_fstat(int fd, struct __stat64 *buf)
{
  return _fstat64(fd,buf);
}

int
lisp_futex(int *uaddr, int op, int val, void *timeout, int *uaddr2, int val3)
{
  errno = ENOSYS;
  return -1;
}


__int64
lisp_ftruncate(int fd, __int64 new_size)
{
  /* Note that _ftruncate only allows 32-bit length */
  return ftruncate(fd,(off_t)new_size);
}

_WDIR *
lisp_opendir(wchar_t *path)
{
  return _wopendir(path);
}

struct _wdirent *
lisp_readdir(_WDIR *dir)
{
  return _wreaddir(dir);
}

__int64
lisp_closedir(_WDIR *dir)
{
  return _wclosedir(dir);
}

int
lisp_pipe(int fd[2])
{
  HANDLE input, output;
  SECURITY_ATTRIBUTES sa;

  sa.nLength= sizeof(SECURITY_ATTRIBUTES);
  sa.lpSecurityDescriptor = NULL;
  sa.bInheritHandle = TRUE;

  if (!CreatePipe(&input, &output, &sa, 0))
    {
      wperror("CreatePipe");
      return -1;
    }
  fd[0] = _open_osfhandle((intptr_t)input, 0);
  fd[1] = _open_osfhandle((intptr_t)output, 0);
  return 0;
}

int
lisp_gettimeofday(struct timeval *tp, void *tzp)
{
  return gettimeofday(tp, tzp);
}

typedef struct {
  char *name;
  void *addr;
} math_fn_entry;


math_fn_entry math_fn_entries [] = {
  {"acos",acos},
  {"acosf",acosf},
  {"acosh",acosh},
  {"acoshf",acoshf},
  {"asin",asin},
  {"asinf",asinf},
  {"asinh",asinh},
  {"asinhf",asinhf},
  {"atan",atan},
  {"atan2",atan2},
  {"atan2f",atan2f},
  {"atanf",atanf},
  {"atanh",atanh},
  {"atanhf",atanhf},
  {"cos",cos},
  {"cosf",cosf},
  {"cosh",cosh},
  {"coshf",coshf},
  {"exp",exp},
  {"expf",expf},
  {"log",log},
  {"logf",logf},
  {"pow",pow},
  {"powf",powf},
  {"sin",sin},
  {"sinf",sinf},
  {"sinh",sinh},
  {"sinhf",sinhf},
  {"tan",tan},
  {"tanf",tanf},
  {"tanh",tanh},
  {"tanhf",tanhf},
  {NULL, 0}};

void *
lookup_math_fn(char *name)
{
  math_fn_entry *p = math_fn_entries;
  char *entry_name;
  
  while ((entry_name = p->name) != NULL) {
    if (!strcmp(name, entry_name)) {
      return p->addr;
    }
    p++;
  }
  return NULL;
}

HMODULE *modules = NULL;
DWORD cbmodules = 0;
HANDLE find_symbol_lock = 0;

void *
windows_find_symbol(void *handle, char *name)
{
  void *addr;

  if ((handle == ((void *)-2L)) ||
      (handle == ((void *)-1L))) {
    handle = NULL;
  }
  if (handle != NULL) {
    addr = GetProcAddress(handle, name);
  } else {
    DWORD cbneeded,  have, i;
    WaitForSingleObject(find_symbol_lock,INFINITE);

    if (cbmodules == 0) {
      cbmodules = 16 * sizeof(HANDLE);
      modules = malloc(cbmodules);
    }
    
    while (1) {
      EnumProcessModules(GetCurrentProcess(),modules,cbmodules,&cbneeded);
      if (cbmodules >= cbneeded) {
        break;
      }
      cbmodules = cbneeded;
      modules = realloc(modules,cbmodules);
    }
    have = cbneeded/sizeof(HANDLE);

    for (i = 0; i < have; i++) {
      addr = GetProcAddress(modules[i],name);

      if (addr) {
        break;
      }
    }
    ReleaseMutex(find_symbol_lock);
    if (addr) {
      return addr;
    }
    return lookup_math_fn(name);
  }
}

/* Note that we're using 8-bit strings here */

void *
windows_open_shared_library(char *path)
{
  HMODULE module = (HMODULE)0;

  /* Try to open an existing module in a way that increments its
     reference count without running any initialization code in
     the dll. */
  if (!GetModuleHandleExA(0,path,&module)) {
    /* If that failed ... */
    module = LoadLibraryA(path);
  }
  return (void *)module;
}


void
init_windows_io()
{
#ifdef WIN_32
  extern void init_win32_ldt(void);
  init_win32_ldt();
#endif
  find_symbol_lock = CreateMutex(NULL,false,NULL);
}

void
init_winsock()
{
  WSADATA data;

  WSAStartup((2<<8)|2,&data);
}
