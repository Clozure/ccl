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

/*
   These aren't really system calls; they're just C runtime functions
   that (a) are built in to the lisp, so they can be called early
   in the cold load, before the FFI is initialized and (b) return negated
   error code on failure, so that it's not necessary to separately
   fetch errno.

   It's reasonable to consider replacing these things with wrappers
   around native functionality (ReadFile, etc.) someday.

   The order of the entries in windows_syscall_table[] should match
   the order of syscall indices defined in
   "ccl:library;x86-win64-syscalls.lisp".

   One last time: these aren't really system calls.
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
#include <stdio.h>


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

#define WSYSCALL_RETURN(form) \
  do { \
    int __result = form; \
\
    if (__result < 0){ \
      return -errno; \
    } \
    return __result; \
  } while (0)



__int64
windows_open(wchar_t *path, int flag, int mode)
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

__int64
windows_close(int fd)
{
  WSYSCALL_RETURN(close(fd));
}

__int64
windows_read(int fd, void *buf, unsigned int count)
{
  HANDLE hfile;
  OVERLAPPED overlapped;
  DWORD err, nread;
  pending_io pending;
  TCR *tcr;
  extern TCR *get_tcr(int);

  hfile = (HANDLE) _get_osfhandle(fd);

  if (hfile == ((HANDLE)-1)) {
    return -EBADF;
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
    return -errno;
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
    return -EINTR;
  default:
    _dosmaperr(err);
    return -errno;
  }
}

__int64
windows_write(int fd, void *buf, unsigned int count)
{
  WSYSCALL_RETURN( _write(fd, buf, count));
}

__int64
windows_fchmod(int fd, int mode)
{
  return -ENOSYS;
}

__int64
windows_lseek(int fd, __int64 offset, int whence)
{
  WSYSCALL_RETURN(lseek64(fd, offset, whence));
}

__int64
windows_stat(wchar_t *path, struct __stat64 *buf)
{
  WSYSCALL_RETURN(_wstat64(path,buf));
}

__int64
windows_fstat(int fd, struct __stat64 *buf)
{
  WSYSCALL_RETURN(_fstat64(fd,buf));
}


__int64
windows_ftruncate(int fd, __int64 new_size)
{
  /* Note that _ftruncate only allows 32-bit length */
  WSYSCALL_RETURN(ftruncate(fd,(off_t)new_size));
}

_WDIR *
windows_opendir(wchar_t *path)
{
  return _wopendir(path);
}

struct _wdirent *
windows_readdir(_WDIR *dir)
{
  return _wreaddir(dir);
}

__int64
windows_closedir(_WDIR *dir)
{
  WSYSCALL_RETURN(_wclosedir(dir));
}

__int64
windows_pipe(int fd[2])
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

void *
windows_syscall_table[] = {
  windows_open,
  windows_close,
  windows_read,
  windows_write,
  windows_fchmod,
  windows_lseek,
  windows_stat,
  windows_fstat,
  windows_ftruncate,
  windows_opendir,
  windows_readdir,
  windows_closedir,
  windows_pipe
};

HMODULE *modules = NULL;
DWORD cbmodules = 0;

void *
windows_find_symbol(void *handle, char *name)
{
  if (handle == ((void *)-2L)) {
    handle = NULL;
  }
  if (handle != NULL) {
    return GetProcAddress(handle, name);
  } else {
    DWORD cbneeded,  have, i;

    if (cbmodules == 0) {
      cbmodules = 16 * sizeof(HANDLE);
      modules = LocalAlloc(LPTR, cbmodules);
    }
    
    while (1) {
      EnumProcessModules(GetCurrentProcess(),modules,cbmodules,&cbneeded);
      if (cbmodules >= cbneeded) {
        break;
      }
      cbmodules = cbneeded;
      modules = LocalReAlloc(modules,cbmodules,0);
    }
    have = cbneeded/sizeof(HANDLE);

    for (i = 0; i < have; i++) {
      void *addr = GetProcAddress(modules[i],name);

      if (addr) {
        return addr;
      }
    }
    return NULL;
  }
}

void
init_windows_io()
{
#if 0
  int fd;
  HANDLE hfile0, hfile1;

  hfile0 = (HANDLE) _get_osfhandle(0);
  hfile1 = ReOpenFile(hfile0,GENERIC_READ,FILE_SHARE_READ,FILE_FLAG_OVERLAPPED);
  if (hfile1 != ((HANDLE)-1)) {
    fd = _open_osfhandle(hfile1,O_RDONLY);
    dup2(fd,0);
    _close(fd);
    SetStdHandle(STD_INPUT_HANDLE,hfile1);
    CloseHandle(hfile0);
  } else {
    wperror("ReOpenFile");
  }

  hfile0 = (HANDLE) _get_osfhandle(1);
  hfile1 = ReOpenFile(hfile0,GENERIC_WRITE,FILE_SHARE_WRITE,FILE_FLAG_OVERLAPPED);
  if (hfile1 != ((HANDLE)-1)) {
    fd = _open_osfhandle(hfile1,O_WRONLY);
    dup2(fd,1);
    _close(fd);
    SetStdHandle(STD_OUTPUT_HANDLE,hfile1);
    CloseHandle(hfile0);
  }

  hfile0 = (HANDLE) _get_osfhandle(2);
  hfile1 = ReOpenFile(hfile0,GENERIC_WRITE,FILE_SHARE_WRITE,FILE_FLAG_OVERLAPPED);
  if (hfile1 != ((HANDLE)-1)) {
    fd = _open_osfhandle(hfile1,O_WRONLY);
    dup2(fd,2);
    _close(fd);
    SetStdHandle(STD_ERROR_HANDLE,hfile1);
    CloseHandle(hfile0);
  }
#endif  
}

void
init_winsock()
{
  WSADATA data;

  WSAStartup((2<<8)|2,&data);
}
