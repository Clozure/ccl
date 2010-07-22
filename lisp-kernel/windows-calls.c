/*
   Copyright (C) 2008-2009, Clozure Associates and contributors,
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
#include <signal.h>
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
  case ERROR_OPERATION_ABORTED:
    errno = EINTR;
    break;
  default:
    errno = EINVAL;
    break;
  }
}
    
#endif

#define MAX_FD 32

HANDLE
lisp_open(wchar_t *path, int flag, int mode)
{
  HANDLE hfile;
  DWORD dwDesiredAccess = 0;
  DWORD dwShareMode = 0;
  DWORD dwCreationDistribution = 0;
  DWORD dwFlagsAndAttributes = 0;
  SECURITY_ATTRIBUTES sa = {sizeof(SECURITY_ATTRIBUTES), NULL, TRUE};

  dwShareMode = FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE;

  if ((flag & _O_WRONLY) == _O_WRONLY) {
    dwDesiredAccess |= GENERIC_WRITE;
  } else if ((flag & _O_RDWR) == _O_RDWR) {
    dwDesiredAccess |= GENERIC_WRITE|GENERIC_READ;
  } else {
    dwDesiredAccess |= GENERIC_READ;
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
    return (HANDLE)-1;
  }
  return hfile;
}

int
wopen(wchar_t *path, int flag, int mode)
{
  HANDLE h = lisp_open(path, flag, mode);

  if (h == (HANDLE)-1) {
    return -1;                  /* errno already set */
  }
  return  _open_osfhandle((intptr_t)h,0);
}

int
lisp_close(HANDLE hfile)
{
  int err;

  if (closesocket((SOCKET)hfile) == 0) {
    return 0;
  }

  err = WSAGetLastError();
  if (err != WSAENOTSOCK) {
    _dosmaperr(err);
    return -1;
  }
  if (CloseHandle(hfile)) {
    return 0;
  }
  _dosmaperr(GetLastError());
  return -1;
}

extern TCR *get_tcr(int);

ssize_t
lisp_standard_read(HANDLE hfile, void *buf, unsigned int count)
{
  HANDLE hevent;
  OVERLAPPED overlapped;
  DWORD err, nread, wait_result;
  pending_io pending;
  TCR *tcr;
  
  
  memset(&overlapped,0,sizeof(overlapped));

  if (GetFileType(hfile) == FILE_TYPE_DISK) {
    overlapped.Offset = SetFilePointer(hfile, 0, &(overlapped.OffsetHigh), FILE_CURRENT);
  }

  tcr = (TCR *)get_tcr(1);
  pending.h = hfile;
  pending.o = &overlapped;
  tcr->pending_io_info = &pending;
  hevent = (HANDLE)(tcr->io_datum);
  overlapped.hEvent = hevent;
  ResetEvent(hevent);
  if (ReadFile(hfile, buf, count, &nread, &overlapped)) {
    tcr->pending_io_info = NULL;
    return nread;
  }

  err = GetLastError();
  
  if (err == ERROR_HANDLE_EOF) {
    tcr->pending_io_info = NULL;
    return 0;
  }

  if (err != ERROR_IO_PENDING) {
    _dosmaperr(err);
    tcr->pending_io_info = NULL;
    return -1;
  }
  
  err = 0;
  
  /* We block here */    
  wait_result = WaitForSingleObjectEx(hevent, INFINITE, true);



  tcr->pending_io_info = NULL;
  if (wait_result == WAIT_OBJECT_0) {
    err = overlapped.Internal;
    if (err == ERROR_HANDLE_EOF) {
      return 0;
    }
    if (err) {
      _dosmaperr(err);
      return -1;
    }
    return overlapped.InternalHigh;
  }

  if (wait_result == WAIT_IO_COMPLETION) {
    CancelIo(hfile);
    errno = EINTR;
    return -1;
  }
  err = GetLastError();
  

  switch (err) {
  case ERROR_HANDLE_EOF: 
    return 0;
  default:
    _dosmaperr(err);
    return -1;
  }
}

ssize_t
pipe_read(HANDLE hfile, void *buf, unsigned int count)
{
  DWORD navail, err;;

  do {
    navail = 0;
    if (PeekNamedPipe(hfile, NULL, 0, NULL, &navail, NULL) == 0) {
      err = GetLastError();
      if (err = ERROR_HANDLE_EOF) {
        return 0;
      } else {
        _dosmaperr(err);
        return -1;
      }
    }
    if (navail != 0) {
      return lisp_standard_read(hfile, buf, count);
    }
    if (SleepEx(50, TRUE) == WAIT_IO_COMPLETION) {
      errno = EINTR;
      return -1;
    }
  } while (1);
}

ssize_t
console_read(HANDLE hfile, void *buf, unsigned int count)
{
  DWORD err, eventcount, i, n;
  INPUT_RECORD ir;

  do {
    err = WaitForSingleObjectEx(hfile, INFINITE, TRUE);
    switch (err) {
    case WAIT_OBJECT_0:
      eventcount = 0;
      GetNumberOfConsoleInputEvents(hfile, &eventcount);
      for (i = 0; i < eventcount; i++) {
        PeekConsoleInput(hfile, &ir, 1, &n);
        if (ir.EventType == KEY_EVENT) {
          return lisp_standard_read(hfile, buf, count);
        } else {
          ReadConsoleInput(hfile, &ir, 1, &n);
        }
      }
      break;
    case WAIT_IO_COMPLETION:
      errno = EINTR;
      return -1;
      break;
    case WAIT_FAILED:
      _dosmaperr(GetLastError());
      return -1;
      break;
    }
  } while (1);
}

ssize_t
lisp_read(HANDLE hfile, void *buf, unsigned int count) {
  switch(GetFileType(hfile)) {
  case FILE_TYPE_CHAR:
    return console_read(hfile, buf, count);
    break;

  case FILE_TYPE_PIPE:          /* pipe or one of these newfangled socket things */
    {
      int socktype, optlen = sizeof(int);
      if ((getsockopt((SOCKET)hfile, SOL_SOCKET, SO_TYPE, (char *)&socktype, &optlen) != 0) && (GetLastError() == WSAENOTSOCK)) {
        return pipe_read(hfile, buf, count);
      }
    }
    /* It's a socket, fall through */
    
  case FILE_TYPE_DISK:
    return lisp_standard_read(hfile, buf, count);
    break;

  default:
    errno = EBADF;
    return -1;
  }
}



ssize_t
lisp_write(HANDLE hfile, void *buf, ssize_t count)
{
  HANDLE hevent;
  OVERLAPPED overlapped;
  DWORD err, nwritten, wait_result;
  pending_io pending;
  TCR *tcr = (TCR *)get_tcr(1);

  hevent = (HANDLE)tcr->io_datum;
  if (hfile == (HANDLE)1) {
    hfile = GetStdHandle(STD_OUTPUT_HANDLE);
  } else if (hfile == (HANDLE) 2) {
    hfile = GetStdHandle(STD_ERROR_HANDLE);
  }


  memset(&overlapped,0,sizeof(overlapped));

  if (GetFileType(hfile) == FILE_TYPE_DISK) {
    overlapped.Offset = SetFilePointer(hfile, 0, &(overlapped.OffsetHigh), FILE_CURRENT);
  }


  pending.h = hfile;
  pending.o = &overlapped;
  tcr->pending_io_info = &pending;
  overlapped.hEvent = hevent;
  ResetEvent(hevent);
  if (WriteFile(hfile, buf, count, &nwritten, &overlapped)) {
    tcr->pending_io_info = NULL;
    return nwritten;
  }
  
  err = GetLastError();
  if (err != ERROR_IO_PENDING) {
    _dosmaperr(err);
    tcr->pending_io_info = NULL;
    return -1;
  }
  err = 0;
  wait_result = WaitForSingleObjectEx(hevent, INFINITE, true);
  tcr->pending_io_info = NULL;
  if (wait_result == WAIT_OBJECT_0) {
    err = overlapped.Internal;
    if (err) {
      _dosmaperr(err);
      return -1;
    }
    return overlapped.InternalHigh;
  }
  if (wait_result == WAIT_IO_COMPLETION) {
    CancelIo(hfile);
    errno = EINTR;
    return -1;
  }
  err = GetLastError();
  _dosmaperr(err);
  return -1;
}

int
lisp_fchmod(HANDLE hfile, int mode)
{
  errno = ENOSYS;
  return -1;
}

__int64
lisp_lseek(HANDLE hfile, __int64 offset, int whence)
{
  DWORD high, low;

  high = ((__int64)offset)>>32;
  low = offset & 0xffffffff;
  low = SetFilePointer(hfile, low, &high, whence);
  if (low != INVALID_SET_FILE_POINTER) {
    return ((((__int64)high)<<32)|low);
  }
  _dosmaperr(GetLastError());
  return -1;
}

#define ALL_USERS(f) ((f) | ((f)>> 3) | ((f >> 6)))
#define STAT_READONLY ALL_USERS(_S_IREAD)
#define STAT_READWRITE ALL_USERS((_S_IREAD|_S_IWRITE))
int
lisp_stat(wchar_t *path, struct __stat64 *buf)
{
  return _wstat64(path,buf);
}

#define UNIX_EPOCH_IN_WINDOWS_EPOCH  116444736000000000LL

__time64_t
filetime_to_unix_time(FILETIME *ft)
{
  __time64_t then = *((__time64_t *) ft);

  then -= UNIX_EPOCH_IN_WINDOWS_EPOCH;
  return then/10000000;
}

int
lisp_fstat(HANDLE hfile, struct __stat64 *buf)
{
  int filetype;

  filetype = GetFileType(hfile) & ~FILE_TYPE_REMOTE;

  if (filetype == FILE_TYPE_UNKNOWN) {
    errno = EBADF;
    return -1;
  }

  memset(buf, 0, sizeof(*buf));
  buf->st_nlink = 1;
  
  switch(filetype) {
  case FILE_TYPE_CHAR:
  case FILE_TYPE_PIPE:
    if (filetype == FILE_TYPE_CHAR) {
      buf->st_mode = _S_IFCHR;
    } else {
      buf->st_mode = _S_IFIFO;
    }
    break;
  case FILE_TYPE_DISK:
    {
      BY_HANDLE_FILE_INFORMATION info;

      if (!GetFileInformationByHandle(hfile, &info)) {
        _dosmaperr(GetLastError());
        return -1;
      }

      if (info.dwFileAttributes & FILE_ATTRIBUTE_READONLY) {
        buf->st_mode = STAT_READONLY;
      } else {
        buf->st_mode = STAT_READWRITE;
      }
      buf->st_mode |= _S_IFREG;
      buf->st_size = ((((__int64)(info.nFileSizeHigh))<<32LL) |
                      ((__int64)(info.nFileSizeLow)));
      buf->st_mtime = filetime_to_unix_time(&info.ftLastWriteTime);
      buf->st_atime = filetime_to_unix_time(&info.ftLastAccessTime);
      buf->st_ctime = filetime_to_unix_time(&info.ftCreationTime);
    }
    break;
  case FILE_TYPE_UNKNOWN:
  default:
    errno = EBADF;
    return -1;
  }
  return 0;
}

int
lisp_futex(int *uaddr, int op, int val, void *timeout, int *uaddr2, int val3)
{
  errno = ENOSYS;
  return -1;
}


__int64
lisp_ftruncate(HANDLE hfile, off_t new_size)
{
  __int64 oldpos;


  oldpos = lisp_lseek(hfile, 0, SEEK_END);
  if (oldpos == -1) {
    return 0;
  }
  if (oldpos < new_size) {
    char buf[4096];
    __int64 n = new_size-oldpos;
    DWORD nwritten, to_write;

    memset(buf,0,sizeof(buf));
    while(n) {
      if (n > 4096LL) {
        to_write = 4096;
      } else {
        to_write = n;
      }
      if (!WriteFile(hfile,buf,to_write,&nwritten,NULL)) {
        _dosmaperr(GetLastError());
        return -1;
      }
      n -= nwritten;
    }
    return 0;
  }
  lisp_lseek(hfile, new_size, SEEK_SET);
  if (SetEndOfFile(hfile)) {
    return 0;
  }
  _dosmaperr(GetLastError());
  return -1;
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
  fd[0] = (int) ((intptr_t)input);
  fd[1] = (int) ((intptr_t)output);
  return 0;
}

int
lisp_gettimeofday(struct timeval *tp, void *tzp)
{
  __time64_t now;

  gettimeofday(tp,tzp);       /* trust it to get time zone right, at least */
  GetSystemTimeAsFileTime((FILETIME*)&now);
  now -= UNIX_EPOCH_IN_WINDOWS_EPOCH;
  now /= 10000;               /* convert time to milliseconds */
  tp->tv_sec = now/1000LL;
  tp->tv_usec = 1000 * (now%1000LL); /* convert milliseconds to microseconds */
  return 0;
}

int
lisp_sigexit(int signum)
{
  signal(signum, SIG_DFL);
  return raise(signum);
}

/* Make sure that the lisp calls these functions, when they do something */
/* This code is taken from the 32-bit mingw library and is in the
   public domain */
double
acosh(double x)
{
  if (isnan (x)) 
    return x;

  if (x < 1.0)
    {
      errno = EDOM;
      return nan("");
    }

  if (x > 0x1p32)
    /*  Avoid overflow (and unnecessary calculation when
        sqrt (x * x - 1) == x). GCC optimizes by replacing
        the long double M_LN2 const with a fldln2 insn.  */ 
    return log (x) + 6.9314718055994530941723E-1L;

  /* Since  x >= 1, the arg to log will always be greater than
     the fyl2xp1 limit (approx 0.29) so just use logl. */ 
  return log (x + sqrt((x + 1.0) * (x - 1.0)));
}

float
acoshf(float x)
{
  if (isnan (x)) 
    return x;
  if (x < 1.0f)
    {
      errno = EDOM;
      return nan("");
    }

 if (x > 0x1p32f)
    /*  Avoid overflow (and unnecessary calculation when
        sqrt (x * x - 1) == x). GCC optimizes by replacing
        the long double M_LN2 const with a fldln2 insn.  */ 
    return log (x) + 6.9314718055994530941723E-1L;

  /* Since  x >= 1, the arg to log will always be greater than
     the fyl2xp1 limit (approx 0.29) so just use logl. */ 
  return log (x + sqrt((x + 1.0) * (x - 1.0)));
}

double
asinh(double x)
{
  double z;
  if (!isfinite (x))
    return x;
  z = fabs (x);

  /* Avoid setting FPU underflow exception flag in x * x. */
#if 0
  if ( z < 0x1p-32)
    return x;
#endif

  /* Use log1p to avoid cancellation with small x. Put
     x * x in denom, so overflow is harmless. 
     asinh(x) = log1p (x + sqrt (x * x + 1.0) - 1.0)
              = log1p (x + x * x / (sqrt (x * x + 1.0) + 1.0))  */

  z = log1p (z + z * z / (sqrt (z * z + 1.0) + 1.0));

  return ( x >= 0.0 ? z : -z);
}

float
asinhf(float x)
{
  float z;
  if (!isfinite (x))
    return x;
  z = fabsf (x);

  /* Avoid setting FPU underflow exception flag in x * x. */
#if 0
  if ( z < 0x1p-32)
    return x;
#endif


  /* Use log1p to avoid cancellation with small x. Put
     x * x in denom, so overflow is harmless. 
     asinh(x) = log1p (x + sqrt (x * x + 1.0) - 1.0)
              = log1p (x + x * x / (sqrt (x * x + 1.0) + 1.0))  */

  z = log1p (z + z * z / (sqrt (z * z + 1.0) + 1.0));

  return ( x >= 0.0 ? z : -z);
}

double
atanh(double x)
{
  double z;
  if (isnan (x))
    return x;
  z = fabs (x);
  if (z == 1.0)
    {
      errno  = ERANGE;
      return (x > 0 ? INFINITY : -INFINITY);
    }
  if (z > 1.0)
    {
      errno = EDOM;
      return nan("");
    }
  /* Rearrange formula to avoid precision loss for small x.

  atanh(x) = 0.5 * log ((1.0 + x)/(1.0 - x))
	   = 0.5 * log1p ((1.0 + x)/(1.0 - x) - 1.0)
           = 0.5 * log1p ((1.0 + x - 1.0 + x) /(1.0 - x)) 
           = 0.5 * log1p ((2.0 * x ) / (1.0 - x))  */
  z = 0.5 * log1p ((z + z) / (1.0 - z));
  return x >= 0 ? z : -z;
}

float
atanhf(float x)
{
  float z;
  if (isnan (x))
    return x;
  z = fabsf (x);
  if (z == 1.0)
    {
      errno  = ERANGE;
      return (x > 0 ? INFINITY : -INFINITY);
    }
  if ( z > 1.0)
    {
      errno = EDOM;
      return nanf("");
    }
  /* Rearrange formula to avoid precision loss for small x.

  atanh(x) = 0.5 * log ((1.0 + x)/(1.0 - x))
	   = 0.5 * log1p ((1.0 + x)/(1.0 - x) - 1.0)
           = 0.5 * log1p ((1.0 + x - 1.0 + x) /(1.0 - x)) 
           = 0.5 * log1p ((2.0 * x ) / (1.0 - x))  */
  z = 0.5 * log1p ((z + z) / (1.0 - z));
  return x >= 0 ? z : -z;
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

