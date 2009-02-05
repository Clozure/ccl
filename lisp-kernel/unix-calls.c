/*
   Copyright (C) 2008, Clozure Associates and contributors
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

/* Provide wrappers around some standard C library functions that
   can't easily be called from CCL's FFI for some reason (or where
   we want to override/extend the function's default behavior.)
 
   Functions in this file should be referenced via the kernel
   imports table.

   Callers should generally expect standard C library error-handling
   conventions (e.g., return -1 or NULL and set errno on error.)
*/

#define _LARGEFILE64_SOURCE
#include <errno.h>
#include <unistd.h>
#include <sys/stat.h>
#include <dirent.h>
#include <sys/syscall.h>
#include <sys/time.h>
#include <stdint.h>

ssize_t
lisp_read(int fd, void *buf, size_t count)
{
  return read(fd,buf,count);
}

ssize_t
lisp_write(int fd, void *buf, size_t count)
{
  return write(fd,buf,count);
}

int
lisp_open(char *path, int flags, mode_t mode)
{
  return open(path,flags,mode);
}

int
lisp_fchmod(int fd, mode_t mode)
{
  return fchmod(fd,mode);
}

int64_t
lisp_lseek(int fd, int64_t offset, int whence)
{
#ifdef LINUX
  return lseek64(fd,offset,whence);
#else
  return lseek(fd,offset,whence);
#endif
}

int
lisp_close(int fd)
{
  return close(fd);
}

int
lisp_ftruncate(int fd, off_t length)
{
  return ftruncate(fd,length);
}

int
lisp_stat(char *path, void *buf)
{
  return stat(path,buf);
}

int
lisp_fstat(int fd, void *buf)
{
  return fstat(fd,buf);
}


int
lisp_futex(int *uaddr, int op, int val, void *timeout, int *uaddr2, int val3)
{
#ifdef LINUX
  return syscall(SYS_futex,uaddr,op,val,timeout,uaddr2,val3);
#else
  errno = ENOSYS;
  return -1;
#endif
}

DIR *
lisp_opendir(char *path)
{
  return opendir(path);
}

struct dirent *
lisp_readdir(DIR *dir)
{
  return readdir(dir);
}

int
lisp_closedir(DIR *dir)
{
  return closedir(dir);
}

int
lisp_pipe(int pipefd[2])
{
  return pipe(pipefd);
}

int
lisp_gettimeofday(struct timeval *tp, void *tzp)
{
  return gettimeofday(tp, tzp);
}
