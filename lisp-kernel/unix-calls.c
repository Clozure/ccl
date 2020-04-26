/*
 * Copyright 2008-2009 Clozure Associates
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

/* Provide wrappers around some standard C library functions that
   can't easily be called from CCL's FFI for some reason (or where
   we want to override/extend the function's default behavior.)
 
   Functions in this file should be referenced via the kernel
   imports table.

   Callers should generally expect standard C library error-handling
   conventions (e.g., return -1 or NULL and set errno on error.)
*/

#ifndef _LARGEFILE64_SOURCE_
#define _LARGEFILE64_SOURCE_
#endif
#include <errno.h>
#include <unistd.h>
#include <sys/stat.h>
#include <dirent.h>
#include <sys/syscall.h>
#include <sys/time.h>
#include <stdint.h>
#include <signal.h>
#include <fcntl.h>
#include <stdlib.h>

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
lisp_lstat(char *path, void *buf)
{
  return lstat(path, buf);
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

int
lisp_sigexit(int signum)
{
  signal(signum, SIG_DFL);
  return kill(getpid(), signum);
}

#ifdef ANDROID_NEEDS_SIGALTSTACK
/* I for one welcome our new Android overlords. */
#ifndef __NR_sigaltstack
#define __NR_sigaltstack		(__NR_SYSCALL_BASE+186)
#endif
int
sigaltstack(stack_t *in, stack_t *out)
{
  return syscall(__NR_sigaltstack,in,out);
}
#endif

char *
lisp_realpath(const char *file_name, char *resolved_name)
{
  return realpath(file_name, resolved_name);
}
