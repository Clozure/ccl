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
#if __FreeBSD_version >= 1200031
/*
 * ino_t, dev_t, nlink_t were extended to 64 bits (thereby changing
 * struct stat). struct dirent changed also.
 *
 * Make the old versions available as freebsd11_stat and freebsd11_dirent.
 * XXX - this should go away when FreeBSD 12 headers are generated
 */
#define _WANT_FREEBSD11_STAT
#define _WANT_FREEBSD11_DIRENT
#include <string.h>
#endif
#include <sys/stat.h>
#include <dirent.h>
#include <sys/syscall.h>
#include <sys/time.h>
#include <stdint.h>
#include <signal.h>
#include <fcntl.h>

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

#if __FreeBSD_version >= 1200031
/*
 * struct stat has changed.  As a temporary workaround until
 * we can generate headers for FreeBSD 12, return stat(2) data in
 * the old version of struct stat that the lisp expects.
 */
void
populate_old_stat(struct stat *sb, struct freebsd11_stat *sb11)
{
    memset(sb11, 0, sizeof(*sb11));
    sb11->st_dev = sb->st_dev;
    sb11->st_ino = sb->st_ino;
    sb11->st_mode = sb->st_mode;
    sb11->st_nlink = sb->st_nlink;
    sb11->st_uid = sb->st_uid;
    sb11->st_gid = sb->st_gid;
    sb11->st_rdev = sb->st_rdev;
    sb11->st_atim = sb->st_atim;
    sb11->st_mtim = sb->st_mtim;
    sb11->st_ctim = sb->st_ctim;
    sb11->st_size = sb->st_size;
    sb11->st_blocks = sb->st_blocks;
    sb11->st_blksize = sb->st_blksize;
    sb11->st_flags = sb->st_flags;
    sb11->st_gen = sb->st_gen;
    sb11->st_birthtim = sb->st_birthtim;
}

int
lisp_stat(char *path, void *buf)
{
  struct stat sb;
  int ret;

  ret = stat(path, &sb);
  if (ret == 0) {
    populate_old_stat(&sb, buf);
    return 0;
  } else {
    return -1;
  }
}

int
lisp_fstat(int fd, void *buf)
{
  struct stat sb;
  int ret;

  ret = fstat(fd, &sb);
  if (ret == 0) {
    populate_old_stat(&sb, buf);
    return 0;
  } else {
    return -1;
  }
}

int
lisp_lstat(char *path, void *buf)
{
  struct stat sb;
  int ret;

  ret = lstat(path, &sb);
  if (ret == 0) {
    populate_old_stat(&sb, buf);
    return 0;
  } else {
    return -1;
  }
}

#else

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
#endif

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

#if __FreeBSD_version >= 1200031
/*
 * struct dirent has changed.  Lisp only cares about :dirent.d_name,
 * so make that available in the old struct dirent.
 * Again, this will all go away when we generate new interfaces for
 * FreeBSD 12.
 */
static __thread struct freebsd11_dirent old_dirent;

struct freebsd11_dirent *
lisp_readdir(DIR *dir)
{
  struct dirent *dp;
  struct freebsd11_dirent d11;

  dp = readdir(dir);
  if (dp != NULL) {
    memset(&old_dirent, 0, sizeof(old_dirent));
    strcpy(old_dirent.d_name, dp->d_name);
    return &old_dirent;
  } else {
    return NULL;
  }
}
#else
struct dirent *
lisp_readdir(DIR *dir)
{
  return readdir(dir);
}
#endif

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
