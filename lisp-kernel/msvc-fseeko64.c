/**
* This file has no copyright assigned and is placed in the Public Domain.
* This file is modified from fseeko64.c from the mingw-w64 runtime package.
* No warranty is given; refer to the file DISCLAIMER.PD within this package.
*/
#include <errno.h>

struct oserr_map {
	unsigned long oscode; /* OS values */
	int errnocode; /* System V codes */
};

static struct oserr_map local_errtab[] = {
	{ ERROR_INVALID_FUNCTION, EINVAL },{ ERROR_FILE_NOT_FOUND, ENOENT },
	{ ERROR_PATH_NOT_FOUND, ENOENT },{ ERROR_TOO_MANY_OPEN_FILES, EMFILE },
	{ ERROR_ACCESS_DENIED, EACCES },{ ERROR_INVALID_HANDLE, EBADF },
	{ ERROR_ARENA_TRASHED, ENOMEM },{ ERROR_NOT_ENOUGH_MEMORY, ENOMEM },
	{ ERROR_INVALID_BLOCK, ENOMEM },{ ERROR_BAD_ENVIRONMENT, E2BIG },
	{ ERROR_BAD_FORMAT, ENOEXEC },{ ERROR_INVALID_ACCESS, EINVAL },
	{ ERROR_INVALID_DATA, EINVAL },{ ERROR_INVALID_DRIVE, ENOENT },
	{ ERROR_CURRENT_DIRECTORY, EACCES },{ ERROR_NOT_SAME_DEVICE, EXDEV },
	{ ERROR_NO_MORE_FILES, ENOENT },{ ERROR_LOCK_VIOLATION, EACCES },
	{ ERROR_BAD_NETPATH, ENOENT },{ ERROR_NETWORK_ACCESS_DENIED, EACCES },
	{ ERROR_BAD_NET_NAME, ENOENT },{ ERROR_FILE_EXISTS, EEXIST },
	{ ERROR_CANNOT_MAKE, EACCES },{ ERROR_FAIL_I24, EACCES },
	{ ERROR_INVALID_PARAMETER, EINVAL },{ ERROR_NO_PROC_SLOTS, EAGAIN },
	{ ERROR_DRIVE_LOCKED, EACCES },{ ERROR_BROKEN_PIPE, EPIPE },
	{ ERROR_DISK_FULL, ENOSPC },{ ERROR_INVALID_TARGET_HANDLE, EBADF },
	{ ERROR_INVALID_HANDLE, EINVAL },{ ERROR_WAIT_NO_CHILDREN, ECHILD },
	{ ERROR_CHILD_NOT_COMPLETE, ECHILD },{ ERROR_DIRECT_ACCESS_HANDLE, EBADF },
	{ ERROR_NEGATIVE_SEEK, EINVAL },{ ERROR_SEEK_ON_DEVICE, EACCES },
	{ ERROR_DIR_NOT_EMPTY, ENOTEMPTY },{ ERROR_NOT_LOCKED, EACCES },
	{ ERROR_BAD_PATHNAME, ENOENT },{ ERROR_MAX_THRDS_REACHED, EAGAIN },
	{ ERROR_LOCK_FAILED, EACCES },{ ERROR_ALREADY_EXISTS, EEXIST },
	{ ERROR_FILENAME_EXCED_RANGE, ENOENT },{ ERROR_NESTING_NOT_ALLOWED, EAGAIN },
	{ ERROR_NOT_ENOUGH_QUOTA, ENOMEM },{ 0, -1 }
};

void mingw_dosmaperr(unsigned long oserrno)
{
	size_t i;

	_doserrno = oserrno;        /* set _doserrno */
								/* check the table for the OS error code */
	i = 0;
	do {
		if (oserrno == local_errtab[i].oscode)
		{
			errno = local_errtab[i].errnocode;
			return;
		}
	} while (local_errtab[++i].errnocode != -1);
	if (oserrno >= ERROR_WRITE_PROTECT && oserrno <= ERROR_SHARING_BUFFER_EXCEEDED)
		errno = EACCES;
	else if (oserrno >= ERROR_INVALID_STARTING_CODESEG && oserrno <= ERROR_INFLOOP_IN_RELOC_CHAIN)
		errno = ENOEXEC;
	else
		errno = EINVAL;
}
