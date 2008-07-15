;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2008 Clozure Associates and contributors
;;;   This file is part of OpenMCL.  
;;;
;;;   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with OpenMCL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   OpenMCL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

(in-package "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "SYSCALL"))

(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::syscall 0 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::exit 1 (:int) :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::forkall 2 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::read 3 (:int :address :size_t) :ssize_t)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::write 4 (:int :address :size_t) :ssize_t)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::open 5 (:address :int :mode_t) :int :min-args 2)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::close 6 (:int) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::wait 7 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::creat 8 (:address :mode_t) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::link 9 (:address :address) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::unlink 10 (:address) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::exec 11 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::chdir 12 (:address) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::time 13 (:address) :time_t)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::mknod 14 (:address :mode_t :dev_t) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::chmod 15 (:address :mode_t) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::chown 16 (:address :uid_t :gid_t) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::brk 17 (:address) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::stat 18 (:address :address) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lseek 19 (:int :off_t :int) :off_t)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getpid 20 () :pid_t)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::mount 21 (:address :address :int :address :address :int :adress :int) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::umount 22 (:address) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::setuid 23 (:uid_t) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getuid 24 () :uid_t)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::stime 25 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::pcsample 26 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::alarm 27 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::fstat 28 (:int :address) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::pause 29 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::utime 30 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::stty 31 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::gtty 32 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::access 33 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::nice 34 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::statfs 35 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sync 36 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::kill 37 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::fstatfs 38 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::pgrpsys 39 ())
 #||
 * subcodes:
 * getpgrp()  :: syscall(39,0)
 * setpgrp()  :: syscall(39,1)
 * getsid(pid)  :: syscall(39,2,pid)
 * setsid()  :: syscall(39,3)
 * getpgid(pid)  :: syscall(39,4,pid)
 * setpgid(pid,pgid) :: syscall(39,5,pid,pgid)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::uucopystr 40 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::dup 41 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::pipe 42 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::times 43 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::profil 44 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::plock 45 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::setgid 46 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getgid 47 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::signal 48 ())
 #||
 * subcodes:
 * signal(sig, f) :: signal(sig, f)  ((sig&SIGNO_MASK) == sig)
 * sigset(sig, f) :: signal(sig|SIGDEFER, f)
 * sighold(sig)  :: signal(sig|SIGHOLD)
 * sigrelse(sig) :: signal(sig|SIGRELSE)
 * sigignore(sig) :: signal(sig|SIGIGNORE)
 * sigpause(sig) :: signal(sig|SIGPAUSE)
 * see <sys/signal.h>
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::msgsys 49 ())
 #||
 * subcodes:
 * msgget(...) :: msgsys(0, ...)
 * msgctl(...) :: msgsys(1, ...)
 * msgrcv(...) :: msgsys(2, ...)
 * msgsnd(...) :: msgsys(3, ...)
 * msgids(...) :: msgsys(4, ...)
 * msgsnap(...) :: msgsys(5, ...)
 * see <sys/msg.h>
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sysi86 50 ())
 #||
 * subcodes:
 * sysi86(code, ...)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::acct 51 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::shmsys 52 ())
 #||
 * subcodes:
 * shmat (...) :: shmsys(0, ...)
 * shmctl(...) :: shmsys(1, ...)
 * shmdt (...) :: shmsys(2, ...)
 * shmget(...) :: shmsys(3, ...)
 * shmids(...) :: shmsys(4, ...)
 * see <sys/shm.h>
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::semsys 53 ())
 #||
 * subcodes:
 * semctl(...) :: semsys(0, ...)
 * semget(...) :: semsys(1, ...)
 * semop (...) :: semsys(2, ...)
 * semids(...) :: semsys(3, ...)
 * semtimedop(...) :: semsys(4, ...)
 * see <sys/sem.h>
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::ioctl 54 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::uadmin 55 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::utssys 57 ())
 #||
 * subcodes (third argument):
 * uname(obuf) (obsolete)  :: syscall(57, obuf, ign, 0)
 *   subcode 1 unused
 * ustat(dev, obuf)  :: syscall(57, obuf, dev, 2)
 * fusers(path, flags, obuf) :: syscall(57, path, flags, 3, obuf)
 * see <sys/utssys.h>
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::fdsync 58 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::execve 59 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::umask 60 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::chroot 61 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::fcntl 62 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::ulimit 63 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::reserved_64 64 #|| 64 reserved ||# ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::reserved_65 65 #|| 65 reserved ||# ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::reserved_66 66 #|| 66 reserved ||# ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::reserved_67 67 #|| 67 reserved ||# ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::reserved_68 68 #|| 68 reserved ||# ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::reserved_69 69 #|| 69 reserved ||# ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::tasksys 70 ())
 #||
 * subcodes:
 * settaskid(...) :: tasksys(0, ...)
 * gettaskid(...) :: tasksys(1, ...)
 * getprojid(...) :: tasksys(2, ...)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::acctctl 71 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::exacctsys 72 ())
 #||
 * subcodes:
 * getacct(...) :: exacct(0, ...)
 * putacct(...) :: exacct(1, ...)
 * wracct(...) :: exacct(2, ...)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getpagesizes 73 ())
 #||
 * subcodes:
 * getpagesizes2(...) :: getpagesizes(0, ...)
 * getpagesizes(...) :: getpagesizes(1, ...) legacy
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::rctlsys 74 ())
 #||
 * subcodes:
 * getrctl(...) :: rctlsys(0, ...)
 * setrctl(...) :: rctlsys(1, ...)
 * rctllist(...) :: rctlsys(2, ...)
 * rctlctl(...) :: rctlsys(3, ...)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sidsys 75 ())
 #||
 * subcodes:
 * allocids(...) :: sidsys(0, ...)
 * idmap_reg(...) :: sidsys(1, ...)
 * idmap_unreg(...) :: sidsys(2, ...)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::fsat 76 ())
 #||
 * subcodes:
 * openat(...) :: fsat(0, ...)
 * openat64(...) :: fsat(1, ...)
 * fstatat64(...) :: fsat(2, ...)
 * fstatat(...) :: fsat(3, ...)
 * renameat(...) :: fsat(4, ...)
 * fchownat(...) :: fsat(5, ...)
 * unlinkat(...) :: fsat(6, ...)
 * futimesat(...) :: fsat(7, ...)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_park 77 ())
 #||
 * subcodes:
 * _lwp_park(timespec_t *, lwpid_t) :: syslwp_park(0, ...)
 * _lwp_unpark(lwpid_t, int) :: syslwp_park(1, ...)
 * _lwp_unpark_all(lwpid_t *, int) :: syslwp_park(2, ...)
 * _lwp_unpark_cancel(lwpid_t *, int) :: syslwp_park(3, ...)
 * _lwp_set_park(lwpid_t *, int)  :: syslwp_park(4, ...)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sendfilev 78 ())
 #||
 * subcodes :
 * sendfilev()  :: sendfilev(0, ...)
 * sendfilev64() :: sendfilev(1, ...)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::rmdir 79 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::mkdir 80 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getdents 81 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::privsys 82 ())
 #||
 * subcodes:
 * setppriv(...) :: privsys(0, ...)
 * getppriv(...) :: privsys(1, ...)
 * getimplinfo(...) :: privsys(2, ...)
 * setpflags(...)  :: privsys(3, ...)
 * getpflags(...)  :: privsys(4, ...)
 * issetugid(); :: privsys(5)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::ucredsys 83 ())
 #||
 * subcodes:
 * ucred_get(...) :: ucredsys(0, ...)
 * getpeerucred(...) :: ucredsys(1, ...)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sysfs 84 ())
 #||
 * subcodes:
 * sysfs(code, ...)
 * see <sys/fstyp.h>
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getmsg 85 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::putmsg 86 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::poll 87 ())

(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lstat 88 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::symlink 89 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::readlink 90 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::setgroups 91 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getgroups 92 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::fchmod 93 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::fchown 94 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sigprocmask 95 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sigsuspend 96 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sigaltstack 97 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sigaction 98 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sigpending 99 ())
 #||
 * subcodes:
 *  subcode 0 unused
 * sigpending(...) :: syscall(99, 1, ...)
 * sigfillset(...) :: syscall(99, 2, ...)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::context 100 ())
 #||
 * subcodes:
 * getcontext(...) :: syscall(100, 0, ...)
 * setcontext(...) :: syscall(100, 1, ...)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::evsys 101 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::evtrapret 102 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::statvfs 103 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::fstatvfs 104 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getloadavg 105 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::nfssys 106 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::waitid 107 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::waitsys syscalls::waitid #|| historical ||# ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sigsendsys 108 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::hrtsys 109 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sigresend 111 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::priocntlsys 112 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::pathconf 113 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::mincore 114 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::mmap 115 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::mprotect 116 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::munmap 117 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::fpathconf 118 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::vfork 119 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::fchdir 120 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::readv 121 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::writev 122 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::xstat 123 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lxstat 124 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::fxstat 125 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::xmknod 126 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::setrlimit 128 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getrlimit 129 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lchown 130 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::memcntl 131 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getpmsg 132 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::putpmsg 133 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::rename 134 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::uname 135 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::setegid 136 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sysconfig 137 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::adjtime 138 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::systeminfo 139 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sharefs 140 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::seteuid 141 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::forksys 142 ())
 #||
 * subcodes:
 * forkx(flags)  :: forksys(0, flags)
 * forkallx(flags) :: forksys(1, flags)
 * vforkx(flags)  :: forksys(2, flags)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::fork1 143 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sigtimedwait 144 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_info 145 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::yield 146 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_sema_wait 147 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_sema_post 148 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_sema_trywait 149 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_detach 150 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::corectl 151 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::modctl 152 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::fchroot 153 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::utimes 154 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::vhangup 155 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::gettimeofday 156 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getitimer 157 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::setitimer 158 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_create 159 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_exit 160 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_suspend 161 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_continue 162 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_kill 163 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_self 164 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_sigmask 165 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_private 166 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_wait 167 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_mutex_wakeup 168 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_mutex_lock 169 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_cond_wait 170 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_cond_signal 171 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_cond_broadcast 172 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::pread 173 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::pwrite 174 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::llseek 175 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::inst_sync 176 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::brand 177 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::kaio 178 ())
 #||
 * subcodes:
 * aioread(...) :: kaio(AIOREAD, ...)
 * aiowrite(...) :: kaio(AIOWRITE, ...)
 * aiowait(...) :: kaio(AIOWAIT, ...)
 * aiocancel(...) :: kaio(AIOCANCEL, ...)
 * aionotify() :: kaio(AIONOTIFY)
 * aioinit() :: kaio(AIOINIT)
 * aiostart() :: kaio(AIOSTART)
 * see <sys/aio.h>
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::cpc  179 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lgrpsys 180 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::meminfosys syscalls::lgrpsys ())
 #||
 * subcodes:
 * meminfo(...) :: meminfosys(MIsyscalls::MEMINFO, ...)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::rusagesys 181 ())
 #||
 * subcodes:
 * getrusage(...) :: rusagesys(RUSAGEsyscalls::GETRUSAGE, ...)
 * getvmusage(...)  :: rusagesys(RUSAGEsyscalls::GETVMUSAGE, ...)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::port 182 ())
 #||
 * subcodes:
 * port_create(...) :: portfs(PORT_CREATE, ...)
 * port_associate(...) :: portfs(PORT_ASSOCIATE, ...)
 * port_dissociate(...) :: portfs(PORT_DISSOCIATE, ...)
 * port_send(...) :: portfs(PORT_SEND, ...)
 * port_sendn(...) :: portfs(PORT_SENDN, ...)
 * port_get(...) :: portfs(PORT_GET, ...)
 * port_getn(...) :: portfs(PORT_GETN, ...)
 * port_alert(...) :: portfs(PORT_ALERT, ...)
 * port_dispatch(...) :: portfs(PORT_DISPATCH, ...)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::pollsys 183 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::labelsys 184 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::acl  185 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::auditsys 186 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::processor_bind 187 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::processor_info 188 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::p_online 189 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sigqueue 190 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::clock_gettime 191 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::clock_settime 192 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::clock_getres 193 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::timer_create 194 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::timer_delete 195 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::timer_settime 196 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::timer_gettime 197 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::timer_getoverrun 198 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::nanosleep 199 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::facl 200 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::door 201 ())
 #||
 * Door Subcodes:
 * 0 door_create
 * 1 door_revoke
 * 2 door_info
 * 3 door_call
 * 4 door_return
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::setreuid 202 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::setregid 203 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::install_utrap 204 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::signotify 205 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::schedctl 206 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::pset 207 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sparc_utrap_install 208 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::resolvepath 209 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_mutex_timedlock 210 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_sema_timedwait 211 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_rwlock_sys 212 ())
 #||
 * subcodes:
 * lwp_rwlock_rdlock(...)  :: syscall(212, 0, ...)
 * lwp_rwlock_wrlock(...)  :: syscall(212, 1, ...)
 * lwp_rwlock_tryrdlock(...) :: syscall(212, 2, ...)
 * lwp_rwlock_trywrlock(...) :: syscall(212, 3, ...)
 * lwp_rwlock_unlock(...)  :: syscall(212, 4, ...)
 ||#
#|| system calls for large file ( > 2 gigabyte) support ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getdents64 213 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::mmap64 214 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::stat64 215 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lstat64 216 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::fstat64 217 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::statvfs64 218 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::fstatvfs64 219 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::setrlimit64 220 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getrlimit64 221 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::pread64 222 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::pwrite64 223 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::creat64 224 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::open64 225 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::rpcsys 226 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::zone 227 ())
 #||
 * subcodes:
 * zone_create(...) :: zone(ZONE_CREATE, ...)
 * zone_destroy(...) :: zone(ZONE_DESTROY, ...)
 * zone_getattr(...) :: zone(ZONE_GETATTR, ...)
 * zone_enter(...) :: zone(ZONE_ENTER, ...)
 * zone_list(...) :: zone(ZONE_LIST, ...)
 * zone_shutdown(...) :: zone(ZONE_SHUTDOWN, ...)
 * zone_lookup(...) :: zone(ZONE_LOOKUP, ...)
 * zone_boot(...) :: zone(ZONE_BOOT, ...)
 * zone_version(...) :: zone(ZONE_VERSION, ...)
 * zone_setattr(...) :: zone(ZONE_SETATTR, ...)
 * zone_add_datalink(...) :: zone(ZONE_ADD_DATALINK, ...)
 * zone_remove_datalink(...) :: zone(ZONE_DEL_DATALINK, ...)
 * zone_check_datalink(...) :: zone(ZONE_CHECK_DATALINK, ...)
 * zone_list_datalink(...) :: zone(ZONE_LIST_DATALINK, ...)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::autofssys 228 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getcwd 229 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::so_socket 230 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::so_socketpair 231 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::bind 232 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::listen 233 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::accept 234 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::connect 235 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::shutdown 236 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::recv 237 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::recvfrom 238 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::recvmsg 239 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::send 240 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sendmsg 241 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sendto 242 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getpeername 243 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getsockname 244 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getsockopt 245 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::setsockopt 246 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sockconfig 247 ())
 #||
 * NTP codes
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::ntp_gettime 248 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::ntp_adjtime 249 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_mutex_unlock 250 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_mutex_trylock 251 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_mutex_register 252 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::cladm 253 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::uucopy 254 ())
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::umount2 255 ())
