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

(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::syscall 0 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::exit 1 (:int) :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::forkall 2 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::read 3 (:int :address :size_t) :ssize_t)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::write 4 (:int :address :size_t) :ssize_t)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::open 5 (:address :int :mode_t) :int :min-args 2)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::close 6 (:int) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::wait 7 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::creat 8 (:address :mode_t) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::link 9 (:address :address) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::unlink 10 (:address) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::exec 11 () :void)
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
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::stime 25 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::pcsample 26 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::alarm 27 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::fstat 28 (:int :address) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::pause 29 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::utime 30 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::stty 31 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::gtty 32 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::access 33 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::nice 34 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::statfs 35 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sync 36 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::kill 37 (:pid_t :int) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::fstatfs 38 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::pgrpsys 39 () :void)
 #||
 * subcodes:
 * getpgrp()  :: syscall(39,0)
 * setpgrp()  :: syscall(39,1)
 * getsid(pid)  :: syscall(39,2,pid)
 * setsid()  :: syscall(39,3)
 * getpgid(pid)  :: syscall(39,4,pid)
 * setpgid(pid,pgid) :: syscall(39,5,pid,pgid)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::uucopystr 40 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::dup 41 (:int) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::pipe 42 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::times 43 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::profil 44 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::plock 45 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::setgid 46 (:gid_t) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getgid 47 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::signal 48 () :void)
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
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::msgsys 49 () :void)
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
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sysi86 50 () :void)
 #||
 * subcodes:
 * sysi86(code, ...)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::acct 51 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::shmsys 52 () :void)
 #||
 * subcodes:
 * shmat (...) :: shmsys(0, ...)
 * shmctl(...) :: shmsys(1, ...)
 * shmdt (...) :: shmsys(2, ...)
 * shmget(...) :: shmsys(3, ...)
 * shmids(...) :: shmsys(4, ...)
 * see <sys/shm.h>
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::semsys 53 () :void)
 #||
 * subcodes:
 * semctl(...) :: semsys(0, ...)
 * semget(...) :: semsys(1, ...)
 * semop (...) :: semsys(2, ...)
 * semids(...) :: semsys(3, ...)
 * semtimedop(...) :: semsys(4, ...)
 * see <sys/sem.h>
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::ioctl 54 (:int :int :address) :int :min-args 2)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::uadmin 55 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::utssys 57 () :void)
 #||
 * subcodes (third argument):
 * uname(obuf) (obsolete)  :: syscall(57, obuf, ign, 0)
 *   subcode 1 unused
 * ustat(dev, obuf)  :: syscall(57, obuf, dev, 2)
 * fusers(path, flags, obuf) :: syscall(57, path, flags, 3, obuf)
 * see <sys/utssys.h>
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::fdsync 58 (:int :int) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::execve 59 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::umask 60 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::chroot 61 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::fcntl 62 (:int :int :address) :int :min-args 2)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::ulimit 63 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::reserved_64 64 #|| 64 reserved ||# () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::reserved_65 65 #|| 65 reserved ||# () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::reserved_66 66 #|| 66 reserved ||# () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::reserved_67 67 #|| 67 reserved ||# () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::reserved_68 68 #|| 68 reserved ||# () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::reserved_69 69 #|| 69 reserved ||# () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::tasksys 70 () :void)
 #||
 * subcodes:
 * settaskid(...) :: tasksys(0, ...)
 * gettaskid(...) :: tasksys(1, ...)
 * getprojid(...) :: tasksys(2, ...)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::acctctl 71 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::exacctsys 72 () :void)
 #||
 * subcodes:
 * getacct(...) :: exacct(0, ...)
 * putacct(...) :: exacct(1, ...)
 * wracct(...) :: exacct(2, ...)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getpagesizes 73 () :void)
 #||
 * subcodes:
 * getpagesizes2(...) :: getpagesizes(0, ...)
 * getpagesizes(...) :: getpagesizes(1, ...) legacy
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::rctlsys 74 () :void)
 #||
 * subcodes:
 * getrctl(...) :: rctlsys(0, ...)
 * setrctl(...) :: rctlsys(1, ...)
 * rctllist(...) :: rctlsys(2, ...)
 * rctlctl(...) :: rctlsys(3, ...)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sidsys 75 () :void)
 #||
 * subcodes:
 * allocids(...) :: sidsys(0, ...)
 * idmap_reg(...) :: sidsys(1, ...)
 * idmap_unreg(...) :: sidsys(2, ...)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::fsat 76 () :void)
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
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_park 77 () :void)
 #||
 * subcodes:
 * _lwp_park(timespec_t *, lwpid_t) :: syslwp_park(0, ...)
 * _lwp_unpark(lwpid_t, int) :: syslwp_park(1, ...)
 * _lwp_unpark_all(lwpid_t *, int) :: syslwp_park(2, ...)
 * _lwp_unpark_cancel(lwpid_t *, int) :: syslwp_park(3, ...)
 * _lwp_set_park(lwpid_t *, int)  :: syslwp_park(4, ...)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sendfilev 78 () :void)
 #||
 * subcodes :
 * sendfilev()  :: sendfilev(0, ...)
 * sendfilev64() :: sendfilev(1, ...)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::rmdir 79 (:address) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::mkdir 80 (:address :mode_t) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getdents 81 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::privsys 82 () :void)
 #||
 * subcodes:
 * setppriv(...) :: privsys(0, ...)
 * getppriv(...) :: privsys(1, ...)
 * getimplinfo(...) :: privsys(2, ...)
 * setpflags(...)  :: privsys(3, ...)
 * getpflags(...)  :: privsys(4, ...)
 * issetugid(); :: privsys(5)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::ucredsys 83 () :void)
 #||
 * subcodes:
 * ucred_get(...) :: ucredsys(0, ...)
 * getpeerucred(...) :: ucredsys(1, ...)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sysfs 84 () :void)
 #||
 * subcodes:
 * sysfs(code, ...)
 * see <sys/fstyp.h>
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getmsg 85 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::putmsg 86 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::poll 87 (:address :nfds_t :int) :int)

(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lstat 88 (:address :address) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::symlink 89 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::readlink 90 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::setgroups 91 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getgroups 92 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::fchmod 93 (:int :mode_t) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::fchown 94 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sigprocmask 95 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sigsuspend 96 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sigaltstack 97 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sigaction 98 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sigpending 99 () :void)
 #||
 * subcodes:
 *  subcode 0 unused
 * sigpending(...) :: syscall(99, 1, ...)
 * sigfillset(...) :: syscall(99, 2, ...)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::context 100 () :void)
 #||
 * subcodes:
 * getcontext(...) :: syscall(100, 0, ...)
 * setcontext(...) :: syscall(100, 1, ...)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::evsys 101 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::evtrapret 102 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::statvfs 103 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::fstatvfs 104 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getloadavg 105 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::nfssys 106 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::waitid 107 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sigsendsys 108 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::hrtsys 109 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sigresend 111 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::priocntlsys 112 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::pathconf 113 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::mincore 114 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::mmap 115 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::mprotect 116 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::munmap 117 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::fpathconf 118 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::vfork 119 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::fchdir 120 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::readv 121 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::writev 122 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::xstat 123 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lxstat 124 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::fxstat 125 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::xmknod 126 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::setrlimit 128 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getrlimit 129 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lchown 130 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::memcntl 131 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getpmsg 132 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::putpmsg 133 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::rename 134 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::uname 135 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::setegid 136 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sysconfig 137 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::adjtime 138 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::systeminfo 139 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sharefs 140 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::seteuid 141 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::forksys 142 () :void)
 #||
 * subcodes:
 * forkx(flags)  :: forksys(0, flags)
 * forkallx(flags) :: forksys(1, flags)
 * vforkx(flags)  :: forksys(2, flags)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::fork1 143 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sigtimedwait 144 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_info 145 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::yield 146 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_sema_wait 147 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_sema_post 148 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_sema_trywait 149 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_detach 150 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::corectl 151 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::modctl 152 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::fchroot 153 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::utimes 154 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::vhangup 155 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::gettimeofday 156 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getitimer 157 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::setitimer 158 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_create 159 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_exit 160 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_suspend 161 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_continue 162 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_kill 163 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_self 164 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_sigmask 165 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_private 166 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_wait 167 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_mutex_wakeup 168 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_mutex_lock 169 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_cond_wait 170 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_cond_signal 171 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_cond_broadcast 172 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::pread 173 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::pwrite 174 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::llseek 175 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::inst_sync 176 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::brand 177 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::kaio 178 () :void)
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
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::cpc  179 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lgrpsys 180 () :void)
 #||
 * subcodes:
 * meminfo(...) :: meminfosys(MIsyscalls::MEMINFO, ...)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::rusagesys 181 (:int :int :address) :int)
 #||
 * subcodes:
 * getrusage(...) :: rusagesys(RUSAGEsyscalls::GETRUSAGE, ...)
 * getvmusage(...)  :: rusagesys(RUSAGEsyscalls::GETVMUSAGE, ...)
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::port 182 () :void)
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
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::pollsys 183 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::labelsys 184 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::acl  185 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::auditsys 186 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::processor_bind 187 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::processor_info 188 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::p_online 189 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sigqueue 190 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::clock_gettime 191 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::clock_settime 192 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::clock_getres 193 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::timer_create 194 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::timer_delete 195 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::timer_settime 196 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::timer_gettime 197 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::timer_getoverrun 198 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::nanosleep 199 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::facl 200 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::door 201 () :void)
 #||
 * Door Subcodes:
 * 0 door_create
 * 1 door_revoke
 * 2 door_info
 * 3 door_call
 * 4 door_return
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::setreuid 202 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::setregid 203 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::install_utrap 204 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::signotify 205 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::schedctl 206 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::pset 207 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sparc_utrap_install 208 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::resolvepath 209 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_mutex_timedlock 210 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_sema_timedwait 211 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_rwlock_sys 212 () :void)
 #||
 * subcodes:
 * lwp_rwlock_rdlock(...)  :: syscall(212, 0, ...)
 * lwp_rwlock_wrlock(...)  :: syscall(212, 1, ...)
 * lwp_rwlock_tryrdlock(...) :: syscall(212, 2, ...)
 * lwp_rwlock_trywrlock(...) :: syscall(212, 3, ...)
 * lwp_rwlock_unlock(...)  :: syscall(212, 4, ...)
 ||#
#|| system calls for large file ( > 2 gigabyte) support ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getdents64 213 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::mmap64 214 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::stat64 215 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lstat64 216 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::fstat64 217 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::statvfs64 218 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::fstatvfs64 219 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::setrlimit64 220 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getrlimit64 221 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::pread64 222 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::pwrite64 223 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::creat64 224 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::open64 225 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::rpcsys 226 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::zone 227 () :void)
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
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::autofssys 228 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getcwd 229 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::so_socket 230 (:int :int :int :address :int) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::so_socketpair 231 (:address) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::bind 232 (:int :address :socklen_t) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::listen 233 (:int :int) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::accept 234 (:int :address :address) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::connect 235 (:int :address :socklen_t) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::shutdown 236 (:int :int) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::recv 237 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::recvfrom 238 (:int :address :size_t :int :address :address) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::recvmsg 239 (:int :address :int) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::send 240 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sendmsg 241 (:int :address :int) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sendto 242 (:int :address :size_t :int :address :socklen_t) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getpeername 243 (:int :address :address) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getsockname 244 (:int :address :address) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::getsockopt 245 (:int :int :int :address :address) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::setsockopt 246 (:int :int :int :address :socklen_t) :int)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::sockconfig 247 () :void)
 #||
 * NTP codes
 ||#
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::ntp_gettime 248 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::ntp_adjtime 249 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_mutex_unlock 250 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_mutex_trylock 251 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::lwp_mutex_register 252 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::cladm 253 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::uucopy 254 () :void)
(define-syscall (logior platform-os-solaris platform-cpu-x86 platform-word-size-64) syscalls::umount2 255 () :void)
