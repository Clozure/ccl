;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2001 Clozure Associates
;;;   This file is part of Clozure CL.  
;;;
;;;   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with Clozure CL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with Clozure CL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   Clozure CL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

(in-package "CCL")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "SYSCALL")
  (defconstant darwinx8664-unix-syscall-mask #x2000000))

(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::exit  (logior darwinx8664-unix-syscall-mask 1) (:int) :void )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::fork  (logior darwinx8664-unix-syscall-mask 2) () :void)
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::read  (logior darwinx8664-unix-syscall-mask 3) (:unsigned-fullword :address :unsigned-long)
		:signed-long )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::write  (logior darwinx8664-unix-syscall-mask 4) (:unsigned-fullword :address :unsigned-long)
		:signed-long )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::open  (logior darwinx8664-unix-syscall-mask 5) (:address :unsigned-fullword :unsigned-fullword) :signed-fullword :min-args 2 )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::close  (logior darwinx8664-unix-syscall-mask 6) (:unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::wait4  (logior darwinx8664-unix-syscall-mask 7) (:unsigned-fullword :address :signed-fullword :address) :unsigned-fullword )
				; 8 is old creat 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::link  (logior darwinx8664-unix-syscall-mask 9) (:address :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::unlink  (logior darwinx8664-unix-syscall-mask 10) (:address) :signed-fullword )
				; 11 is obsolete execv 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::chdir  (logior darwinx8664-unix-syscall-mask 12) (:address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::fchdir  (logior darwinx8664-unix-syscall-mask 13) (:unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::mknod  (logior darwinx8664-unix-syscall-mask 14)  (:address :unsigned-fullword :unsigned-fullword)
		:signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::chmod  (logior darwinx8664-unix-syscall-mask 15) (:address :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::lchown  (logior darwinx8664-unix-syscall-mask 16) (:address :unsigned-fullword :unsigned-fullword)
		:signed-fullword)
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::getpid  (logior darwinx8664-unix-syscall-mask 20) () :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::setuid  (logior darwinx8664-unix-syscall-mask 23) (:unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::getuid  (logior darwinx8664-unix-syscall-mask 24) () :unsigned-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::geteuid  (logior darwinx8664-unix-syscall-mask 25) () :unsigned-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::recvmsg  (logior darwinx8664-unix-syscall-mask 27) (:unsigned-fullword :address :unsigned-fullword):signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::sendmsg  (logior darwinx8664-unix-syscall-mask 28) (:unsigned-fullword :address :unsigned-fullword):signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::recvfrom  (logior darwinx8664-unix-syscall-mask 29) (:unsigned-fullword :address :unsigned-long :unsigned-fullword :address :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::accept  (logior darwinx8664-unix-syscall-mask 30) (:unsigned-fullword :address :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::getpeername  (logior darwinx8664-unix-syscall-mask 31) (:unsigned-fullword :address :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::getsockname  (logior darwinx8664-unix-syscall-mask 32) (:unsigned-fullword :address :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::kill  (logior darwinx8664-unix-syscall-mask 37) (:signed-fullword :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::sync  (logior darwinx8664-unix-syscall-mask 36) () :unsigned-fullword )
				; 38 is old stat 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::getppid  (logior darwinx8664-unix-syscall-mask 39) ()  :unsigned-fullword)
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::dup  (logior darwinx8664-unix-syscall-mask 41) (:unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::pipe  (logior darwinx8664-unix-syscall-mask 42) () :signed-doubleword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::getgid  (logior darwinx8664-unix-syscall-mask 47) ()  :unsigned-fullword)
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::ioctl  (logior darwinx8664-unix-syscall-mask 54) (:unsigned-fullword :signed-fullword :address) :signed-fullword :min-args 2 )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::dup2  (logior darwinx8664-unix-syscall-mask 90) (:unsigned-fullword :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::fcntl  (logior darwinx8664-unix-syscall-mask 92) (:unsigned-fullword :signed-fullword :signed-fullword) :signed-fullword :min-args 2 )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::select  (logior darwinx8664-unix-syscall-mask 93) (:unsigned-fullword :address :address
                                                  :address :address)
                :signed-fullword)
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::fsync  (logior darwinx8664-unix-syscall-mask 95) (:unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::socket  (logior darwinx8664-unix-syscall-mask 97) (:unsigned-fullword :unsigned-fullword :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::connect  (logior darwinx8664-unix-syscall-mask 98) (:unsigned-fullword :address :unsigned-fullword) :signed-fullword)
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::bind  (logior darwinx8664-unix-syscall-mask 104) (:unsigned-fullword :address :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::setsockopt  (logior darwinx8664-unix-syscall-mask 105) (:unsigned-fullword :signed-fullword :signed-fullword :address :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::listen  (logior darwinx8664-unix-syscall-mask 106) (:unsigned-fullword :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::gettimeofday  (logior darwinx8664-unix-syscall-mask 116) (:address :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::getrusage  (logior darwinx8664-unix-syscall-mask 117) (:signed-fullword :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::getsockopt  (logior darwinx8664-unix-syscall-mask 118) (:unsigned-fullword :signed-fullword :unsigned-fullword :address :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::fchmod  (logior darwinx8664-unix-syscall-mask 124) (:unsigned-fullword :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::rename  (logior darwinx8664-unix-syscall-mask 128) (:address :address) :signed-fullword)
				; 129 is old truncate 
				; 130 is old ftruncate 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::sendto  (logior darwinx8664-unix-syscall-mask 133) (:unsigned-fullword :address :unsigned-fullword :unsigned-fullword :address :unsigned-fullword) :signed-fullword )

(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::shutdown  (logior darwinx8664-unix-syscall-mask 134) (:unsigned-fullword :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::socketpair  (logior darwinx8664-unix-syscall-mask 135) (:unsigned-fullword :unsigned-fullword :unsigned-fullword :address) :signed-fullword )

(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::mkdir  (logior darwinx8664-unix-syscall-mask 136) (:address :unsigned-fullword) :signed-fullword)
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::rmdir  (logior darwinx8664-unix-syscall-mask 137) (:address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::mount  (logior darwinx8664-unix-syscall-mask 167) (:address :address :unsigned-fullword :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::setgid  (logior darwinx8664-unix-syscall-mask 181) (:unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::stat  (logior darwinx8664-unix-syscall-mask 188) (:address :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::fstat  (logior darwinx8664-unix-syscall-mask 189) (:unsigned-fullword :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::lstat  (logior darwinx8664-unix-syscall-mask 190) (:address :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::lseek  (logior darwinx8664-unix-syscall-mask 199) (:unsigned-fullword :signed-doubleword :unsigned-fullword) :signed-doubleword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::truncate  (logior darwinx8664-unix-syscall-mask 200) (:address :unsigned-doubleword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::ftruncate  (logior darwinx8664-unix-syscall-mask 201) (:unsigned-fullword :unsigned-doubleword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::poll (logior darwinx8664-unix-syscall-mask 230) ((:* (:struct :pollfd)) :int :int) :int)
#+notdefinedyet
(progn
				; 17 is obsolete sbreak 
				; 18 is old getfsstat 
				; 19 is old lseek 
				; 21 is obsolete mount 
				; 22 is obsolete umount 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::ptrace  (logior darwinx8664-unix-syscall-mask 26) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::access  (logior darwinx8664-unix-syscall-mask 33) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::chflags  (logior darwinx8664-unix-syscall-mask 34) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::fchflags  (logior darwinx8664-unix-syscall-mask 35) () )
				; 40 is old lstat 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::getegid  (logior darwinx8664-unix-syscall-mask 43) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::profil  (logior darwinx8664-unix-syscall-mask 44) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::ktrace  (logior darwinx8664-unix-syscall-mask 45) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::sigaction  (logior darwinx8664-unix-syscall-mask 46) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::sigprocmask  (logior darwinx8664-unix-syscall-mask 48) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::getlogin  (logior darwinx8664-unix-syscall-mask 49) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::setlogin  (logior darwinx8664-unix-syscall-mask 50) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::acct  (logior darwinx8664-unix-syscall-mask 51) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::sigpending  (logior darwinx8664-unix-syscall-mask 52) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::sigaltstack  (logior darwinx8664-unix-syscall-mask 53) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::reboot  (logior darwinx8664-unix-syscall-mask 55) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::revoke  (logior darwinx8664-unix-syscall-mask 56) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::symlink  (logior darwinx8664-unix-syscall-mask 57) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::readlink  (logior darwinx8664-unix-syscall-mask 58) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::execve  (logior darwinx8664-unix-syscall-mask 59) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::umask  (logior darwinx8664-unix-syscall-mask 60) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::chroot  (logior darwinx8664-unix-syscall-mask 61) () )
				; 62 is old fstat 
				; 63 is unused 
				; 64 is old getpagesize 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::msync  (logior darwinx8664-unix-syscall-mask 65) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::vfork  (logior darwinx8664-unix-syscall-mask 66) () )
				; 67 is obsolete vread 
				; 68 is obsolete vwrite 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::sbrk  (logior darwinx8664-unix-syscall-mask 69) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::sstk  (logior darwinx8664-unix-syscall-mask 70) () )
				; 71 is old mmap 
				; 72 is obsolete vadvise 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::munmap  (logior darwinx8664-unix-syscall-mask 73) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::mprotect  (logior darwinx8664-unix-syscall-mask 74) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::madvise  (logior darwinx8664-unix-syscall-mask 75) () )
				; 76 is obsolete vhangup 
				; 77 is obsolete vlimit 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::mincore  (logior darwinx8664-unix-syscall-mask 78) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::getgroups  (logior darwinx8664-unix-syscall-mask 79) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::setgroups  (logior darwinx8664-unix-syscall-mask 80) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::getpgrp  (logior darwinx8664-unix-syscall-mask 81) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::setpgid  (logior darwinx8664-unix-syscall-mask 82) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::setitimer  (logior darwinx8664-unix-syscall-mask 83) () )
				; 84 is old wait 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::swapon  (logior darwinx8664-unix-syscall-mask 85) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::getitimer  (logior darwinx8664-unix-syscall-mask 86) () )
				; 87 is old gethostname 
				; 88 is old sethostname 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::getdtablesize  (logior darwinx8664-unix-syscall-mask 89) () )


				; 94 is obsolete setdopt 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::setpriority  (logior darwinx8664-unix-syscall-mask 96) () )
				; 99 is old accept 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::getpriority  (logior darwinx8664-unix-syscall-mask 100) () )
				; 101 is old send 
				; 102 is old recv 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::sigreturn  (logior darwinx8664-unix-syscall-mask 103) () )
				; 107 is obsolete vtimes 
				; 108 is old sigvec 
				; 109 is old sigblock 
				; 110 is old sigsetmask 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::sigsuspend  (logior darwinx8664-unix-syscall-mask 111) () )
				; 112 is old sigstack 
				; 113 is old recvmsg 
				; 114 is old sendmsg 
				; 115 is obsolete vtrace 
				; 119 is obsolete resuba 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::readv  (logior darwinx8664-unix-syscall-mask 120) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::writev  (logior darwinx8664-unix-syscall-mask 121) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::settimeofday  (logior darwinx8664-unix-syscall-mask 122) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::fchown  (logior darwinx8664-unix-syscall-mask 123) () )
				; 125 is old recvfrom 
				; 126 is old setreuid 
				; 127 is old setregid 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::flock  (logior darwinx8664-unix-syscall-mask 131) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::mkfifo  (logior darwinx8664-unix-syscall-mask 132) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::utimes  (logior darwinx8664-unix-syscall-mask 138) () )
				; 139 is unused 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::adjtime  (logior darwinx8664-unix-syscall-mask 140) () )
				; 141 is old getpeername 
				; 142 is old gethostid 
				; 143 is old sethostid 
				; 144 is old getrlimit 
				; 145 is old setrlimit 
				; 146 is old killpg 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::setsid  (logior darwinx8664-unix-syscall-mask 147) () )
				; 148 is obsolete setquota 
				; 149 is obsolete quota 
				; 150 is old getsockname 
				; 151 is reserved 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::setprivexec  (logior darwinx8664-unix-syscall-mask 152) () )
				; 153 is reserved 
				; 154 is reserved 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::nfssvc  (logior darwinx8664-unix-syscall-mask 155) () )
				; 156 is old getdirentries 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::statfs  (logior darwinx8664-unix-syscall-mask 157) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::fstatfs  (logior darwinx8664-unix-syscall-mask 158) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::unmount  (logior darwinx8664-unix-syscall-mask 159) () )
				; 160 is obsolete async_daemon 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::getfh  (logior darwinx8664-unix-syscall-mask 161) () )
				; 162 is old getdomainname 
				; 163 is old setdomainname 
				; 164 is obsolete pcfs_mount 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::quotactl  (logior darwinx8664-unix-syscall-mask 165) () )
				; 166 is obsolete exportfs	

				; 168 is obsolete ustat 
				; 169 is unused 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::table  (logior darwinx8664-unix-syscall-mask 170) () )
				; 171 is old wait_3 
				; 172 is obsolete rpause 
				; 173 is unused 
				; 174 is obsolete getdents 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::gc_control  (logior darwinx8664-unix-syscall-mask 175) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::add_profil  (logior darwinx8664-unix-syscall-mask 176) () )
				; 177 is unused 
				; 178 is unused 
				; 179 is unused 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::kdebug_trace  (logior darwinx8664-unix-syscall-mask 180)        () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::setegid  (logior darwinx8664-unix-syscall-mask 182) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::seteuid  (logior darwinx8664-unix-syscall-mask 183) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::lfs_bmapv  (logior darwinx8664-unix-syscall-mask 184) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::lfs_markv  (logior darwinx8664-unix-syscall-mask 185) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::lfs_segclean  (logior darwinx8664-unix-syscall-mask 186) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::lfs_segwait  (logior darwinx8664-unix-syscall-mask 187) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::pathconf  (logior darwinx8664-unix-syscall-mask 191) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::fpathconf  (logior darwinx8664-unix-syscall-mask 192) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::getrlimit  (logior darwinx8664-unix-syscall-mask 194) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::setrlimit  (logior darwinx8664-unix-syscall-mask 195) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::getdirentries  (logior darwinx8664-unix-syscall-mask 196) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::mmap  (logior darwinx8664-unix-syscall-mask 197) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::__syscall  (logior darwinx8664-unix-syscall-mask 198) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::__sysctl  (logior darwinx8664-unix-syscall-mask 202) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::mlock  (logior darwinx8664-unix-syscall-mask 203) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::munlock  (logior darwinx8664-unix-syscall-mask 204) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::undelete  (logior darwinx8664-unix-syscall-mask 205) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::ATsocket  (logior darwinx8664-unix-syscall-mask 206) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::ATgetmsg  (logior darwinx8664-unix-syscall-mask 207) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::ATputmsg  (logior darwinx8664-unix-syscall-mask 208) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::ATPsndreq  (logior darwinx8664-unix-syscall-mask 209) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::ATPsndrsp  (logior darwinx8664-unix-syscall-mask 210) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::ATPgetreq  (logior darwinx8664-unix-syscall-mask 211) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::ATPgetrsp  (logior darwinx8664-unix-syscall-mask 212) () )
				; 213-215 are reserved for AppleTalk 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::mkcomplex  (logior darwinx8664-unix-syscall-mask 216)  () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::statv  (logior darwinx8664-unix-syscall-mask 217)		 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::lstatv  (logior darwinx8664-unix-syscall-mask 218) 			 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::fstatv  (logior darwinx8664-unix-syscall-mask 219) 			 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::getattrlist  (logior darwinx8664-unix-syscall-mask 220) 		 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::setattrlist  (logior darwinx8664-unix-syscall-mask 221)		 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::getdirentriesattr  (logior darwinx8664-unix-syscall-mask 222) 	 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::exchangedata  (logior darwinx8664-unix-syscall-mask 223) 				 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::checkuseraccess  (logior darwinx8664-unix-syscall-mask 224)  () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::searchfs  (logior darwinx8664-unix-syscall-mask 225) () )

       				; 226 - 230 are reserved for HFS expansion 
       				; 231 - 249 are reserved  
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::minherit  (logior darwinx8664-unix-syscall-mask 250) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::semsys  (logior darwinx8664-unix-syscall-mask 251) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::msgsys  (logior darwinx8664-unix-syscall-mask 252) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::shmsys  (logior darwinx8664-unix-syscall-mask 253) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::semctl  (logior darwinx8664-unix-syscall-mask 254) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::semget  (logior darwinx8664-unix-syscall-mask 255) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::semop  (logior darwinx8664-unix-syscall-mask 256) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::semconfig  (logior darwinx8664-unix-syscall-mask 257) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::msgctl  (logior darwinx8664-unix-syscall-mask 258) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::msgget  (logior darwinx8664-unix-syscall-mask 259) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::msgsnd  (logior darwinx8664-unix-syscall-mask 260) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::msgrcv  (logior darwinx8664-unix-syscall-mask 261) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::shmat  (logior darwinx8664-unix-syscall-mask 262) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::shmctl  (logior darwinx8664-unix-syscall-mask 263) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::shmdt  (logior darwinx8664-unix-syscall-mask 264) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::shmget  (logior darwinx8664-unix-syscall-mask 265) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::shm_open  (logior darwinx8664-unix-syscall-mask 266) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::shm_unlink  (logior darwinx8664-unix-syscall-mask 267) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::sem_open  (logior darwinx8664-unix-syscall-mask 268) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::sem_close  (logior darwinx8664-unix-syscall-mask 269) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::sem_unlink  (logior darwinx8664-unix-syscall-mask 270) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::sem_wait  (logior darwinx8664-unix-syscall-mask 271) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::sem_trywait  (logior darwinx8664-unix-syscall-mask 272) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::sem_post  (logior darwinx8664-unix-syscall-mask 273) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::sem_getvalue  (logior darwinx8664-unix-syscall-mask 274) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::sem_init  (logior darwinx8664-unix-syscall-mask 275) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::sem_destroy  (logior darwinx8664-unix-syscall-mask 276) () )
       				; 277 - 295 are reserved  
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::load_shared_file  (logior darwinx8664-unix-syscall-mask 296) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::reset_shared_file  (logior darwinx8664-unix-syscall-mask 297) () )
       				; 298 - 323 are reserved  
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::mlockall  (logior darwinx8664-unix-syscall-mask 324) () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::munlockall  (logior darwinx8664-unix-syscall-mask 325) () )
				; 326 is reserved 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-64) syscalls::issetugid  (logior darwinx8664-unix-syscall-mask 327) () )
)
