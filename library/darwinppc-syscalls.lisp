;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 2001 Clozure Associates
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

(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::exit 1 (:int) :void )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::fork 2 () :void)
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::read 3 (:unsigned-fullword :address :unsigned-long)
		:signed-long )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::write 4 (:unsigned-fullword :address :unsigned-long)
		:signed-long )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::open 5 (:address :unsigned-fullword :unsigned-fullword) :signed-fullword :min-args 2 )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::close 6 (:unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::wait4 7 (:unsigned-fullword :address :signed-fullword :address) :unsigned-fullword )
				; 8 is old creat 
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::link 9 (:address :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::unlink 10 (:address) :signed-fullword )
				; 11 is obsolete execv 
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::chdir 12 (:address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::fchdir 13 (:unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::mknod 14  (:address :unsigned-fullword :unsigned-fullword)
		:signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::chmod 15 (:address :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::lchown 16 (:address :unsigned-fullword :unsigned-fullword)
		:signed-fullword)
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::getpid 20 () :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::setuid 23 (:unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::getuid 24 () :unsigned-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::geteuid 25 () :unsigned-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::recvmsg 27 (:unsigned-fullword :address :unsigned-fullword):signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::sendmsg 28 (:unsigned-fullword :address :unsigned-fullword):signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::recvfrom 29 (:unsigned-fullword :address :unsigned-long :unsigned-fullword :address :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::accept 30 (:unsigned-fullword :address :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::getpeername 31 (:unsigned-fullword :address :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::getsockname 32 (:unsigned-fullword :address :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::kill 37 (:signed-fullword :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::sync 36 () :unsigned-fullword )
				; 38 is old stat 
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::getppid 39 ()  :unsigned-fullword)
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::dup 41 (:unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::pipe 42 () :signed-doubleword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::getgid 47 ()  :unsigned-fullword)
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::ioctl 54 (:unsigned-fullword :signed-fullword :address) :signed-fullword :min-args 2 )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::dup2 90 (:unsigned-fullword :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::fcntl 92 (:unsigned-fullword :signed-fullword :signed-fullword) :signed-fullword :min-args 2 )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::select 93 (:unsigned-fullword :address :address
                                                  :address :address)
                :signed-fullword)
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::fsync 95 (:unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::socket 97 (:unsigned-fullword :unsigned-fullword :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::connect 98 (:unsigned-fullword :address :unsigned-fullword) :signed-fullword)
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::bind 104 (:unsigned-fullword :address :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::setsockopt 105 (:unsigned-fullword :signed-fullword :signed-fullword :address :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::listen 106 (:unsigned-fullword :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::gettimeofday 116 (:address :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::getrusage 117 (:signed-fullword :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::getsockopt 118 (:unsigned-fullword :signed-fullword :unsigned-fullword :address :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::fchmod 124 (:unsigned-fullword :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::rename 128 (:address :address) :signed-fullword)
				; 129 is old truncate 
				; 130 is old ftruncate 
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::sendto 133 (:unsigned-fullword :address :unsigned-fullword :unsigned-fullword :address :unsigned-fullword) :signed-fullword )

(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::shutdown 134 (:unsigned-fullword :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::socketpair 135 (:unsigned-fullword :unsigned-fullword :unsigned-fullword :address) :signed-fullword )

(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::mkdir 136 (:address :unsigned-fullword) :signed-fullword)
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::rmdir 137 (:address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::mount 167 (:address :address :unsigned-fullword :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::setgid 181 (:unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::stat 188 (:address :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::fstat 189 (:unsigned-fullword :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::lstat 190 (:address :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::lseek 199 (:unsigned-fullword :signed-doubleword :unsigned-fullword) :signed-doubleword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::truncate 200 (:address :unsigned-doubleword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::ftruncate 201 (:unsigned-fullword :unsigned-doubleword) :signed-fullword )

#+notdefinedyet
(progn
				; 17 is obsolete sbreak 
				; 18 is old getfsstat 
				; 19 is old lseek 
				; 21 is obsolete mount 
				; 22 is obsolete umount 
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::ptrace 26 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::access 33 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::chflags 34 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::fchflags 35 () )
				; 40 is old lstat 
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::getegid 43 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::profil 44 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::ktrace 45 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::sigaction 46 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::sigprocmask 48 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::getlogin 49 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::setlogin 50 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::acct 51 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::sigpending 52 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::sigaltstack 53 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::reboot 55 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::revoke 56 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::symlink 57 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::readlink 58 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::execve 59 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::umask 60 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::chroot 61 () )
				; 62 is old fstat 
				; 63 is unused 
				; 64 is old getpagesize 
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::msync 65 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::vfork 66 () )
				; 67 is obsolete vread 
				; 68 is obsolete vwrite 
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::sbrk 69 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::sstk 70 () )
				; 71 is old mmap 
				; 72 is obsolete vadvise 
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::munmap 73 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::mprotect 74 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::madvise 75 () )
				; 76 is obsolete vhangup 
				; 77 is obsolete vlimit 
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::mincore 78 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::getgroups 79 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::setgroups 80 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::getpgrp 81 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::setpgid 82 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::setitimer 83 () )
				; 84 is old wait 
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::swapon 85 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::getitimer 86 () )
				; 87 is old gethostname 
				; 88 is old sethostname 
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::getdtablesize 89 () )


				; 94 is obsolete setdopt 
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::setpriority 96 () )
				; 99 is old accept 
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::getpriority 100 () )
				; 101 is old send 
				; 102 is old recv 
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::sigreturn 103 () )
				; 107 is obsolete vtimes 
				; 108 is old sigvec 
				; 109 is old sigblock 
				; 110 is old sigsetmask 
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::sigsuspend 111 () )
				; 112 is old sigstack 
				; 113 is old recvmsg 
				; 114 is old sendmsg 
				; 115 is obsolete vtrace 
				; 119 is obsolete resuba 
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::readv 120 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::writev 121 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::settimeofday 122 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::fchown 123 () )
				; 125 is old recvfrom 
				; 126 is old setreuid 
				; 127 is old setregid 
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::flock 131 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::mkfifo 132 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::utimes 138 () )
				; 139 is unused 
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::adjtime 140 () )
				; 141 is old getpeername 
				; 142 is old gethostid 
				; 143 is old sethostid 
				; 144 is old getrlimit 
				; 145 is old setrlimit 
				; 146 is old killpg 
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::setsid 147 () )
				; 148 is obsolete setquota 
				; 149 is obsolete quota 
				; 150 is old getsockname 
				; 151 is reserved 
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::setprivexec 152 () )
				; 153 is reserved 
				; 154 is reserved 
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::nfssvc 155 () )
				; 156 is old getdirentries 
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::statfs 157 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::fstatfs 158 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::unmount 159 () )
				; 160 is obsolete async_daemon 
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::getfh 161 () )
				; 162 is old getdomainname 
				; 163 is old setdomainname 
				; 164 is obsolete pcfs_mount 
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::quotactl 165 () )
				; 166 is obsolete exportfs	

				; 168 is obsolete ustat 
				; 169 is unused 
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::table 170 () )
				; 171 is old wait_3 
				; 172 is obsolete rpause 
				; 173 is unused 
				; 174 is obsolete getdents 
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::gc_control 175 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::add_profil 176 () )
				; 177 is unused 
				; 178 is unused 
				; 179 is unused 
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::kdebug_trace 180        () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::setegid 182 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::seteuid 183 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::lfs_bmapv 184 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::lfs_markv 185 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::lfs_segclean 186 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::lfs_segwait 187 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::pathconf 191 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::fpathconf 192 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::getrlimit 194 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::setrlimit 195 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::getdirentries 196 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::mmap 197 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::__syscall 198 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::__sysctl 202 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::mlock 203 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::munlock 204 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::undelete 205 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::ATsocket 206 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::ATgetmsg 207 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::ATputmsg 208 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::ATPsndreq 209 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::ATPsndrsp 210 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::ATPgetreq 211 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::ATPgetrsp 212 () )
				; 213-215 are reserved for AppleTalk 
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::mkcomplex 216  () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::statv 217		 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::lstatv 218 			 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::fstatv 219 			 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::getattrlist 220 		 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::setattrlist 221		 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::getdirentriesattr 222 	 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::exchangedata 223 				 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::checkuseraccess 224  () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::searchfs 225 () )

       				; 226 - 230 are reserved for HFS expansion 
       				; 231 - 249 are reserved  
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::minherit 250 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::semsys 251 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::msgsys 252 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::shmsys 253 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::semctl 254 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::semget 255 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::semop 256 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::semconfig 257 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::msgctl 258 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::msgget 259 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::msgsnd 260 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::msgrcv 261 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::shmat 262 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::shmctl 263 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::shmdt 264 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::shmget 265 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::shm_open 266 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::shm_unlink 267 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::sem_open 268 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::sem_close 269 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::sem_unlink 270 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::sem_wait 271 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::sem_trywait 272 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::sem_post 273 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::sem_getvalue 274 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::sem_init 275 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::sem_destroy 276 () )
       				; 277 - 295 are reserved  
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::load_shared_file 296 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::reset_shared_file 297 () )
       				; 298 - 323 are reserved  
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::mlockall 324 () )
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::munlockall 325 () )
				; 326 is reserved 
(define-syscall (logior platform-os-darwin platform-cpu-ppc) syscalls::issetugid 327 () )
)
