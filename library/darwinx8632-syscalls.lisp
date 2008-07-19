(in-package "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "SYSCALL"))

(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::exit 1 (:int) :void )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::fork 2 () :void)
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::read 3 (:unsigned-fullword :address :unsigned-long)
		:signed-long )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::write 4 (:unsigned-fullword :address :unsigned-long)
		:signed-long )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::open 5 (:address :unsigned-fullword :unsigned-fullword) :signed-fullword :min-args 2 )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::close 6 (:unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::wait4 7 (:unsigned-fullword :address :signed-fullword :address) :unsigned-fullword )
				; 8 is old creat 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::link 9 (:address :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::unlink 10 (:address) :signed-fullword )
				; 11 is obsolete execv 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::chdir 12 (:address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::fchdir 13 (:unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::mknod 14  (:address :unsigned-fullword :unsigned-fullword)
		:signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::chmod 15 (:address :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::lchown 16 (:address :unsigned-fullword :unsigned-fullword)
		:signed-fullword)
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::getpid 20 () :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::setuid 23 (:unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::getuid 24 () :unsigned-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::geteuid 25 () :unsigned-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::recvmsg 27 (:unsigned-fullword :address :unsigned-fullword):signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::sendmsg 28 (:unsigned-fullword :address :unsigned-fullword):signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::recvfrom 29 (:unsigned-fullword :address :unsigned-long :unsigned-fullword :address :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::accept 30 (:unsigned-fullword :address :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::getpeername 31 (:unsigned-fullword :address :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::getsockname 32 (:unsigned-fullword :address :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::kill 37 (:signed-fullword :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::sync 36 () :unsigned-fullword )
				; 38 is old stat 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::getppid 39 ()  :unsigned-fullword)
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::dup 41 (:unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::pipe 42 () :signed-doubleword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::getgid 47 ()  :unsigned-fullword)
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::ioctl 54 (:unsigned-fullword :signed-fullword :address) :signed-fullword :min-args 2 )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::dup2 90 (:unsigned-fullword :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::fcntl 92 (:unsigned-fullword :signed-fullword :signed-fullword) :signed-fullword :min-args 2 )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::select 93 (:unsigned-fullword :address :address
                                                  :address :address)
                :signed-fullword)
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::fsync 95 (:unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::socket 97 (:unsigned-fullword :unsigned-fullword :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::connect 98 (:unsigned-fullword :address :unsigned-fullword) :signed-fullword)
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::bind 104 (:unsigned-fullword :address :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::setsockopt 105 (:unsigned-fullword :signed-fullword :signed-fullword :address :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::listen 106 (:unsigned-fullword :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::gettimeofday 116 (:address :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::getrusage 117 (:signed-fullword :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::getsockopt 118 (:unsigned-fullword :signed-fullword :unsigned-fullword :address :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::fchmod 124 (:unsigned-fullword :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::rename 128 (:address :address) :signed-fullword)
				; 129 is old truncate 
				; 130 is old ftruncate 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::sendto 133 (:unsigned-fullword :address :unsigned-fullword :unsigned-fullword :address :unsigned-fullword) :signed-fullword )

(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::shutdown 134 (:unsigned-fullword :unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::socketpair 135 (:unsigned-fullword :unsigned-fullword :unsigned-fullword :address) :signed-fullword )

(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::mkdir 136 (:address :unsigned-fullword) :signed-fullword)
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::rmdir 137 (:address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::mount 167 (:address :address :unsigned-fullword :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::setgid 181 (:unsigned-fullword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::stat 188 (:address :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::fstat 189 (:unsigned-fullword :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::lstat 190 (:address :address) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::lseek 199 (:unsigned-fullword :signed-doubleword :unsigned-fullword) :signed-doubleword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::truncate 200 (:address :unsigned-doubleword) :signed-fullword )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::ftruncate 201 (:unsigned-fullword :unsigned-doubleword) :signed-fullword )

#+notdefinedyet
(progn
				; 17 is obsolete sbreak 
				; 18 is old getfsstat 
				; 19 is old lseek 
				; 21 is obsolete mount 
				; 22 is obsolete umount 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::ptrace 26 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::access 33 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::chflags 34 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::fchflags 35 () )
				; 40 is old lstat 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::getegid 43 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::profil 44 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::ktrace 45 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::sigaction 46 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::sigprocmask 48 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::getlogin 49 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::setlogin 50 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::acct 51 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::sigpending 52 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::sigaltstack 53 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::reboot 55 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::revoke 56 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::symlink 57 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::readlink 58 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::execve 59 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::umask 60 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::chroot 61 () )
				; 62 is old fstat 
				; 63 is unused 
				; 64 is old getpagesize 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::msync 65 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::vfork 66 () )
				; 67 is obsolete vread 
				; 68 is obsolete vwrite 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::sbrk 69 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::sstk 70 () )
				; 71 is old mmap 
				; 72 is obsolete vadvise 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::munmap 73 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::mprotect 74 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::madvise 75 () )
				; 76 is obsolete vhangup 
				; 77 is obsolete vlimit 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::mincore 78 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::getgroups 79 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::setgroups 80 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::getpgrp 81 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::setpgid 82 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::setitimer 83 () )
				; 84 is old wait 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::swapon 85 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::getitimer 86 () )
				; 87 is old gethostname 
				; 88 is old sethostname 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::getdtablesize 89 () )


				; 94 is obsolete setdopt 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::setpriority 96 () )
				; 99 is old accept 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::getpriority 100 () )
				; 101 is old send 
				; 102 is old recv 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::sigreturn 103 () )
				; 107 is obsolete vtimes 
				; 108 is old sigvec 
				; 109 is old sigblock 
				; 110 is old sigsetmask 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::sigsuspend 111 () )
				; 112 is old sigstack 
				; 113 is old recvmsg 
				; 114 is old sendmsg 
				; 115 is obsolete vtrace 
				; 119 is obsolete resuba 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::readv 120 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::writev 121 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::settimeofday 122 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::fchown 123 () )
				; 125 is old recvfrom 
				; 126 is old setreuid 
				; 127 is old setregid 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::flock 131 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::mkfifo 132 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::utimes 138 () )
				; 139 is unused 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::adjtime 140 () )
				; 141 is old getpeername 
				; 142 is old gethostid 
				; 143 is old sethostid 
				; 144 is old getrlimit 
				; 145 is old setrlimit 
				; 146 is old killpg 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::setsid 147 () )
				; 148 is obsolete setquota 
				; 149 is obsolete quota 
				; 150 is old getsockname 
				; 151 is reserved 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::setprivexec 152 () )
				; 153 is reserved 
				; 154 is reserved 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::nfssvc 155 () )
				; 156 is old getdirentries 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::statfs 157 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::fstatfs 158 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::unmount 159 () )
				; 160 is obsolete async_daemon 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::getfh 161 () )
				; 162 is old getdomainname 
				; 163 is old setdomainname 
				; 164 is obsolete pcfs_mount 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::quotactl 165 () )
				; 166 is obsolete exportfs	

				; 168 is obsolete ustat 
				; 169 is unused 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::table 170 () )
				; 171 is old wait_3 
				; 172 is obsolete rpause 
				; 173 is unused 
				; 174 is obsolete getdents 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::gc_control 175 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::add_profil 176 () )
				; 177 is unused 
				; 178 is unused 
				; 179 is unused 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::kdebug_trace 180        () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::setegid 182 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::seteuid 183 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::lfs_bmapv 184 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::lfs_markv 185 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::lfs_segclean 186 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::lfs_segwait 187 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::pathconf 191 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::fpathconf 192 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::getrlimit 194 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::setrlimit 195 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::getdirentries 196 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::mmap 197 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::__syscall 198 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::__sysctl 202 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::mlock 203 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::munlock 204 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::undelete 205 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::ATsocket 206 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::ATgetmsg 207 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::ATputmsg 208 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::ATPsndreq 209 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::ATPsndrsp 210 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::ATPgetreq 211 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::ATPgetrsp 212 () )
				; 213-215 are reserved for AppleTalk 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::mkcomplex 216  () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::statv 217		 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::lstatv 218 			 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::fstatv 219 			 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::getattrlist 220 		 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::setattrlist 221		 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::getdirentriesattr 222 	 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::exchangedata 223 				 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::checkuseraccess 224  () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::searchfs 225 () )

       				; 226 - 230 are reserved for HFS expansion 
       				; 231 - 249 are reserved  
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::minherit 250 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::semsys 251 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::msgsys 252 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::shmsys 253 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::semctl 254 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::semget 255 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::semop 256 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::semconfig 257 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::msgctl 258 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::msgget 259 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::msgsnd 260 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::msgrcv 261 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::shmat 262 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::shmctl 263 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::shmdt 264 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::shmget 265 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::shm_open 266 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::shm_unlink 267 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::sem_open 268 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::sem_close 269 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::sem_unlink 270 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::sem_wait 271 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::sem_trywait 272 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::sem_post 273 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::sem_getvalue 274 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::sem_init 275 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::sem_destroy 276 () )
       				; 277 - 295 are reserved  
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::load_shared_file 296 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::reset_shared_file 297 () )
       				; 298 - 323 are reserved  
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::mlockall 324 () )
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::munlockall 325 () )
				; 326 is reserved 
(define-syscall (logior platform-os-darwin platform-cpu-x86 platform-word-size-32) syscalls::issetugid 327 () )
)


