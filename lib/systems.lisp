;-*-Mode: LISP; Package: CCL -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package "CCL")

;;; module-name       binary                    (source . files-depends-on)
;;; -----------       ------                    ---------------------------
(defparameter *ccl-system*
  '(
    (level-1          "ccl:ccl;level-1"          ("ccl:l1;level-1.lisp"))
    (runtime          "ccl:ccl;runtime"          ("ccl:l1;runtime.lisp"))
    (level-1-test     "ccl:level-1-test"         ("ccl:l1;level-1-test.lisp"))
    (l1-cl-package    "ccl:l1f;l1-cl-package"    ("ccl:l1;l1-cl-package.lisp"))
    (l1-utils         "ccl:l1f;l1-utils"         ("ccl:l1;l1-utils.lisp"))
    (l1-numbers       "ccl:l1f;l1-numbers"       ("ccl:l1;l1-numbers.lisp"))
    (l1-init          "ccl:l1f;l1-init"          ("ccl:l1;l1-init.lisp"))
    (version          "ccl:l1f;version"          ("ccl:l1;version.lisp"))
    (l1-boot-1        "ccl:l1f;l1-boot-1"        ("ccl:l1;l1-boot-1.lisp"))
    (l1-boot-2        "ccl:l1f;l1-boot-2"        ("ccl:l1;l1-boot-2.lisp"))
    (l1-boot-3        "ccl:l1f;l1-boot-3"        ("ccl:l1;l1-boot-3.lisp"))
    (l1-boot-lds      "ccl:l1f;l1-boot-lds"      ("ccl:l1;l1-boot-lds.lisp"))
    (l1-files         "ccl:l1f;l1-files"         ("ccl:l1;l1-files.lisp"))
    (l1-sort          "ccl:l1f;l1-sort"          ("ccl:l1;l1-sort.lisp"))
    (l1-dcode         "ccl:l1f;l1-dcode"         ("ccl:l1;l1-dcode.lisp"))
    (l1-clos-boot     "ccl:l1f;l1-clos-boot"    ("ccl:l1;l1-clos-boot.lisp"))
    (l1-clos          "ccl:l1f;l1-clos"          ("ccl:l1;l1-clos.lisp"))
    (l1-io            "ccl:l1f;l1-io"            ("ccl:l1;l1-io.lisp"))
    (l1-unicode       "ccl:l1f;l1-unicode"       ("ccl:l1;l1-unicode.lisp"))

    (l1-streams       "ccl:l1f;l1-streams"       ("ccl:l1;l1-streams.lisp"))
    (l1-events        "ccl:l1f;l1-events"        ("ccl:l1;l1-events.lisp"))
    (ppc-trap-support "ccl:l1f;ppc-trap-support" ("ccl:l1;ppc-trap-support.lisp"))
    (x86-trap-support "ccl:l1f;x86-trap-support" ("ccl:l1;x86-trap-support.lisp"))

    (arm-trap-support "ccl:l1f;arm-trap-support" ("ccl:l1;arm-trap-support.lisp"))
    (arm64-trap-support "ccl:l1f;arm64-trap-support" ("ccl:l1;arm64-trap-support.lisp"))
    (l1-format        "ccl:l1f;l1-format"        ("ccl:l1;l1-format.lisp"))
    (l1-readloop      "ccl:l1f;l1-readloop"      ("ccl:l1;l1-readloop.lisp"))
    (l1-readloop-lds  "ccl:l1f;l1-readloop-lds"  ("ccl:l1;l1-readloop-lds.lisp"))
    (l1-reader        "ccl:l1f;l1-reader"        ("ccl:l1;l1-reader.lisp"))
    (l1-error-system  "ccl:l1f;l1-error-system"  ("ccl:l1;l1-error-system.lisp"))
    (ppc-error-signal "ccl:l1f;ppc-error-signal" ("ccl:l1;ppc-error-signal.lisp"))
    (x86-error-signal "ccl:l1f;x86-error-signal" ("ccl:l1;x86-error-signal.lisp"))
    (arm-error-signal "ccl:l1f;arm-error-signal" ("ccl:l1;arm-error-signal.lisp"))
    (arm64-error-signal "ccl:l1f;arm64-error-signal" ("ccl:l1;arm64-error-signal.lisp"))
    (l1-error-signal  "ccl:l1f;l1-error-signal"  ("ccl:l1;l1-error-signal.lisp"))
    (l1-aprims        "ccl:l1f;l1-aprims"        ("ccl:l1;l1-aprims.lisp"))
    (l1-callbacks     "ccl:l1f;l1-callbacks"    ("ccl:l1;l1-callbacks.lisp"))
    (ppc-callback-support "ccl:l1f;ppc-callback-support" ("ccl:l1;ppc-callback-support.lisp"))
    (x86-callback-support "ccl:l1f;x86-callback-support" ("ccl:l1;x86-callback-support.lisp"))
    (arm-callback-support "ccl:l1f;arm-callback-support" ("ccl:l1;arm-callback-support.lisp"))
    (arm64-callback-support "ccl:l1f;arm64-callback-support" ("ccl:l1;arm64-callback-support.lisp"))
    (l1-sysio         "ccl:l1f;l1-sysio"         ("ccl:l1;l1-sysio.lisp"))
    (l1-symhash       "ccl:l1f;l1-symhash"       ("ccl:l1;l1-symhash.lisp"))
    (l1-pathnames     "ccl:l1f;l1-pathnames"     ("ccl:l1;l1-pathnames.lisp"))
    (l1-lisp-threads  "ccl:l1f;l1-lisp-threads"  ("ccl:l1;l1-lisp-threads.lisp"))
    (l1-sockets       "ccl:l1f;l1-sockets"       ("ccl:l1;l1-sockets.lisp"))
    (ppc-threads-utils "ccl:l1f;ppc-threads-utils" ("ccl:l1;ppc-threads-utils.lisp"))
    (x86-threads-utils "ccl:l1f;x86-threads-utils" ("ccl:l1;x86-threads-utils.lisp"))
    (arm-threads-utils "ccl:l1f;arm-threads-utils" ("ccl:l1;arm-threads-utils.lisp"))
    (arm64-threads-utils "ccl:l1f;arm64-threads-utils" ("ccl:l1;arm64-threads-utils.lisp"))
    (l1-application   "ccl:l1f;l1-application"   ("ccl:l1;l1-application.lisp"))
    (l1-processes     "ccl:l1f;l1-processes"     ("ccl:l1;l1-processes.lisp"))

    (l1-typesys       "ccl:l1f;l1-typesys"       ("ccl:l1;l1-typesys.lisp"))
    (sysutils         "ccl:l1f;sysutils"         ("ccl:l1;sysutils.lisp"))
    (nx               "ccl:l1f;nx"               ("ccl:compiler;nx.lisp"
                                                  "ccl:compiler;nx0.lisp"
                                                  "ccl:compiler;lambda-list.lisp"
                                                  "ccl:compiler;nx-basic.lisp"
                                                  "ccl:compiler;nx1.lisp"))
    (nxenv            "ccl:bin;nxenv"            ("ccl:compiler;nxenv.lisp"))
    (nx2              "ccl:bin;nx2"              ("ccl:compiler;nx2.lisp"))
    (acode-rewrite    "ccl:bin;acode-rewrite"    ("ccl:compiler;acode-rewrite.lisp"))
    (nx-base-app      "ccl:l1f;nx-base-app"      ("ccl:compiler;nx-base-app.lisp"
                                                  "ccl:compiler;lambda-list.lisp"))
    (dll-node         "ccl:bin;dll-node"         ("ccl:compiler;dll-node.lisp"))
    (ppc32-arch       "ccl:bin;ppc32-arch"       ("ccl:compiler;PPC;PPC32;ppc32-arch.lisp"))
    (ppc-arch         "ccl:bin;ppc-arch"         ("ccl:compiler;PPC;ppc-arch.lisp"))
    (x86-arch         "ccl:bin;x86-arch"         ("ccl:compiler;X86;x86-arch.lisp"))
    (ppc64-arch       "ccl:bin;ppc64-arch"       ("ccl:compiler;PPC;PPC64;ppc64-arch.lisp"))
    (x8632-arch       "ccl:bin;x8632-arch"       ("ccl:compiler;X86;X8632;x8632-arch.lisp"))
    (x8664-arch       "ccl:bin;x8664-arch"       ("ccl:compiler;X86;X8664;x8664-arch.lisp"))
    (arm-arch         "ccl:bin;arm-arch"         ("ccl:compiler;ARM;arm-arch.lisp"))
    (arm64-arch         "ccl:bin;arm64-arch"         ("ccl:compiler;ARM64;arm64-arch.lisp"))
    (arch             "ccl:bin;arch"             ("ccl:compiler;arch.lisp"))
    (ppcenv           "ccl:bin;ppcenv"           ("ccl:lib;ppcenv.lisp"))
    (x8664env         "ccl:bin;x8664env"         ("ccl:lib;x8664env.lisp"))
    (x8632env         "ccl:bin;x8632env"         ("ccl:lib;x8632env.lisp"))
    (armenv           "ccl:bin;armenv"           ("ccl:lib;armenv.lisp"))
    (arm64env           "ccl:bin;arm64env"           ("ccl:lib;arm64env.lisp"))
    (vreg             "ccl:bin;vreg"             ("ccl:compiler;vreg.lisp"))
    (ppc-asm          "ccl:bin;ppc-asm"          ("ccl:compiler;PPC;ppc-asm.lisp"))
    (x86-asm          "ccl:bin;x86-asm"          ("ccl:compiler;X86;x86-asm.lisp"))
    (arm-asm          "ccl:bin;arm-asm"          ("ccl:compiler;ARM;arm-asm.lisp"))
    (arm64-asm          "ccl:bin;arm64-asm"          ("ccl:compiler;ARM64;arm64-asm.lisp"))
    (vinsn            "ccl:bin;vinsn"            ("ccl:compiler;vinsn.lisp"))
    (ppc32-vinsns     "ccl:bin;ppc32-vinsns"     ("ccl:compiler;PPC;PPC32;ppc32-vinsns.lisp"))
    (ppc64-vinsns     "ccl:bin;ppc64-vinsns"     ("ccl:compiler;PPC;PPC64;ppc64-vinsns.lisp"))
    (x8632-vinsns     "ccl:bin;x8632-vinsns"     ("ccl:compiler;X86;X8632;x8632-vinsns.lisp"))
    (x8664-vinsns     "ccl:bin;x8664-vinsns"     ("ccl:compiler;X86;X8664;x8664-vinsns.lisp"))
    (arm-vinsns       "ccl:bin;arm-vinsns"       ("ccl:compiler;ARM;arm-vinsns.lisp"))
    (arm64-vinsns       "ccl:bin;arm64-vinsns"       ("ccl:compiler;ARM64;arm-vinsns.lisp"))
    (reg              "ccl:bin;reg"              ("ccl:compiler;reg.lisp"))
    (subprims         "ccl:bin;subprims"         ("ccl:compiler;subprims.lisp"))
    (risc-lap         "ccl:bin;risc-lap"         ("ccl:compiler;risc-lap.lisp"))
    (ppc-lap          "ccl:bin;ppc-lap"          ("ccl:compiler;PPC;ppc-lap.lisp"))
    (x86-lap          "ccl:bin;x86-lap"          ("ccl:compiler;X86;x86-lap.lisp"))
    (arm-lap          "ccl:bin;arm-lap"          ("ccl:compiler;ARM;arm-lap.lisp"))
    (arm64-lap          "ccl:bin;arm64-lap"          ("ccl:compiler;ARM64;arm-lap.lisp"))
    (backend          "ccl:bin;backend"          ("ccl:compiler;backend.lisp"))
    (ppc32-backend    "ccl:bin;ppc32-backend"    ("ccl:compiler;PPC;PPC32;ppc32-backend.lisp"))			   
    (ppc64-backend    "ccl:bin;ppc64-backend"    ("ccl:compiler;PPC;PPC64;ppc64-backend.lisp"))
    (ppc-backend      "ccl:bin;ppc-backend"      ("ccl:compiler;PPC;ppc-backend.lisp"))
    (x8632-backend    "ccl:bin;x8632-backend"    ("ccl:compiler;X86;X8632;x8632-backend.lisp"))
    (x8664-backend    "ccl:bin;x8664-backend"    ("ccl:compiler;X86;X8664;x8664-backend.lisp"))
    (x86-backend      "ccl:bin;x86-backend"      ("ccl:compiler;X86;x86-backend.lisp"))
    (arm-backend      "ccl:bin;arm-backend"      ("ccl:compiler;ARM;arm-backend.lisp"))
    (arm64-backend      "ccl:bin;arm64-backend"      ("ccl:compiler;ARM64;arm-backend.lisp"))
    (ppc2             "ccl:bin;ppc2"             ("ccl:compiler;PPC;ppc2.lisp"))
    (x862             "ccl:bin;x862"             ("ccl:compiler;X86;x862.lisp"))
    (arm2             "ccl:bin;arm2"             ("ccl:compiler;ARM;arm2.lisp"))
    (arm642             "ccl:bin;arm642"             ("ccl:compiler;ARM64;arm642.lisp"))
    (ppc-lapmacros    "ccl:bin;ppc-lapmacros"    ("ccl:compiler;PPC;ppc-lapmacros.lisp"))
    (x86-lapmacros    "ccl:bin;x86-lapmacros"    ("ccl:compiler;X86;x86-lapmacros.lisp"))
    (arm-lapmacros    "ccl:bin;arm-lapmacros"    ("ccl:compiler;ARM;arm-lapmacros.lisp"))
    (arm64-lapmacros    "ccl:bin;arm64-lapmacros"    ("ccl:compiler;ARM64;arm-lapmacros.lisp"))
    (ppc-disassemble  "ccl:bin;ppc-disassemble"  ("ccl:compiler;PPC;ppc-disassemble.lisp"))
    (x86-disassemble  "ccl:bin;x86-disassemble"  ("ccl:compiler;X86;x86-disassemble.lisp"))
    (arm-disassemble  "ccl:bin;arm-disassemble"  ("ccl:compiler;ARM;arm-disassemble.lisp"))
    (arm64-disassemble  "ccl:bin;arm64-disassemble"  ("ccl:compiler;ARM64;arm-disassemble.lisp"))
    (xfasload         "ccl:xdump;xfasload"       ("ccl:xdump;xfasload.lisp"))
    (xppcfasload      "ccl:xdump;xppcfasload"    ("ccl:xdump;xppcfasload.lisp"))
    (xx8632fasload    "ccl:xdump;xx8632-fasload"  ("ccl:xdump;xx8632-fasload.lisp"))
    (xx8664fasload    "ccl:xdump;xx8664-fasload"  ("ccl:xdump;xx8664-fasload.lisp"))
    (xarmfasload      "ccl:xdump;xarm-fasload"   ("ccl:xdump;xarmfasload.lisp"))
    (xarm64fasload      "ccl:xdump;xarm64-fasload"   ("ccl:xdump;xarm64fasload.lisp"))
    (heap-image       "ccl:xdump;heap-image"     ("ccl:xdump;heap-image.lisp"))
    (xsym             "ccl:xdump;xsym"           ("ccl:xdump;xsym.lisp"))
    (number-macros "ccl:bin;number-macros"    ("ccl:lib;number-macros.lisp"))
    (number-case-macro  "ccl:bin;number-case-macro" ("ccl:lib;number-case-macro.lisp"))
    (optimizers       "ccl:bin;optimizers"       ("ccl:compiler;optimizers.lisp")) 
    (backquote        "ccl:bin;backquote"        ("ccl:lib;backquote.lisp"))
    (lispequ          "ccl:library;lispequ"      ("ccl:library;lispequ.lisp"))
    (sysequ           "ccl:bin;sysequ"           ("ccl:lib;sysequ.lisp"))
    (toolequ          "ccl:bin;toolequ"          ("ccl:lib;toolequ.lisp"))
    (level-2          "ccl:bin;level-2"          ("ccl:lib;level-2.lisp"))
    (macros           "ccl:bin;macros"           ("ccl:lib;macros.lisp"))
    (defstruct-macros "ccl:bin;defstruct-macros" ("ccl:lib;defstruct-macros.lisp"))
    (foreign-types    "ccl:bin;foreign-types"    ("ccl:lib;foreign-types.lisp"))
    (ffi-linuxppc32   "ccl:bin;ffi-linuxppc32"   ("ccl:lib;ffi-linuxppc32.lisp"))
    (ffi-darwinppc32  "ccl:bin;ffi-darwinppc32"  ("ccl:lib;ffi-darwinppc32.lisp"))
    (ffi-darwinppc64  "ccl:bin;ffi-darwinppc64"  ("ccl:lib;ffi-darwinppc64.lisp"))
    (ffi-linuxppc64   "ccl:bin;ffi-linuxppc64"   ("ccl:lib;ffi-linuxppc64.lisp"))
    (ffi-darwinx8632  "ccl:bin;ffi-darwinx8632"  ("ccl:lib;ffi-darwinx8632.lisp"))
    (ffi-linuxx8664   "ccl:bin;ffi-linuxx8664"   ("ccl:lib;ffi-linuxx8664.lisp"))
    (ffi-darwinx8664  "ccl:bin;ffi-darwinx8664"  ("ccl:lib;ffi-darwinx8664.lisp"))
    (ffi-freebsdx8664 "ccl:bin;ffi-freebsdx8664" ("ccl:lib;ffi-freebsdx8664.lisp"))
    (ffi-solarisx8664 "ccl:bin;ffi-solarisx8664" ("ccl:lib;ffi-solarisx8664.lisp"))
    (ffi-win64 "ccl:bin;ffi-win64" ("ccl:lib;ffi-win64.lisp"))
    (ffi-linuxx8632  "ccl:bin;ffi-linuxx8632" ("ccl:lib;ffi-linuxx8632.lisp"))
    (ffi-win32 "ccl:bin;ffi-win32" ("ccl:lib;ffi-win32.lisp"))
    (ffi-solarisx8632 "ccl:bin;ffi-solarisx8632" ("ccl:lib;ffi-solarisx8632.lisp"))
    (ffi-freebsdx8632 "ccl:bin;ffi-freebsdx8632" ("ccl:lib;ffi-freebsdx8632.lisp"))
    (ffi-linuxarm     "ccl:bin;ffi-linuxarm"     ("ccl:lib;ffi-linuxarm.lisp"))
    (ffi-darwinarm    "ccl:bin;ffi-darwinarm"    ("ccl:lib;ffi-darwinarm.lisp"))
    (ffi-androidarm     "ccl:bin;ffi-androidarm"     ("ccl:lib;ffi-androidarm.lisp"))
    (db-io            "ccl:bin;db-io"            ("ccl:lib;db-io.lisp"))
    (hash             "ccl:bin;hash"             ("ccl:lib;hash.lisp"))
    (nfcomp           "ccl:bin;nfcomp"           ("ccl:lib;nfcomp.lisp"))
    (lists            "ccl:bin;lists"            ("ccl:lib;lists.lisp"))
    (chars            "ccl:bin;chars"            ("ccl:lib;chars.lisp"))
    (streams          "ccl:bin;streams"          ("ccl:lib;streams.lisp"))
    (pathnames        "ccl:bin;pathnames"        ("ccl:lib;pathnames.lisp"))
    (describe         "ccl:bin;describe"         ("ccl:lib;describe.lisp")) 
    (mcl-compat       "ccl:bin;mcl-compat"       ("ccl:lib;mcl-compat.lisp"))
    (backtrace        "ccl:bin;backtrace"        ("ccl:lib;backtrace.lisp"))
    (ppc-backtrace    "ccl:bin;ppc-backtrace"    ("ccl:lib;ppc-backtrace.lisp"))
    (x86-backtrace    "ccl:bin;x86-backtrace"    ("ccl:lib;x86-backtrace.lisp"))
    (arm-backtrace    "ccl:bin;arm-backtrace"    ("ccl:lib;arm-backtrace.lisp"))
    (arm64-backtrace    "ccl:bin;arm64-backtrace"    ("ccl:lib;arm64-backtrace.lisp"))
    (x86-watch        "ccl:bin;x86-watch"        ("ccl:lib;x86-watch.lisp"))
    (backtrace-lds    "ccl:bin;backtrace-lds"    ("ccl:lib;backtrace-lds.lisp"))
    (apropos          "ccl:bin;apropos"          ("ccl:lib;apropos.lisp"))
    (numbers          "ccl:bin;numbers"          ("ccl:lib;numbers.lisp"))
    (dumplisp         "ccl:bin;dumplisp"         ("ccl:lib;dumplisp.lisp"))
    (defstruct        "ccl:bin;defstruct"        ("ccl:lib;defstruct.lisp"
                                                  "ccl:lib;defstruct-macros.lisp"))
    (defstruct-lds    "ccl:bin;defstruct-lds"    ("ccl:lib;defstruct-lds.lisp"
                                                  "ccl:lib;defstruct-macros.lisp"))
    (method-combination
     "ccl:bin;method-combination"
     ("ccl:lib;method-combination.lisp"))
    (encapsulate      "ccl:bin;encapsulate"      ("ccl:lib;encapsulate.lisp"))
    (read             "ccl:bin;read"           ("ccl:lib;read.lisp"))
    (misc             "ccl:bin;misc"           ("ccl:lib;misc.lisp"))
    (arrays-fry       "ccl:bin;arrays-fry"     ("ccl:lib;arrays-fry.lisp"))
    (sequences        "ccl:bin;sequences"      ("ccl:lib;sequences.lisp"))
    (sort             "ccl:bin;sort"           ("ccl:lib;sort.lisp"))
    (setf             "ccl:bin;setf"           ("ccl:lib;setf.lisp"))
    (setf-runtime     "ccl:bin;setf-runtime"   ("ccl:lib;setf-runtime.lisp"))
    (format           "ccl:bin;format"         ("ccl:lib;format.lisp"))
    (case-error       "ccl:bin;case-error"     ("ccl:lib;case-error.lisp"))
    (pprint           "ccl:bin;pprint"         ("ccl:lib;pprint.lisp"))
    (time             "ccl:bin;time"           ("ccl:lib;time.lisp"))
    (print-db         "ccl:bin;print-db"       ("ccl:lib;print-db.lisp"))
; (eval             "ccl:bin;eval"           ("ccl:lib;eval.lisp"))

    (arglist          "ccl:bin;arglist"          ("ccl:lib;arglist.lisp"))

    (edit-callers	   "ccl:bin;edit-callers"   ("ccl:lib;edit-callers.lisp"))
    ;; (hash-cons        "ccl:library;hash-cons"    ("ccl:library;hash-cons.lisp"))
    ;; (step             "ccl:bin;step"           ("ccl:lib;step.lisp"))
    (ccl-export-syms  "ccl:bin;ccl-export-syms"  ("ccl:lib;ccl-export-syms.lisp"))
    (systems          "ccl:bin;systems"        ("ccl:lib;systems.lisp"))
    (compile-ccl      "ccl:bin;compile-ccl"    ("ccl:lib;compile-ccl.lisp"))
    (ppc-init-ccl     "ccl:bin;ppc-init-ccl"   ("ccl:lib;ppc-init-ccl.lisp"))
    (distrib-inits    "ccl:bin;distrib-inits"  ("ccl:lib;distrib-inits.lisp"))
    (lisp-package     "ccl:library;lisp-package" ("ccl:library;lisp-package.lisp"))
    ;; need to add swapping, xdump to CCL's *module-search-path*
    (xdump            "ccl:xdump;xdump"          ("ccl:xdump;xdump.lisp"))
    (fasload          "ccl:xdump;fasload"        ("ccl:xdump;fasload.lisp"))
    (loop             "ccl:library;loop"         ("ccl:library;loop.lisp"))
    (linux-files      "ccl:l1f;linux-files"      ("ccl:level-1;linux-files.lisp"))
    (sockets          "ccl:library;sockets"      ("ccl:library;sockets.lisp"))
    (source-files     "ccl:bin;source-files"     ("ccl:lib;source-files.lisp"))
    (swink            "ccl:bin;swink"            ("ccl:lib;swink.lisp"))
    (cover            "ccl:bin;cover"            ("ccl:library;cover.lisp"))
    (leaks            "ccl:bin;leaks"            ("ccl:library;leaks.lisp"))
    (core-files       "ccl:bin;core-files"       ("ccl:library;core-files.lisp"))
    (dominance        "ccl:bin;dominance"        ("ccl:library;dominance.lisp"))
    (swank-loader     "ccl:bin;swank-loader"     ("ccl:library;swank-loader.lisp"))    
    (remote-lisp      "ccl:bin;remote-lisp"      ("ccl:library;remote-lisp.lisp" "ccl:lib;swink.lisp"))
 
    (prepare-mcl-environment "ccl:bin;prepare-mcl-environment" ("ccl:lib;prepare-mcl-environment.lisp"))
    (defsystem        "ccl:tools;defsystem"      ("ccl:tools;defsystem.lisp"))
    (asdf             "ccl:tools;asdf"           ("ccl:tools;asdf.lisp"))
    (jp-encode        "ccl:bin;jp-encode"        ("ccl:library;jp-encode.lisp"))
    (cn-encode        "ccl:bin;cn-encode"        ("ccl:library;cn-encode.lisp"))
    (hashenv          "ccl:bin;hashenv"          ("ccl:xdump;hashenv.lisp"))
    (prefixed-stream  "ccl:bin;prefixed-stream"  ("ccl:library;prefixed-stream.lisp"))
    (timestamped-stream "ccl:bin;timestamped-stream" ("ccl:library;timestamped-stream.lisp"))))
