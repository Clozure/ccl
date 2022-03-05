;;;-*-Mode: LISP; Package: CCL -*-
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

(defconstant array-total-size-limit
  #.(expt 2 (- target::nbits-in-word target::num-subtag-bits))
  "the exclusive upper bound on the total number of elements in an array")


;Features for #+/- conditionalization:
(defparameter *features*
  '(:common-lisp
    :openmcl
    :ccl
    :ccl-1.2
    :ccl-1.3
    :ccl-1.4
    :ccl-1.5
    :ccl-1.6
    :ccl-1.7
    :ccl-1.8
    :ccl-1.9
    :ccl-1.10
    :ccl-1.11
    :ccl-1.12
    :clozure
    :clozure-common-lisp
    :ansi-cl
    :ieee-floating-point
    #-windows-target :unix
    :openmcl-unicode-strings
    :ipv6
    ;; Threads and MOP stuff is pretty redundant.
    :openmcl-native-threads
    :openmcl-partial-mop
    :mcl-common-mop-subset
    :openmcl-mop-2
    ;; Thread-private hash-tables were introduced in version 1.0
    :openmcl-private-hash-tables
    ;; Hash-consing support (special primitives for allocating
    ;; and managing statically allocated CONS cells) will be
    ;; added in 1.1
    ;; Was dropped in 1.2
    #+(and x86-target 64-bit-target)
    :static-conses-should-work-with-egc-in-ccl 
    ;; :openmcl-hash-consing
    ;; 1.12 introduces package-local-nicknames
    :package-local-nicknames
    #+eabi-target :eabi-target
    #+ppc-target :powerpc
    #+ppc-target :ppc-target
    #+ppc-target :ppc-clos              ; used in encapsulate
    #+ppc32-target :ppc32-target
    #+ppc32-target :ppc32-host
    #+ppc64-target :ppc64-target
    #+ppc64-target :ppc64-host
    #+x8632-target :x8632-target
    #+x8632-target :x8632-host
    #+x8664-target :x86-64
    #+x8664-target :x86_64
    #+x8632-target :x86
    #+x86-target :x86-target
    #+x86-target :x86-host
    #+x8664-target :x8664-target
    #+x8664-target :x8664-host
    #+arm-target :arm
    #+arm-target :arm-target
    #+linux-target :linux-host
    #+linux-target :linux-target
    #+linuxppc-target :linuxppc-target
    #+linuxppc-target :linuxppc-host
    #+linuxx86-target :linuxx86-target
    #+linuxx8664-target :linuxx8664-target
    #+linuxx8664-target :linuxx8664-host
    #+linuxx8632-target :linuxx8632-target
    #+linuxx8632-target :linuxx8632-host
    #+linuxarm-target :linuxarm-target
    #+linuxarm-target :linuxarm-host
    #+darwinarm-target :darwinarm-target
    #+darwinarm-target :darwinarm-host
    #+darwinppc-target :darwinppc-target
    #+darwinppc-target :darwinppc-host
    #+darwinppc-target :darwin-target
    #+freebsd-target :freebsd-host
    #+freebsd-target :freebsd-target
    #+freebsdx86-target :freebsdx86-target
    #+freebsdx8664-target :freebsdx8664-target
    #+freebsdx8664-target :freebsdx8664-host
    #+freebsdx8632-target :freebsdx8632-target
    #+freebsdx8632-target :freebsdx8632-host
    #+darwin-target :darwin-host
    #+darwin-target :darwin-target
    #+darwinx86-target :darwinx86-target
    #+darwinx8632-target :darwinx8632-target
    #+darwinx8632-target :darwinx8632-host
    #+darwinx8664-target :darwinx8664-target
    #+darwinx8664-target :darwinx8664-host
    #+windows-target :windows-host
    #+windows-target :windows-target
    #+win64-target :win64-target
    #+win64-target :win64-host
    #+win32-target :win32-target
    #+win32-target :win32-host
    #+solaris-target :solaris-host
    #+solaris-target :solaris-target
    #+solarisx86-target :solarisx86-target
    #+solarisx8664-target :solarisx8664-target
    #+solarisx8664-target :solarisx8664-host
    #+solarisx8632-target :solarisx8632-target
    #+solarisx8632-target :solarisx8632-host
    #+android-target :android-host
    #+android-target :android-target
    #+androidarm-target :androidarm-target
    #+androidarm-target :androidarm-host
    #+(and ppc-target poweropen-target) :poweropen-target
    #+64-bit-target :64-bit-target
    #+64-bit-target :64-bit-host
    #+32-bit-target :32-bit-target
    #+32-bit-target :32-bit-host
    #+darwin-target :darwin
    #+linux-target :linux
    #+freebsd-target :freebsd
    #+solaris-target :solaris
    #+windows-target :windows
    #+android-target :android
    #+little-endian-target :little-endian-target
    #+little-endian-target :little-endian-host
    #+big-endian-target :big-endian-target
    #+big-endian-target :big-endian-host
    )
  "a list of symbols that describe features provided by the
   implementation")

(defparameter *optional-features* () "Set by build process")

(defparameter *load-verbose* nil
  "the default for the :VERBOSE argument to LOAD")

;All Lisp package variables... Dunno if this still matters, but it
;used to happen in the kernel...
(dolist (x '(* ** *** *APPLYHOOK* *DEBUG-IO*
             *DEFAULT-PATHNAME-DEFAULTS* *ERROR-OUTPUT* *EVALHOOK*
             *FEATURES* *LOAD-VERBOSE* *MACROEXPAND-HOOK* *MODULES*
             *PACKAGE* *PRINT-ARRAY* *PRINT-BASE* *PRINT-CASE* *PRINT-CIRCLE*
             *PRINT-ESCAPE* *PRINT-GENSYM* *PRINT-LENGTH* *PRINT-LEVEL*
             *PRINT-PRETTY* *PRINT-RADIX* *QUERY-IO* *RANDOM-STATE* *READ-BASE*
             *READ-DEFAULT-FLOAT-FORMAT* *READ-SUPPRESS* *READTABLE*
             *STANDARD-INPUT* *STANDARD-OUTPUT* *TERMINAL-IO* *TRACE-OUTPUT*
             + ++ +++ - / // /// ARRAY-DIMENSION-LIMIT ARRAY-RANK-LIMIT
             ARRAY-TOTAL-SIZE-LIMIT BOOLE-1 BOOLE-2 BOOLE-AND BOOLE-ANDC1
             BOOLE-ANDC2 BOOLE-C1 BOOLE-C2 BOOLE-CLR BOOLE-EQV BOOLE-IOR
             BOOLE-NAND BOOLE-NOR BOOLE-ORC1 BOOLE-ORC2 BOOLE-SET BOOLE-XOR
             CALL-ARGUMENTS-LIMIT CHAR-CODE-LIMIT
             DOUBLE-FLOAT-EPSILON DOUBLE-FLOAT-NEGATIVE-EPSILON
             INTERNAL-TIME-UNITS-PER-SECOND LAMBDA-LIST-KEYWORDS
             LAMBDA-PARAMETERS-LIMIT LEAST-NEGATIVE-DOUBLE-FLOAT
             LEAST-NEGATIVE-LONG-FLOAT LEAST-NEGATIVE-SHORT-FLOAT
             LEAST-NEGATIVE-SINGLE-FLOAT LEAST-POSITIVE-DOUBLE-FLOAT
             LEAST-POSITIVE-LONG-FLOAT LEAST-POSITIVE-SHORT-FLOAT
             LEAST-POSITIVE-SINGLE-FLOAT LONG-FLOAT-EPSILON
             LONG-FLOAT-NEGATIVE-EPSILON MOST-NEGATIVE-DOUBLE-FLOAT
             MOST-NEGATIVE-FIXNUM MOST-NEGATIVE-LONG-FLOAT
             MOST-NEGATIVE-SHORT-FLOAT MOST-NEGATIVE-SINGLE-FLOAT
             MOST-POSITIVE-DOUBLE-FLOAT MOST-POSITIVE-FIXNUM
             MOST-POSITIVE-LONG-FLOAT MOST-POSITIVE-SHORT-FLOAT
             MOST-POSITIVE-SINGLE-FLOAT MULTIPLE-VALUES-LIMIT PI
             SHORT-FLOAT-EPSILON SHORT-FLOAT-NEGATIVE-EPSILON
             SINGLE-FLOAT-EPSILON SINGLE-FLOAT-NEGATIVE-EPSILON))
  (%symbol-bits x (%ilogior2 (%symbol-bits x) (ash 1 $sym_bit_special))))

(defparameter *loading-file-source-file* nil)
(defparameter *loading-toplevel-location* nil)

(defvar *nx-speed* 1)
(defvar *nx-space* 1)
(defvar *nx-safety* 1)
(defvar *nx-cspeed* 1)
(defvar *nx-debug* 1)

(defparameter *case-sensitive-filesystem* t)

;;; end
