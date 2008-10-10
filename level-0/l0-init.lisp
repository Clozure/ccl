;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
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

(defconstant array-total-size-limit
  #.(expt 2 (- target::nbits-in-word target::num-subtag-bits))
  "the exclusive upper bound on the total number of elements in an array")


;Features for #+/- conditionalization:
(defparameter *features*
  '(:common-lisp
    :openmcl
    :ccl
    :ccl-1.2
    :clozure
    :clozure-common-lisp
    :ansi-cl
    #-windows-target :unix
    :openmcl-unicode-strings
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
    ;; :openmcl-hash-consing
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
    #+x8632-target :x86)
    #+x86-target :x86-target
    #+x86-target :x86-host
    #+x8664-target :x8664-target
    #+x8664-target :x8664-host
    #+linux-target :linux-host
    #+linux-target :linux-target
    #+linuxppc-target :linuxppc-target
    #+linuxppc-target :linuxppc-host
    #+linuxx86-target :linuxx86-target
    #+linuxx8664-target :linuxx8664-target
    #+linuxx8664-target :linuxx8664-host
    #+linuxx8632-target :linuxx8632-target
    #+linuxx8632-target :linuxx8632-host
    #+darwinppc-target :darwinppc-target
    #+darwinppc-target :darwinppc-host
    #+darwinppc-target :darwin-target
    #+freebsd-target :freebsd-host
    #+freebsd-target :freebsd-target
    #+freebsdx86-target :freebsdx86-target
    #+freebsdx8664-target :freebsdx8664-target
    #+freebsdx8664-target :freebsdx8664-host
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
    #+poweropen-target :poweropen-target
    #+64-bit-target :64-bit-target
    #+64-bit-target :64-bit-host
    #+32-bit-target :32-bit-target
    #+32-bit-target :32-bit-host
    #+ppc-target :big-endian-target
    #+ppc-target :big-endian-host
    #+x86-target :little-endian-target
    #+x86-target :little-endian-host
    #+darwin-target :darwin
    #+linux-target :linux
    #+freebsd-target :freebsd
    #+solaris-target :solaris
    #+windows-target :windows
    ;; :mcl                                ;deprecated
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

(defvar *nx-speed* 1)
(defvar *nx-space* 1)
(defvar *nx-safety* 1)
(defvar *nx-cspeed* 1)
(defvar *nx-debug* 1)

;;; end
