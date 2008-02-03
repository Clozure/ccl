;;;-*- Mode: Lisp; Package: CCL -*-
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

;; Compile-time environment for fasl dumper/loader.

; loader state istruct
(def-accessors (faslstate) %svref
  ()
  faslstate.faslfname
  faslstate.faslevec
  faslstate.faslecnt
  faslstate.faslfd
  faslstate.faslval
  faslstate.faslstr
  faslstate.oldfaslstr
  faslstate.faslerr
  faslstate.iobuffer
  faslstate.bufcount
  faslstate.faslversion
  faslstate.faslepush
  faslstate.faslgsymbols
  faslstate.fasldispatch)

;;; loader framework istruct
(def-accessors (faslapi) %svref
  ()
  ;; these represent all users of faslstate.iobuffer, .bufcount, and
  ;; .faslfd -- I think these are all the important file- and
  ;; buffer-IO-specific slots in faslstate; encapsulating these allows
  ;; sophisticated users to load fasl data from nonstandard sources
  ;; without too much trouble
  faslapi.fasl-open
  faslapi.fasl-close
  faslapi.fasl-init-buffer
  faslapi.fasl-set-file-pos
  faslapi.fasl-get-file-pos
  faslapi.fasl-read-buffer
  faslapi.fasl-read-byte
  faslapi.fasl-read-n-bytes)

(defconstant numfaslops 80 "Number of fasl file opcodes, roughly")
(defconstant $fasl-epush-bit 7)
(defconstant $fasl-file-id #xff00)
(defconstant $fasl-file-id1 #xff01)
(defconstant $fasl-vers #x52)
(defconstant $fasl-min-vers #x51)
(defconstant $faslend #xff)
(defconstant $fasl-buf-len 2048)
(defmacro deffaslop (n arglist &body body)
  `(setf (svref *fasl-dispatch-table* ,n)
         #'(lambda ,arglist ,@body)))


(defconstant $fasl-noop 0)              ;<nada:zilch>.  
(defconstant $fasl-s32-vector 1)        ;<count> Make a (SIMPLE-ARRAY (SIGNED-BYTE 32) <count>)
(defconstant $fasl-code-vector 2)       ;<count> words of code
(defconstant $fasl-clfun 3)             ;<size:count><codesize:count>code,size-codesize exprs
(defconstant $fasl-lfuncall 4)          ;<lfun:expr> funcall the lfun.
(defconstant $fasl-globals 5)           ;<expr> global symbols vector
(defconstant $fasl-char 6)              ;<char:byte> Make a char
(defconstant $fasl-fixnum 7)            ;<value:long> Make a (4-byte) fixnum
(defconstant $fasl-dfloat 8)            ;<hi:long><lo:long> Make a DOUBLE-FLOAT
(defconstant $fasl-bignum32 9)          ;<count> make a bignum with count digits
(defconstant $fasl-word-fixnum 10)      ;<value:word> Make a fixnum
(defconstant $fasl-double-float-vector 11) ;<count> make a (SIMPLE-ARRAY DOUBLE-FLOAT <count>)
(defconstant $fasl-single-float-vector 12) ;<count> make a (SIMPLE-ARRAY SINGLE-FLOAT <count>)
(defconstant $fasl-bit-vector 13)       ;<count> make a (SIMPLE-ARRAY BIT <count>)
(defconstant $fasl-u8-vector 14)        ;<count> make a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) <count>)
(defconstant $fasl-cons 15)             ;<car:expr><cdr:expr> Make a cons
(defconstant $fasl-s8-vector 16)        ;<count> make a (SIMPLE-ARRAY (SIGNED-BYTE 8) <count>)
(defconstant $fasl-t-vector 17)         ;<count> make a (SIMPLE-ARRAY T <count>)
(defconstant $fasl-nil 18)              ; Make nil
(defconstant $fasl-timm 19)             ;<n:long>
(defconstant $fasl-function 20)         ;<count> Make function
(defconstant $fasl-vstr 21)             ;<vstring> Make a string
(defconstant $fasl-vmksym 22)           ;<vstring> Make an uninterned symbol
(defconstant $fasl-platform 23)         ;<n:byte> Ensure that file's loadable on platform n.
(defconstant $fasl-vetab-alloc 24)      ;<count:count> Make a new expression table
                                        ; with count slots.  Current etab gets lost.
(defconstant $fasl-veref 25)            ;<index:count> Get the value from an etab slot.
(defconstant $fasl-fixnum8 26)          ;<high:long><low:long> Make an 8-byte fixnum.
(defconstant $fasl-symfn 27)            ;<sym:expr> 
(defconstant $fasl-eval 28)             ;<expr> Eval <expr> and return value.
(defconstant $fasl-u16-vector 29)       ;<count> Make a (SIMPLE-ARRAY (UNSIGNED-BYTE 16) <count>)
(defconstant $fasl-s16-vector 30)       ;<count> Make a (SIMPLE-ARRAY (SIGNED-BYTE 16) <count>)
(defconstant $fasl-vintern 31)          ;<vstring> Intern in current pkg.
(defconstant $fasl-vpkg-intern 32)      ;<pkg:expr><vstring> Make a sym in pkg.
(defconstant $fasl-vpkg 33)             ;<vstring> Returns the package of given name
(defconstant $fasl-vgvec 34)            ;<subtype:byte><n:count><n exprs>
(defconstant $fasl-defun 35)            ;<fn:expr><doc:expr>
(defconstant $fasl-macro 37)            ;<fn:expr><doc:expr>
(defconstant $fasl-defconstant 38)      ;<sym:expr><val:expr><doc:expr>
(defconstant $fasl-defparameter 39)     ;<sym:expr><val:expr><doc:expr>
(defconstant $fasl-defvar 40)           ;<sym:expr>
(defconstant $fasl-defvar-init 41)      ;<sym:expr><val:expr><doc:expr>
(defconstant $fasl-vivec 42)            ;<subtype:byte><n:count><n data bytes>
(defconstant $fasl-prog1 43)            ;<expr><expr> - Second <expr> is for side-affects only
(defconstant $fasl-vlist 44)            ;<n:count> <data: n+1 exprs> Make a list
(defconstant $fasl-vlist* 45)           ;<n:count> <data:n+2 exprs> Make an sexpr
(defconstant $fasl-sfloat 46)           ;<long> Make SINGLE-FLOAT from bits
(defconstant $fasl-src 47)              ;<expr> - Set *loading-file-source-file * to <expr>.
(defconstant $fasl-u32-vector 48)       ;<count> Make a (SIMPLE-ARRAY (UNSIGNED-BYTE 32) <count>)
(defconstant $fasl-provide 49)          ;<string:expr>
(defconstant $fasl-u64-vector 50)       ;<count> Make a (SIMPLE-ARRAY (UNSIGNED-BYTE 64) <count>)
(defconstant $fasl-s64-vector 51)       ;<count> Make a (SIMPLE-ARRAY (SIGNED-BYTE 64) <count>)
(defconstant $fasl-istruct 52)          ;<count> Make an ISTRUCT with <count> elements
(defconstant $fasl-complex 53)          ;<real:expr><imag:expr>
(defconstant $fasl-ratio 54)            ;<num:expr><den:expr>
(defconstant $fasl-vector-header 55)    ;<count> Make a vector header
(defconstant $fasl-array-header 56)     ;<count> Make an array header.
(defconstant $fasl-s32 57)              ;<4bytes> Make a (SIGNED-BYTE 32)
(defconstant $fasl-vintern-special 58)  ;<vstring> Intern in current pkg, ensure that it has a special binding index
(defconstant $fasl-s64 59)              ;<8bytes> Make a (SIGNED-BYTE 64)
(defconstant $fasl-vpkg-intern-special 60) ;<pkg:expr><vstring> Make a sym in pkg, ensure that it has a special binding index
(defconstant $fasl-vmksym-special 61)   ;<vstring> Make an uninterned symbol, ensure special binding index
(defconstant $fasl-nvmksym-special 62)  ;<nvstring> Make an uninterned symbol, ensure special binding index
(defconstant $fasl-nvpkg-intern-special 63) ;<pkg:expr><nvstring> Make a sym in pkg, ensure that it has a special binding index
(defconstant $fasl-nvintern-special 64)  ;<nvstring> Intern in current pkg, ensure that it has a special binding index
(defconstant $fasl-nvpkg 65)            ;<vstring> Returns the package of given name
(defconstant $fasl-nvpkg-intern 66)     ;<nvstring> Intern in current pkg.
(defconstant $fasl-nvintern 67)         ;<pkg:expr><nvstring> Make a sym in pkg.
(defconstant $fasl-nvmksym 68)          ;<nvstring> Make a string
(defconstant $fasl-nvstr 69)            ;<nvstring> Make an uninterned symbol


;;; <string> means <size><size bytes> (this is no longer used)
;;; <size> means either <n:byte> with n<#xFF, or <FF><n:word> with n<#xFFFF or
;;;   <FFFF><n:long>
;;; <count> is a variable-length encoding of an unsigned integer, written
;;;  7 bits per octet, the least significant bits written first and the most
;;;  significant octet having bit 7 set, so 127 would be written as #x00 and
;;;  128 as #x00 #x81
;;; <vstring> is a <count> (string length) followed by count octets of
;;; 8-bit charcode data.
;;; <nvstring> is a <count> (string length) followd by count <counts> of
;;;  variable-length charcode data.  This encodes ASCII/STANDARD-CHAR as
;;;  compactly as the <vstring> encoding, which should probably be deprecated.



(defconstant $fasl-end #xFF)    ;Stop reading.

(defconstant $fasl-epush-mask #x80)  ;Push value on etab if this bit is set in opcode.

(defmacro fasl-epush-op (op) `(%ilogior2 ,$fasl-epush-mask ,op))

(provide "FASLENV")
