;;;
;;;   Copyright (C) 2009, Clozure Associates and contributors
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

;; Functions to examine core files.

(in-package :ccl)

#+:linuxx8664-target
(progn

(export '(open-core close-core
          core-heap-utilization map-core-areas map-core-pointers
          core-q core-l core-w core-b
          core-consp core-symbolp core-functionp core-listp core-nullp core-uvector-p
          core-uvtype core-uvtypep core-uvref core-uvsize
          core-car core-cdr core-object-type core-istruct-type
          copy-from-core core-list
          core-keyword-package core-find-package core-find-symbol
          core-package-names core-package-name
          core-map-symbols
          core-symbol-name core-symbol-value core-symbol-package
          core-gethash core-hash-table-count
          core-lfun-name core-lfun-bits
          core-find-class
          core-instance-class
          core-instance-p
          core-instance-class-name
          core-string-equal
          core-all-processes core-process-name
          core-find-process-for-id
          core-print
          core-print-call-history
          ))

(eval-when (:compile-toplevel :execute)
  (require "HASHENV" "ccl:xdump;hashenv"))

;; The intended way to use these facilities is to open up a particular core file once,
;; and then repeatedly call functions to examine it.  So for convenience, we keep the
;; core file in a global var, rather than making all user functions take an extra arg.
;; There is nothing intrinsic that would prevent having multiple core files open at once.

(defvar *current-core* nil)


(defstruct core-info
  pathname
  sections
  ;; uses either stream or ivector, determined at runtime
  stream
  mapped-ivector
  raw-ivector
  ;; caches
  symbol-ptrs
  classes-hash-table-ptr
  lfun-names-table-ptr
  process-class
  )

(defmethod print-object :around ((core core-info) (stream t))
  (let ((*print-array* nil))
    (call-next-method)))

(declaim (type (or null core-info) *current-core*)
         (ftype (function () core-info) current-core)
         (inline current-core))

(defun current-core ()
  (or *current-core* (require-type *current-core* 'core-info)))

(defun close-core ()
  (let ((core *current-core*))
    (setq *current-core* nil)
    (when core
      (when (core-info-stream core)
        (close (core-info-stream core)))
      (when (core-info-mapped-ivector core)
        (unmap-ivector (core-info-mapped-ivector core)))
      t)))

;; TODO: after load sections, check if highest heap address is a fixnum, and
;; arrange to use fixnum-only versions of the reading functions.
(defun open-core (pathname &key (method :mmap))
  (when *current-core*
    (close-core))
  (let* ((sections (readelf-sections pathname))
         (core (make-core-info :pathname pathname :sections sections)))
    (ecase method
      (:mmap   (let ((mapped-vector (map-file-to-ivector pathname '(unsigned-byte 8))))
                 (multiple-value-bind (vector offset) (array-data-and-offset mapped-vector)
                   (loop for data across sections do (incf (cdr data) offset))
                   (setf (core-info-mapped-ivector core) mapped-vector)
                   (setf (core-info-raw-ivector core) vector))))
      (:stream (setf (core-info-stream core)
                     (open pathname :element-type '(unsigned-byte 8)))))
    (setq *current-core* core))
  pathname)

;; Kinda stupid to call external program for this...
(defun readelf-sections (pathname)
  (flet ((split (line start end)
           (loop while (setq start (position-if-not #'whitespacep line :start start :end end))
                 as match = (cdr (assq (char line start) '((#\[ . #\]) (#\( . #\)) (#\< . #\>))))
                 as next = (if match
                             (1+ (or (position match line :start (1+ start) :end end)
                                     (error "Unmatched ~c at position ~s" (char line start) start)))
                             (or (position-if #'whitespacep line :start start :end end) end))
                 collect (subseq line start next)
                 do (setq start next))))
    (let* ((file (native-translated-namestring pathname))
           (string (with-output-to-string (output)
                     (ccl:run-program "readelf" `("--sections" ,file) :output output)))
           (sections (loop
                       for start = (1+ (position #\newline string
                                                 :start (1+ (position #\newline string
                                                                      :start (position #\[ string)))))
                         then next
                       for next = (1+ (position #\newline string
                                                :start (1+ (position #\newline string :start start))))
                       while (eql #\space (aref string next))
                       nconc
                       (destructuring-bind (number name type address filepos size &optional ent-size flags link info align)
                           (split string start next)
                         (assert (and (eql (char number 0) #\[) (eql (char number (1- (length number))) #\])))
                         (setq number (read-from-string number :start 1 :end (1- (length number))))
                         (when (eql number 0)
                           (shiftf align info link flags ent-size size filepos address type name ""))
                         (setq address (parse-integer address :radix 16))
                         (setq filepos  (parse-integer filepos :radix 16))
                         (setq size (parse-integer size :radix 16))
                         (setq ent-size (parse-integer ent-size :radix 16))
                         (unless (eql size 0)
                           (assert (and (equal link "0") (equal info "0") (equal align "1")))
                           (list (list address filepos size))))))
           (sections (cons (list most-positive-fixnum 0 0) sections));; hack for loop below
           (sections (sort sections #'< :key #'car));; sort by address
           (sections (loop
                       with cur-address = -1
                       with cur-filepos = -1
                       with cur-end = cur-address
                       for (address filepos size) in sections
                       unless (or (= (+ cur-filepos (- address cur-address)) filepos)
                                  (= cur-address cur-end))
                         collect (cons cur-address cur-filepos)
                       do (if (= (+ cur-filepos (- address cur-address)) filepos)
                            (setq cur-end (max (+ address size) cur-end))
                            (progn
                              (assert (<= cur-end address));; no overlap.
                              (setq cur-address address cur-filepos filepos cur-end (+ address size)))))))
      (coerce sections 'vector))))

(declaim (inline core-ivector-readb core-ivector-readw core-ivector-readl core-ivector-readq
                 core-stream-readb core-stream-readw core-stream-readl core-stream-readq))
(declaim (ftype (function (t t) (unsigned-byte 8)) core-ivector-readb core-stream-readb)
         (ftype (function (t t) (unsigned-byte 16)) core-ivector-readw core-stream-readw)
         (ftype (function (t t) (unsigned-byte 32)) core-ivector-readl core-stream-readl)
         (ftype (function (t t) (unsigned-byte 64)) core-ivector-readq core-stream-readq)
         (ftype (function (integer) fixnum) core-offset-for-address))

(defun core-offset-for-address (address)
  ;; sections are sorted, so could do binary search if this became a bottleneck.
  ;; (there are around 50 sections)
  (or (loop for prev = nil then sect as sect across (core-info-sections (current-core))
            do (when (< address (car sect))
                 (return (and prev (+ (cdr prev) (- address (car prev)))))))
      (error "Unknown core address x~x" address)))

(defun core-stream-readb (s offset)
  (declare (type basic-input-stream s) (optimize (speed 3) (safety 0)))
  (when offset (stream-position s offset))
  (read-byte s))

(defun core-stream-readw (s offset)
  (declare (type basic-input-stream s) (optimize (speed 3) (safety 0)))
  (when offset (stream-position s offset))
  (%i+ (core-stream-readb s nil) (ash (core-stream-readb s nil) 8)))

(defun core-stream-readl (s offset)
  (declare (type basic-input-stream s) (optimize (speed 3) (safety 0)))
  (when offset (stream-position s offset))
  (%i+ (core-stream-readw s nil) (ash (core-stream-readw s nil) 16)))

(defun core-stream-readq (s offset)
  (declare (type basic-input-stream s) (optimize (speed 3) (safety 0)))
  (when offset (stream-position s offset))
  (+ (core-stream-readl s nil) (ash (core-stream-readl s nil) 32)))

(defun core-ivector-readb (vec offset)
  (declare (type (simple-array (unsigned-byte 8) (*)) vec) (fixnum offset)
           (optimize (speed 3) (safety 0)))
  (aref vec offset))

(defun core-ivector-readw (vec offset)
  (declare (optimize (speed 3) (safety 0)))
  (%i+ (core-ivector-readb vec offset) (ash (core-ivector-readb vec (%i+ offset 1)) 8)))

(defun core-ivector-readl (vec offset)
  (declare (optimize (speed 3) (safety 0)))
  (%i+ (core-ivector-readw vec offset) (ash (core-ivector-readw vec (%i+ offset 2)) 16)))

(defun core-ivector-readq (vec offset)
  (declare (optimize (speed 3) (safety 0)))
  (+ (core-ivector-readl vec offset) (ash (core-ivector-readl vec (%i+ offset 4)) 32)))


(defun core-q (address &optional (offset 0))
  (declare (optimize (speed 3) (safety 0)))
  (let* ((core (current-core))
         (ivector (core-info-raw-ivector core)))
    (declare (type core-info core))
    (if ivector
      (core-ivector-readq ivector (core-offset-for-address (+ address offset)))
      (core-stream-readq (core-info-stream core) (core-offset-for-address (+ address offset))))))

(defun core-l (address &optional (offset 0))
  (declare (optimize (speed 3) (safety 0)))
  (let* ((core (current-core))
         (ivector (core-info-raw-ivector core)))
    (declare (type core-info core))
    (if ivector
      (core-ivector-readl ivector (core-offset-for-address (+ address offset)))
      (core-stream-readl (core-info-stream core) (core-offset-for-address (+ address offset))))))

(defun core-w (address &optional (offset 0))
  (declare (optimize (speed 3) (safety 0)))
  (let* ((core (current-core))
         (ivector (core-info-raw-ivector core)))
    (declare (type core-info core))
    (if ivector
      (core-ivector-readw ivector (core-offset-for-address (+ address offset)))
      (core-stream-readw (core-info-stream core) (core-offset-for-address (+ address offset))))))

(defun core-b (address &optional (offset 0))
  (declare (optimize (speed 3) (safety 0)))
  (let* ((core (current-core))
         (ivector (core-info-raw-ivector core)))
    (declare (type core-info core))
    (if ivector
      (core-ivector-readb ivector (core-offset-for-address (+ address offset)))
      (core-stream-readb (core-info-stream core) (core-offset-for-address (+ address offset))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; general utilities

;; NIL is constant, assume is same in core as here.
(defun kernel-global-address (global)
  (check-type global symbol)
  (+ (target-nil-value) (target::%kernel-global global)))

(defun nil-relative-symbol-address (sym)
  (+ (target-nil-value)
     #x20  ;;; dunno why
     (* (or (position sym x86::*x86-nil-relative-symbols* :test #'eq)
            (error "Not a nil-relative symbol ~s" sym))
        target::symbol.size)
     (- target::fulltag-symbol target::fulltag-nil)))

(defun core-area-name (code)
  (or (heap-area-name code)
      (and (integerp code)
           (not (logtest code (1- (ash 1 target::fixnum-shift))))
           (heap-area-name (ash code (- target::fixnum-shift))))))

(defx86lapfunction %%raw-obj ((address arg_z))
  (unbox-fixnum address arg_z)
  (single-value-return))

(declaim (inline uvheader-p uvheader-typecode uvheader-size))

(defun uvheader-p (header)
  (let ((tag (logand header target::fulltagmask)))
    (declare (fixnum tag))
    (and (<= target::fulltag-nodeheader-0 tag)
         (<= tag target::fulltag-immheader-2)
         (neq tag target::fulltag-odd-fixnum))))

(defun uvheader-typecode (header)
  (the fixnum (logand #xFF header)))

(defun uvheader-size (header)
  (ash header (- target::num-subtag-bits)))

(defun uvheader-byte-size (header)
  (x8664::x8664-misc-byte-count (uvheader-typecode header) (uvheader-size header)))

(defun uvheader-type (header)
  (let* ((typecode (uvheader-typecode header))
         (low4 (logand typecode target::fulltagmask))
         (high4 (ash typecode (- target::ntagbits))))
    (declare (type (unsigned-byte 8) typecode)
             (type (unsigned-byte 4) low4 high4))
    (cond ((eql low4 x8664::fulltag-immheader-0)
           (%svref *immheader-0-types* high4))
          ((eql low4 x8664::fulltag-immheader-1)
           (%svref *immheader-1-types* high4))
          ((eql low4 x8664::fulltag-immheader-2)
           (%svref *immheader-2-types* high4))
          ((eql low4 x8664::fulltag-nodeheader-0)
           (%svref *nodeheader-0-types* high4))
          ((eql low4 x8664::fulltag-nodeheader-1)
           (%svref *nodeheader-1-types* high4))
          (t 'bogus))))

(defun uvheader-type-typecode (symbol &aux pos)
  (unless (eq symbol 'bogus)
    (cond ((setq pos (position symbol *immheader-0-types*))
           (logior (ash pos target::ntagbits) target::fulltag-immheader-0))
          ((setq pos (position symbol *immheader-1-types*))
           (logior (ash pos target::ntagbits) target::fulltag-immheader-1))
          ((setq pos (position symbol *immheader-2-types*))
           (logior (ash pos target::ntagbits) target::fulltag-immheader-2))
          ((setq pos (position symbol *nodeheader-0-types*))
           (logior (ash pos target::ntagbits) target::fulltag-nodeheader-0))
          ((setq pos (position symbol *nodeheader-1-types*))
           (logior (ash pos target::ntagbits) target::fulltag-nodeheader-1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Core heap

(defun map-core-areas (function &key area)
  (setq area (cond ((or (eq area t) (eq area nil)) nil)
                   ((consp area) (mapcar #'heap-area-code area))
                   (t (list (heap-area-code area)))))
  (loop for area-ptr = (core-q (core-q (kernel-global-address 'all-areas)) target::area.succ)
          then (core-q area-ptr target::area.succ)
        as code = (ash (core-q area-ptr target::area.code) (- target::fixnum-shift))
        until (= code area-void)
        do (when (and (<= area-readonly code)
                      (<= code area-dynamic)
                      (or (null area) (member code area))
                      (< (core-q area-ptr target::area.low) (core-q area-ptr target::area.active)))
             #+debug
             (format t "~& AREA at x~x, type = ~a low = x~x active = x~x (size = x~x out of x~x)"
                     area-ptr (core-area-name code)
                     (core-q area-ptr target::area.low)
                     (core-q area-ptr target::area.active)
                     (- (core-q area-ptr target::area.active) (core-q area-ptr target::area.low))
                     (- (core-q area-ptr target::area.high) (core-q area-ptr target::area.low)))
             (map-core-area area-ptr function))))

(defun map-core-area (area-ptr fun)
  (let* ((ptr (core-q area-ptr target::area.low))
         (end (core-q area-ptr target::area.active)))
    (loop
      (when (>= ptr end) (return))
      (let ((header (core-q ptr)))
        (cond ((uvheader-p header)
               (let ((subtag (uvheader-typecode header)))
                 (funcall fun
                          (+ ptr (cond ((eq subtag target::subtag-symbol) target::fulltag-symbol)
                                       ((eq subtag target::subtag-function) target::fulltag-function)
                                       (t target::fulltag-misc)))))
               (let* ((bytes (uvheader-byte-size header))
                      (total (logandc2 (%i+ bytes (+ target::node-size (1- target::dnode-size)))
                                       (1- target::dnode-size))))
                 (declare (fixnum bytes total))
                 (incf ptr total)))
              (t
               (funcall fun (+ ptr target::fulltag-cons))
               (incf ptr target::cons.size)))))))


(declaim (inline core-consp core-symbolp core-functionp core-listp core-nullp))

(defun core-consp (ptr)
  (eq (logand ptr target::fulltagmask) target::fulltag-cons))

(defun core-symbolp (ptr)
  (eq (logand ptr target::fulltagmask) target::fulltag-symbol))

(defun core-functionp (ptr)
  (eq (logand ptr target::fulltagmask) target::fulltag-function))

(defun core-listp (ptr)
  (eq (logand ptr target::tagmask) target::tag-list))

(defun core-nullp (obj)
  (eq (logand obj target::fulltagmask) target::fulltag-nil))

;; uvector utilities
(declaim (inline core-uvector-p core-uvheader core-uvtypecode core-uvtype))

(defun core-uvector-p (ptr)
  (%i>= (logand ptr target::fulltagmask) target::fulltag-misc))

(defun core-uvheader (vec-ptr)
  (core-q (logandc2 vec-ptr target::fulltagmask)))

(defun core-uvtypecode (vec-ptr)
  (uvheader-typecode (core-uvheader vec-ptr)))

(defun core-uvtype (vec-ptr)
  (uvheader-type (core-uvheader vec-ptr)))

(defmacro core-uvtypep (vec-ptr type &aux temp)
  (when (keywordp type)
    (setq type (type-keyword-code type)))
  (when (and (or (symbolp (setq temp type))
                 (and (quoted-form-p type)
                      (symbolp (setq temp (cadr type)))))
             (setq temp (find-symbol (symbol-name temp) :ccl))
             (setq temp (uvheader-type-typecode temp)))
    (setq type temp))
  (when (constant-symbol-p type)
    (setq temp (symbol-value type))
    (when (<= 0 temp #xFF) (setq type temp)))
  `(let ((vec-ptr ,vec-ptr))
     (and (core-uvector-p vec-ptr)
          (eq (core-uvtypecode vec-ptr) ,type))))

(defun core-uvref (vec-ptr index)
  (let* ((header (core-uvheader vec-ptr))
         (addr (+ (logandc2 vec-ptr target::fulltagmask) target::node-size))
         (typecode (uvheader-typecode header))
         (tag (logand typecode target::fulltagmask))
         (len (uvheader-size header)))
    (assert (< -1 index len))
    (cond ((or (eql tag target::fulltag-nodeheader-0)
               (eql tag target::fulltag-nodeheader-1))
           (core-q addr (ash index target::word-shift)))
          ((eql tag target::ivector-class-64-bit)
           (cond ((eq typecode target::subtag-double-float-vector)
                  (error "~s not implemented yet" 'target::subtag-double-float-vector))
                 (t
                  (core-q addr (ash index target::word-shift)))))
          ((eq tag target::ivector-class-32-bit)
           (cond ((eq typecode target::subtag-simple-base-string)
                  (code-char (core-l addr (ash index 2))))
                 ((eq typecode target::subtag-single-float-vector)
                  (error "~s not implemented yet" 'target::subtag-single-float-vector))
                 (t (core-l addr (ash index 2)))))
          ((eq typecode target::subtag-bit-vector)
           (let ((byte (core-b addr (ash (+ index 7) -3))))
             (error "not implemented, for ~b" byte)))
          ((>= typecode target::min-8-bit-ivector-subtag)
           (core-b addr index))
          (t (core-w addr (ash index 1))))))

(defun core-uvsize (vec-ptr)
  (uvheader-size (core-uvheader vec-ptr)))

(defun core-car (obj)
  (assert (core-listp obj))
  (core-q obj target::cons.car))

(defun core-cdr (obj)
  (assert (core-listp obj))
  (core-q obj target::cons.cdr))

(defun core-object-type (obj)
  (let ((fulltag (logand obj target::fulltagmask)))
    (cond ((eq fulltag target::fulltag-cons) 'cons)
          ((eq fulltag target::fulltag-nil) 'null)
          ((eq (logand fulltag target::tagmask) target::tag-fixnum) 'fixnum)
          ((and (or (eq fulltag target::fulltag-imm-0)
                    (eq fulltag target::fulltag-imm-1))
                (fixnump obj))
           ;; Assumes we're running on same architecture as core file.
           (type-of (%%raw-obj obj)))
          ((eq (logand fulltag target::tagmask) target::tag-tra) 'tagged-return-address)
          ((eq fulltag target::fulltag-misc) (core-uvtype obj))
          ((eq fulltag target::fulltag-symbol) 'symbol)
          ;; TODO: Could get hairier based on lfun-bits, but usually don't care.
          ((eq fulltag target::fulltag-function) 'function)
          (t (cerror "treat as ~*~s" "Invalid object tag at #x~x" obj 'bogus)
           'bogus))))

(defun core-istruct-type (obj)
  (and (core-uvtypep obj :istruct)
       (core-car (core-uvref obj 0))))
       

(defun core-object-type-and-size (obj)
  (let ((fulltag (logand obj target::fulltagmask)))
    (if (eq fulltag target::fulltag-cons)
      (values 'cons target::dnode-size target::dnode-size)
      (if (%i<= target::fulltag-misc fulltag)
        (let* ((header (core-uvheader obj))
               (logsize (uvheader-byte-size header))
               ;; total including header and alignment.
               (total (logandc2 (+ logsize target::node-size (1- target::dnode-size))
                                (1- target::dnode-size))))
          (values (uvheader-type header) logsize total))))))

(defun core-heap-utilization (&key area unit sort)
  (let* ((hash (make-hash-table :shared nil))
         (total-physsize 0)
         (div (ecase unit
                ((nil) 1)
                (:kb 1024.0d0)
                (:mb (* 1024.0d0 1024.0d0))
                (:gb (* 1024.0d0 1024.0d0 1024.0d0))))
         (sort-key (ecase sort
                     (:count #'cadr)
                     (:logical-size #'caddr)
                     ((:physical-size nil) #'cdddr)))
         (all nil))
    (map-core-areas (lambda (obj)
                      (multiple-value-bind (type logsize physsize) (core-object-type-and-size obj)
                        (let ((a (or (gethash type hash)
                                     (setf (gethash type hash) (list* 0 0 0)))))
                          (incf (car a))
                          (incf (cadr a) logsize)
                          (incf (cddr a) physsize))))
                    :area area)
    (maphash (lambda (type data)
               (incf total-physsize (cddr data))
               (push (cons type data) all))
             hash)
    (setq all (sort all #'> :key sort-key))
    (format t "~&Object type~42tCount    Logical size   Physical size   % of Heap~%~50t~a~66t~:*~a"
            (ecase unit
              ((nil) " (in bytes)")
              (:kb   "(in kilobytes)")
              (:mb   "(in megabytes)")
              (:gb   "(in gigabytes)")))
    (loop for (type count logsize . physsize) in all
          do (if unit
               (format t "~&~a~36t~11d~16,2f~16,2f~11,2f%"
                       type
                       count
                       (/ logsize div)
                       (/ physsize div)
                       (* 100.0 (/ physsize total-physsize)))
               (format t "~&~a~36t~11d~16d~16d~11,2f%"
                       type
                       count
                       logsize
                       physsize
                       (* 100.0 (/ physsize total-physsize)))))
    (if unit
      (format t "~&Total~63t~16,2f" (/ total-physsize div))
      (format t "~&Total~63t~16d" total-physsize)))
  (values))


(defstruct unresolved-address address)

(defmethod print-object ((obj unresolved-address) stream)
  (let* ((address (unresolved-address-address obj)))
    (format stream "#<Core ~S~@[[~d]~] #x~x >" 
            (core-object-type address)
            (and (core-uvector-p address) (core-uvsize address))
            address)))

(defun copy-from-core (obj &key (depth 1))
  (check-type depth (integer 0))
  (when (unresolved-address-p obj)
    (setq obj (unresolved-address-address obj)))
  (let ((fulltag (logand obj target::fulltagmask)))
    (cond ((eq fulltag target::fulltag-nil) nil)
          ((eq (logand fulltag target::tagmask) target::tag-fixnum)
           (ash obj (- target::fixnum-shift)))
          ((and (fixnump obj)
                (or (eq fulltag target::fulltag-imm-0)
                    (eq fulltag target::fulltag-imm-1)))
           (%%raw-obj obj))
          ((< (decf depth) 0)
           (make-unresolved-address :address obj))
          ((%i<= target::fulltag-misc fulltag)
           (or (and (core-uvtypep obj :package)
                    (find-package (core-package-name obj)))
               (let ((v (%copy-uvector-from-core obj depth)))
                 (when (and (symbolp v) (<= depth 1))
                   ;; Need to fix up the package slot else it's not useful
                   (let ((pp (%svref (symptr->symvector v) target::symbol.package-predicate-cell)))
                     (when (unresolved-address-p pp)
                       (setq pp (copy-from-core pp :depth 1)))
                     (when (and (consp pp) (unresolved-address-p (car pp)))
                       (let ((pkg (unresolved-address-address (car pp))))
                         (when (and (core-uvtypep pkg :package)
                                    (setq pkg (find-package (core-package-name pkg))))
                           (setf (car pp) pkg))))
                     (setf (%svref (symptr->symvector v) target::symbol.package-predicate-cell) pp))
                   ;; ditto for pname
                   (let ((pp (%svref (symptr->symvector v) target::symbol.pname-cell)))
                     (when (unresolved-address-p pp)
                       (setf (%svref (symptr->symvector v) target::symbol.pname-cell)
                             (copy-from-core pp :depth 1)))))
                 v)))
          ((eq fulltag target::fulltag-cons)
           (cons (copy-from-core (core-car obj) :depth depth)
                 (copy-from-core (core-cdr obj) :depth depth)))
          (t (make-unresolved-address :address obj)))))

(defun %copy-uvector-from-core (vec-ptr depth)
  (let* ((header (core-uvheader vec-ptr))
         (addr (+ (logandc2 vec-ptr target::fulltagmask) target::node-size))
         (typecode (uvheader-typecode header))
         (tag (logand typecode target::fulltagmask))
         (len (uvheader-size header))
         (vec (%alloc-misc len typecode)))
    (cond ((or (eq tag target::fulltag-nodeheader-0)
               (eq tag target::fulltag-nodeheader-1))
           (when (eql typecode target::subtag-function)
             ;; Don't bother copying the code for now
             (let ((skip (core-l addr)))
               (assert (<= 0 skip len))
               (incf addr (ash skip target::word-shift))
               (decf len skip)))
           (dotimes (i len)
             (setf (%svref vec i)
                   (copy-from-core (core-q addr (ash i target::word-shift)) :depth depth)))
           (let ((ptrtag (logand vec-ptr target::fulltagmask)))
             (cond ((eql ptrtag target::fulltag-symbol)
                    (%symvector->symptr vec))
                   ((eql ptrtag target::fulltag-function)
                    (%function-vector-to-function vec))
                   (t vec))))
          ((eq tag target::ivector-class-64-bit)
           (cond ((eq typecode target::subtag-double-float-vector)
                  (warn "~s not implemented yet" 'target::subtag-double-float-vector)
                  (make-unresolved-address :address vec-ptr))
                 (t
                  (dotimes (i len vec)
                    (setf (uvref vec i) (core-q addr (ash i target::word-shift)))))))
          ((eq tag target::ivector-class-32-bit)
           (cond ((eq typecode target::subtag-simple-base-string)
                  (dotimes (i len vec)
                    (setf (uvref vec i) (code-char (core-l addr (ash i 2))))))
                 ((eq typecode target::subtag-single-float-vector)
                  (warn "~s not implemented yet" 'target::subtag-single-float-vector)
                  (make-unresolved-address :address vec-ptr))
                 (t
                  (dotimes (i len vec)
                    (setf (uvref vec i) (core-l addr (ash i 2)))))))
          ((eq typecode target::subtag-bit-vector)
           (warn "bit vector not implemented yet")
           (make-unresolved-address :address vec-ptr))
          ((>= typecode target::min-8-bit-ivector-subtag)
           (dotimes (i len vec)
             (setf (uvref vec i) (core-b addr i))))
          (t
           (dotimes (i len vec)
             (setf (uvref vec i) (core-w addr (ash i 1))))))))

(defun map-core-pointers (fn &key area)
  (map-core-areas (lambda (obj)
                    (cond ((core-consp obj)
                           (funcall fn (core-car obj) obj 0)
                           (funcall fn (core-cdr obj) obj 1))
                          (t
                           (let* ((header (core-uvheader obj))
                                  (subtag (logand header target::fulltagmask)))
                             (when (or (eq subtag target::fulltag-nodeheader-0)
                                       (eq subtag target::fulltag-nodeheader-1))
                               (let* ((typecode (uvheader-typecode header))
                                      (len (uvheader-size header))
                                      (addr (+ (logandc2 obj target::fulltagmask) target::node-size)))
                                 (when (eql typecode target::subtag-function)
                                   (let ((skip (core-l addr)))
                                     (assert (<= 0 skip len))
                                     (incf addr (ash skip target::word-shift))
                                     (decf len skip)))
                                 (dotimes (i len)
                                   (funcall fn (core-q addr (ash i target::word-shift)) obj i))))))))
                  :area area))

(defun core-find-tra-function (tra)
  (assert (eq (logand tra target::tagmask) target::tag-tra))
  (map-core-areas (lambda (obj)
                    (when (core-uvtypep obj :function)
                      (let* ((addr (+ (logandc2 obj target::fulltagmask) target::node-size))
                             (skip  (core-l addr))
                             (offset (- tra addr)))
                        (when (<= 0 offset (ash skip target::word-shift))
                          (return-from core-find-tra-function (values obj (+ offset (- target::node-size
                                                                                       (logand obj target::fulltagmask)))))))))))

(defun core-instance-class (obj)
  (when (core-uvtypep obj :slot-vector)
    (setq obj (core-uvref obj slot-vector.instance)))
  (assert (core-uvtypep obj :instance))
  (core-uvref (core-uvref obj instance.class-wrapper) %wrapper-class))

(defun core-instance-p (obj class)
  (and (core-uvtypep obj :instance)
       (labels ((matchp (iclass)
                  (or (eql iclass class)
                      (loop for supers = (core-uvref (core-uvref iclass instance.slots) %class.local-supers)
                              then (core-cdr supers)
                            while (core-consp supers)
                            thereis (matchp (core-car supers))))))
         (matchp (core-instance-class obj)))))


(defun core-instance-class-name (obj)
  (let* ((class (core-instance-class obj))
         (class-slots (core-uvref class instance.slots))
         (name (core-uvref class-slots %class.name)))
    (core-symbol-name name)))

(defun core-symptr (obj)
  (if (core-nullp obj)
    (nil-relative-symbol-address 'nil)
    (when (core-uvtypep obj :symbol)
      (let ((tag (logand obj target::fulltagmask)))
        (unless (eq tag target::fulltag-symbol)
          (incf obj (%i- target::fulltag-symbol tag))))
      obj)))
    
(defun core-symbol-name (obj)
  (when (setq obj (core-symptr obj))
    (copy-from-core (core-q obj target::symbol.pname) :depth 1)))

(defun core-symbol-value (obj)
  (when (setq obj (core-symptr obj))
    (core-q obj target::symbol.vcell)))

(defun core-symbol-package (obj)
  (when (setq obj (core-symptr obj))
    (let ((cell (core-q obj target::symbol.package-predicate)))
      (if (core-consp cell)
        (core-car cell)
        cell))))

(defun core-all-packages-ptr ()
  (core-symbol-value (nil-relative-symbol-address '%all-packages%)))

(defun core-keyword-package ()
  (core-symbol-value (nil-relative-symbol-address '*keyword-package*)))

(defun core-symbol-pointers ()
  (or (core-info-symbol-ptrs (current-core))
      (let ((vector (make-array 1000 :adjustable t :fill-pointer 0))
            (keys (core-keyword-package)))
        (map-core-areas (lambda (obj)
                          (when (core-symbolp obj)
                            (unless (eq (core-symbol-package obj) keys)
                              (vector-push-extend obj vector)))))
        (setf (core-info-symbol-ptrs (current-core)) vector))))

(defun core-map-symbols (fun)
  (loop for sym-ptr across (core-symbol-pointers) do (funcall fun sym-ptr)))


(defun core-string-equal (ptr string &aux (len (length string)))
  (assert (core-uvtypep ptr :simple-string))
  (when (eq (core-uvsize ptr) len)
    (loop for i from 0 below len
          always (eql (core-uvref ptr i) (aref string i)))))

(defun core-find-package (name &key error)
  (setq name (string name))
  (or (loop for list-ptr = (core-all-packages-ptr) then (core-cdr list-ptr)
            while (core-consp list-ptr)
            as pkg-ptr = (core-car list-ptr)
            when (loop for names-ptr = (core-uvref pkg-ptr pkg.names) then (core-cdr names-ptr)
                       while (core-consp names-ptr)
                       as name-ptr = (core-car names-ptr)
                       thereis (core-string-equal name-ptr name))
              do (return pkg-ptr))
      (and error (error "No package named ~s" name))))

(defun core-package-names (pkg-ptr)
  (assert (core-uvtypep pkg-ptr :package))
  (copy-from-core (core-uvref pkg-ptr pkg.names) :depth 2))

(defun core-package-name (pkg-ptr)
  (assert (core-uvtypep pkg-ptr :package))  
  (copy-from-core (core-car (core-uvref pkg-ptr pkg.names)) :depth 1))

(defun core-find-symbol (name &optional (package (symbol-package name)))
  ;; Unlike the real cl:find-symbol, this doesn't look for inherited symbols,
  ;; you have to get the package right.
  (let* ((symbol-name (string name))
         (name-len (length symbol-name))
         (pkg-ptr (if (integerp package)
                    package
                    (core-find-package (if (packagep package)
                                         (package-name package)
                                         (string package))
                                       :error t))))
    (assert (core-uvtypep pkg-ptr :package))
    (multiple-value-bind (primary secondary) (hash-pname symbol-name name-len)
      (flet ((findsym (htab-ptr)
               (let* ((vec-ptr (core-car htab-ptr))
                      (vlen (core-uvsize vec-ptr)))
                 (loop for idx = (fast-mod primary vlen) then (+ i secondary)
                       for i = idx then (if (>= idx vlen) (- idx vlen) idx)
                       as sym = (core-uvref vec-ptr i)
                       until (eql sym 0)
                       do (when (and (core-symbolp sym)
                                     (core-string-equal (core-q sym target::symbol.pname) symbol-name))
                            (return (if (eq sym (nil-relative-symbol-address 'nil))
                                      (target-nil-value)
                                      sym)))))))
        (or (findsym (core-uvref pkg-ptr pkg.itab))
            (findsym (core-uvref pkg-ptr pkg.etab)))))))

(defun core-gethash (key-ptr hash-ptr)
  (when (core-uvtypep hash-ptr :istruct)
    (setq hash-ptr (core-uvref hash-ptr nhash.vector)))
  (assert (core-uvtypep hash-ptr :hash-vector))
  (loop for i from $nhash.vector_overhead below (core-uvsize hash-ptr) by 2
        do (when (eq (core-uvref hash-ptr i) key-ptr)
             (return (core-uvref hash-ptr (1+ i))))))

(defun core-hash-table-count (hash-ptr)
  (when (core-uvtypep hash-ptr :istruct)
    (setq hash-ptr (core-uvref hash-ptr nhash.vector)))
  (assert (core-uvtypep hash-ptr :hash-vector))
  (loop with rehashing = (%fixnum-address-of (%slot-unbound-marker))
        with free = (%fixnum-address-of (%unbound-marker))
        for i from $nhash.vector_overhead below (core-uvsize hash-ptr) by 2
        count (let ((value (core-uvref hash-ptr (1+ i))))
                (when (eq value rehashing)
                  (error "This table is being rehashed"))
                (neq value free))))

(defun core-classes-hash-table-ptr ()
  (or (core-info-classes-hash-table-ptr (current-core))
      (setf (core-info-classes-hash-table-ptr (current-core))
            (core-symbol-value (core-find-symbol '%find-classes%)))))

(defun core-find-class (name)
  (let* ((name-ptr (etypecase name
                     (integer 
                        (assert (core-symbolp name))
                        name)
                     (symbol (core-find-symbol name))))
         (hash-ptr (core-classes-hash-table-ptr))
         (cell (core-gethash name-ptr hash-ptr))
         (class (and cell (core-uvref cell class-cell-class))))
    (and class (core-uvtypep class :instance) class)))

(defun core-lfun-names-table-ptr ()
  (or (core-info-lfun-names-table-ptr (current-core))
      (setf (core-info-lfun-names-table-ptr (current-core))
            (core-symbol-value (core-find-symbol '*lfun-names*)))))

(defun core-closure-function (fun)
  (while (and (core-functionp fun)
              (logbitp $lfbits-trampoline-bit (core-lfun-bits fun)))
    (let* ((addr (+ (logandc2 fun target::fulltagmask) target::node-size)))
      (setq fun (core-q addr (ash (core-l addr) target::word-shift)))
      (when (core-uvtypep fun :simple-vector)
        (setq fun (core-uvref fun 0)))
      #+gz (assert (core-functionp fun))))
  fun)

    
(defun core-lfun-name (fn)
  (assert (core-functionp fn))
  (flet ((lfun-name (fn)
           (or (core-gethash fn (core-lfun-names-table-ptr))
               (let* ((lfbits (core-lfun-bits fn))
                      (name (if (and (logbitp $lfbits-gfn-bit lfbits)
                                     (not (logbitp $lfbits-method-bit lfbits)))
                                (core-uvref (core-uvref fn gf.slots) sgf.name)
                                (unless (logbitp $lfbits-noname-bit lfbits)
                                  (core-uvref fn (- (core-uvsize fn) 2))))))
                 (and name
                      (not (eql name (%fixnum-address-of (%slot-unbound-marker))))
                      (not (core-nullp name))
                      name)))))
    (or (lfun-name fn)
        (let ((inner-fn (core-closure-function fn)))
          (and (core-functionp inner-fn)
               (not (eql inner-fn fn))
               (lfun-name inner-fn))))))

(defun core-list (ptr)
  (let ((cars (loop while (core-consp ptr)
                    collect (core-car ptr)
                    do (setq ptr (core-cdr ptr)))))
    (if (core-nullp ptr)
      cars
      (nconc cars ptr))))

(defun core-all-processes ()
  (let* ((sym (core-find-symbol 'all-processes))
         (closure (core-uvref sym target::symbol.fcell-cell))
         (imm-start (core-l (logandc2 closure target::fulltagmask) target::node-size))
         (imm-end (core-uvsize closure))
         (vcell (loop for idx from (1+ imm-start) below imm-end as imm = (core-uvref closure idx)
                      when (core-uvtypep imm :value-cell) return imm))
         (val (core-uvref vcell target::value-cell.value-cell))
         (processes (core-list val)))
    processes))

(defun core-process-name (proc)
  (assert (core-uvtypep proc :instance))
  (let ((slots (core-uvref proc ccl::instance.slots)))
    (copy-from-core (core-uvref slots 1) :depth 1)))

(defun core-process-tcr (proc)
  (assert (core-uvtypep proc :instance))
  (let* ((slots (core-uvref proc ccl::instance.slots))
         (thread (core-uvref slots 2)))
    (core-uvref thread ccl::lisp-thread.tcr)))

(defun core-find-process-for-id (lwp)
  (loop for proc in (core-all-processes)
        when (eql lwp (core-q (core-process-tcr proc) target::tcr.native-thread-id))
          return proc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun core-process-class ()
  (or (core-info-process-class (current-core))
      (setf (core-info-process-class (current-core))
            (core-find-class 'process))))

(defun core-print (obj &optional (stream t) depth)
  ;; TODO: could dispatch on core-object-type...
  (cond ((core-nullp obj) (format stream "NIL"))
        ((core-symbolp obj)
         (core-print-symbol obj stream))
        ((core-uvtypep obj :function)
         (core-print-function obj stream))
        ((core-instance-p obj (core-process-class))
         (core-print-process obj stream))
        ((and depth (< (decf depth) 0))
         (format stream "x~x" obj))
        ((core-consp obj)
         (loop for sep = "(" then " "
               for i from 0 below (or *print-length* 100)
               while (core-consp obj)
               do (format stream sep)
               do (core-print (core-car obj) stream depth)
               do (setq obj (core-cdr obj)))
         (unless (core-nullp obj)
           (format stream " . ")
           (core-print obj stream depth))
         (format stream ")"))
        (t (format stream "#<core ~s x~x>"
                   (core-object-type obj) obj))))

(defun core-print-symbol (sym stream)
  (let ((package (core-symbol-package sym)))
    (cond ((core-nullp package)
           (format stream "#:"))
          ((eq package (core-keyword-package))
           (format stream ":"))
          (t (let ((pkgname (core-package-name package)))
               (unless (string-equal pkgname "COMMON-LISP")
                 (format stream "~a::" pkgname)))))
    (format stream "~a" (core-symbol-name sym))))

(defun core-lfun-bits (fun)
  (ash (core-uvref fun (1- (core-uvsize fun))) (- target::fixnum-shift)))

(defun core-print-function (fun stream)
  (let* ((lfbits (core-lfun-bits fun))
         (name (core-lfun-name fun)))
    (format stream "#<")
    (cond ((or (null name) (core-nullp name))
           (format stream "Anonymous function"))
          ((logbitp $lfbits-method-bit lfbits)
           (assert (core-uvtypep name :instance))
           (let* ((slot-vector (core-uvref name instance.slots))
                  (method-qualifiers (core-uvref slot-vector %method.qualifiers))
                  (method-specializers (core-uvref slot-vector %method.specializers))
                  (method-name (core-uvref slot-vector %method.name)))
             (format stream "Method-Function ")
             (core-print method-name stream)
             (format stream " ")
             (unless (core-nullp method-qualifiers)
               (if (core-nullp (core-cdr method-qualifiers))
                 (core-print (core-car method-qualifiers) stream)
                 (core-print method-qualifiers stream))
               (format stream " "))
             ;; print specializer list but print names instead of classes.
             (loop for sep = "(" then " "
                   while (core-consp method-specializers)
                   do (format stream sep)
                   do (let ((spec (core-car method-specializers)))
                        (if (core-uvtypep spec :instance)
                          (core-print (core-uvref (core-uvref spec instance.slots) %class.name) stream)
                          (core-print spec stream)))
                   do (setq method-specializers (core-cdr method-specializers)))
             (unless (core-nullp method-specializers)
               (format stream " . ")
               (core-print method-specializers stream))
             (format stream ")")))
          (t
           (if (logbitp $lfbits-gfn-bit lfbits)
               (format stream "Generic Function ")
               (format stream "Function "))
           (core-print name stream)))
    (format stream " x~x>" fun)))

(defun core-print-process (proc stream)
  (format stream "#<~a ~s LWP(~d) #x~x>"
          (core-instance-class-name proc)
          (core-process-name proc)
          (core-q (core-process-tcr proc) target::tcr.native-thread-id)
          proc))

(defun dwim-core-frame-pointer (tcr &optional end)
  (let* ((ret1valn (core-q (kernel-global-address 'ret1valaddr)))
         (lexprs (list (core-q (kernel-global-address 'lexpr-return))
                       (core-q (kernel-global-address 'lexpr-return1v))))
         (stack-area (core-q tcr target::tcr.vs-area))
         (fp (core-q stack-area target::area.high))
         (low (core-q stack-area target::area.low)))
    (flet ((validp (pp)
             (let ((tra (core-q pp target::lisp-frame.return-address)))
               (when (eql tra ret1valn)
                 (setq tra (core-q pp target::lisp-frame.xtra)))
               (or (eql (logand tra target::tagmask) target::tag-tra)
                   (eql tra 0)
                   (member tra lexprs)))))
      (decf fp (* 2 target::node-size))
      (when (and end (<= low end fp))
        (setq low (- end 8)))
      (loop while
            (loop for pp downfrom (- fp target::node-size) above low by target::node-size
                  do (when (eql (core-q pp target::lisp-frame.backptr) fp)
                       (when (validp pp)
                         (return (setq fp pp))))))
      fp)))

(defun core-stack-frame-values (tcr fp)
  (let* ((bottom (core-q fp target::lisp-frame.backptr))
         (top (if (eql 0 (core-q fp target::lisp-frame.return-address))
                (+ fp target::xcf.size)
                (+ fp (if (eql (core-q fp target::lisp-frame.return-address)
                               (core-q (kernel-global-address 'ret1valaddr)))
                        target::lisp-frame.size
                        target::lisp-frame.xtra))))
         (db-link (loop as db = (core-q tcr target::tcr.db-link) then (core-q db)
                        until (or (eql db 0) (>= db bottom))
                        when (<= top db) return db)))
    (loop for vsp from top below bottom by target::node-size
          when (eql vsp db-link)
            ;; The db-link will be followed by var and val, which we'll just collect normally
            do (setq db-link (core-q db-link) vsp (+ vsp target::node-size))
            and collect `(:db-link ,db-link)
          collect (core-q vsp))))

(defun core-print-call-history (process &key (stream t) origin detailed-p)
  (flet ((fp-backlink (fp vs-end)
           (let ((backlink (core-q fp target::lisp-frame.backptr)))
             (when (or (eql backlink 0)
                       (<= vs-end backlink)
                       (<= vs-end (core-q backlink target::lisp-frame.backptr)))
               (setq backlink vs-end))
             (assert (< fp backlink))
             backlink))
         (fp-tra (fp)
           (let ((tra (core-q fp target::lisp-frame.return-address)))
             (if (eql tra (core-q (kernel-global-address 'ret1valaddr)))
               (core-q fp target::lisp-frame.xtra)
               tra)))
         (recover-fn (pc)
           (when (and (eql (logand pc target::tagmask) target::tag-tra)
                      (eql (core-w pc) target::recover-fn-from-rip-word0)
                      (eql (core-b pc 2) target::recover-fn-from-rip-byte2))
             (+ pc target::recover-fn-from-rip-length
                (- (core-l pc target::recover-fn-from-rip-disp-offset)
                   #x100000000)))))
    (format stream "~&")
    (core-print process stream)
    (let* ((tcr (core-process-tcr process))
           (vs-area (core-q tcr target::tcr.vs-area))
           (vs-end (core-q vs-area target::area.high))
           (valence (core-q tcr target::tcr.valence))
           (fp (or origin
                   ;; TODO: find the registers in the core file!
                   (case valence
                     ;; TCR_STATE_LISP
                     (0 (let ((xp (core-q tcr target::tcr.suspend-context)))
                          (format stream "~&")
                          (if (eql xp 0)
                            (format stream "Unknown lisp context, guessing frame pointer:")
                            (core-print (core-q xp (* 10 target::node-size)) stream)) ;; r13 = fn
                          (if (eql xp 0)
                            (dwim-core-frame-pointer tcr)
                            ;; uc_mcontext.gregs[rbp]
                            (core-q xp (* 15 target::node-size)))))
                     ;; TCR_STATE_FOREIGN
                     (1 (format stream "~&In foreign code")
                        ;; the save-rbp seems to include some non-lisp frames sometimes,
                        ;; shave them down.
                        #+no (core-q tcr target::tcr.save-rbp)
                        (dwim-core-frame-pointer tcr (core-q tcr target::tcr.save-rbp)))
                     ;; TCR_STATE_EXCEPTION_WAIT
                     (2 (let ((xp (core-q tcr target::tcr.pending-exception-context)))
                          ;; regs start at index 5, in this order:
                          ;; arg_x temp1 ra0 save3 save2 fn save1 save0 arg_y arg_z
                          ;; rbp temp0 imm1 imm0 nargs rsp rip
                          (format stream " exception-wait")
                          (if (zerop xp)
                            (format stream "~&context unknown")
                            (let* ((fn (core-q xp (* 10 target::node-size)))
                                   (sp (core-q xp (* 20 target::node-size)))
                                   (ra (core-q sp)))
                              (if (and (core-functionp fn)
                                       (and (<= fn ra)
                                            (< ra (+ fn (* (core-uvsize fn) target::node-size)))))
                                (progn
                                  (format stream "~&")
                                  (core-print fn stream)
                                  (format stream " + ~d" (- ra fn)))
                                (progn
                                  (format stream "~&top of stack = x~x, r13 = " ra)
                                  (core-print fn stream)))))
                          (unless (zerop xp)
                            (core-q xp (* 15 target::node-size))))))
                   (error "Cannot find frame pointer"))))
      (unless (<= (core-q vs-area target::area.low) fp vs-end)
        (error "frame pointer x~x is not in stack area" fp))
      (loop while (< fp vs-end) for pc = (fp-tra fp) for fun = (recover-fn pc)
            do (format stream "~&fp: x~x  pc: x~x : " fp pc)
            do (cond (fun
                      (core-print fun stream)
                      (format stream " + ~d " (- pc fun)))
                     ((eql pc 0) ;; exception frame
                      (let* ((nominal-function (core-q fp target::xcf.nominal-function))
                             (obj (core-q fp target::xcf.containing-object)))
                        (when (core-functionp nominal-function)
                          (format stream "exception ")
                          (core-print nominal-function stream)
                          (format stream " + ~d"
                                  (if (eq (- obj target::fulltag-misc)
                                          (- nominal-function target::fulltag-function))
                                    (- (core-q fp target::xcf.relative-pc) target::tag-function)
                                    (let ((pc (core-q fp target::xcf.ra0)))
                                      (when (eql nominal-function (recover-fn pc))
                                        (- pc nominal-function))))))))
                     ((eql pc (core-q (kernel-global-address 'lexpr-return)))
                      (format stream "lexpr return"))
                     ((eql pc (core-q (kernel-global-address 'lexpr-return1v)))
                      (format stream "lexpr1v return"))
                     (t
                      (if (eql (logand pc target::tagmask) target::tag-tra)
                        (format stream " # couldn't recover function")
                        (unless (core-nullp pc)
                          (format stream "bad frame!")))
                      ;; can't trust backlink
                      (return)))
               ;; TODO: print stack addressses
            do (when detailed-p
                 (loop for val in (core-stack-frame-values tcr fp)
                       do (format stream "~&     ")
                       do (if (integerp val)
                            (handler-case (core-print val stream)
                              (error () (format stream "#<Error printing value @x~x>" val)))
                            (format stream "~a x~x" (car val) (cadr val)))))
            do (setq fp (fp-backlink fp vs-end))))))


)                             ; :x8664-target
