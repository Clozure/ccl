;;;
;;;   Copyright (C) 2009-2010 Clozure Associates and contributors
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


(defconstant $image-nsections 7)
(defconstant $image-data-offset-64 9)
(defconstant $image-header-size 16)

(defconstant $image-sect-code 0)
(defconstant $image-sect-size 4)
(defconstant $image-sect-header-size 8)

(export '(open-core close-core
          core-heap-utilization map-core-areas map-core-region map-core-pointers
          core-q core-l core-w core-b
          core-consp core-symbolp core-functionp core-listp core-nullp core-uvector-p
          core-uvtype core-uvtypep core-uvref core-uvsize
          core-car core-cdr core-object-typecode-type
          core-istruct-type core-struct-type core-instance-type core-function-type
          core-object-type-key  core-type-string
          copy-from-core core-list
          core-keyword-package core-find-package core-find-symbol
          core-package-names core-package-name
          core-map-symbols
          core-symbol-name core-symbol-value core-symbol-package core-symbol-plist
          core-gethash core-hash-table-count
          core-lfun-name core-lfun-bits core-nth-immediate
          core-find-class
          core-instance-class
          core-instance-p
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


(eval-when (load eval #-BOOTSTRAPPED compile)

(defstruct core-info
  pathname
  sections
  ;; uses either stream or ivector, determined at runtime
  streams
  ivectors
  ;; caches
  symbol-ptrs
  classes-hash-table-ptr
  lfun-names-table-ptr
  process-class
  )
)

(defmethod print-object :around ((core core-info) (stream t))
  (let ((*print-array* nil)
        (*print-simple-bit-vector* nil))
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
      (map nil #'close (core-info-streams core))
      (map nil #'unmap-ivector (core-info-ivectors core))
      t)))

;
(defmacro area-loop (with ptrvar &body body)
  (assert (eq with 'with))
  (let ((before (loop while (eq (car body) 'with)
                      nconc (list (pop body) (pop body) (pop body) (pop body)))))
    `(loop ,@before
           for ,ptrvar = (core-q (core-q (kernel-global-address 'all-areas)) target::area.succ)
             then (core-q ,ptrvar target::area.succ)
           until (eq (core-q area-ptr target::area.code) (ash area-void target::fixnum-shift))
           ,@body)))

(def-accessor-macros %svref
  %core-sect.start-addr
  %core-sect.offset
  %core-sect.end-addr
  %core-sect.ivector
  %core-sect.stream)

(defun make-core-sect (&key start end offset ivector stream)
  (vector start offset end ivector stream))


(defvar *core-info-class* 'core-info)

;; TODO: after load sections, check if highest heap address is a fixnum, and
;; arrange to use fixnum-only versions of the reading functions.
(defun open-core (pathname &key (image nil) (method :mmap) (core-info nil))
  (when *current-core*
    (close-core))
  (let* ((sections (read-sections pathname))
         (core (require-type (or core-info (make-instance *core-info-class*)) 'core-info)))
    (setf (core-info-pathname core) pathname)
    (setf (core-info-sections core) sections)
    (setf (core-info-symbol-ptrs core) nil)
    (setf (core-info-classes-hash-table-ptr core) nil)
    (setf (core-info-lfun-names-table-ptr core) nil)
    (setf (core-info-process-class core) nil)
    (setf (core-info-ivectors core) nil)
    (setf (core-info-streams core) nil)
    (ecase method
      (:mmap   (let ((mapped-vector (map-file-to-ivector pathname '(unsigned-byte 8))))
                 (multiple-value-bind (vector offset) (array-data-and-offset mapped-vector)
                   (push mapped-vector (core-info-ivectors core))
                   (loop for sect across sections
                         do (incf (%core-sect.offset sect) offset)
                         do (setf (%core-sect.ivector sect) vector)))))
      (:stream (let ((stream (open pathname :element-type '(unsigned-byte 8)
                                   :sharing :lock)))
                 (push stream (core-info-streams core))
                 (loop for sect across sections do (setf (%core-sect.stream sect) stream)))))
    (setq *current-core* core))
  ;;(unless (every (lambda (sect) (fixnump (car sect))) (core-info-sections (current-core)))
  ;;  (error "Non-fixnum addresses not supported"))
  (when (and image
             (area-loop with area-ptr
                        thereis (and (eq (core-q area-ptr target::area.code)
                                         (ash area-readonly target::fixnum-shift))
                                     (< (core-q area-ptr target::area.low) (core-q area-ptr target::area.active))
                                     (not (core-section-for-address (core-q area-ptr target::area.low))))))
    ;; Have a missing readonly section, and an image file that might contain it.
    (add-core-sections-from-image image))
  pathname)

;; Kinda stupid to call external program for this...
(defun read-sections (pathname)
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
                     #+readelf (ccl:run-program "readelf" `("--sections" "--wide" ,file) :output output)
                     #-readelf (ccl:run-program "objdump" `("-h" "-w" ,file) :output output)))
           (header-pos (or #+readelf (position #\[ string)
                           #-readelf (search "Idx Name" string)
                           (error "Cannot parse: ~%~a" string)))
           (sections (loop
                       for start = (1+ (position #\newline string :start header-pos)) then (1+ end)
                       for end = (or (position #\newline string :start start) (length string))
                       while (and (< start end) (find (aref string start) " 123456789"))
                       nconc
                       (multiple-value-bind (name address filepos size)
                         #+readelf
                         (destructuring-bind (number name type address filepos size &rest flags)
                             (split string start end)
                           (declare (ignore flags))
                           (assert (and (eql (char number 0) #\[) (eql (char number (1- (length number))) #\])))
                           (setq number (read-from-string number :start 1 :end (1- (length number))))
                           (when (eql number 0)
                             (shiftf size filepos address type))
                           (values name address filepos size))
                         #-readelf
                         (destructuring-bind (number name size address lma filepos &rest flags)
                             (split string start end)
                           (declare (ignore lma flags))
                           (parse-integer number :radix 10) ;; error checking only
                           (values name address filepos size))
                         (unless (or (equal name "") (eql (char name 0) #\.))
                           (setq address (parse-integer address :radix 16))
                           (setq filepos  (parse-integer filepos :radix 16))
                           (setq size (parse-integer size :radix 16))
                           (unless (eql size 0)
                             (list (list address filepos size)))))))
           (sections (sort sections #'< :key #'car));; sort by address
           (sections (let ((last (car (last sections))))  ;; hack for loop below
                       (nconc sections (list (list (+ (car last) (caddr last) 1) 0 0)))))
           (sections (loop
                       with cur-address = -1
                       with cur-filepos = -1
                       with cur-end = cur-address
                       for (address filepos size) in sections
                       unless (or (= (+ cur-filepos (- address cur-address)) filepos)
                                  (= cur-address cur-end))
                         collect (make-core-sect
                                      :start cur-address
                                      :end cur-end
                                      :offset cur-filepos)
                       do (if (= (+ cur-filepos (- address cur-address)) filepos)
                            (setq cur-end (max (+ address size) cur-end))
                            (progn
                              (assert (<= cur-end address));; no overlap.
                              (setq cur-address address cur-filepos filepos cur-end (+ address size)))))))
      (coerce sections 'vector))))


(defun add-core-sections-from-image (pathname)
  (with-open-file (header-stream  pathname :element-type '(signed-byte 32))
    (labels ((read-at (&optional pos)
               (when pos (file-position header-stream pos))
               (read-byte header-stream))
             (readn (pos) (+ (logand #xFFFFFFFF (read-at pos)) (ash (read-at) 32))))
      (let* ((sig '(#x4F70656E #x4D434C49 #x6D616765 #x46696C65))
             (end (file-length header-stream))
             (page-mask (1- *host-page-size*))
             (header (+ end (/ (read-at (1- end)) 4))))
        (unless (progn
                  (file-position header-stream (- end 4))
                  (loop repeat 3 as s in sig always (eql s (read-at))))
          (error "~s is not a ccl image file" pathname))
        (assert (and (integerp header) (< header end) (<= 0 header)))
        (file-position header-stream header)
        (assert (loop for s in sig always (eql s (read-at))))
        (let* ((nsections (read-at (+ header $image-nsections)))
               (offset
                #+64-bit-host (/ (+ (ash (read-at (+ header $image-data-offset-64)) 32)
                                    (logand #xFFFFFFFF (read-at))) 4)
                #-64-bit-host 0)
               (sections (loop repeat nsections
                               for pos upfrom (+ header $image-header-size) by $image-sect-header-size
                               for epos = (* 4 (+ header $image-header-size
                                                         (* nsections $image-sect-header-size)
                                                         offset))
                                 then (+ fpos mem-size)
                               as fpos = (logandc2 (+ epos page-mask) page-mask)
                               as mem-size = (readn (+ pos $image-sect-size))
                               when (eq (readn (+ pos $image-sect-code))
                                        (ash area-readonly target::fixnum-shift))
                                 collect (cons fpos mem-size)))
               (new (area-loop with area-ptr
                               when (and (eq (core-q area-ptr target::area.code)
                                             (ash area-readonly target::fixnum-shift))
                                         (< (core-q area-ptr target::area.low)
                                            (core-q area-ptr target::area.active))
                                         (not (core-section-for-address (core-q area-ptr target::area.low))))
                               collect (let* ((size (- (core-q area-ptr target::area.active)
                                                       (core-q area-ptr target::area.low)))
                                              (matches (remove size sections :key 'cdr :test-not 'eql)))

                                         ;; **** should just do nothing if not found
                                         (assert (eql (length matches) 1))
                                         (make-core-sect
                                          :start (core-q area-ptr target::area.low)
                                          :end (core-q area-ptr target::area.active)
                                          :offset (caar matches)))))
               (image-stream (open pathname :element-type '(unsigned-byte 8) :sharing :lock)))
          (unwind-protect
               (let ((core (current-core)))
                 (setf (core-info-sections core)
                       (sort (concatenate 'vector new (core-info-sections core))
                             #'< :key (lambda (s) (%core-sect.start-addr s))))
                 (push image-stream (core-info-streams core))
                 (loop for s in new do (setf (%core-sect.stream s) image-stream))
                 (setq image-stream nil))
            (when image-stream (close image-stream :abort t))))))))


(declaim (inline core-ivector-readb core-ivector-readw core-ivector-readl core-ivector-readq
                 core-stream-readb core-stream-readw core-stream-readl core-stream-readq))
(declaim (ftype (function (t t) (unsigned-byte 8)) core-ivector-readb core-stream-readb)
         (ftype (function (t t) (unsigned-byte 16)) core-ivector-readw core-stream-readw)
         (ftype (function (t t) (unsigned-byte 32)) core-ivector-readl core-stream-readl)
         (ftype (function (t t) (unsigned-byte 64)) core-ivector-readq core-stream-readq)
         (ftype (function (simple-vector) fixnum) core-section-for-address))

(define-condition invalid-core-address (simple-error)
  ()
  (:default-initargs :format-control "Unknown core address x~x"))

(declaim (inline core-section-for-address))
(defun core-section-for-address (address)
  (loop with sections = (core-info-sections (current-core))
        with len fixnum = (length sections)
        with low fixnum = -1
        with high fixnum = len
        do (let ((half (the fixnum (ash (%i+ high low) -1))))
             (declare (fixnum half))
             (when (eq half low)
               (return (and (%i<= 0 half)
                            (%i< half len)
                            (let ((sect (%svref sections half)))
                              (and (< address (%core-sect.end-addr (%svref sections half))) sect)))))
             (let ((sect (%svref sections half)))
               (if (%i<= (%core-sect.start-addr sect) address)
                 (setq low half)
                 (setq high half))))))

(defun core-heap-address-p (address)
  (core-section-for-address address))


(defun core-stream-readb (s offset)
  (declare (type basic-input-stream s) (optimize (speed 3) (safety 0)))
  (when offset (stream-position s offset))
  (read-byte s))

(defun core-stream-readw (s offset)
  (declare (type basic-input-stream s) (optimize (speed 3) (safety 0)))
  (when offset (stream-position s offset))
  (%i+ (core-stream-readb s nil) (%ilsl 8 (core-stream-readb s nil))))

(defun core-stream-readl (s offset)
  (declare (type basic-input-stream s) (optimize (speed 3) (safety 0)))
  (when offset (stream-position s offset))
  (%i+ (core-stream-readw s nil) (%ilsl 16 (core-stream-readw s nil))))

(defun core-stream-readq (s offset)
  (declare (type basic-input-stream s) (optimize (speed 3) (safety 0)))
  (when offset (stream-position s offset))
  (+ (core-stream-readl s nil) (ash (the fixnum (core-stream-readl s nil)) 32)))

(defun core-ivector-readb (vec offset)
  (declare (type (simple-array (unsigned-byte 8) (*)) vec) (fixnum offset)
           (optimize (speed 3) (safety 0)))
  (aref vec offset))

(defun core-ivector-readw (vec offset)
  (declare (optimize (speed 3) (safety 0)))
  (%i+ (core-ivector-readb vec offset) (%ilsl 8 (core-ivector-readb vec (+ offset 1)))))

(defun core-ivector-readl (vec offset)
  (declare (optimize (speed 3) (safety 0)))
  (%i+ (core-ivector-readw vec offset) (%ilsl 16 (core-ivector-readw vec (+ offset 2)))))

(defun core-ivector-readq (vec offset)
  (declare (optimize (speed 3) (safety 0)))
  (+ (core-ivector-readl vec offset) (ash (core-ivector-readl vec (+ offset 4)) 32)))


(defun core-q (address &optional (offset 0))
  (declare (optimize (speed 3) (safety 0)))
  (incf address offset)
  (let* ((sect (or (core-section-for-address address)
                   (error 'invalid-core-address
                          :format-arguments (list address))))
         (ivector (%core-sect.ivector sect))
         (pos (+ (%core-sect.offset sect) (- address (%core-sect.start-addr sect)))))
    (if ivector
      (core-ivector-readq ivector pos)
      (core-stream-readq (%core-sect.stream sect) pos))))


(defun core-l (address &optional (offset 0))
  (declare (optimize (speed 3) (safety 0)))
  (incf address offset)
  (let* ((sect (or (core-section-for-address address)
                   (error 'invalid-core-address
                          :format-arguments (list address))))
         (ivector (%core-sect.ivector sect))
         (pos (+ (%core-sect.offset sect) (- address (%core-sect.start-addr sect)))))
    (if ivector
      (core-ivector-readl ivector pos)
      (core-stream-readl (%core-sect.stream sect) pos))))

(defun core-w (address &optional (offset 0))
  (declare (optimize (speed 3) (safety 0)))
  (incf address offset)
  (let* ((sect (or (core-section-for-address address)
                   (error 'invalid-core-address
                          :format-arguments (list address))))
         (ivector (%core-sect.ivector sect))
         (pos (+ (%core-sect.offset sect) (- address (%core-sect.start-addr sect)))))
    (if ivector
      (core-ivector-readw ivector pos)
      (core-stream-readw (%core-sect.stream sect) pos))))

(defun core-b (address &optional (offset 0))
  (declare (optimize (speed 3) (safety 0)))
  (incf address offset)
  (let* ((sect (or (core-section-for-address address)
                   (error 'invalid-core-address
                          :format-arguments (list address))))
         (ivector (%core-sect.ivector sect))
         (pos (+ (%core-sect.offset sect) (- address (%core-sect.start-addr sect)))))
    (if ivector
      (core-ivector-readb ivector pos)
      (core-stream-readb (%core-sect.stream sect) pos))))

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
  (the fixnum (ash header (- target::num-subtag-bits))))

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
           (%ilogior (%ilsl target::ntagbits pos) target::fulltag-immheader-0))
          ((setq pos (position symbol *immheader-1-types*))
           (%ilogior (%ilsl target::ntagbits pos) target::fulltag-immheader-1))
          ((setq pos (position symbol *immheader-2-types*))
           (%ilogior (%ilsl target::ntagbits pos) target::fulltag-immheader-2))
          ((setq pos (position symbol *nodeheader-0-types*))
           (%ilogior (%ilsl target::ntagbits pos) target::fulltag-nodeheader-0))
          ((setq pos (position symbol *nodeheader-1-types*))
           (%ilogior (%ilsl target::ntagbits pos) target::fulltag-nodeheader-1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Core heap


(defun core-heap-area-code (area)
  (let ((code (heap-area-code area))
        (dynamic (ash (core-q (core-q (core-q (kernel-global-address 'all-areas))
                                      target::area.succ)
                              target::area.code)
                      (- target::fixnum-shift))))
    (if (or (fixnump area)
            (eq dynamic area-dynamic)
            ;; account for watched area having been inserted
            (<= code area-watched))
      code
      (1- code))))

(defun map-core-areas (function &key area)
  (if (eq area :tenured)
    (map-core-area (core-q (kernel-global-address 'tenured-area)) function)
    (area-loop with area-ptr
               with area = (cond ((or (eq area t) (eq area nil)) nil)
                                 ((consp area) (mapcar #'core-heap-area-code area))
                                 (t (list (core-heap-area-code area))))
               as code = (ash (core-q area-ptr target::area.code) (- target::fixnum-shift))
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
                    (map-core-area area-ptr function)))))

(defun map-core-area (area-ptr fun)
  (map-core-region (core-q area-ptr target::area.low)
		   (core-q area-ptr target::area.active)
		   fun))

(defun map-core-region (ptr end fun)
  (loop
    while (< ptr end) as header = (core-q ptr)
    do (cond ((uvheader-p header)
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
              (incf ptr target::cons.size)))))


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
         (tag (%ilogand typecode target::fulltagmask))
         (len (uvheader-size header)))
    (assert (< -1 index len))
    (cond ((or (eq tag target::fulltag-nodeheader-0)
               (eq tag target::fulltag-nodeheader-1))
           (core-q addr (%ilsl target::word-shift index)))
          ((eq tag target::ivector-class-64-bit)
           (cond ((eq typecode target::subtag-double-float-vector)
                  (error "~s not implemented yet" 'target::subtag-double-float-vector))
                 (t
                  (core-q addr (%ilsl target::word-shift index)))))
          ((eq tag target::ivector-class-32-bit)
           (cond ((eq typecode target::subtag-simple-base-string)
                  (%code-char (core-l addr (%ilsl 2 index))))
                 ((eq typecode target::subtag-single-float-vector)
                  (error "~s not implemented yet" 'target::subtag-single-float-vector))
                 (t (core-l addr (%ilsl 2 index)))))
          ((eq typecode target::subtag-bit-vector)
           (let ((byte (core-b addr (%iasr 3 (%i+ index 7)))))
             (error "not implemented, for ~b" byte)))
          ((>= typecode target::min-8-bit-ivector-subtag)
           (core-b addr index))
          (t (core-w addr (%ilsl 1 index))))))

(defun core-uvsize (vec-ptr)
  (uvheader-size (core-uvheader vec-ptr)))

(defun core-car (obj)
  (assert (core-listp obj))
  (core-q obj target::cons.car))

(defun core-cdr (obj)
  (assert (core-listp obj))
  (core-q obj target::cons.cdr))

(defun core-object-typecode-type (obj)
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
          ((eq fulltag target::fulltag-misc)
           ;; (core-uvtype obj)
           (handler-case (core-uvtype obj) (invalid-core-address () 'unmapped)))
          ((eq fulltag target::fulltag-symbol) 'symbol)
          ;; TODO: Could get hairier based on lfun-bits, but usually don't care.
          ((eq fulltag target::fulltag-function) 'function)
          (t (cerror "treat as ~*~s" "Invalid object tag at #x~x" obj 'bogus)
           'bogus))))

(defun core-object-type-key (obj)
  ;; Returns either a symbol (for built-in types) or a pointer to type symbol or class.
  ;; Whatever it returns must be suitable for use in an eql hash table; use core-type-string
  ;; to get a printable rep.
  (let ((type (core-object-typecode-type obj)))
    (case type
      (function (core-function-type obj))
      (internal-structure (core-istruct-type obj))
      (structure (core-struct-type obj))
      (instance (core-instance-type obj))
      (t type))))

(defun core-function-type (obj)
  (and (core-uvtypep obj :function)
       (let ((bits (core-lfun-bits obj)))
         (declare (fixnum bits))
         (or (if (logbitp $lfbits-trampoline-bit bits)
               (let* ((inner-fn (core-closure-function obj))
                      (inner-bits (core-lfun-bits inner-fn)))
                 (if (neq inner-fn obj)
                   (if (logbitp $lfbits-method-bit inner-bits)
                     'compiled-lexical-closure
                     (unless (logbitp $lfbits-gfn-bit inner-bits)
                       (if (logbitp $lfbits-cm-bit inner-bits)
                         'combined-method
                         'compiled-lexical-closure)))
                   'compiled-lexical-closure))
               (if (logbitp  $lfbits-method-bit bits)
                 'method-function
                 (unless (logbitp $lfbits-gfn-bit bits)
                   (if (logbitp $lfbits-cm-bit bits)
                     'combined-method
                     'function))))
             (core-class-name
              (core-uvref
               (core-nth-immediate obj gf.instance.class-wrapper)
               %wrapper-class))))))

(defun core-type-string (object-type)
  (with-output-to-string (s)
    (if (fixnump object-type)
      (core-print object-type s)
      (prin1 object-type s))))

(defun core-istruct-type (obj)
  (and (core-uvtypep obj :istruct)
       (core-car (core-uvref obj 0))))
       
(defun core-struct-type (obj)
  (and (core-uvtypep obj :struct)
       (core-uvref (core-car (core-uvref obj 0)) 1)))

(defun core-instance-type (obj)
  (and (core-uvtypep obj :instance)
       (core-class-name (core-instance-class obj))))

(defun core-class-name (class)
  (core-uvref (core-uvref class instance.slots) %class.name))

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

(defun core-heap-utilization (&key (stream *debug-io*) area unit (sort :size) classes (threshold 0.00005))
  (let* ((obj-hash (make-hash-table :shared nil))
         (slotv-hash (make-hash-table :shared nil))
         (all nil))
    (map-core-areas (lambda (obj &aux (hash obj-hash))
                      (multiple-value-bind (type logsize physsize) (core-object-type-and-size obj)
                        (when classes
                          (when (core-uvtypep obj :slot-vector)
                            (setq hash slotv-hash
                                  obj (core-uvref obj slot-vector.instance)))
                          (setq type (core-object-type-key obj)))
                        (let ((a (or (gethash type hash)
                                     (setf (gethash type hash) (list 0 0 0)))))
                          (incf (car a))
                          (incf (cadr a) logsize)
                          (incf (caddr a) physsize))))
                    :area area)
    (maphash (lambda (type data)
               (push (cons (core-type-string type) data) all))
             obj-hash)
    (maphash (lambda (type data)
               (push (cons (concatenate 'string (core-type-string type) " slot-vector") data) all))
             slotv-hash)
    (report-heap-utilization all :stream stream :unit unit :sort sort :threshold threshold)))


(defstruct unresolved-address address)

(defmethod print-object ((obj unresolved-address) stream)
  (let* ((address (unresolved-address-address obj)))
    (if (and (core-uvector-p address)
             (not (handler-case (core-uvheader address) (invalid-core-address () nil))))
      (format stream "#<Unmapped #x~x >" address)
      (format stream "#<Core ~A~@[[~d]~] #x~x >"
              (or (ignore-errors (core-type-string (core-object-type-key address)))
                  (core-object-typecode-type address))
              (and (core-uvector-p address) (core-uvsize address))
            address))))

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
          ((and (%i<= target::fulltag-misc fulltag)
                (handler-case (core-uvheader obj) (invalid-core-address nil)))
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
    (declare (type fixnum typecode tag len))
    (cond ((or (eq tag target::fulltag-nodeheader-0)
               (eq tag target::fulltag-nodeheader-1))
           (when (eq typecode target::subtag-function)
             ;; Don't bother copying the code for now
             (let ((skip (core-l addr)))
	       (declare (fixnum skip))
               (assert (<= 0 skip len))
               (incf addr (ash skip target::word-shift))
               (decf len skip)))
           (dotimes (i len)
	     (declare (fixnum i))
             (setf (%svref vec i)
                   (copy-from-core (core-q addr (%ilsl target::word-shift i)) :depth depth)))
           (let ((ptrtag (logand vec-ptr target::fulltagmask)))
             (cond ((eq ptrtag target::fulltag-symbol)
                    (%symvector->symptr vec))
                   ((eq ptrtag target::fulltag-function)
                    (%function-vector-to-function vec))
                   (t vec))))
          ((eq tag target::ivector-class-64-bit)
           (cond ((eq typecode target::subtag-double-float-vector)
                  (warn "~s not implemented yet" 'target::subtag-double-float-vector)
                  (make-unresolved-address :address vec-ptr))
                 (t
                  (dotimes (i len vec)
                    (setf (uvref vec i) (core-q addr (%ilsl target::word-shift i)))))))
          ((eq tag target::ivector-class-32-bit)
           (cond ((eq typecode target::subtag-simple-base-string)
                  (dotimes (i len vec)
                    (setf (uvref vec i) (%code-char (core-l addr (%ilsl 2 i))))))
                 ((eq typecode target::subtag-single-float-vector)
                  (warn "~s not implemented yet" 'target::subtag-single-float-vector)
                  (make-unresolved-address :address vec-ptr))
                 (t
                  (dotimes (i len vec)
                    (setf (uvref vec i) (core-l addr (%ilsl 2 i)))))))
          ((eq typecode target::subtag-bit-vector)
           (warn "bit vector not implemented yet")
           (make-unresolved-address :address vec-ptr))
          ((>= typecode target::min-8-bit-ivector-subtag)
           (dotimes (i len vec)
             (setf (uvref vec i) (core-b addr i))))
          (t
           (dotimes (i len vec)
             (setf (uvref vec i) (core-w addr (%ilsl 1 i))))))))

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
                                 (declare (fixnum typecode len))
                                 (when (eq typecode target::subtag-function)
                                   (let ((skip (core-l addr)))
                                     (declare (fixnum skip))
                                     (assert (<= 0 skip len))
                                     (incf addr (%ilsl target::word-shift skip))
                                     (decf len skip)))
                                 (dotimes (i len)
                                   (funcall fn (core-q addr (%ilsl target::word-shift i)) obj i))))))))
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

(defun core-symbol-plist (obj)
  (when (setq obj (core-symptr obj))
    (core-cdr (core-q obj target::symbol.plist))))

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

(defun core-nth-immediate (fn i)
  (assert (core-uvtypep fn :function))
  (let ((addr (+ (logandc2 fn target::fulltagmask) target::node-size)))
    (core-q addr (%ilsl target::word-shift (+ (core-l addr) i -1)))))

(defun core-closure-function (fun)
  (while (and (core-functionp fun)
              (logbitp $lfbits-trampoline-bit (core-lfun-bits fun)))
    (setq fun (core-nth-immediate fun 1))
    (when (core-uvtypep fun :simple-vector)
      (setq fun (core-uvref fun 0)))
    #+gz (assert (core-functionp fun)))
  fun)

(defun core-lfun-name (fn)
  (assert (core-functionp fn))
  (flet ((lfun-name (fn)
           (or (core-gethash fn (core-lfun-names-table-ptr))
               (let* ((lfbits (core-lfun-bits fn))
                      (name (if (and (logbitp $lfbits-gfn-bit lfbits)
                                     (not (logbitp $lfbits-method-bit lfbits)))
                                (core-uvref (core-nth-immediate fn gf.slots) sgf.name)
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
  ;; TODO: could dispatch on core-object-typecode-type...
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
        (t (format stream "#<core ~a x~x>"
		   (or (ignore-errors (core-type-string (core-object-type-key obj)))
		       (core-object-typecode-type obj))
		   obj))))

(defun core-print-symbol (sym stream)
  (let ((package (core-symbol-package sym)))
    (cond ((core-nullp package)
           (format stream "#:"))
          ((eq package (core-keyword-package))
           (format stream ":"))
          (t (let ((pkgname (core-package-name package)))
               (etypecase pkgname
                 (unresolved-address (format stream "@~x::" (unresolved-address-address pkgname)))
                 (string (unless (string-equal pkgname "COMMON-LISP")
                           (format stream "~a::" pkgname)))))))
    (let ((symname (core-symbol-name sym)))
      (etypecase symname
        (unresolved-address (format stream "@~x" (unresolved-address-address symname)))
        (string (format stream "~a" symname)))))
  (values))

(defun core-lfun-bits (fun)
  (let ((unsigned (core-uvref fun (1- (core-uvsize fun)))))
    (ash (if (logbitp (1- (* target::node-size 8)) unsigned)
           (logior (ash -1 (* target::node-size 8)) unsigned)
           unsigned)
         (- target::fixnum-shift))))


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
                          (let ((slots (core-uvref spec instance.slots)))
                            ;; specializer is either a class or a ccl::eql-specializer
                            (if (eql (core-uvsize slots) 3)
                              (progn
                                (format stream "(EQL ")
                                (core-print (core-uvref slots 2) stream)
                                (format stream ")"))
                              (core-print (core-uvref slots %class.name) stream)))
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
          (core-symbol-name (core-instance-type proc))
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

