;;;-*- Mode: Lisp; Package: (ARM :use CL) -*-
;;;
;;;   Copyright (C) 2005-2009 Clozure Associates and contributors.
;;;   This file is part of Clozure CL.
;;;
;;;   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License   known as the LLGPL and distributed with Clozure CL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL
;;;   which is distributed with Clozure CL as the file "LGPL".  Where these
;;;   conflict  the preamble takes precedence.
;;;
;;;   Clozure CL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

(eval-when (:compile-toplevel :load-toplevel :execute)
(require "ARM-ASM")
)

(defparameter *hide-spjump-internals* t)

(defstruct (arm-disassembled-instruction (:conc-name adi-))
  (opcode 0 :type (unsigned-byte 32))
  (labeled nil)
  (mnemonic nil)
  (condition-name nil)
  (operands nil))

(defun arm-gpr-name (regno)
  `(:gpr ,regno))

(defun arm-fprd-name (regno)
  `(:double ,regno))

(defun arm-fprs-name (regno)
  `(:single ,regno))



(defun find-arm-instruction-template (opcode)
  (dotimes (i (length arm::*arm-instruction-table*))
    (let* ((template (svref arm::*arm-instruction-table* i))
           (value (arm::arm-instruction-template-val template))
           (masks  (arm::arm-instruction-template-mask-list template)))
      (if
        (if (atom masks)
          (= (logand opcode masks) value)
          (dolist (mask masks)
            (if (atom mask)
              (if (= (logand opcode mask) value)
                (return t))
              (if (= (logand opcode (cdr mask)) (car mask))
                (return t)))))
        (return template)))))


(defun extract-arm-rd-operand (opcodes i)
  (let* ((opcode (adi-opcode (svref opcodes i))))
    (arm-gpr-name (ldb (byte 4 12) opcode))))

(defun extract-arm-rn-operand (opcodes i)
  (let* ((opcode (adi-opcode (svref opcodes i))))
    (arm-gpr-name (ldb (byte 4 16) opcode))))

(defun extract-arm-rs-operand (opcodes i)
  (let* ((opcode (adi-opcode (svref opcodes i))))
    (arm-gpr-name (ldb (byte 4 8) opcode))))

(defun extract-arm-fpaddr-operand (opcodes i)
  (let* ((opcode (adi-opcode (svref opcodes i)))
         (offset (ash (ldb (byte 8 0) opcode) 2))
         (rn (ldb (byte 4 16) opcode)))
    (unless (logbitp 23 opcode)
      (setq offset (- offset)))
    (cond ((eql rn arm::pc)
           (let* ((target (+ i 2 (ash offset -2))))
             (when (and (>= target 0)
                        (< target (uvsize opcodes)))
               (setf (adi-labeled (uvref opcodes target)) t))
             `(:= (:label ,target))))
          (t `(:@ ,(arm-gpr-name rn) (:$ ,offset))))))

(defun extract-arm-@rn-operand (opcodes i)
  (let* ((opcode (adi-opcode (svref opcodes i))))
    `(:@ ,(arm-gpr-name (ldb (byte 4 16) opcode)))))
         
  

(defparameter *arm-shift-ops* #(:lsl :lsr :asr :ror))


(defun extract-arm-shifter-operand (opcodes i)
  (let* ((opcode (adi-opcode (svref opcodes i))))
    (if (logbitp 25 opcode)
      (let* ((count (ash (ldb (byte 4 8) opcode) 1))
             (value (arm::arm-rotate-left (ldb (byte 8 0) opcode) (logand 31 (- 32 count)))))
        `(:$ ,value))
      (let* ((rn (arm-gpr-name (ldb (byte 4 0) opcode)))
             (register-shifted (logbitp 4 opcode)))
        (if register-shifted
          `(,(svref *arm-shift-ops* (ldb (byte 2 5) opcode))
            ,rn
            ,(arm-gpr-name (ldb (byte 4 8) opcode)))
          (let* ((shift-type (ldb (byte 2 5) opcode))
                 (shift-count (ldb (byte 5 7) opcode)))
            (if (and (eql shift-type 0)
                     (eql shift-count 0))
              rn
              (if (and (eql shift-type 3)
                       (eql shift-count 0))
                `(:rrx ,rn)
                `(,(svref *arm-shift-ops* shift-type)
                  ,rn
                  (:$ ,shift-count))))))))))

              

(defun extract-arm-m12-operand (opcodes i)
  (let* ((opcode (adi-opcode (svref opcodes i))))
    (let* ((immediate (not (logbitp 25 opcode)))
           (disp (ldb (byte 12 0) opcode))
           (p (logbitp 24 opcode))
           (u (logbitp 23 opcode))
           (w (logbitp 21 opcode))
           (rnval (ldb (byte 4 16) opcode))
           (rn (arm-gpr-name rnval)))
      (cond (immediate
              (unless u (setq disp (- disp)))
              (if (and u
                       p
                       (not w)
                       (eql arm::fn rnval)
                       (eql (mod (- disp arm::misc-data-offset) 4) 0))
                `(:@ ,rn (:constant ,(ash (- disp arm::misc-data-offset) -2)))
                (if (and p (not w) (eql arm::pc rnval) (not (logtest 3 disp)))
                  (let* ((target (+ i 2 (ash disp -2))))
                    (when (< target (uvsize opcodes))
                      (setf (adi-labeled (uvref opcodes target)) t))
                    `(:= (:label ,target)))
                  (if p
                    (if w
                      `(:@! ,rn (:$ ,disp))
                      `(:@ ,rn (:$ ,disp)))
                    `(:@+ ,rn (:$ ,disp))))))
            (t
             (let* ((shift-op (ldb (byte 2 5) opcode))
                    (shift-count (ldb (byte 5 7) opcode))
                    (rm (arm-gpr-name (ldb (byte 4 0) opcode)))
                    (memop
                     (if p
                       (if w
                         (if u :+@! :-@!)
                         (if u :+@ :-@))
                       (if u :@+ :@-))))
               (if (and (zerop shift-count) (zerop shift-op))
                 `(,memop ,rn ,rm)
                 (if (and (eql 3 shift-op) (zerop shift-count))
                   `(,memop ,rn (:rrx ,rm))
                   `(,memop ,rn (,(svref *arm-shift-ops* shift-op)
                                 ,rm
                                 (:$ ,shift-count)))))))))))


(defun extract-arm-reglist-operand (opcodes i)
  (let* ((opcode (adi-opcode (svref opcodes i))))
    (let* ((mask (ldb (byte 16 0) opcode))
           (regs ()))
      (declare (type (unsigned-byte 16) i))
      (do* ((i 15 (1- i)))
           ((< i 0) `(:reglist ,regs))
        (declare ((signed-byte 4) i))
        (when (logbitp i mask)
          (push i regs))))))

(defun extract-arm-rnw-operand (opcodes i)
  (let* ((opcode (adi-opcode (svref opcodes i))))
    (let* ((regname (arm-gpr-name (ldb (byte 4 16) opcode))))
      (if (logbitp 21 opcode)
        `(:! ,regname)
        regname))))

(defun extract-arm-uuoa-operand (opcodes i)
  (let* ((opcode (adi-opcode (svref opcodes i))))
    (arm-gpr-name (ldb (byte 4 8) opcode))))

(defun extract-arm-uuo-unary-operand (opcodes i)
  (let* ((opcode (adi-opcode (svref opcodes i))))
    `(:$ ,(ldb (byte 8 12) opcode))))

(defun extract-arm-uuob-operand (opcodes i)
  (let* ((opcode (adi-opcode (svref opcodes i))))
    (arm-gpr-name (ldb (byte 4 12) opcode))))

(defun extract-arm-uuoc-operand (opcodes i)
  (let* ((opcode (adi-opcode (svref opcodes i))))
    (arm-gpr-name (ldb (byte 4 16) opcode))))

(defun extract-arm-fpux-operand (opcodes i)
  (let* ((opcode (adi-opcode (svref opcodes i))))
    (case (ldb (byte 4 16) opcode)
      (0 :fpsid)
      (1 :fpscr)
      (8 :fpexc)
      (t (list :fpu (ldb (byte 4 16) opcode))))))

(defun extract-arm-imm16-operand (opcodes i)
  (let* ((opcode (adi-opcode (svref opcodes i))))
    `(:$
      ,(dpb (ldb (byte 4 16) opcode)
         (byte 4 12)
         (ldb (byte 12 0) opcode)))))

(defun extract-arm-rm-operand (opcodes i)
  (let* ((opcode (adi-opcode (svref opcodes i))))
    (arm-gpr-name (ldb (byte 4 0) opcode))))

(defun extract-arm-b-operand (opcodes i)
  (let* ((adi (svref opcodes i))
         (opcode (adi-opcode adi))
         (b-field (ldb (byte 24 0) opcode)))
    (when (logbitp 23 b-field)
      (setq b-field (- b-field (ash 1 24))))
    (let* ((target (+ i 2 b-field)))
      (when (and (>= target 0)
                 (< target (length opcodes)))
        (let* ((target-op (svref opcodes target))
               (target-op-label (adi-labeled target-op)))
          (cond  ((and target-op-label
                       (not (eq t target-op-label)))
                  (when *hide-spjump-internals*
                    (setf (adi-mnemonic adi)
                          (if (logbitp 24 opcode)
                            "bla"
                            "ba")))
                  `(:spname ,target-op-label))
                 (t
                  (setf (adi-labeled (svref opcodes target)) t)
                  `(:label ,target))))))))



(defun extract-arm-m8-operand (opcodes i)
  (let* ((opcode (adi-opcode (svref opcodes i))))
    (let* ((immediate (logbitp 22 opcode))
           (disp (dpb (ldb (byte 4 8) opcode)
                      (byte 4 4)
                      (ldb (byte 4 0) opcode)))
           (p (logbitp 24 opcode))
           (u (logbitp 23 opcode))
           (w (logbitp 21 opcode))
           (rnval (ldb (byte 4 16) opcode))
           (rn (arm-gpr-name rnval)))
      (cond (immediate
             (unless u (setq disp (- disp)))
             (if p
               (if w
                 `(:@! ,rn (:$ ,disp))
                 `(:@ ,rn (:$ ,disp)))
               `(:@+ ,rn (:$ ,disp))))
            (t
             (let* ((rm (arm-gpr-name (ldb (byte 4 0) opcode))))
               `(,(if p
                      (if w
                        (if u :+@! :-@!)
                        (if u :+@ :-@))
                      (if u :@+ :@-)) ,rn ,rm)))))))

(defun extract-arm-dd-operand (opcodes i)
  (let* ((opcode (adi-opcode (svref opcodes i))))
    (arm-fprd-name (ldb (byte 4 12) opcode))))

(defun extract-arm-dm-operand (opcodes i)
  (let* ((opcode (adi-opcode (svref opcodes i))))
    (arm-fprd-name (ldb (byte 4 0) opcode))))

(defun extract-arm-sd-operand (opcodes i)
  (let* ((opcode (adi-opcode (svref opcodes i))))
    (arm-fprs-name (logior (ash (ldb (byte 4 12) opcode) 1)
                           (ldb (byte 1 22) opcode)))))

(defun extract-arm-sm-operand (opcodes i)
  (let* ((opcode (adi-opcode (svref opcodes i))))
    (arm-fprs-name (logior (ash (ldb (byte 4 0) opcode) 1)
                           (ldb (byte 1 5) opcode)))))

(defun extract-arm-dn-operand (opcodes i)
  (let* ((opcode (adi-opcode (svref opcodes i))))
    (arm-fprd-name (ldb (byte 4 16) opcode))))

(defun extract-arm-sn-operand (opcodes i)
  (let* ((opcode (adi-opcode (svref opcodes i))))
    (arm-fprs-name (logior (ash (ldb (byte 4 16) opcode) 1)
                           (ldb (byte 1 7) opcode)))))

(defun extract-arm-srcount-operand (opcodes i)
  (let* ((opcode (adi-opcode (svref opcodes i))))
    (ldb (byte 8 0) opcode)))

(defun extract-arm-drcount-operand (opcodes i)
  (let* ((opcode (adi-opcode (svref opcodes i))))
    (ldb (byte 7 1) opcode)))

(defun extract-arm-spentry-operand (opcodes i)
  (let* ((opcode (adi-opcode (svref opcodes i)))
         (val (ldb (byte 12 0) opcode)))
    `(:spname ,(or (arm::arm-subprimitive-name val)
                   (format nil "??? subprim ~d" val)))))
  

(defparameter *arm-operand-extract-functions*
  #(extract-arm-rd-operand
    extract-arm-rn-operand
    extract-arm-shifter-operand
    extract-arm-m12-operand
    extract-arm-reglist-operand
    extract-arm-rnw-operand
    extract-arm-uuoa-operand
    extract-arm-uuo-unary-operand
    extract-arm-uuob-operand
    extract-arm-rm-operand
    extract-arm-b-operand
    obsolete
    extract-arm-m8-operand
    extract-arm-dd-operand
    extract-arm-dm-operand
    extract-arm-sd-operand
    extract-arm-sm-operand
    extract-arm-dn-operand
    extract-arm-sn-operand
    extract-arm-rd-operand                  ;rde
    extract-arm-rs-operand
    extract-arm-fpaddr-operand
    extract-arm-@rn-operand
    extract-arm-uuoc-operand
    extract-arm-fpux-operand
    extract-arm-imm16-operand
    extract-arm-srcount-operand
    extract-arm-drcount-operand
    extract-arm-spentry-operand
    ))

(defun make-adi-vector (code-vector)
  (let* ((n (uvsize code-vector))
         (v (make-array n)))
    (declare (fixnum n) (simple-vector v))
    (dotimes (i n v)
      (setf (svref v i)
            (make-arm-disassembled-instruction :opcode (uvref code-vector i))))))

(defun process-adi-vector (adi-vector)
  (let* ((n (length adi-vector))
         (data nil)
         (data-count 0))
    (declare (fixnum n))
    (do* ((i 0 (1+ i)))
         ((= i n) adi-vector)
      (declare (fixnum i))
      (let* ((adi (svref adi-vector i))
             (opcode (adi-opcode adi)))
        (cond (data (setq data-count opcode data nil))
              ((> data-count 0)
               (setf (adi-mnemonic adi) ":word"
                     (adi-operands adi) (list (adi-opcode adi)))
               (decf data-count))
              ((= opcode 0)
               (setq data t)
               (incf i))
              (t
               (let* ((template (find-arm-instruction-template opcode)))
                 (if (null template)
                   (setf (adi-mnemonic adi) :???
                         (adi-operands adi) (list opcode))
                   (collect ((operands))
                     (setf (adi-mnemonic adi)
                           (arm::arm-instruction-template-name template))
                     (unless (logtest (arm::encode-arm-instruction-flag :non-conditional) (arm::arm-instruction-template-flags template))
                       (let* ((cond (ldb (byte 4 28) opcode))
                              (cond-name (if (< cond 14) (arm::lookup-arm-condition-value cond))))
                         (when cond-name
                           (if (logtest (arm::encode-arm-instruction-flag :prefer-separate-cond) (arm::arm-instruction-template-flags template))
                             (operands `(:? ,cond-name))
                             (setf (adi-condition-name adi) cond-name)))))
                     
                     (dolist (type (arm::arm-instruction-template-operand-types template))
                       (operands (funcall (svref *arm-operand-extract-functions* type) adi-vector i)))
                     (setf (adi-operands adi) (operands)))))))))))

(defparameter *arm-gpr-names*
  #("imm0" "imm1" "nargs" "rcontext" "arg_z" "arg_y" "arg_x" "temp0"
    "temp1" "temp2" "vsp" "fn" "allocptr" "sp" "lr" "pc"))



(defun disassemble-arm-xfunction (xfunction &optional (stream *debug-io*) (*hide-spjump-internals* *hide-spjump-internals*))
  (let* ((adi-vector (process-adi-vector (make-adi-vector (uvref xfunction 1))))
         (functionp (typep xfunction 'function)) ;not cross-compiling
         (previous-source-note nil)
         (pc-counter 0))
    (labels ((format-spname (name stream)
               (let* ((string (string name))
                      (n (length string))
                      (copy (make-string n)))
                 (declare (dynamic-extent copy))
                 (dotimes (i n (format stream "~a" copy))
                   (let* ((ch (char string i)))
                     (setf (schar copy i)
                           (if (< i 3)
                             ch
                             (char-downcase ch))))))))      
      (when functionp
        (let ((source-note (function-source-note xfunction)))
          (when source-note
            ;; Fetch text from file if don't already have it
            (ensure-source-note-text source-note)
            (if (source-note-filename source-note)
              (format t ";; Source: ~S:~D-~D"
                      (source-note-filename source-note)
                      (source-note-start-pos source-note)
                      (source-note-end-pos source-note))
              (let* ((source-text (source-note-text source-note)))
                (when source-text
                  (format t ";;; ~A" (string-sans-most-whitespace source-text 100))))))))
      (dotimes (i (length adi-vector))
        (when functionp
          (let ((source-note (find-source-note-at-pc xfunction (* i 4))))
            (unless (eql (source-note-file-range source-note)
                         (source-note-file-range previous-source-note))
              (setf previous-source-note source-note)
              (let* ((source-text (source-note-text source-note))
                     (text (if source-text
                             (string-sans-most-whitespace source-text 100)
                             "#<no source text>")))
                (format stream "~&~%;;; ~A" text)
                (setq pc-counter 3)))))
        (let* ((info (svref adi-vector i))
               (labeled (adi-labeled info)))
          (when labeled
            (setq pc-counter 0)
            (if (eq t labeled)
              (format stream "~&L~d~&" (ash i 2))
              (if *hide-spjump-internals*
                (return)
                (format-spname labeled stream))))
          (let* ((name (adi-mnemonic info))
                 (use-fixnum-syntax nil))            
            (when name
              (let* ((condition-name (or (adi-condition-name info) "")))
                (format stream "~&  (~a~a" name condition-name))
              (let* ((ngpr 0)
                     (nnode 0))
                (declare (fixnum ngpr nnode))
                (dolist (op (adi-operands info))
                  (when (and (consp op) (eq (car op) :gpr))
                    (incf ngpr)
                    (when (logbitp (cadr op) arm-node-regs)
                      (incf nnode))))
                (unless (zerop ngpr)
                  (setq use-fixnum-syntax (eql nnode ngpr))))
              (labels ((format-operand (operand &optional toplevel)
                         (write-char #\space stream)
                         (if (atom operand)
                           (if (and (typep operand 'integer)
                                    (> (abs operand) 100))
                             (format stream "#x~x" operand)
                             (format stream "~d" operand))
                           (ecase (car operand)
                             (:= (format stream "(:=")
                                 (format-operand (cadr operand))
                                 (write-char #\) stream))
                             (:label
                              (let* ((target (if (< (cadr operand) (length adi-vector))
                                               (svref adi-vector (cadr operand))))
                                     (target-labeled (and target (adi-labeled target)))
                                     (target-label (and (not (eq target-labeled t))
                                                        target-labeled)))
                                (if target-label
                                  (format stream "~a" target-label)
                                  (format stream "L~d" (ash (cadr operand) 2)))))
                             (:constant (format stream "~s" (list 'quote (uvref xfunction (cadr operand)))))
                             ((:lsl :lsr :asr :ror :rrx)
                              (format stream "(:~a" (string-downcase (car operand)))
                              (dolist (sub (cdr operand))
                                (format-operand sub))
                              (write-char #\) stream))
                             (:spname
                              (format-spname (cadr operand) stream))
                             (:$
                              (let* ((val (cadr operand)))
                                (cond ((eql val arm::nil-value)
                                       (format stream "'nil"))
                                      ((and toplevel
                                           use-fixnum-syntax
                                           (typep val 'integer)
                                           (not (logtest arm::fixnummask val)))
                                       (let* ((unboxed (ash val (- arm::fixnumshift))))
                                         (if (> (abs unboxed) 100)
                                           (format stream "'#x~x" unboxed)
                                           (format stream "'~d" unboxed))))
                                      (t
                                       (progn
                                         (format stream "(:$")
                                         (format-operand val)
                                         (write-char #\) stream))))))
                             (:? (format stream "(:? ~a)" (cadr operand)))
                             (:gpr (format stream "~a" (svref *arm-gpr-names* (cadr operand))))
                             (:single
                              (if (eql (cadr operand)
                                       (hard-regspec-value arm::single-float-zero))
                                (format stream "single-float-zero")
                                (format stream "s~d" (cadr operand))))
                              (:double
                              (if (eql (cadr operand)
                                       (hard-regspec-value arm::double-float-zero))
                                (format stream "double-float-zero")
                                (format stream "d~d" (cadr operand))))
                             (:reglist (format stream "~a"
                                               (mapcar (lambda (r)
                                                         (svref *arm-gpr-names* r))
                                                       (cadr operand))))
                             ((:@ :@! :+@ :+@! :-@ :-@! :@+ :@-)
                              (format stream "(~s" (car operand))
                              (dolist (sub (cdr operand))
                                (format-operand sub))
                              (write-char #\) stream))
                             (:!
                              (format stream "(:!")
                              (format-operand (cadr operand))
                              (write-char #\) stream))))))
                (dolist (op (adi-operands info))
                  (format-operand op t))
                (write-char #\) stream)
                (when (eql (incf pc-counter) 4)
                  (setq pc-counter 0)
                  (format stream "~40t;[~d]" (* i 4)))))))))))

                             
                       
             
(defun arm-xdisassemble (function)
  (disassemble-arm-xfunction function *standard-output*))

;;; Help arithmetic-error handlers
(defun arithmetic-error-operation-from-instruction (template)
  (let* ((name (make-keyword (string-upcase (arm::arm-instruction-template-name template)))))
    (case name
      ((:fdivs :fdivd) '/)
      ((:fmuls :fmuld) '*)
      ((:fadds :faddd) '+)
      ((:fsubs :fsubd) '-)
      (t 'coerce))))

(defun arithmetic-error-operands-from-instruction (template instruction regvals xp)
  (let* ((adi (make-arm-disassembled-instruction :opcode instruction))
         (adi-vector (vector adi))
         (parsed-ops (mapcar (lambda (type)
                               (funcall (svref *arm-operand-extract-functions* type) adi-vector 0))
                             (arm::arm-instruction-template-operand-types template)))
         (singles (make-array 32 :element-type 'single-float))
         (doubles (make-array 16 :element-type 'double-float)))
    (declare (dynamic-extent singles doubles))
    (%copy-ivector-to-ivector regvals 4 singles 0 (* 32 4))
    (%copy-ivector-to-ivector regvals 4 doubles 4 (* 16 8))
    (collect ((opvals))
      (dolist (op (cdr parsed-ops))
        (ecase (car op)
          (:double (opvals (aref doubles (cadr op))))
          (:single (opvals (aref singles (cadr op))))
          (:gpr (opvals (xp-gpr-signed-long xp (cadr op))))))
      (when (null (cddr parsed-ops))
        (opvals (case (caar parsed-ops)
                  (:single 'single-float)
                  (:double 'double-float))))
      (opvals))))
    
    
#+arm-target
(defun disassemble-lines (function)
  (declare (ignore function))
  (error "DISASSEMBLE-LINES isn't implemented yet for ARM."))
