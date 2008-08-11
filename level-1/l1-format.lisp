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

;; L1-format.lisp
;
; This file contains the definition for SUB-FORMAT, the dispatching part
; of FORMAT. It also contains an interim definition for FORMAT and a few
; incompletely implemented directives.

(in-package "CCL")

(eval-when (eval compile #-bccl load)  ;Load-time as well so CCL can use it.
  (defmacro defformat (char name &rest def)
    `(progn
       (add-format-char ,char (nfunction ,name (lambda . ,def)))
       ',name))
  )

(defparameter *format-char-table* (let* ((x (make-array 128 :initial-element nil))) x))

(defun add-format-char (char def)
  (unless (and (characterp char) (%i< (%char-code char) 128))
    (report-bad-arg char 'standard-char))
  (setf (svref *format-char-table* (%char-code (char-upcase char))) def))

(proclaim '(special *format-original-arguments*   ;For ~*
                    *format-arguments*            ;For pop-format-arg
                    *format-control-string*       ;For ~?, ~{
                    *format-index*
                    *format-length*
                    *format-pprint*               ;~I,~W,~_,~:T seen?
                    *format-justification-semi*   ;~<..~:;..~> seen?
            ))

(defun pop-format-arg (&aux (args *format-arguments*))
  (if (null args)
      (format-error "Missing argument"))
    (progn
     (setq *format-arguments* (cdr args))
     (%car args)))
 
;SUB-FORMAT parses (a range of) the control string, finding the directives
;and applying them to their parameters.
;Implicit arguments to SUB-FORMAT: *format-control-string*, *format-arguments*,
;*format-original-arguments*, *standard-output*, *format-char-table*
;*format-control-string* must be a simple string.
;Directive functions' arglist should be (colon-p atsign-p &rest params)
;In addition when the directive is called, *format-index* and *format-length*
;are bound to start and end pos (in *format-control-string*) of the rest of the
; control string.  The directive may modify *format-index*, but not
; *format-control-string* and *format-length*, before returning.

(defun sub-format (stream *format-index* *format-length* &aux char)
  (prog* ((string (require-type *format-control-string* 'simple-string))
          (length *format-length*)
          (i *format-index*)
          (lastpos i))
    (declare (fixnum i length lastpos) (type simple-string string))
    (go START)
    EOF-ERROR
    (setq *format-index* *format-length*)
    (format-error "Premature end of control string")
    START
    (do* ()
         ((= i length) (unless (= i lastpos) 
                         (write-string string stream :start  lastpos :end i)))
      (setq char (schar string i) i (1+ i))
      (when (eq char #\~)
        (let* ((limit (the fixnum (1- i))))
          (unless (= limit lastpos) 
            (write-simple-string string stream  lastpos limit)))
        (let ((params nil) (fn) (colon nil) (atsign nil))
          (block nil
            (tagbody
              NEXT
              (if (= i length) (go EOF-ERROR))
              (setq char (schar string i) i (1+ i))
              (cond ((eq char #\#)
                     (push (list-length *format-arguments*) params))
                    ((eq char #\')
                     (if (= i length) (go EOF-ERROR))
                     (push (schar string i) params)
                     (incf i))
                    ((eq char #\,)
                     (push nil params)
                     (go NEXT))
                    ((or (eq char #\V) (eq char #\v))
                     (push (pop-format-arg) params))
                    ((or (eq char #\-) (eq char #\+) (digit-char-p char))
                     (let ((start (%i- i 1)) n)
                       (loop
                         (when (= i length) (go EOF-ERROR))
                         (unless (digit-char-p (schar string i)) (return))
                         (incf i))
                       (when (null (setq n (%parse-number-token string start i)))
                         (setq *format-index* i)
                         (format-error "Illegal parameter"))
                       (push n params)))
                    (t (return)))
              (if (= i length) (go EOF-ERROR))
              (setq char (schar string i) i (1+ i))
              (when (neq char #\,) (return))
              (go NEXT)))
          (cond ((eq char #\:) 
                 (if (= i length) (go EOF-ERROR))
                 (setq colon t char (schar string i) i (1+ i))
                 (when (eq char #\@)
                   (if (= i length) (go EOF-ERROR))                     
                   (setq atsign t char (schar string i) i (1+ i))))
                ((eq char #\@)
                 (if (= i length) (go EOF-ERROR))
                 (setq atsign t char (schar string i) i (1+ i))
                 (when (eq char #\:)
                   (if (= i length) (go EOF-ERROR))
                   (setq colon t char (schar string i) i (1+ i)))))
          (setq *format-index* (%i- i 1))
          (if (setq fn (svref *format-char-table* (%char-code (char-upcase char))))
            (apply fn stream colon atsign (nreverse params))
            (format-error "Unknown directive"))
          (setq i (%i+ *format-index* 1)
                lastpos i))))))


#||
(eval-when (load)
  ;The non-consing version.
(defun sub-format (stream *format-index* *format-length*)
  (declare (special *format-index* *format-length*))
  (old-lap-inline (stream)
    (preserve_regs #(asave0 asave1 dsave0 dsave1 dsave2))
    (defreg Control-string asave0 Index dsave0 Length dsave1 NumParams dsave2 Stream asave1)
    (move.l acc Stream)
    (move.l (special *format-index*) Index)       ; *format-index*
    (move.l (special *format-length*) Length)      ; *format-length*
    (specref *format-control-string*)
    (move.l acc Control-string)

    ;Make sure everything is in bounds, so don't have to worry about
    ;boxing, bounds checking, etc.
start
    (movereg Control-string arg_z)
    (jsr_subprim $sp-length)
    (ccall <= '0 Index Length acc)
    (cmp.l nilreg acc)
    (beq done)
    (move.l Index db)
    (loop#
      (if# (eq Length Index)
        (cmp.l db Index)
        (beq done)
        (ccall 'stream-write-string Stream Control-string db Index)
        (bra done))
      (move.l Index da)
      (getint da)
      (move.l ($ $t_imm_char 0) acc)
      (move.b (Control-string da.l $v_data) acc)
      (add.l (fixnum 1) Index)
      (cmp.b ($ #\~) acc)
      (beq tilde))

nextchar
    (if# (eq Length Index)
      (move.l '"Premature end of format control string" arg_z)
      (add.w ($ 4) sp)                  ; flush internal bsr.
      (bra error))
    (move.l Index da)
    (getint da)
    (move.b (Control-string da.l $v_data) acc)
    (add.l (fixnum 1) Index)
    (if# (and (ge (cmp.b ($ #\a) acc)) (le (cmp.b ($ #\z) acc)))
      (sub.b ($ 32) acc))
    (rts)

tilde
    (move.l Index da)
    (sub.l (fixnum 1) da)
    (if# (not (eq da db))      
      (ccall 'stream-write-string Stream Control-string db da))
    (vpush Stream)
    (vpush nilreg)             ;assume no :
    (vpush nilreg)             ;assume no @
    (move.l (fixnum 3) NumParams)
do-param
    (bsr nextchar)
    (if# (or (eq (cmp.b ($ #\+) acc))
             (eq (cmp.b ($ #\-) acc))
             (and (ge (cmp.b ($ #\0) acc)) (le (cmp.b ($ #\9) acc))))
      (move.l Index da)
      (sub.l (fixnum 1) da)
      (vpush da)
      (prog#
       (bsr nextchar)
       (until# (or (lt (cmp.b ($ #\0) acc)) (gt (cmp.b ($ #\9) acc)))))
      (sub.l (fixnum 1) Index)   ;unread the non-digit char
      (ccall %parse-number-token Control-string vsp@+ Index)
      (cmp.l nilreg acc)
      (bne push-param)
      (move.l '"Illegal format parameter" arg_z)
      (bra error))

    (if# (eq (cmp.b ($ #\#) acc))
      (move.l (special *format-arguments*) acc)
      (jsr_subprim $sp-length)
      (bra push-param))

    (if# (eq (cmp.b ($ #\') acc))
      (bsr nextchar)
      (move.l ($ $t_imm_char 0) acc)
      (move.b (Control-string da.l $v_data) acc)  ;Get the non-uppercased version...
      (swap acc)
      (bra push-param))

    (if# (eq (cmp.b ($ #\,) acc))
      (sub.l (fixnum 1) Index)   ;Re-read the comma.
      (move.l nilreg acc)
      (bra push-param))

    (if# (eq (cmp.b ($ #\V) acc))
      (ccall 'pop-format-arg)
      ;(bra push-param)
     push-param
      (vpush acc)
      (add.l (fixnum 1) NumParams)
      (bsr nextchar)
      (cmp.b ($ #\,) acc)
      (beq do-param))

    (move.l NumParams nargs)
    (vscale.l nargs)
    (cmp.b ($ #\:) acc)
    (if# eq
      (bsr nextchar)
      (cmp.b ($ #\@) acc)
      (bne @a)
      (move.l (a5 $t) (vsp nargs.w -12))
     else#
      (cmp.b ($ #\@) acc)
      (bne @b)
      (move.l (a5 $t) (vsp nargs.w -12))
      (bsr nextchar)
      (cmp.b ($ #\:) acc)
      (bne @b))
    (bsr nextchar)
@a  (move.l (a5 $t) (vsp nargs.w -8))
@b  (moveq 127 da)
    (and.w acc da)
    (bif (ne (cmp.b da acc)) nofun)
    (lsl.w 2 da)
    (move.l (special *format-char-table*) atemp0)
    (move.l (atemp0 da.w $v_data) atemp0)
    (cmp.l atemp0 nilreg)
    (beq nofun)
    (move.l Index da)
    (sub.l (fixnum 1) da)
    (move.l da (special *format-index*))
    (move.l NumParams nargs)
    (vscale.l nargs)                    ; at least 3 args.
    (movem.l vsp@+ #(arg_z arg_y arg_x))
    (jsr_subprim $sp-funcall)
    (specref '*format-index*)
    (add.l (fixnum 1) acc)
    (move.l acc Index)
    (bra start)

nofun
    (move.l '"Unknown format directive" acc)
error
    (move.l Index (special *format-index*))
    (fsymevalapply 'format-error 1)

done
    (restore_regs)
    ))
) ;end of eval-when (load)

||#

;;;Interim definitions

;;;This function is shadowed by CCL in order to use ~{ to print error messages.
(defun format (stream control-string &rest format-arguments)
  (declare (dynamic-extent format-arguments))
  (when (null stream)
   (return-from format 
    (with-output-to-string (x)
     (apply #'format x control-string format-arguments))))
  (if (eq stream t)
    (setq stream *standard-output*)
    (unless (streamp stream) (report-bad-arg stream 'stream)))
  (if (functionp control-string)
    (apply control-string stream format-arguments)
    (progn
      (setq control-string (ensure-simple-string control-string))
      (let* ((*format-original-arguments* format-arguments)
             (*format-arguments* format-arguments)
             (*format-control-string* control-string))
        (catch 'format-escape
         (sub-format stream 0 (length control-string)))
        nil))))

(defun format-error (&rest args)
   (format t "~&FORMAT error at position ~A in control string ~S "
             *format-index* *format-control-string*)
   (apply #'error args))

(defun format-no-flags (colon atsign)
  (when (or colon atsign) (format-error "Flags not allowed")))

;Redefined later
(defformat #\A format-a (stream colon atsign)
   (declare (ignore colon atsign))
   (princ (pop-format-arg) stream))

;Redefined later
(defformat #\S format-s (stream colon atsign)
  (declare (ignore colon atsign))
  (prin1 (pop-format-arg) stream))

;Redefined later
(defformat #\^ format-escape (stream colon atsign)
  (declare (ignore stream colon atsign))
  (throw 'format-escape t))

;Final version
(defformat #\% format-% (stream colon atsign &optional repeat-count)
  (cond ((or (not repeat-count)
            (and repeat-count (fixnump repeat-count)
                 (> repeat-count -1)))
         (format-no-flags colon atsign)
         (dotimes (i (or repeat-count 1)) (declare (fixnum i)) (terpri stream)))
        (t (format-error "Bad repeat-count."))))

;Final version
(defformat #\& format-& (stream colon atsign &optional repeat-count)
  (format-no-flags colon atsign)
  (unless (eq repeat-count 0)
    (fresh-line stream)
    (dotimes (i (1- (or repeat-count 1))) (declare (fixnum i)) (terpri stream))))

;Final version
(defformat #\~ format-~ (stream colon atsign &optional repeat-count)
  (format-no-flags colon atsign)
  (dotimes (i (or repeat-count 1)) (declare (fixnum i)) (write-char #\~ stream)))

;Final version
(defformat #\P format-p (stream colon atsign)
  (when colon
     (let ((end *format-arguments*) (list *format-original-arguments*))
        (tagbody loop
           (if list
             (when (neq (cdr list) end)
               (setq list (%cdr list))
               (go loop))
             (format-error "No previous argument")))
        (setq *format-arguments* list)))
   (%write-string (if (eq (pop-format-arg) 1)
                    (if atsign "y" "")
                    (if atsign "ies" "s"))
                  stream))

;Final version
(defformat #\* format-* (stream colon atsign &optional count)
  (declare (ignore stream)(special *circularity-hash-table*))
  (let* ((orig *format-original-arguments*)
         (where (- (list-length orig)   ; will error if args circular
                   (list-length *format-arguments*)))
         (to (if atsign 
               (or count 0) ; absolute
               (progn
                 (when (null count)(setq count 1))
                 (when colon (setq count (- count)))
                 (%i+ where count))))
         (args (nthcdr-no-overflow to orig)))
    ; avoid bogus circularity indication
    (when (and nil (consp args) (<= to where) *circularity-hash-table*)
      ; copy only from to thru where in case  some real shared structure
      (let ((l args) new)
        (dotimes (i (1+  (- where to)))
          (declare (fixnum i))
          (push (car l) new)
          (setq l (cdr l)))
        (setq args (nreconc new (nthcdr (1+ where) orig))))) ;(copy-list args)))
    (setq *format-arguments* args)))

; Redefined later.
(defformat #\Newline format-newline (&rest ignore)
  (declare (ignore ignore))
  (do* ((i *format-index* (1+ i))
        (s *format-control-string*)
        (n *format-length*))
       ((or (= i n)
            (not (whitespacep (schar s i))))
        (setq *format-index* (1- i)))))
        
(defun nthcdr-no-overflow (count list)
  "If cdr beyond end of list return :error"  
  (if (or (> count (list-length list)) (< count 0))
    nil ;:error
    (nthcdr count list)))

;Redefined later
(defformat #\X format-x (stream colon atsign)
  (declare (ignore colon atsign))
  (let* ((*print-base* 16.)
         (*print-radix* nil))
    (prin1 (pop-format-arg) stream)))

;Redefined later
(defformat #\D format-d (stream colon atsign &rest ignore)
  (declare (ignore colon atsign ignore))
  (let* ((*print-base* 10.)
         (*print-radix* nil))
    (prin1 (pop-format-arg) stream)))
