; -*- Mode:Lisp; Package:CCL; -*-
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

(declaim (special *|`,|* *|`,.|* *|`,@|*))

;;;Backquote reads in as a call to the BACKQUOTE-EXPANDER macro.
;;;This makes it a little obscure to look at raw, but makes it possible for
;;;the pretty-printer to print things read in with backquote.

(defvar *backquote-expand* t "If non-NIL, expand at read-time")

(defmacro backquote-expander (*|`,|* *|`,.|* *|`,@|* form)
   (declare (special *|`,|* *|`,.|* *|`,@|*))
   (multiple-value-bind (form constantp) (backquote-aux form)
     (backq-form form constantp)))

(defun backquote-aux (form)
  ;;Doesn't try to optimize multiple CONS's into LIST/LIST*'s, leaving it up
  ;;to the compiler.  The code here is mainly concerned with folding
  ;;constants, since the compiler is not allowed to do that in general.
  (cond
   ((simple-vector-p form)
    (let ((elts ()) (i (length form)))
      (until (%izerop i) (push (svref form (setq i (%i- i 1))) elts))
      (multiple-value-bind (elts quotedp) (backquote-aux elts)
        (if quotedp
          (values (list-to-vector elts) t)
          (list 'list-to-vector elts)))))
   ((self-evaluating-p form) (values form t))
   ((atom form) (values form t))
   ((eq (%car form) 'backquote-expander) (backquote-aux (macroexpand-1 form)))
   ((eq (%car form) *|`,|*) (%cdr form))
   ((eq (%car form) *|`,@|*) (error "Misplaced ,@~S after backquote" (%cdr form)))
   ((eq (%car form) *|`,.|*) (error "Misplaced ,.~S after backquote" (%cdr form)))
   (t (let* ((car (%car form))
             (splice (and (consp car) (if (eq (%car car) *|`,@|*) 'append
                                        (if (eq (%car car) *|`,.|*) 'nconc)))))
        (multiple-value-bind (cdr qd) (backquote-aux (%cdr form))
          (if splice
            (cond ((null (%cdr car)) (values cdr qd))
                  ((null cdr) (values (%cdr car) (self-evaluating-p (%cdr car))))
                  (t (list splice (%cdr car) (backq-form cdr qd))))
            (multiple-value-bind (car qa) (backquote-aux car)
              (cond ((and qa qd) (values (cons car cdr) t))
                    ((null cdr) (list 'list car))
                    (t (list 'list*     ; was CONS
                             (backq-form car qa) (backq-form cdr qd)))))))))))

(defun backq-form (form constantp)
  (if (and constantp (not (self-evaluating-p form))) (list 'quote form) form))

(defparameter *backquote-stack* ())

(set-macro-character
 #\`
 (nfunction
  |` reader|
  (lambda (stream char &aux form)
    (declare (ignore char))
    (setq form
          (let* ((|`,| (make-symbol "`,"))
                 (|`,.| (make-symbol "`,."))
                 (|`,@| (make-symbol "`,@")))
            (list 'backquote-expander |`,| |`,.| |`,@|
                  (let ((*backquote-stack* (list* |`,| |`,.| |`,@| *backquote-stack*)))
                    (read stream t nil t)))))
    (if *backquote-expand* (values (macroexpand-1 form)) form))))

(set-macro-character
 #\,
 (nfunction
  |, reader|
  (lambda (stream char &aux (stack *backquote-stack*))
    (when (null stack)
      (signal-reader-error stream "Comma not inside backquote"))
    (let ((*backquote-stack* (cdddr stack)))
      (setq char (tyi stream))
      (cond ((eq char #\@)
             (cons (%caddr stack) (read stream t nil t)))
            ((eq char #\.)
             (cons (%cadr stack) (read stream t nil t)))
            (t
             (untyi char stream)
             (cons (%car stack) (read stream t nil t))))))))

(provide 'backquote)
