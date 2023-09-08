;;; -*- Mode: Lisp; Package: CCL -*-
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

;;; Functions to implement FORMAT.
;;;

(in-package "CCL")

;;; Special variables local to FORMAT
;;; why do these have top-level bindings ????? - seems wrong or at least unnecessary

(defvar *format-control-string* ""
  "The current FORMAT control string")

(defvar *format-index* 0
  "The current index into *format-control-string*")

(defvar *format-length* 0
  "The length of the current FORMAT control string")

(defvar *format-arguments* ()
  "Arguments to the current call of FORMAT")

(defvar *format-original-arguments* ()
  "Saved arglist from top-level FORMAT call for ~* and ~@*")

(defvar *format-arguments-variance* nil
  "Non-NIL only during compile-time scanning of a format string, in which case it is the
number of additional elements at the front of *format-arguments* that may be already used
up at runtime.  I.e. the actual *format-arguments* may be anything between *format-arguments*
and (nthcdr *format-arguments-variance* *format-arguments*)")

(def-standard-initial-binding *format-stream-stack* nil "A stack of string streams for collecting FORMAT output")

(defvar *format-pprint* nil
  "Has a pprint format directive (~W ~I ~_ ~:T) or logical-block directive been seen?")

(defvar *format-justification-semi* nil
  "Has a ~<...~:;...~> been seen?")

(defvar *format-colon-rest* nil
  )

;;; prevent circle checking rest args. Really EVIL when dynamic-extent
(def-standard-initial-binding *format-top-level* nil)


;;; ERRORS

;;; Since errors may occur while an indirect control string is being
;;; processed, i.e. by ~? or ~{~:}, some sort of backtrace is necessary
;;; in order to indicate the location in the control string where the
;;; error was detected.  To this end, errors detected by format are
;;; signalled by throwing a list of the form ((control-string args))
;;; to the tag FORMAT-ERROR.  This throw will be caught at each level
;;; of indirection, and the list of error messages re-thrown with an
;;; additional message indicating that indirection was present CONSed
;;; onto it.  Ultimately, the last throw will be caught by the top level
;;; FORMAT function, which will then signal an error to the Slisp error
;;; system in such a way that all the errror messages will be displayed
;;; in reverse order.

(defun format-error (complaint &rest args)
  (throw 'format-error
         (list (list "~1{~:}~%~S~%~V@T^" complaint args
                    *format-control-string* (1+ *format-index*)))))


;;; MACROS

;;; This macro establishes the correct environment for processing
;;; an indirect control string.  CONTROL-STRING is the string to
;;; process, and FORMS are the forms to do the processing.  They 
;;; invariably will involve a call to SUB-FORMAT.  CONTROL-STRING
;;; is guaranteed to be evaluated exactly once.
(eval-when (compile eval #-bccl load)

; does this need to exist?????
#|| ; put it out of its misery
(defmacro format-with-control-string (control-string &rest forms)
  `(let ((string (if (simple-string-p ,control-string)
                     ,control-string
                     (coerce ,control-string 'simple-base-string))))
        (declare (simple-string string))
        (let ((error (catch 'format-error
                            (let ((*format-control-string* string)
                                  (*format-length* (length string))
                                  (*format-index* 0))
                                 ,@forms
                                 nil))))
          
             (when error
                   (throw 'format-error
                          (cons (list "While processing indirect control string~%~S~%~V@T^"
                                      *format-control-string*
                                      (1+ *format-index*))
                                error))))))
||#
(defmacro format-indirect-error (error)
  `(throw 'format-error
         (cons (list "While processing indirect control string~%~S~%~V@T^"
                     *format-control-string*
                     (1+ *format-index*))
               ,error)))


(defmacro get-a-format-string-stream ()
  '(or (pop *format-stream-stack*) (make-string-output-stream :element-type 'base-char))) ; ??

;;; This macro rebinds collects output to the standard output stream
;;; in a string.  For efficiency, we avoid consing a new stream on
;;; every call.  A stack of string streams is maintained in order to
;;; guarantee re-entrancy.

(defmacro with-format-string-output (stream-sym &rest forms)
  `(let ((,stream-sym nil))
     (unwind-protect
       (progn
         (setq ,stream-sym (get-a-format-string-stream))
         ,@forms
         (prog1
           (get-output-stream-string ,stream-sym)
           (push ,stream-sym *format-stream-stack*)))
       (when ,stream-sym (file-position ,stream-sym 0)))))

;;; This macro decomposes the argument list returned by PARSE-FORMAT-OPERATION.
;;; PARMVAR is the list of parameters.  PARMDEFS is a list of lists of the form
;;; (<var> <default>).  The FORMS are evaluated in an environment where each 
;;; <var> is bound to either the value of the parameter supplied in the 
;;; parameter list, or to its <default> value if the parameter was omitted or
;;; explicitly defaulted.

(defmacro with-format-parameters (parmvar parmdefs &body  body &environment env)
  (do ((parmdefs parmdefs (cdr parmdefs))
       (bindings () (cons `(,(caar parmdefs) (or (if ,parmvar (pop ,parmvar))
                                                 ,(cadar parmdefs)))
                          bindings)))
      ((null parmdefs)
       (multiple-value-bind (forms decls) (parse-body body env)
         `(let ,(nreverse bindings)
            ,@decls
            (when ,parmvar
              (format-error "Too many parameters"))
            ,@forms)))))



;;; Returns the index of the first occurrence of the specified character
;;; between indices START (inclusive) and END (exclusive) in the control
;;; string.


(defmacro format-find-char (char start end)
  `(%str-member  ,char *format-control-string*
                   ,start ,end))


) ;end of eval-when for macros

;;; CONTROL STRING PARSING 

;;; The current control string is kept in *format-control-string*. 
;;; The variable *format-index* is the position of the last character
;;; processed, indexing from zero.  The variable *format-length* is the
;;; length of the control string, which is one greater than the maximum
;;; value of *format-index*.  


;;; Gets the next character from the current control string.  It is an
;;; error if there is none.  Leave *format-index* pointing to the
;;; character returned.

(defun format-nextchar ()
  (let ((index (%i+ 1 *format-index*)))    
    (if (%i< (setq *format-index* index) *format-length*)
      (schar *format-control-string* index)
      (format-error "Syntax error"))))



;;; Returns the current character, i.e. the one pointed to by *format-index*.

(defmacro format-peek ()
  `(schar *format-control-string* *format-index*))




;;; Attempts to parse a parameter, starting at the current index.
;;; Returns the value of the parameter, or NIL if none is found. 
;;; On exit, *format-index* points to the first character which is
;;; not a part of the recognized parameter.

(defun format-get-parameter (ch)
  (case ch
    (#\# (format-nextchar)
     (let ((n (or *format-arguments-variance* 0))
           (len (length *format-arguments*)))
       (declare (fixnum n len))
       (if (eql n 0)
         len
         `(the (integer ,(- len n) ,len) (length *format-arguments*)))))
    ((#\V #\v)
     (prog1 (pop-format-arg) (format-nextchar)))
    (#\' (prog1 (format-nextchar) (format-nextchar)))
    (t (cond ((or (eq ch #\-) (eq ch #\+) (digit-char-p ch))
              (let ((neg-parm (eq ch #\-)))
                (unless (setq ch (digit-char-p ch))
                  (unless (setq ch (digit-char-p (format-nextchar)))
                    (format-error "Illegal parameter")))
                (do ((number ch (+ ch (* number 10))))
                    ((not (setq ch (digit-char-p (format-nextchar))))
                     (if neg-parm (- number) number)))))
             (t nil)))))

(defun format-skip-parameter (ch) ; only caller is parse-format-operation
  "Might someday want to add proper format error checking for negative 
      parameters"
  (let ()
    (case ch
      ((#\V #\v #\#)
       (format-nextchar))
      (#\' (format-nextchar) (format-nextchar))
      (#\,)
      (t (when (or (eq ch #\-) (eq ch #\+)) (format-nextchar))
         (while (digit-char-p (format-nextchar)))))))

(defun format-no-semi (char &optional colon atsign)
  (when *format-justification-semi*
    (format-error "~~~:[~;:~]~:[~;@~]~c illegal in this context" colon atsign char))
  (setq *format-pprint* t))

;;; Parses a format directive, including flags and parameters.  On entry,
;;; *format-index* should point to the "~" preceding the command.  On
;;; exit, *format-index* points to the command character itself.
;;; Returns the list of parameters, the ":" flag, the "@" flag, and the
;;; command character as multiple values.  Explicitly defaulted parameters
;;; appear in the list of parameters as NIL.  Omitted parameters are simply 
;;; not included in the list at all.

(defun parse-format-operation (&optional get-params) ; only caller is format-find-command
  (let ((ch (format-nextchar)) parms colon atsign)
    (when (or (digit-char-p ch)
              ;(%str-member ch ",#Vv'"))
              (memq ch '(#\- #\, #\# #\V #\v #\')))      
      (cond (get-params
             (setq parms (list (format-get-parameter ch)))
             (until (neq (setq ch (format-peek)) #\,)
               (setq ch (format-nextchar))
               (push (format-get-parameter ch) parms)))
            (t (setq parms t)  ; tell caller there were some so we get correct error msgs
               (format-skip-parameter ch)
               (until (neq (setq ch (format-peek)) #\,)
                 (setq ch (format-nextchar))
                 (format-skip-parameter ch)))))
    ; allow either order
    (case ch
      (#\: (setq colon t ch (format-nextchar))
           (when (eq ch #\@)
             (setq atsign t ch (format-nextchar))))
      (#\@ (setq atsign t ch (format-nextchar))
           (when (eq ch #\:)
             (setq colon t ch (format-nextchar)))))
    (values (if (consp parms) (nreverse parms) parms)
            colon
            atsign
            ch)))


;;; Starting at the current value of *format-index*, finds the first
;;; occurrence of one of the specified directives. Embedded constructs,
;;; i.e. those inside ~(~), ~[~], ~{~}, or ~<~>, are ignored.  And error is
;;; signalled if no satisfactory command is found.  Otherwise, the
;;; following are returned as multiple values:
;;;
;;;     The value of *format-index* at the start of the search
;;;     The index of the "~" character preceding the command
;;;     The parameter list of the command
;;;     The ":" flag
;;;     The "@" flag
;;;     The command character
;;;
;;; Implementation note:  The present implementation is not particulary
;;; careful with storage allocation.  It would be a good idea to have
;;; a separate function for skipping embedded constructs which did not
;;; bother to cons parameter lists and then throw them away. This issue has been addressed. (akh)
;;;
;;; We go to some trouble here to use POSITION for most of the searching.
;;; God only knows why!!!!

;; and interesting note - the only caller who wants parameters is format-get-segments for
;; ~< .... ~n:; ...~>
(defun format-find-command (command-list &optional get-params evil-commands)
  (let* ((start *format-index*)
         (length *format-length*)
         tilde)
    (loop
      (setq tilde (format-find-char #\~ *format-index* length))
      (if (not tilde) (format-error "Expecting one of ~S" command-list))
      (setq *format-index* tilde)
      (multiple-value-bind (parms colon atsign command)
                           (parse-format-operation get-params)
        (when (memq command command-list)
          (return (values start tilde parms colon atsign command)))
        (when (and evil-commands
                   (or (memq command  '(#\w #\_ #\i #\W #\I))
                       (and colon (memq command '(#\t #\T)))))
          (format-error "Illegal in this context"))
        (case command
          (#\{ (format-nextchar) (format-find-command '(#\})))
          (#\( (format-nextchar) (format-find-command '(#\))))
          (#\[ (format-nextchar) (format-find-command '(#\])))
          (#\< (format-nextchar) 
               (multiple-value-bind (prev tilde parms colon atsign cmd)
                   (format-find-command '(#\>))
                 (declare (ignore prev tilde parms atsign cmd))
                 (if (and evil-commands colon)
                     (format-error "Logical-block directive not allowed inside justification directive"))))
	  (#\~ (format-nextchar))
          ((#\} #\> #\) #\])
           (format-error "No matching bracket")))))))

(defun format-find-command-no-params (command-list &key (colon t) (atsign t))
  (multiple-value-bind (prev tilde parms colon-flag atsign-flag command)
                       (format-find-command command-list)
    (with-format-parameters parms ()
      (format-no-flags (and (not colon) colon-flag) (and (not atsign) atsign-flag)))
    (values prev tilde command colon-flag atsign-flag)))

;;; This is the FORMAT top-level function.

(defun format (stream control-string &rest format-arguments)
  (declare (dynamic-extent format-arguments))
  (if (null stream)
    (with-output-to-string (s)
      (apply #'format s control-string format-arguments))
    (if (stringp stream)
      (with-output-to-string (s stream)
	(apply #'format s control-string format-arguments))
      (let ((*format-top-level* t))
	(when (xp-structure-p stream)
	  (setq stream (xp-stream-stream stream))) ; for xp tests only! They call format on a structure
	(setq stream (if (eq stream t)
		       *standard-output*
		       (require-type stream 'stream)))     
	(if (functionp control-string)
	  (apply control-string stream format-arguments)
	  (let* ((control-string (ensure-simple-string control-string))
                 (*format-control-string* control-string)
                 (*format-pprint* nil)
                 (*format-justification-semi* nil))
            (declare (type simple-string control-string))
	    (cond
	      ;; Try to avoid pprint overhead in this case.
	      ((not (position #\~ control-string))
	       (write-string control-string stream))
	      ((and (or *print-pretty* *print-circle*)
		    (not (typep stream 'xp-stream)))
	       (maybe-initiate-xp-printing
		#'(lambda (s o)
		    (do-sub-format-1 s o))
		stream format-arguments))
	      (t 
	       (let ((*format-original-arguments* format-arguments)
		     (*format-arguments* format-arguments)
		     (*format-colon-rest* 'error)) ; what should this be??
		 (do-sub-format stream))))))
	nil))))

(defun format-to-string (string control-string &rest format-arguments)
  (declare (dynamic-extent format-arguments))
  (if string
    (with-output-to-string (stream string)
      (apply #'format stream control-string format-arguments))
    (with-output-to-string (stream)
      (apply #'format stream control-string format-arguments))))

(defun do-sub-format (stream)
  (let (errorp)
    (setq errorp
          (catch 'format-error
            (catch 'format-escape 
              (sub-format stream 0 (length *format-control-string*)))
            nil))    
    (when errorp
      (error "~%~:{~@?~%~}" (nreverse errorp)))))



;;; This function does the real work of format.  The segment of the control
;;; string between indiced START (inclusive) and END (exclusive) is processed
;;; as follows: Text not part of a directive is output without further
;;; processing.  Directives are parsed along with their parameters and flags,
;;; and the appropriate handlers invoked with the arguments COLON, ATSIGN, and
;;; PARMS. 
;;;

;;; POP-FORMAT-ARG also defined in l1-format

; in l1-format
(def-standard-initial-binding *logical-block-xp* nil)

(without-duplicate-definition-warnings
 (defun pop-format-arg (&aux (args *format-arguments*) (xp *logical-block-xp*) (av *format-arguments-variance*))
   (when (and (null args) (null xp))
     (format-error "Missing argument"))
   (when xp
     (if (null av)
       (when (pprint-pop-check+ args xp)    ; gets us level and length stuff in logical block
         (throw 'logical-block nil))
       ;; Could record that might exit here, but nobody cares.
       #+no (note-format-scan-option *logical-block-options*)))
   (if (or (null av) (eql av 0))
     (progn
       (setq *format-arguments* (cdr args))
       (%car args))
     (let ((types (loop for x in args as i from 0 below av
                    collect (nx-form-type x))))
       (when (eql av (length args))
         (setq *format-arguments-variance* (1- av)))
       (setq *format-arguments* (cdr args))
       `(the (or ,@types) (car *format-arguments*))))))

; SUB-FORMAT is now defined in L1-format.lisp
; DEFFORMAT is also defined there.

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pretty-printing stuff
;;; 

(defformat #\W format-write (stream colon atsign)
  (format-no-semi #\W)
  (let ((arg (pop-format-arg)))
    (cond (atsign
       (let ((*print-level* nil)
             (*print-length* nil))
         (if colon
           (let ((*print-pretty* t))
             (write-1 arg stream))
           (write-1 arg stream))))
      (t (if colon
           (let ((*print-pretty* t))
             (write-1 arg stream))
           (write-1 arg stream))))))

(defformat #\I format-indent (stream colon atsign &rest parms)
  (declare (dynamic-extent parms))
  (declare (ignore atsign))
  (format-no-semi #\I)
  (with-format-parameters parms ((n 0))
    (pprint-indent (if colon :current :block) n stream)))

(defformat #\_ format-conditional-newline (stream colon atsign)
  (format-no-semi #\_)
  (let ((option
         (cond (atsign
                (cond (colon  :mandatory)
                      (t :miser)))
               (colon :fill)
               (t :linear))))
    (pprint-newline option stream)))

;;; Tabulation  ~T 

(defformat #\T format-tab (stream colon atsign &rest parms)
  (declare (dynamic-extent parms))
  (when colon
    (format-no-semi #\T t))
  (with-format-parameters parms ((colnum 1) (colinc 1))
    (cond ((or (typep stream 'xp-stream) (xp-structure-p stream))
           (let ((kind (if colon
                           (if atsign :section-relative :section)
                           (if atsign :line-relative :line))))
             (cond ((xp-structure-p stream)
                    (pprint-tab+ kind colnum colinc stream))
                   ((typep stream 'xp-stream)
                    (pprint-tab+ kind colnum colinc
                                 (slot-value stream 'xp-structure))))))
          ((not colon)
           (pprint-tab-not-pretty stream colnum colinc atsign)))))

(defun pprint-tab-not-pretty (stream colnum colinc &optional atsign)
  (let* ((position (column stream))
         (count (if atsign
                  (if position
                    (if (zerop colinc)
                      colnum (+ colnum (mod (- (+ position colnum)) colinc)))
                    colnum)
                  (if position
                    (if (<= colnum position)
                      (if (zerop colinc)
                        0 (- colinc (mod (- position colnum) colinc)))
                      (- colnum position))
                    2))))
    (while (> count 0)
      (write-string "                                                                                "
                           stream :start 
                           0 :end (min count 80))
      (setq count (- count 80)))))


;;; ~/ call function
(defformat #\/ format-call-function (stream colon atsign &rest parms)
  (let* ((string *format-control-string*)
         (ipos (1+ *format-index*))
         (epos (format-find-char #\/ ipos *format-length*)))    
    ; the spec is DUMB here - it requires that : and :: be treated the same
    (when (not epos) (format-error "Unmatched ~~/"))
    (let ((cpos (format-find-char #\: ipos epos))
          package)
      (cond (cpos 
             (setq package (or (find-package (string-upcase (%substr string ipos cpos)))
                               (format-error "Unknown package")))
             (when (eql #\: (schar string (%i+ 1 cpos)))
               (setq cpos (%i+ cpos 1)))
             (setq ipos (%i+ cpos 1)))
            (t (setq package (find-package "CL-USER"))))
      (let ((thing (intern (string-upcase (%substr string ipos epos)) package)))
        (setq *format-index* epos) ; or 1+ epos?
	(apply thing stream (pop-format-arg) colon atsign parms)))))

;;; Conditional case conversion  ~( ... ~)

#| coral's old version
(defformat #\( format-capitalization (stream colon atsign)
  (format-nextchar)
  (multiple-value-bind (prev tilde) (format-find-command-no-params '(#\)))
   (let* (finished
          (string (with-format-string-output stream
                    (setq finished (catch 'format-escape (sub-format stream prev tilde) t)))))
     (write-string
         (cond ((and atsign colon)
                (nstring-upcase string))
               (colon
                (nstring-capitalize string))
               (atsign
                (let ((strlen (length string)))
                     ;; Capitalize the first word only
                     (nstring-downcase string)
                     (do ((i 0 (1+ i)))
                         ((or (<= strlen i) (alpha-char-p (char string i)))
                          (setf (char string i) (char-upcase (char string i)))
                          string))))
               (t (nstring-downcase string)))
         stream :start 
         0 :end (length string))
     (unless finished (throw 'format-escape nil)))))

|#

(defformat #\( format-capitalization (stream colon atsign)
  (format-nextchar)
  (multiple-value-bind (prev tilde) (format-find-command-no-params '(#\)))
    (let (catchp)
      (cond ((typep stream 'xp-stream)
             (let ((xp (slot-value stream 'xp-structure)))
               (push-char-mode xp (cond ((and colon atsign) :UP)
				         (colon :CAP1)
				         (atsign :CAP0)
				         (T :DOWN)))
               (setq catchp
                     (catch 'format-escape
                       (sub-format stream prev tilde)
                       nil))
	       (pop-char-mode xp)))
            (t
             (let* ((string (with-format-string-output stream                      
                              (setq catchp (catch 'format-escape
                                             (sub-format stream prev tilde)
                                             nil)))))
               (write-string
                (cond ((and atsign colon)
                       (nstring-upcase string))
                      (colon
                       (nstring-capitalize string))
                      (atsign
                       ;; Capitalize the first word only
                       (nstring-downcase string)
                       (dotimes (i (length string) string)
                         (let ((ch (char string i)))
                           (when (alpha-char-p ch)
                             (setf (char string i) (char-upcase ch))
                             (return string)))))
                      (t (nstring-downcase string)))         
                stream :start 
                0 :end (length string)))))
      (when catchp
        (throw 'format-escape catchp))
      )))

;;; Up and Out (Escape)  ~^

(defformat #\^ format-escape (stream colon atsign &optional p1 p2 p3)
  (declare (ignore stream))
  (when atsign
    (format-error "FORMAT command ~~~:[~;:~]@^ is undefined" colon))
  (when (cond (p3 (etypecase p2
                    (real
                     (<= p1 p2 p3))
                    (character
                     (char< p1 p2 p3))))
              (p2 (equal p1 p2))
              (p1 (eql p1 0))
              (t (null (if colon *format-colon-rest* *format-arguments*))))
    (throw 'format-escape (if colon 'format-colon-escape t))))

;;; Conditional expression  ~[ ... ]


;;; ~[  - Maybe these guys should deal with ~^ too - i.e. catch format-escape etc.
;;; but I cant think of a case where just throwing to the { catcher fails

(defun format-untagged-condition (stream)
  (let ((test (pop-format-arg)))
    (unless (integerp test)
      (format-error "Argument to ~~[ must be integer - ~S" test))
    (do ((count 0 (1+ count)))
        ((= count test)
         (multiple-value-bind (prev tilde cmd colon atsign)
                              (format-find-command-no-params '(#\; #\]) :atsign nil)
           (declare (ignore colon atsign))
           (sub-format stream prev tilde)
           (unless (eq cmd #\])
             (format-find-command '(#\])))))
      (multiple-value-bind (prev tilde cmd colon atsign)
                           (format-find-command-no-params '(#\; #\]) :atsign nil)
        (declare (ignore prev tilde atsign))
        (when (eq cmd #\]) (return))
        (format-nextchar)
        (when colon
          (multiple-value-bind (prev tilde cmd colon atsign)
                               (format-find-command-no-params '(#\; #\]))
            (declare (ignore colon atsign))
            (sub-format stream prev tilde)
            (unless (eq cmd #\])
              (format-find-command-no-params '(#\]))))
          (return))))))


;;; ~@[

(defun format-funny-condition (stream)
  (multiple-value-bind (prev tilde) (format-find-command-no-params '(#\]))
    (if *format-arguments*
      (if (car *format-arguments*)
        (sub-format stream prev tilde)
        (pop *format-arguments*))
      (format-error "Missing argument"))))


;;; ~:[ 

(defun format-boolean-condition (stream)
  (multiple-value-bind (prev tilde command) (format-find-command-no-params '(#\; #\]))
    (when (eq command #\])
      (format-error "Two clauses separated by ~~; are required for ~~:["))
    (format-nextchar)
    (if (pop-format-arg)
      (multiple-value-bind (prev tilde)
          (format-find-command-no-params '(#\]) :colon nil :atsign nil)
        (sub-format stream prev tilde))
      (progn
        (sub-format stream prev tilde)
        (format-find-command-no-params '(#\]))))))


(defformat #\[ format-condition (stream colon atsign &optional p)
  (when p (push p *format-arguments*))
  (format-nextchar)
  (cond (colon
         (when atsign
           (format-error  "~~:@[ undefined"))
         (format-boolean-condition stream))
        (atsign
         (format-funny-condition stream))
        (t (format-untagged-condition stream))))


;;; Iteration  ~{ ... ~}

(defformat #\{ format-iteration (stream colon atsign &rest parms)
  (declare (dynamic-extent parms))
  (with-format-parameters parms ((max-iter -1))
    (format-nextchar)
    (multiple-value-bind (prev tilde end-cmd end-colon end-atsign)
                         (format-find-command-no-params '(#\}) :atsign nil)
      (declare (ignore end-cmd end-atsign))
      (if (= prev tilde)
        ;; Use an argument as the control string if ~{~} is empty
        (let ((string (pop-format-arg)))
          (cond ((stringp string)
                 (when (not (simple-string-p string)) ; fix here too
                   (setq string (coerce string 'simple-string))))
                ((not (functionp string))
                 (format-error "Control string is not a string or function")))          
          (let ((error 
                 (catch 'format-error
                   (cond
                    ((stringp string)
                     (let* ((length (length (the simple-string string)))
                            (*format-control-string* string)
                            (*format-length* length)
                            (*format-index* 0))
                       (format-do-iteration stream 0 length
                                            max-iter colon atsign end-colon)))
                    (t ;(functionp string)
                     (format-do-iteration stream string nil 
                                          max-iter colon atsign end-colon)))
                   nil)))
            (when error (format-indirect-error error))))
        (format-do-iteration stream prev tilde 
                             max-iter colon atsign end-colon)))))


;;; The two catch tags FORMAT-ESCAPE and FORMAT-COLON-ESCAPE are needed here
;;; to correctly implement ~^ and ~:^.  The former aborts only the current
;;; iteration, but the latter aborts the entire iteration process.
;;; ~{ arg is a list  ~:{ arg is list of sublists, ~@{  arg is spread ~:@{ spread lists
;;; We have nuked two catch tags. Instead throw two different values:
;;; T if ~^ and 'format-colon-escape if ~:^

(defun format-do-iteration (stream start end max-iter colon atsign at-least-once-p)
  (flet ((do-iteration-1 (stream start end colon at-least-once-p)
           (let (catchp)
             (do* ((count 0 (1+ count)))
                  ((or (= count max-iter)
                       (and (null *format-arguments*)
                            (if (= count 0) (not at-least-once-p) t))))
               (setq catchp
                     (catch 'format-escape
                       (if colon
                         (let* ((args (unless (and at-least-once-p (null *format-arguments*))
                                        (pop-format-arg)))
                                (*format-top-level* nil)
                                (*format-colon-rest* *format-arguments*)
                                (*format-arguments* args)
                                (*format-original-arguments* args))
                           (unless (listp *format-arguments*)
                             (report-bad-arg *format-arguments* 'list))
                           (if (functionp start)
                             (apply start stream args)
                             (sub-format stream start end)))
                         (let ((*format-original-arguments* *format-arguments*))
                           (if (functionp start)
                             (setq *format-arguments* (apply start stream *format-arguments*))
                             (sub-format stream start end))))
                       nil))
               (when (or (eq catchp 'format-colon-escape)
                         (and catchp (null colon)))
                 (return-from do-iteration-1  nil))))))
      (if atsign
        (do-iteration-1 stream start end colon at-least-once-p)        
        ; no atsign - munch on first arg
        (let* ((*format-arguments* (pop-format-arg))
               (*format-top-level* nil)
               (*format-original-arguments* *format-arguments*))
          (unless (listp *format-arguments*)
            (report-bad-arg *format-arguments* 'list))
          (do-iteration-1 stream start end colon at-least-once-p)))))
  

;;; Justification  ~< ... ~>

;;; Parses a list of clauses delimited by ~; and terminated by ~>.
;;; Recursively invoke SUB-FORMAT to process them, and return a list
;;; of the results, the length of this list, and the total number of
;;; characters in the strings composing the list.


(defun format-get-trailing-segments ()
  (format-nextchar)
  (multiple-value-bind (prev tilde parms colon atsign cmd)
                       (format-find-command '(#\; #\>) nil T)
    (with-format-parameters parms ()
      (when colon
        (format-error "~~:; allowed only after first segment in ~~<"))
      (format-no-flags nil atsign))
    (let ((str (catch 'format-escape
                 (with-format-string-output stream
                   (sub-format stream prev tilde)))))      
      (if (stringp str)
        (if (eq cmd #\;)
          (multiple-value-bind
            (segments numsegs numchars)
            (format-get-trailing-segments)
            (values (cons str segments)
                    (1+ numsegs)
                    (+ numchars
                       (length str))))
          (values (list str)
                  1
                  (length str)))
        (progn
          (unless (eq cmd #\>) (format-find-command '(#\>) nil T))
          (values () 0 0))))))


;;; Gets the first segment, which is treated specially.  Call 
;;; FORMAT-GET-TRAILING-SEGMENTS to get the rest.

(defun format-get-segments ()
  (let (ignore)
    (declare (ignore-if-unused ignore)) ; why??
    (multiple-value-bind (prev tilde parms colon atsign cmd)
                         (format-find-command '(#\; #\>) nil T) ; skipping
      (when atsign
        (format-error "Atsign flag not allowed"))
      ;(setq *format-arguments* blech)
      (let ((first-seg (catch 'format-escape
                         (with-format-string-output stream
                           (sub-format stream prev tilde)))))
        (if (stringp first-seg)
          (if (eq cmd #\;)
            (progn
              (when parms
                (setq *format-index* tilde)
                ; now get the parameters if any - do this way cause of the V thingies
                ; maybe only necessary in the : case
                (multiple-value-setq (ignore ignore parms)
                                     (format-find-command '(#\; #\>) t T)))              
              (multiple-value-bind
                (segments numsegs numchars)
                (format-get-trailing-segments)
                (if colon
                  (values first-seg parms segments numsegs numchars)
                  (values nil nil (cons first-seg segments)
                          (1+ numsegs)
                          (+ (length first-seg) numchars)))))
            (values nil nil (list first-seg) 1 (length first-seg)))
          (progn
            (unless (eq cmd #\>) (format-find-command '(#\>) nil T))
            (values nil nil () 0 0)))))))


#|
;;; Given the total number of SPACES needed for padding, and the number
;;; of padding segments needed (PADDINGS), returns a list of such segments.
;;; We try to allocate the spaces equally to each segment.  When this is
;;; not possible, we allocate the left-over spaces randomly, to improve the
;;; appearance of many successive lines of justified text.
;;; 
;;; Query:  Is this right?  Perhaps consistency might be better for the kind
;;; of applications ~<~> is used for.

(defun make-pad-segs (spaces paddings)
  (do* ((extra-space () (and (plusp extra-spaces)
                             (< (random (float 1)) (/ segs extra-spaces))))
        (result () (cons (if extra-space (1+ min-space) min-space) result))
        (min-space (truncate spaces paddings))
        (extra-spaces (- spaces (* paddings min-space))
                      (if extra-space (1- extra-spaces) extra-spaces))
        (segs paddings (1- segs)))
       ((zerop segs) result)))
|#
(defun make-pad-segs (spaces segments)
  (multiple-value-bind (min-space extra-spaces) (truncate spaces segments)
    (declare (fixnum min-space extra-spaces))
    (let* ((result (make-list segments :initial-element min-space))
           (res result))
      (setq min-space (1+ min-space))
      (dotimes (i extra-spaces)
        (rplaca res min-space)
        (setq res (%cdr res)))
      result)))

;;; Determine the actual width to be used for a field requiring WIDTH
;;; characters according to the following rule:  If WIDTH is less than or
;;; equal to MINCOL, use WIDTH as the actual width.  Otherwise, round up 
;;; to MINCOL + k * COLINC for the smallest possible positive integer k.

(defun format-round-columns (width mincol colinc)
  (if (< width mincol)
    (+ width (* colinc (ceiling (- mincol width) colinc)))
    width))

(defun format-justification-round-columns (width mincol colinc)
  (if (< width mincol)
    mincol
    (+ mincol (* colinc (ceiling (- width mincol) colinc)))))

(defformat #\< format-justification (stream colon atsign &rest parms)
  (declare (dynamic-extent parms))
  (multiple-value-bind (start tilde ecmd ecolon eatsign)
                       (format-find-command-no-params '(#\>)) ; bumps format-index
    (declare (ignore tilde ecmd))
    (cond
     (ecolon
      (format-logical-block stream colon atsign eatsign start *format-index* parms))
     (t (setq *format-index* start)
        (with-format-parameters parms ((mincol 0) (colinc 1) (minpad 0) (padchar #\space))
          (unless (integerp mincol)
            (format-error "Mincol must be an integer - ~S" mincol))
          (unless (and (integerp colinc) (plusp colinc))
            (format-error "Colinc must be a positive integer - ~S" colinc))
          (unless (integerp minpad)
            (format-error "Minpad must be an integer - ~S" minpad))
          (unless (characterp padchar)
            (if (typep padchar `(integer 0 #.char-code-limit))
              (setq padchar (code-char padchar))
              (format-error "Padchar must be a character or integer from 0 to ~a - ~S"
                            char-code-limit padchar)))
          (format-nextchar)
          (multiple-value-bind (special-arg special-parms segments numsegs numchars)
                               (format-get-segments)
            (when (= numsegs 1) (setq minpad 0))
            (when segments
              (let* ((padsegs (+ (if (or colon (= numsegs 1)) 1 0)
                                 (1- numsegs)
                                 (if atsign 1 0)))
                     (width (format-justification-round-columns (+ numchars (* minpad padsegs))
                                                  mincol colinc))
                     (spaces (if (and atsign (not colon) (= numsegs 1)) ;dirty but works
                                 (list 0 (- width numchars))
                                 (append (if (or colon (= numsegs 1)) () '(0))
                                         (make-pad-segs (- width numchars) padsegs)
                                         (if atsign () '(0))))))
                (when special-arg
                  (if *format-pprint*
                      (format-error "Justification illegal in this context"))
                  (setq *format-justification-semi* t)
                  (with-format-parameters special-parms ((spare 0)
                                                         (linel (stream-line-length stream)))
                      
                    (let ((pos (column stream)))
                      (when (> (+ pos width spare) linel)
                        (stream-write-entire-string stream special-arg)))))
                (do ((segs segments (cdr segs))
                     (spcs spaces (cdr spcs)))
                    ((null segs) (dotimes (i (car spcs)) (write-char padchar stream)))
                  (dotimes (i (car spcs)) (write-char padchar stream))
                  (stream-write-entire-string stream (car segs)))))))))))


(defun format-logical-block (stream colon atsign end-atsign start end &rest parms)
  (declare (ignore parms))
  (flet ((format-check-simple (str)
           (when (and str (or (%str-member #\~ str) (%str-member #\newline str)))
             (format-error "Suffix and prefix must be simple")))
         (first-block-p (start)
           (let* ((*format-index* 0))
             (loop
               (parse-format-operation)
               (when (eq (format-peek) #\<)
                 (cond ((eq *format-index* start)
                        (return t))
                       (t (return nil))))))))
    (format-no-semi #\<)
    (let ((format-string *format-control-string*)
          (prefix (if colon "(" ""))
          (suffix (if colon ")" ""))
          body-string start1 tilde ignore colon1 atsign1 per-line-p)
      (declare (ignore-if-unused ignore colon1))
      (setq *format-index* start)
      (multiple-value-setq (start1 tilde ignore colon1 atsign1)
        (format-find-command  '(#\; #\>)))
      (setq body-string (%substr format-string (1+ start) tilde))
      (when (not (eql *format-index* end)) ; > 1 segment
        (setq prefix body-string)
        (if atsign1 (setq per-line-p t))
        (multiple-value-setq (start1 tilde)
          (format-find-command '(#\; #\>)))
        (setq body-string (%substr format-string (1+ start1) tilde))
        (when (neq *format-index* end)
          (multiple-value-setq (start1 tilde)(format-find-command  '(#\; #\>)))
          (setq suffix (%substr format-string (1+ start1) tilde))
          (when (neq *format-index* end)
            (format-error "Too many chunks"))))
      (when end-atsign (setq body-string (format-fill-transform body-string)))
      (format-check-simple prefix)
      (format-check-simple suffix)
      (let ((args (if (not atsign)
                      ; This piece of garbage is needed to avoid double length counting from (formatter ...) things
                      ; but also to allow (flet . t) not to barf.
                      ; Was formerly simply  (if *format-arguments* (pop-format-arg))
                      ; Actually wanna not count the arg iff the ~< is at the top level
                      ; in a format string i.e. "is this the first ~< in THIS string?"                    
                      (if *format-arguments*
                          (if  (and (listp *format-arguments*)
                                    (first-block-p start))
                               (pop *format-arguments*)  ; dont count
                               (pop-format-arg)) ; unless not listp or not first
                          (progn
                            (setq *format-index* start)
                            (format-error "Missing argument")))
                      (prog1 *format-arguments*
                        (setq *format-arguments* nil))))
            (*format-control-string* body-string)
            (*format-top-level* (and atsign *format-top-level*)))
        (let ((*logical-block-p* t)
              (xp-struct (cond ((xp-structure-p stream) stream)
                               ((typep stream 'xp-stream)
                                (slot-value stream 'xp-structure)))))
          ; lets avoid unnecessary closures
          (cond (xp-struct (logical-block-sub xp-struct args  prefix suffix per-line-p atsign))
                (t (maybe-initiate-xp-printing
                    #'(lambda (s o)
                        (logical-block-sub s o  prefix suffix per-line-p atsign))
                    stream args))))))))


    
; flet?
(defun logical-block-sub (stream args  prefix suffix per-line-p atsign)
  ;(push (list args body-string) barf)
  (let ((circle-chk (not (or *format-top-level* (and atsign (eq *current-length* -1)))))) ; i.e. ~<~@<
    (let ((*current-level* (1+ *current-level*)) ; these are for pprint
          (*current-length* -1))
      (declare (special *current-level* *current-length*))
      (unless (check-block-abbreviation stream args circle-chk) ;(neq args *format-original-arguments*)) ;??
        (start-block stream prefix per-line-p suffix)
        (let ((*logical-block-xp* stream)    ; for pop-format-arg
              (my-stream (if (xp-structure-p stream) (get-xp-stream stream) stream)))
          (catch 'logical-block
            (do-sub-format-1 my-stream args)))
        (end-block stream suffix)))))

; bash in fill conditional newline after white space (except blanks after ~<newline>)
; I think this is silly!
(defun format-fill-transform (string)
  (let ((pos 0)(end (length (the string string)))(result "") ch)
    (while (%i< pos end)
      (let ((wsp-pos (min (or (%str-member #\space string pos) end)
                          (or (%str-member #\tab string pos) end)))
            (yes nil))
        (when (%i< wsp-pos end)
          (when (not (and (%i> wsp-pos 1)
                          (eq (schar string (%i- wsp-pos 1)) #\newline)
                          (or (eq (setq ch (schar string (%i- wsp-pos 2))) #\~)
                              (and (%i> wsp-pos 2)
                                   (memq ch '(#\: #\@))
                                   (eq (schar string (%i- wsp-pos 3)) #\~)))))
            (setq yes t))
          (loop 
            (while (%i< wsp-pos end)
              (setq ch (schar string wsp-pos))
              (when (Not (%str-member ch wsp)) (return))
              (setq wsp-pos (%i+ 1 wsp-pos)))
            (return)))
        (setq result (%str-cat result (%substr string pos  wsp-pos) (if yes "~:_" "")))
      (setq pos wsp-pos)))
    result))


;;;;some functions needed for dealing with floats

;;;; Floating Point printing
;;;
;;;  Written by Bill Maddox
;;;
;;;
;;;
;;; FLONUM-TO-STRING (and its subsidiary function FLOAT-STRING) does most of 
;;; the work for all printing of floating point numbers in the printer and in
;;; FORMAT.  It converts a floating point number to a string in a free or 
;;; fixed format with no exponent.  The interpretation of the arguments is as 
;;; follows:
;;;
;;;     X        - The floating point number to convert, which must not be
;;;                negative.
;;;     WIDTH    - The preferred field width, used to determine the number
;;;                of fraction digits to produce if the FDIGITS parameter
;;;                is unspecified or NIL.  If the non-fraction digits and the
;;;                decimal point alone exceed this width, no fraction digits
;;;                will be produced unless a non-NIL value of FDIGITS has been
;;;                specified.  Field overflow is not considerd an error at this
;;;                level.
;;;     FDIGITS  - The number of fractional digits to produce. Insignificant
;;;                trailing zeroes may be introduced as needed.  May be
;;;                unspecified or NIL, in which case as many digits as possible
;;;                are generated, subject to the constraint that there are no
;;;                trailing zeroes.
;;;     SCALE    - If this parameter is specified or non-NIL, then the number
;;;                printed is (* x (expt 10 scale)).  This scaling is exact,
;;;                and cannot lose precision.
;;;     FMIN     - This parameter, if specified or non-NIL, is the minimum
;;;                number of fraction digits which will be produced, regardless
;;;                of the value of WIDTH or FDIGITS.  This feature is used by
;;;                the ~E format directive to prevent complete loss of
;;;                significance in the printed value due to a bogus choice of
;;;                scale factor.
;;;
;;; Most of the optional arguments are for the benefit for FORMAT and are not
;;; used by the printer.
;;;
;;; Returns:
;;; (VALUES DIGIT-STRING DIGIT-LENGTH LEADING-POINT TRAILING-POINT DECPNT)
;;; where the results have the following interpretation:
;;;
;;;     DIGIT-STRING    - The decimal representation of X, with decimal point.
;;;     DIGIT-LENGTH    - The length of the string DIGIT-STRING.
;;;     LEADING-POINT   - True if the first character of DIGIT-STRING is the
;;;                       decimal point.
;;;     TRAILING-POINT  - True if the last character of DIGIT-STRING is the
;;;                       decimal point.
;;;     POINT-POS       - The position of the digit preceding the decimal
;;;                       point.  Zero indicates point before first digit.
;;;     NZEROS          - number of zeros after point
;;;
;;; WARNING: For efficiency, there is a single string object *digit-string*
;;; which is modified destructively and returned as the value of
;;; FLONUM-TO-STRING.  Thus the returned value is not valid across multiple 
;;; calls.
;;;
;;; NOTE:  FLONUM-TO-STRING goes to a lot of trouble to guarantee accuracy.
;;; Specifically, the decimal number printed is the closest possible 
;;; approximation to the true value of the binary number to be printed from 
;;; among all decimal representations  with the same number of digits.  In
;;; free-format output, i.e. with the number of digits unconstrained, it is 
;;; guaranteed that all the information is preserved, so that a properly-
;;; rounding reader can reconstruct the original binary number, bit-for-bit, 
;;; from its printed decimal representation. Furthermore, only as many digits
;;; as necessary to satisfy this condition will be printed.
;;;
;;;
;;; FLOAT-STRING actually generates the digits for positive numbers.  The
;;; algorithm is essentially that of algorithm Dragon4 in "How to Print 
;;; Floating-Point Numbers Accurately" by Steele and White.  The current 
;;; (draft) version of this paper may be found in [CMUC]<steele>tradix.press.
;;; DO NOT EVEN THINK OF ATTEMPTING TO UNDERSTAND THIS CODE WITHOUT READING 
;;; THE PAPER!

#| Improvements beyond Steele and White:
 Steele and White (original and retrospective included): http://kurtstephens.com/files/p372-steele.pdf
 Bryan O'Sullivan: http://www.serpentine.com/blog/2011/06/29/here-be-dragons-advances-in-problems-you-didnt-even-know-you-had/
 Burger and Dybvig: http://www.cs.indiana.edu/~dyb/pubs/FP-Printing-PLDI96.pdf
 Loitsch: http://www.cs.tufts.edu/~nr/cs257/archive/florian-loitsch/printf.pdf
 Ryan Juckett: http://www.ryanjuckett.com/programming/printing-floating-point-numbers/part-2/
 Gay: http://citeseer.ist.psu.edu/viewdoc/summary?doi=10.1.1.31.4049
|#

(defun flonum-to-string (n &optional width fdigits scale)
  (let ((*print-radix* nil))
    (cond ((zerop n)(values "" 0 0))
          ((and (not (or width fdigits scale))
                (double-float-p n)
                ; cheat for the only (?) number that fails to be aesthetically pleasing
                (= n 1e23))
           (values "1" 24 23))
          (t (let ((string (make-array 12 :element-type 'base-char
                                       :fill-pointer 0 :adjustable t)))
               (multiple-value-bind (sig exp)(integer-decode-float n)
                 (float-string string sig exp (integer-length sig) width fdigits scale)))))))

;;; if width given and fdigits nil then if exponent is >= 0 returns at
;;; most width-1 digits if exponent is < 0 returns (- width (- exp) 1)
;;; digits if fdigits given width is ignored, returns fdigits after
;;; (implied) point The Steele/White algorithm can produce a leading
;;; zero for 1e23 which lies exactly between two double floats -
;;; rounding picks the float whose rational is
;;; 99999999999999991611392. This guy wants to print as
;;; 9.999999999999999E+22. The untweaked algorithm generates a leading
;;; zero in this case.  (actually wants to print as 1e23!)  If we
;;; choose s such that r < s - m/2, and r = s/10 - m/2 (which it does
;;; in this case) then r * 10 < s => first digit is zero and
;;; (remainder (* r 10) s) is r * 10 = new-r, 10 * m = new-m new-r = s
;;; - new-m/2 so high will be false and she won't round up we do r *
;;; (expt 2 (- e (- scale))) and s * (expt 5 (- scale)) i.e. both less
;;; by (expt 2 (- scale))

(defun float-string (string f e p &optional width fdigits scale)
  (macrolet ((nth-digit (n) `(%code-char (%i+ ,n (%char-code #\0)))))    
    (let ((r f)(s 1)(m- 1)(m+ 1)(k 0) cutoff roundup (mm nil))
      (when (= f (if (eql p 53) #.(ash 1 52) (ash 1 (1- p))))
        (setq mm t))
      (when (or (null scale)(zerop scale))
        ; approximate k
        (let ((fudge 0))
          (setq fudge (truncate (*  (%i+ e p) .301)))
          (when (neq fudge 0)
            (setq k fudge)
            (setq scale (- k)))))
      (when (and scale (not (eql scale 0)))      
        (if (minusp scale)
          (setq s (* s (5-to-e  (- scale))))
          (let ((scale-factor (5-to-e scale)))
            (setq r (* r scale-factor))
            (setq m+ scale-factor)
            (when mm (setq m- scale-factor)))))
      (let ((shift (- e (if scale (- scale) 0))))
        (declare (fixnum shift))
        ;(print (list e scale shift))
        (cond ((> shift 0)
               (setq r (ash f shift))
               (setq m+ (ash m+ shift))
               (when mm (setq m- (ash m- shift))))
              ((< shift 0)
               (setq s (ash s (- shift))))))
      (when mm
        (setq m+ (+ m+ m+))
        (setq r (+ r r))
        (setq s (+ s s)))    
      (let ((ceil (ceiling s 10))(fudge 1))
        (while (< r ceil)
          (setq k (1- k))
          (setq r (* r 10))
          (setq fudge (* fudge 10)))
        (when (> fudge 1)
          (setq m+ (* m+ fudge))
          (when mm (setq m- (* m- fudge)))))    
      (let ((2r (+ r r)))
        (loop
          (let ((2rm+ (+ 2r m+)))          
            (while
              (if (not roundup)  ; guarantee no leading zero
                (> 2rm+ (+ s s))
                (>=  2rm+ (+ s s)))
              (setq s (* s 10))
              (setq k (1+ k))))
          (when (not (or fdigits width))(return))
          (cond 
           (fdigits (setq cutoff (- fdigits)))
           (width
            (setq cutoff
                  (if (< k 0) (- 1 width)(1+ (- k width))))
            ;(if (and fmin (> cutoff (- fmin))) (setq cutoff (- fmin)))
            ))
          (let ((a (if cutoff (- cutoff k) 0))
                (y s))
            (DECLARE (FIXNUM A))
            (if (>= a 0)
              (when (> a 0)(setq y (* y (10-to-e a))))
              (setq y (ceiling y (10-to-e (the fixnum (- a))))))
            (when mm (setq m- (max y m-)))
            (setq m+ (max y m+))
            (when (= m+ y) (setq roundup t)))
          (when (if (not roundup)   ; tweak as above
                  (<= (+ 2r m+)(+ s s))
                  (< (+ 2r m+)(+ s s)))
            (return))))
      (let* ((h k)
             (half-m+ (* m+ 5))  ; 10 * m+/2
             (half-m- (if mm (* m- 5)))
             u high low 
             )
        ;(print (list r s m+ roundup))
        (unless (and fdigits (>= (- k) fdigits))
          (loop
            (setq k (1- k))
            (multiple-value-setq (u r) (truncate (* r 10) s))          
            (setq low (< r (if mm half-m- half-m+)))
            (setq high 
                  (if (not roundup)
                    (> r (- s half-m+))
                    (>= r (- s half-m+))))                   
            (if (or low high)
              (return)
              (progn
                (vector-push-extend (nth-digit u) string)))
            (when mm (setq half-m- (* half-m- 10) ))
            (setq half-m+ (* half-m+ 10)))
          ;(print (list r s  high low h k))
          (vector-push-extend
           (nth-digit (cond
                       ((and low (not high)) u) 
                       ((and high (not low))(+ u 1))
                       
                       (t ;(and high low)
                        (if (<= (+ r r) s) u (1+ u)))))
           string))
        ; second value is exponent, third is exponent - # digits generated
        (values string h k)))))


(defparameter integer-powers-of-10 (make-array (+ 12 (floor 324 12))))

; e better be positive
(defun 10-to-e (e)
  (declare (fixnum e)(optimize (speed 3)(safety 0)))
  (if (> e 335)
    (* (10-to-e 334) (10-to-e (%i- e 334)))
    (if (< e 12)
      (svref integer-powers-of-10 e)
      (multiple-value-bind (q r) (truncate e 12)
        (declare (fixnum q r))        
        (if (eql r 0)
          (svref integer-powers-of-10 (%i+ q 11))
          (* (svref integer-powers-of-10 r)
             (svref integer-powers-of-10 (%i+ q 11))))))))


(let ((array integer-powers-of-10))
  (dotimes (i 12)
    (setf (svref array i)  (expt 10 i)))
  (dotimes (i (floor 324 12))
    (setf (svref array (+ i 12)) (expt 10 (* 12 (1+ i))))))
#|
(defun 10-to-e (e)
  (ash (5-to-e e) e))
|#
      
;;; Given a non-negative floating point number, SCALE-EXPONENT returns a
;;; new floating point number Z in the range (0.1, 1.0] and and exponent
;;; E such that Z * 10^E is (approximately) equal to the original number.
;;; There may be some loss of precision due the floating point representation.
;;; JUST do the EXPONENT since thats all we use

(defconstant single-float-min-e
  (nth-value 1 (decode-float least-positive-single-float)))
(defconstant double-float-min-e
  (nth-value 1 (decode-float least-positive-double-float)))

;;; Adapted from CMUCL.

;; This is a modified version of the scale computation from Burger and
;; Dybvig's paper "Printing floating-point quickly and accurately."
;; We only want the exponent, so most things not needed for the
;; computation of the exponent have been removed.  We also implemented
;; the floating-point log approximation given in Burger and Dybvig.
;; This is very noticeably faster for large and small numbers.  It is
;; slower for intermediate sized numbers.
(defun accurate-scale-exponent (v)
  (declare (type float v))
  (if (zerop v)
      1
      (let ((float-radix 2)		; b
	    (float-digits (float-digits v)) ; p
	    (min-e
	     (etypecase v
	       (single-float single-float-min-e)
	       (double-float double-float-min-e))))
	(multiple-value-bind (f e)
	    (integer-decode-float v)
	  (let ( ;; FIXME: these even tests assume normal IEEE rounding
		;; mode.  I wonder if we should cater for non-normal?
		(high-ok (evenp f)))
	    ;; We only want the exponent here.
	    (labels ((flog (x)
		       (declare (type (float (0.0)) x))
		       (let ((xd (etypecase x
				   (single-float
				    (float x 1d0))
				   (double-float
				    x))))
			 (ceiling (- (the (double-float -400d0 400d0)
					  (log xd 10d0))
				     1d-10))))
		     (fixup (r s m+ k)
		       (if (if high-ok
			       (>= (+ r m+) s)
			       (> (+ r m+) s))
			   (+ k 1)
			   k))
		     (scale (r s m+)
		       (let* ((est (flog v))
			      (scale (the integer (10-to-e (abs est)))))
			 (if (>= est 0)
			     (fixup r (* s scale) m+ est)
			     (fixup (* r scale) s (* m+ scale) est)))))
	      (let (r s m+)
		(if (>= e 0)
		    (let* ((be (expt float-radix e))
			   (be1 (* be float-radix)))
		      (if (/= f (expt float-radix (1- float-digits)))
			  (setf r (* f be 2)
				s 2
				m+ be)
			  (setf r (* f be1 2)
				s (* float-radix 2)
				m+ be1)))
		    (if (or (= e min-e) 
			    (/= f (expt float-radix (1- float-digits))))
			(setf r (* f 2)
			      s (* (expt float-radix (- e)) 2)
			      m+ 1)
			(setf r (* f float-radix 2)
			      s (* (expt float-radix (- 1 e)) 2)
			      m+ float-radix)))
		(scale r s m+))))))))

;;; Page  ~|

(defformat #\| format-page (stream colon atsign &rest parms)
  (declare (dynamic-extent parms))
  (format-no-flags colon atsign)
  (with-format-parameters parms ((repeat-count 1))
    (declare (fixnum repeat-count))
    (dotimes (i repeat-count) (write-char #\page stream))))


(defun format-eat-whitespace ()
  (do* ((i *format-index* (1+ i))
        (s *format-control-string*)
        (n *format-length*))
       ((or (= i n)
            (not (whitespacep (schar s i))))
        (setq *format-index* (1- i)))))

(defun format-newline (stream colon atsign parms)
  (with-format-parameters parms ()
    (cond (colon
           (when atsign
             (format-error "~:@<newline> is undefined")))
          (atsign (terpri stream) (format-eat-whitespace))
          (t (format-eat-whitespace)))))
  
(defformat  #\newline format-newline (stream colon atsign &rest parms)
  (declare (dynamic-extent parms))
  (format-newline stream colon atsign parms))

(defformat #\return format-newline (stream colon atsign &rest parms)
  (declare (dynamic-extent parms))
  (format-newline stream colon atsign parms))

;;; Indirection  ~?

(defformat #\? format-indirection (stream colon atsign)
  (format-no-flags colon nil)
  (let ((string (pop-format-arg)))
    (unless (or (stringp string)(functionp string))
      (format-error "Indirected control string is not a string or function"))
    (when (and (stringp string) (not (simple-string-p string)))
      (setq string (coerce string 'simple-string)))
    (catch 'format-escape
      (let ((error 
             (catch 'format-error
               (cond 
                ((stringp string)
                 (let* ((length (length (the simple-string string)))
                        (*format-control-string* string)
                        (*format-length* length)
                        (*format-index* 0))
                    (if atsign
                      (sub-format stream 0 length)
                      (let ((args (pop-format-arg)))
                        (let ((*format-top-level* nil)
                              (*format-arguments* args)
                              (*format-original-arguments* args))
                          (sub-format stream 0 length))))))
                (T ;(functionp string)
                 (if (not atsign)
                   (apply string stream (pop-format-arg))
                   ; account for the args it eats
                   (setq *format-arguments* (apply string stream *format-arguments*)))))
               nil)))
        (when error (format-indirect-error error))))))




;;; Ascii  ~A

(defformat #\A format-princ (stream colon atsign &rest parms)
  (declare (dynamic-extent parms))
  (let ((arg (pop-format-arg)))
    (if (null parms)
      (princ (or arg (if colon "()" nil)) stream)
      (with-format-parameters parms ((mincol 0) (colinc 1) (minpad 0) (padchar #\space))
        (format-write-field
         stream
         (if (or arg (not colon))
           (princ-to-string arg)
           "()")
         mincol colinc minpad padchar atsign)))))



;;; S-expression  ~S
	    
(defformat #\S format-prin1 (stream colon atsign &rest parms)
  (declare (dynamic-extent parms))
  (let ((arg (pop-format-arg)))
    (if (null parms)
      (if (or arg (not colon)) (prin1 arg stream) (princ "()" stream))
      (with-format-parameters parms ((mincol 0) (colinc 1) (minpad 0) (padchar #\space))
        (format-write-field
         stream
         (if (or arg (not colon))
           (prin1-to-string arg)
           "()")
         mincol colinc minpad padchar atsign)))))



;;; Character  ~C

(defformat #\C format-print-character (stream colon atsign)
  (let* ((char (character (pop-format-arg)))
         (code (char-code char))
         (name (char-name char)))
    (cond ((and atsign (not colon))
           (prin1 char stream))
          (colon
           (if (or (eql char #\space)
                   (not (graphic-char-p char)))
             (princ name stream)
             (write-char char stream)))
          ((not (or atsign colon))
           (write-char char stream))
          ((and (< code 32) atsign)
	   (setq char (code-char (logxor code 64)))
           (if (or colon (%str-member char "@CGHIJKLM[\\]^_"))
               (princ name stream)
               (progn
                 (write-char #\^ stream)
                 (write-char char stream)))
           (princ " (" stream)
           (princ "Control " stream)
           (write-char char stream)
           (write-char #\) stream))
          (name (princ name stream))
          (t (write-char char stream)))))


;;; NUMERIC PRINTING



;;; Output a string in a field at MINCOL wide, padding with PADCHAR.
;;; Pads on the left if PADLEFT is true, else on the right.  If the
;;; length of the string plus the minimum permissible padding, MINPAD,
;;; is greater than MINCOL, the actual field size is rounded up to
;;; MINCOL + k * COLINC for the smallest possible positive integer k.

(defun format-write-field (stream string mincol colinc minpad padchar padleft)
  (unless (or (null mincol)
              (integerp mincol))
    (format-error "Mincol must be an integer - ~S" mincol))
  (unless (and (integerp colinc) (plusp colinc))
    (format-error "Colinc must be a positive integer - ~S" colinc))
  (unless (integerp minpad)
    (format-error "Minpad must be an integer - ~S" minpad))
  (unless (characterp padchar)
    (if (typep padchar `(integer 0 #.char-code-limit))
      (setq padchar (code-char padchar))
      (format-error "Padchar must be a character or integer from 0 to ~a - ~S"
                    char-code-limit padchar)))
  (let* ((strlen (length (the string string)))
         (strwid (+ strlen minpad))
         (width (if mincol
                  (format-round-columns strwid mincol colinc)
                  strwid)))
    (if padleft
      (dotimes (i (the fixnum (- width strlen))) (write-char padchar stream)))
    (write-string string stream :start  0 :end strlen)
    (unless padleft
      (dotimes (i (the fixnum (- width strlen))) (write-char padchar stream)))))


;;; This functions does most of the work for the numeric printing
;;; directives.  The parameters are interpreted as defined for ~D.

(defun format-print-number (stream number radix print-commas-p print-sign-p parms)
  (declare (dynamic-extent parms))
  (declare (type t number) (type fixnum radix))
  #+wrong
  (when (> (length parms) 2) (setq print-commas-p t)) ; print commas if char or interval provided
  (if (not (integerp number))
      (let ((*print-base* radix)
            (*print-escape* nil)
            (*print-radix* nil))
        (declare (special *print-base* *print-radix*))
        (princ number stream))
    (with-format-parameters parms
          ((mincol 0) (padchar #\space) (commachar #\,) (commainterval 3))
      ; look out for ",0D" - should be ",'0D"
      (unless (characterp padchar)
        (error "Use '~A instead of ~A for padchar in format directive" padchar padchar))
       (setq print-sign-p 
             (cond ((and print-sign-p (>= number 0)) #\+)
                   ((< number 0) #\-)))
       (setq number (abs number))
       (block HAIRY
         (block SIMPLE
           (if (and (not print-commas-p) (eql 0 mincol))
             (return-from SIMPLE))
           (let ((lg 0)
                 (commas 0))
             (declare (type fixnum lg commas))
             (do ((n (abs number) (floor n radix)))
                 ((%i< n radix))
               (declare (type integer n))
               (setq lg (%i+ lg 1))) ; lg is 1- significant digits             
             (setq commas (if print-commas-p
                              (floor lg commainterval)
                              0))
             (when print-sign-p
               (setq lg (1+ lg)))
             (when (and (eq commas 0)
                        (%i<= mincol lg))
               (return-from SIMPLE))
             ;; Cons-o-rama no more !
             (let* ((s (make-string-output-stream)))
               (when  (neq padchar #\space)
                 (dotimes (i (- mincol (+ lg commas) 1))
                   (write-char padchar s)))
               (when print-sign-p (write-char print-sign-p s))
               (%pr-integer  number radix s)                           
               (dotimes (i (the fixnum commas)) (write-char commachar s))
               (let ((text (get-output-stream-string s)))
                 (declare (type string text))
                 ;; -1234567,, => -1,234,567
                 (when (%i> commas 0)
                   (do* ((dest (%i- (length text) 1))
                         (source (%i- dest commas)))
                        ((= source dest))
                     (declare (type fixnum dest source))
                     (dotimes (i (the fixnum commainterval))
                       (setf (char text dest) (char text source)
                             dest (1- dest) 
                             source (1- source)))
                     (setf (char text dest) commachar
                           dest (1- dest))))
                 (format-write-field stream text mincol 1 0 padchar t)
                 (return-from HAIRY)))))
         ;; SIMPLE case         
         (when print-sign-p (write-char print-sign-p stream))
         (%pr-integer number radix stream))))
  nil)

;;; Print a cardinal number in English

(eval-when (:compile-toplevel :execute)
(defmacro cardinal-ones ()
  "Table of cardinal ones-place digits in English"
        '#(nil "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))
(defmacro cardinal-tens ()
  "Table of cardinal tens-place digits in English"
        '#(nil nil "twenty" "thirty" "forty"
           "fifty" "sixty" "seventy" "eighty" "ninety"))
(defmacro cardinal-teens ()
        '#("ten" "eleven" "twelve" "thirteen" "fourteen"  ;;; RAD
	   "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"))
)


(defun format-print-small-cardinal (stream n)
  (multiple-value-bind (hundreds rem) (truncate n 100)
    (when (plusp hundreds)
      (write-string (svref (cardinal-ones) hundreds) stream)
      (write-string " hundred" stream)
      (when (plusp rem) (write-char #\space stream)))    ; ; ; RAD
    (when (plusp rem)
      (multiple-value-bind (tens ones) (truncate rem 10)
        (cond ((< 1 tens)
               (write-string (svref (cardinal-tens) tens) stream)
               (when (plusp ones)
                 (write-char #\- stream)
                 (write-string (svref (cardinal-ones) ones) stream)))
              ((= tens 1)
               (write-string (svref (cardinal-teens) ones) stream))
              ((plusp ones)
               (write-string (svref (cardinal-ones) ones) stream)))))))

(eval-when (:compile-toplevel :execute)
  (defmacro cardinal-periods ()
    "Table of cardinal 'teens' digits in English"
    '#("" " thousand" " million" " billion" " trillion" " quadrillion"
       " quintillion" " sextillion" " septillion" " octillion" " nonillion" 
       " decillion"))
)


(defun format-print-cardinal (stream n)
  (cond ((minusp n)
         (stream-write-entire-string stream "negative ")
         (format-print-cardinal-aux stream (- n) 0 n))
        ((zerop n)
         (stream-write-entire-string stream "zero"))
        (t (format-print-cardinal-aux stream n 0 n))))

(defun format-print-cardinal-aux (stream n period err)
  (multiple-value-bind (beyond here) (truncate n 1000)
    (unless (<= period 10)
      (format-error "Number too large to print in English: ~:D" err))
    (unless (zerop beyond)
      (format-print-cardinal-aux stream beyond (1+ period) err))
    (unless (zerop here)
      (unless (zerop beyond) (write-char #\space stream))
      (format-print-small-cardinal stream here)
      (stream-write-entire-string stream (svref (cardinal-periods) period)))))


;;; Print an ordinal number in English


(eval-when (:compile-toplevel :execute)
(defmacro ordinal-ones ()
  "Table of ordinal ones-place digits in English"
  '#(nil "first" "second" "third" "fourth"
         "fifth" "sixth" "seventh" "eighth" "ninth"))
(defmacro ordinal-tens ()
  "Table of ordinal tens-place digits in English"
  '#(nil "tenth" "twentieth" "thirtieth" "fortieth"
         "fiftieth" "sixtieth" "seventieth" "eightieth" "ninetieth"))
)

(defun format-print-ordinal (stream n)
  (when (minusp n)
    (stream-write-entire-string stream "negative "))
  (let ((number (abs n)))
    (multiple-value-bind (top bot) (truncate number 100)
      (unless (zerop top) (format-print-cardinal stream (- number bot)))
      (when (and (plusp top) (plusp bot)) (write-char #\space stream))
      (multiple-value-bind (tens ones) (truncate bot 10)
        (cond ((= bot 12) (stream-write-entire-string stream "twelfth"))
              ((= tens 1)
               (stream-write-entire-string stream (svref (cardinal-teens) ones));;;RAD
               (stream-write-entire-string stream "th"))
              ((and (zerop tens) (plusp ones))
               (stream-write-entire-string stream (svref (ordinal-ones) ones)))
              ((and (zerop ones)(plusp tens))
               (stream-write-entire-string stream (svref (ordinal-tens) tens)))
              ((plusp bot)
               (stream-write-entire-string stream (svref (cardinal-tens) tens))
               (write-char #\- stream)
               (stream-write-entire-string stream (svref (ordinal-ones) ones)))
              ((plusp number) (write-string "th" stream :start  0 :end 2))
              (t (stream-write-entire-string stream "zeroth")))))))


;;; Print Roman numerals

(defun format-print-old-roman (stream n)
  (unless (< 0 n 5000)
          (format-error "Number out of range for old Roman numerals: ~:D" n))
  (do ((char-list '(#\D #\C #\L #\X #\V #\I) (cdr char-list))
       (val-list '(500 100 50 10 5 1) (cdr val-list))
       (cur-char #\M (car char-list))
       (cur-val 1000 (car val-list))
       (start n (do ((i start (progn (write-char cur-char stream) (- i cur-val))))
                    ((< i cur-val) i))))
      ((zerop start))))


(defun format-print-roman (stream n)
  (unless (< 0 n 4000)
          (format-error "Number out of range for Roman numerals: ~:D" n))
  (do ((char-list '(#\D #\C #\L #\X #\V #\I) (cdr char-list))
       (val-list '(500 100 50 10 5 1) (cdr val-list))
       (sub-chars '(#\C #\X #\X #\I #\I) (cdr sub-chars))
       (sub-val '(100 10 10 1 1 0) (cdr sub-val))
       (cur-char #\M (car char-list))
       (cur-val 1000 (car val-list))
       (cur-sub-char #\C (car sub-chars))
       (cur-sub-val 100 (car sub-val))
       (start n (do ((i start (progn (write-char cur-char stream) (- i cur-val))))
                    ((< i cur-val)
                     (cond ((<= (- cur-val cur-sub-val) i)
                            (write-char cur-sub-char stream)
                            (write-char cur-char stream)
                            (- i (- cur-val cur-sub-val)))
                           (t i))))))
      ((zerop start))))


;;; Decimal  ~D

(defformat #\D format-print-decimal (stream colon atsign &rest parms)
  (declare (dynamic-extent parms))
  (format-print-number stream (pop-format-arg) 10 colon atsign parms))


;;; Binary  ~B

(defformat #\B format-print-binary (stream colon atsign &rest parms)
  (declare (dynamic-extent parms))
  (format-print-number stream (pop-format-arg) 2 colon atsign parms))


;;; Octal  ~O

(defformat #\O format-print-octal (stream colon atsign &rest parms)
  (declare (dynamic-extent parms))
  (format-print-number stream (pop-format-arg) 8 colon atsign parms))


;;; Hexadecimal  ~X

(defformat #\X format-print-hexadecimal (stream colon atsign &rest parms)
  (declare (dynamic-extent parms))
  (format-print-number stream (pop-format-arg) 16 colon atsign parms))


;;; Radix  ~R

(defformat #\R format-print-radix (stream colon atsign &rest parms)
  (declare (dynamic-extent parms))
  (let ((number (pop-format-arg))
        (parm (if parms (pop parms) nil)))
    (if parm
        (format-print-number stream number parm colon atsign parms)
        (if atsign
            (if colon
                (format-print-old-roman stream number)
                (format-print-roman stream number))
            (if colon
                (format-print-ordinal stream number)
                (format-print-cardinal stream number))))))

;;; FLOATING-POINT NUMBERS


;;; Fixed-format floating point  ~F

(defformat #\F format-fixed (stream colon atsign &rest parms)
  (declare (dynamic-extent parms))
  (when colon
    (format-error "Colon flag not allowed"))
  (with-format-parameters parms ((w nil) (d nil) (k nil) (ovf nil) (pad #\space))
    ;;Note that the scale factor k defaults to nil.  This is interpreted as
    ;;zero by flonum-to-string, but more efficiently.
    (let ((number (pop-format-arg))(*print-escape* nil))
      (if (floatp number)
        (format-fixed-aux stream number w d k ovf pad atsign)
        (if (rationalp number)
          (format-fixed-aux stream (coerce number 'float) w d k ovf pad atsign)
          (let ((*print-base* 10))
            (format-write-field stream (princ-to-string number) w 1 0 #\space t)))))))

; do something ad hoc if d > w - happens if (format nil "~15g" (- 2.3 .1))
; called with w = 11 d = 16 - dont do it after all.

(defun format-fixed-aux (stream number w d k ovf pad atsign)
  (and w (<= w 0) (setq w nil))  ; if width is unreasonable, ignore it.
  (if (and (not k)
	   (not (or w d)))
    (print-float-free-form number stream)
    (let ((spaceleft w)
          (abs-number (abs number))
          strlen zsuppress flonum-to-string-width)
      (when (and w (or atsign (minusp number)))
        (decf spaceleft))
      (when (and d w (<= w (+ 1 d (if atsign 1 0))))
        (setq zsuppress t))
      (when (and d (minusp d))
          (format-error "Illegal value for d"))
      (setq flonum-to-string-width
            (and w
                 (if (and (< abs-number 1) (not zsuppress))
                   (1- spaceleft)   ; room for leading 0
                   spaceleft)))
      (when (and w (not (plusp flonum-to-string-width)))
        (if ovf 
          (progn
            (dotimes (i w) (write-char ovf stream))
            (return-from format-fixed-aux))
          (setq spaceleft nil w nil flonum-to-string-width nil)))
      (multiple-value-bind (str before-pt after-pt)
                           (flonum-to-string abs-number
                                             flonum-to-string-width
                                             d k)
        (setq strlen (length str))
        (cond (w (decf spaceleft (+ (max before-pt 0) 1))
                 (when (and (< before-pt 1) (not zsuppress))
                   (decf spaceleft))
                 (if d
                   (decf spaceleft d)
                   (setq d (max (min spaceleft (- after-pt))
                                (if (> spaceleft 0) 1 0))
                         spaceleft (- spaceleft d))))
              ((null d) (setq d (max (- after-pt) 1))))
        (cond ((and w (< spaceleft 0) ovf)
               ;;field width overflow
               (dotimes (i w) (declare (fixnum i)) (write-char ovf stream)))
              (t (when w (dotimes (i spaceleft) (declare (fixnum i)) (write-char pad stream)))
                 (if (minusp (float-sign number)) ; 5/25
                   (write-char #\- stream)
                   (if atsign (write-char #\+ stream)))
                 (cond
                  ((> before-pt 0)
                   (cond ((> strlen before-pt)
                          (write-string str stream :start  0 :end before-pt)
                          (write-char #\. stream)
                          (write-string str stream :start  before-pt :end strlen)
                          (dotimes (i (- d (- strlen before-pt)))
                            (write-char #\0 stream)))
                         (t ; 0's after
                          (stream-write-entire-string stream str)
                          (dotimes (i (-  before-pt strlen))
                            (write-char #\0 stream))
                          (write-char #\. stream)
                          (dotimes (i d)
                            (write-char #\0 stream)))))
                  (t (unless zsuppress (write-char #\0 stream))
                     (write-char #\. stream)
                     (dotimes (i (- before-pt))	 
                       (write-char #\0 stream))
                     (stream-write-entire-string stream str)
                     (dotimes (i (+ d after-pt)) 
                      (write-char #\0 stream))))))))))
#|
; (format t "~7,3,-2f" 8.88)
; (format t "~10,5,2f" 8.88)
; (format t "~10,5,-2f" 8.88)
; (format t "~10,5,2f" 0.0)
; (format t "~10,5,2f" 9.999999999)
; (format t "~7,,,-2e" 8.88) s.b. .009e+3 ??
; (format t "~10,,2f" 8.88)
; (format t "~10,,-2f" 8.88)
; (format t "~10,,2f" 0.0)
; (format t "~10,,2f" 0.123454)
; (format t "~10,,2f" 9.9999999)
 (defun foo (x)
    (format nil "~6,2f|~6,2,1,'*f|~6,2,,'?f|~6f|~,2f|~F"
     x x x x x x))

|#

                  

;;; Exponential-format floating point  ~E


(defformat #\E format-exponential (stream colon atsign &rest parms)
  (declare (dynamic-extent parms))
  (when colon
    (format-error "Colon flag not allowed"))
  (with-format-parameters parms ((w nil) (d nil) (e nil) (k 1) (ovf nil) (pad #\space) (marker nil))
    (let ((number (pop-format-arg)))
      (if (floatp number)
        (format-exp-aux stream number w d e k ovf pad marker atsign)
        (if (rationalp number)
          (format-exp-aux stream (coerce number 'float) w d e k ovf pad marker atsign)
          (let ((*print-base* 10))
            (format-write-field stream (princ-to-string number) w 1 0 #\space t)))))))
#|
(defun format-exponent-marker (number)
  (if (typep number *read-default-float-format*)
      #\E
      (cond ((double-floatp) #\D)
            ((short-floatp number) #\S)
            ((single-floatp number) #\F)
            ((long-floatp) #\L))))
|#
(eval-when (eval compile #-bccl load)
  (defmacro format-exponent-marker (number)
    `(float-exponent-char ,number))
)

;;;Here we prevent the scale factor from shifting all significance out of
;;;a number to the right.  We allow insignificant zeroes to be shifted in
;;;to the left right, athough it is an error to specify k and d such that this
;;;occurs.  Perhaps we should detect both these condtions and flag them as
;;;errors.  As for now, we let the user get away with it, and merely guarantee
;;;that at least one significant digit will appear.
;;; THE ABOVE COMMENT no longer applies

(defun format-exp-aux (stream number w d e k ovf pad marker atsign &optional string exp)
  (when (not k) (setq k 1))
  (if (not (or w d e marker (neq k 1)))
    (print-a-float number stream t)
    (prog () 
      (when d
        (when (or (minusp d)
                  (and (plusp k)(>= k (+ d 2)))
                  (and (minusp k)(< k (- d))))
          (format-error "incompatible values for k and d")))
      (when (not exp) (setq exp (accurate-scale-exponent (abs number))))
      AGAIN
      (let* ((expt (- exp (if (and d (not w)) 1 k)))
             (estr (let ((*print-base* 10))
                     (princ-to-string (abs expt))))
             (elen (max (length estr) (or e 0)))
             (spaceleft (if w (- w 2 elen) nil))
             (fwidth) scale)
        (when (and w (or atsign (minusp (float-sign number)))) ; 5/25
          (setq spaceleft (1- spaceleft)))
        (if w
          (progn 
          (setq fwidth (if d 
                         (if (> k 0)(+ d 2)(+ d k 2))
                         (if (> k 0) spaceleft (+ spaceleft k))))
          (when (minusp exp) ; i don't claim to understand this
            (setq fwidth (- fwidth exp))
            (when (< k 0) (setq fwidth (1- fwidth)))))          
          (when (and d  (not (zerop number))) ; d and no w
            (setq scale (- 2  k exp))))  ; 2 used to be 1  - 5/31
        (when (or (and w e ovf (> elen e))(and w fwidth (not (plusp fwidth))))
          ;;exponent overflow
          (dotimes (i w) (declare (fixnum i)) (write-char ovf stream))
          (if (plusp fwidth)
            (return-from format-exp-aux nil)
            (setq fwidth nil)))
        (when (not string)
          (multiple-value-bind (new-string before-pt) (flonum-to-string number fwidth 
                                                                        (if (not fwidth) d)
                                                                        (if (not fwidth) scale))
            (setq string new-string)
            (when scale (setq before-pt (- (+ 1 before-pt) k scale))) ; sign right?            
            (when (neq exp before-pt)
              ;(print (list 'agn exp before-pt))
              ;(setq string new-string)
              (setq exp before-pt)
              (go again))))
          (let ((strlen (length string)))
            (when w
              (if d 
                (setq spaceleft (- spaceleft (+ d 2)))
                (if (< k 1)
                  (setq spaceleft (- spaceleft (+ 2 (- k)(max strlen 1))))
                  (setq spaceleft (- spaceleft (+ 1 k (max 1 (- strlen k))))))))
            (when (and w (< spaceleft 0))
              (if (and ovf (or (plusp k)(< spaceleft -1)))            
                (progn (dotimes (i w) (declare (fixnum i)) (write-char ovf stream))
                       (return-from format-exp-aux nil))))
            (when w
              (dotimes (i  spaceleft)
                (declare (fixnum i))
                (write-char pad stream)))
            (if (minusp (float-sign number)) ; 5/25
              (write-char #\- stream)
              (if atsign (write-char #\+ stream)))
            (cond 
             ((< k 1)
              (when (or (null spaceleft) (not (minusp spaceleft)))
                (write-char #\0 stream))
              (write-char #\. stream)
              (dotimes (i (- k))
                (write-char #\0 stream))
              (if (and (eq strlen 0)(not d))
                (write-char #\0 stream)
                (stream-write-entire-string stream string))
              (if d
                (dotimes (i (- (+ d k) strlen))
                  (write-char #\0 stream))))
             (t 
              (write-string string stream :start 0 :end (min k strlen))
              (dotimes (i (- k strlen))
                (write-char #\0 stream))                    
              (write-char #\. stream)
              (when (> strlen k)
                (write-string string stream :start k :end strlen))
              (if (not d) 
                (when (<= strlen k)(write-char #\0 stream))
                (dotimes (i (1+ (- d k (max 0 (- strlen k)))))
                  (write-char #\0 stream)))))
            (write-char (if marker
                          marker
                          (format-exponent-marker number))
                        stream)
            (write-char (if (minusp expt) #\- #\+) stream)
            (when e 
              ;;zero-fill before exponent if necessary
              (dotimes (i (- e (length estr)))
                (declare (fixnum i))
                (write-char #\0 stream)))
            (stream-write-entire-string stream estr))))))
#|
; (format t "~7,3,,-2e" 8.88) s.b. .009e+3 
; (format t "~10,5,,2e" 8.888888888) ; "88.8889E-1"
; (format t "~10,5,,-2e" 8.88)   "0.00888E+3"
; (format t "~10,5,,-2e" .00123445) ; "0.00123E+0"
; (format t "~10,5,,-3e" .00123445) ; "0.00012E+1"
; (format t "~10,,,-2e" .123445)
; (format t "~10,5,,2e" .0012349999e-4)
; (format t "~10,5,,2e" 9.9999999)
; (format t "~10,5,,2e" 0.0)
; (format t "~10,5,,0e" 40000000.0)
; (format t "~10,5,,2e" 9.9999999)
; (format t "~7,,,-2e" 8.88) s.b. .009e+3 ??
; (format t "~10,,,2e" 8.888888)
; (format t "~10,,,-2e" 8.88)
; (format t "~10,,,-2e" 0.0)
; (format t "~10,,,2e" 0.0) 
; (format t "~10,,,2e" 9.9999999)
; (format t "~10,,,2e" 9.9999999e100)
; (format t "~10,5,3,2,'xe" 10e100)
; (format t "~9,3,2,-2e" 1100.0)
(defun foo (x)
  (format nil
          "~9,2,1,,'*e|~10,3,2,2,'?,,'$e|~9,3,2,-2,'%@e|~9,2e"
          x x x x))
|#


;;; General Floating Point -  ~G

(defformat #\G format-general-float (stream colon atsign &rest parms)
  (declare (dynamic-extent parms))
  (when colon
    (format-error "Colon flag not allowed"))
  (with-format-parameters parms ((w nil) (d nil) (e nil) (k nil) (ovf nil) (pad #\space) (marker nil))
    (let ((number (pop-format-arg)))
      ;;The Excelsior edition does not say what to do if
      ;;the argument is not a float.  Here, we adopt the
      ;;conventions used by ~F and ~E.
      (if (floatp number)
        (format-general-aux stream number w d e k ovf pad marker atsign)
        (if (rationalp number)
          (format-general-aux stream (coerce number 'float) w d e k ovf pad marker atsign)
          (let ((*print-base* 10))
            (format-write-field stream (princ-to-string number) w 1 0 #\space t)))))))

#|
; completely broken
(defun foo (x)
  (format nil
          "~9,2,1,,'*g|~10,3,2,2,'?,,'$g|~9,3,2,-2,'%@g|~9,2g"
          x x x x))
|#


(defun format-general-aux (stream number w d e k ovf pad marker atsign)
  (multiple-value-bind (str n #|after-pt|#)(flonum-to-string number)
    ;;Default d if omitted.  The procedure is taken directly
    ;;from the definition given in the manual, and is not
    ;;very efficient, since we generate the digits twice.
    ;;Future maintainers are encouraged to improve on this.
    (let* ((d2 (or d (max (length str) (min n 7))))
           (ee (if e (+ e 2) 4))
           (ww (if w (- w ee) nil))
           (dd (- d2 n)))
      (cond ((<= 0 dd d2)
             ; this causes us to print 1.0 as 1. - seems weird
             (format-fixed-aux stream number ww dd nil ovf pad atsign)
             (dotimes (i ee) (declare (fixnum i)) (write-char #\space stream)))
            (t (format-exp-aux stream number w d e (or k 1) ovf pad marker atsign nil n))))))


;;; Dollars floating-point format  ~$

(defformat #\$ format-dollars (stream colon atsign &rest parms)
  (declare (dynamic-extent parms))
  (with-format-parameters parms ((d 2) (n 1) (w 0) (pad #\space))
    (let* ((number (float (pop-format-arg)))
           (signstr (if (minusp (float-sign number)) "-" (if atsign "+" "")))
           (spaceleft)
           strlen)
      (multiple-value-bind (str before-pt after-pt) (flonum-to-string number nil d)
        (setq strlen (length str))
        (setq spaceleft (- w (+ (length signstr) (max before-pt n) 1 d)))
        (when colon (stream-write-entire-string stream signstr))
        (dotimes (i spaceleft) (write-char pad stream))
        (unless colon (stream-write-entire-string stream signstr))
        (cond
         ((> before-pt 0)
          (dotimes (i (- n before-pt))
            (write-char #\0 stream))
          (cond ((> strlen before-pt)
                 (write-string str stream :start 0 :end before-pt)
                 (write-char #\. stream)
                 (write-string str stream :start before-pt :end strlen)
                 (dotimes (i (- d (- strlen before-pt)))
                   (write-char #\0 stream)))
                (t ; 0's after
                 (stream-write-entire-string stream str)
                 (dotimes (i (-  before-pt strlen))
                   (write-char #\0 stream))
                 (write-char #\. stream)
                 (dotimes (i d)
                   (write-char #\0 stream)))))
         (t (dotimes (i n)
              (write-char #\0 stream))
            (write-char #\. stream)
            (dotimes (i (- before-pt))
              (write-char #\0 stream))
            (stream-write-entire-string stream str)
            (dotimes (i (+ d after-pt))
              (write-char #\0 stream))))))))

(defun y-or-n-p (&optional format-string &rest arguments &aux response)
  "Y-OR-N-P prints the message, if any, and reads characters from
   *QUERY-IO* until the user enters y or Y as an affirmative, or either
   n or N as a negative answer. It asks again if you enter any other
   characters."
  (declare (dynamic-extent arguments))
  (with-terminal-input
      (clear-input *query-io*)
      (loop
        (when format-string
          (fresh-line *query-io*)
          (apply 'format *query-io* format-string arguments))
        (princ " (y or n)  " *query-io*)
	(setq response (read-char *query-io*))
        ;; Consume input up to trailing newline
        (when (peek-char #\NewLine *query-io* nil)
          ;; And consume the #\newline
          (read-char *query-io*))
        (clear-input *query-io*)
	(if (char-equal response #\y) (return t))
	(if (char-equal response #\n) (return nil))
	(format *query-io* "Please answer y or n."))))

(defun yes-or-no-p (&optional format-string &rest arguments &aux response)
  "YES-OR-NO-P is similar to Y-OR-N-P, except that it clears the
   input buffer, beeps, and uses READ-LINE to get the strings
   YES or NO."
  (declare (dynamic-extent arguments))
  (with-terminal-input
      (loop
        (when format-string
          (fresh-line *query-io*)
          (apply 'format *query-io* format-string arguments))
        (princ " (yes or no)  " *query-io*)
        (format *query-io* "~A" #\Bell)
        (setq response (read-line *query-io*))
        (clear-input *query-io*)
	(when response
	  (setq response (string-trim wsp response))
	  (if (string-equal response "yes") (return t))
	  (if (string-equal response "no") (return nil))
          (format *query-io* "Please answer yes or no.")))))


;; Compile-time format-scanning support.
;;
;; All this assumes it's called from the compiler, but it has to be kept in sync with code
;; here more than with the code in the compiler, so keep it in here.

(defun note-format-scan-option (cell)
  (when cell
    (if (null (cdr cell))
      (setf (car cell) *format-arguments* (cdr cell) *format-arguments-variance*)
      (let* ((new-args *format-arguments*)
             (new-var *format-arguments-variance*)
             (new-max (length new-args))
             (old-args (car cell))
             (old-var (cdr cell))
             (old-max (length old-args))
             (min (min (- new-max new-var) (- old-max old-var))))
        (if (>= new-max old-max)
          (setf (car cell) new-args (cdr cell) (- new-max min))
          (setf (cdr cell) (- old-max min))))))
  cell)

(defmacro with-format-scan-options ((var) &body body)
  (let ((cell (gensym)))
    ;; CELL is used to record range of arg variations that should be deferred til the end
    ;; of BODY because they represent possible non-local exits.
    `(let* ((,cell (cons nil nil))
            (,var ,cell))
       (declare (dynamic-extent ,cell))
       (prog1
           (progn
             ,@body)
         (setq *format-arguments* (car ,cell)
               *format-arguments-variance* (cdr ,cell))))))

(defvar *format-escape-options* nil)

(defun nx1-check-format-call (control-string format-arguments &optional (env *nx-lexical-environment*))
  "Format-arguments are expressions that will evaluate to the actual arguments.
  Pre-scan process the format string, nx1-whine if find errors"
  (let* ((*nx-lexical-environment* env)
         (*format-top-level* t)
         (*logical-block-xp* nil)
         (*format-pprint* nil)
         (*format-justification-semi* nil))
    (let ((error (catch 'format-error
		   (format-scan control-string format-arguments 0)
                   nil)))
      (when error
	(setf (cadar error) (concatenate 'string (cadar error) " in format string:"))
	(nx1-whine :format-error (nreverse error))
	t))))

(defun format-scan (string args var)
  (let ((*format-original-arguments* args)
	(*format-arguments* args)
	(*format-arguments-variance* var)
	(*format-colon-rest* 'error)
	(*format-control-string* (ensure-simple-string string)))
    (with-format-scan-options (*format-escape-options*)
      (catch 'format-escape
	(sub-format-scan 0 (length *format-control-string*))
	(note-format-scan-option *format-escape-options*)))
    (when (> (length *format-arguments*) *format-arguments-variance*)
      (format-error "Too many format arguments"))))

(defun sub-format-scan (i end)
  (let ((*format-index* i)
        (*format-length* end)
        (string *format-control-string*))
    (loop while (setq *format-index* (position #\~ string :start *format-index* :end end)) do
      (multiple-value-bind (params colon atsign char) (parse-format-operation t)
	(setq char (char-upcase char))
	(let ((code (%char-code char)))
	  (unless (and (< -1 code (length *format-char-table*))
		       (svref *format-char-table* code))
	    (format-error "Unknown directive ~c" char)))
        (format-scan-directive char colon atsign params)
        (incf *format-index*)))))

(defun nx-could-be-type (form type &optional transformed &aux (env *nx-lexical-environment*))
  (unless transformed (setq form (nx-transform form env)))
  (if (nx-form-constant-p form env)
    (typep (nx-form-constant-value form env) type env)
    (not (types-disjoint-p (nx-form-type form env) type env))))

(defun format-require-type (form type &optional description)
  (unless (nx-could-be-type form type)
    (format-error "~a must be of type ~s" (or description form) type)))


(defun format-scan-directive (char colon atsign parms)
  (ecase char
    ((#\% #\& #\~ #\|)
     (with-format-parameters parms ((repeat-count 1))
       (format-no-flags colon atsign)
       (format-require-type repeat-count '(integer 0))))
    ((#\newline #\return)
     (with-format-parameters parms ()
       (when (and atsign colon) (format-error "~:@<newline> is undefined"))
       (unless colon
	 (format-eat-whitespace))))
    ((#\P)
     (with-format-parameters parms ()
       (when colon
	 (loop with end = *format-arguments*
	    for list on *format-original-arguments*
	    when (eq (cdr list) end) return (setq *format-arguments* list)
	    finally (if (> (or *format-arguments-variance* 0) 0)
			(decf *format-arguments-variance*)
			(format-error "No previous argument"))))
       (pop-format-arg)))
    ((#\A #\S)
     (with-format-parameters parms ((mincol 0) (colinc 1) (minpad 0) (padchar #\space))
       (format-require-type mincol 'integer "mincol (first parameter)")
       (format-require-type colinc '(integer 1) "colinc (second parameter)")
       (format-require-type minpad 'integer "minpad (third parameter)")
       (format-require-type padchar '(or (integer 0 #.char-code-limit) character) "padchar (fourth parameter)"))
     (pop-format-arg))
    ((#\I)
     (with-format-parameters parms ((n 0))
       (format-no-flags nil atsign)
       (format-no-semi char)
       (format-require-type n 'real)))
    ((#\_)
     (with-format-parameters parms ()
       (format-no-semi char)))
    ((#\T)
     (with-format-parameters parms ((colnum 1) (colinc 1))
       (when colon
	 (format-no-semi char t))
       (format-require-type colnum 'integer "colnum (first parameter)")
       (format-require-type colinc 'integer "colinc (second parameter)")))
    ((#\W)
     (with-format-parameters parms ()
       (format-no-semi #\W))
     (pop-format-arg))
    ((#\C)
     (with-format-parameters parms ())
     (format-require-type (pop-format-arg) '(or character fixnum (string 1))))
    ((#\D #\B #\O #\X #\R)
     (when (eql char #\R)
       (let ((radix (pop parms)))
	 (when radix
	   (format-require-type radix '(integer 2 36)))))
     (with-format-parameters parms ((mincol 0) (padchar #\space) (commachar #\,) (commainterval 3))
       (format-require-type mincol 'integer "mincol (first parameter)")
       (format-require-type padchar 'character "padchar (second parameter)")
       (format-require-type commachar 'character "comma char (third parameter)")
       (format-require-type commainterval 'integer "comma interval (fourth parameter)"))
     (pop-format-arg))
    ((#\F)
     (format-no-flags colon nil)
     (with-format-parameters parms ((w nil) (d nil) (k nil) (ovf nil) (pad #\space))
       (format-require-type w '(or null (integer 0)) "w (first parameter)")
       (format-require-type d '(or null (integer 0)) "d (second parameter)")
       (format-require-type k '(or null integer) "k (third parameter)")
       (format-require-type ovf '(or null character) "overflowchar (fourth parameter)")
       (format-require-type pad '(or null character) "padchar (fifth parameter)"))
     (pop-format-arg))
    ((#\E #\G)
     (format-no-flags colon nil)
     (with-format-parameters parms ((w nil) (d nil) (e nil) (k 1) (ovf nil) (pad #\space) (marker nil))
       (format-require-type w '(or null (integer 0)) "w (first parameter)")
       (format-require-type d '(or null (integer 0)) "d (second parameter)")
       (format-require-type e '(or null (integer 0)) "e (third parameter)")
       (format-require-type k '(or null integer) "k (fourth parameter)")
       (format-require-type ovf '(or null character) "overflowchar (fifth parameter)")
       (format-require-type pad '(or null character) "padchar (sixth parameter)")
       (format-require-type marker '(or null character) "exponentchar (seventh parameter)"))
     (pop-format-arg))
    ((#\$)
     (with-format-parameters parms ((d 2) (n 1) (w 0) (pad #\space))
       (format-require-type d '(or null (integer 0)) "d (first parameter)")
       (format-require-type n '(or null (integer 0)) "n (second parameter)")
       (format-require-type w '(or null (integer 0)) "w (third parameter)")
       (format-require-type pad '(or null character) "pad (fourth parameter)"))
     (format-require-type (pop-format-arg) 'real))
    ((#\*)
     (with-format-parameters parms ((count nil))
       (when count
	 (format-require-type count 'integer "count parameter"))
       (if (typep (setq count (nx-transform count)) '(or null integer))
	 (format-scan-goto colon atsign count)
	 ;; Else can't tell how much going back or forth, could be anywhere.
	 (setq *format-arguments* *format-original-arguments*
	       *format-arguments-variance* (length *format-arguments*)))))
    ((#\?)
     (with-format-parameters parms ()
       (format-no-flags colon nil))
     (let ((string (pop-format-arg)))
       (format-require-type string '(or string function))
       (if atsign
	 (setq *format-arguments-variance* (length *format-arguments*))
	 (let ((arg (pop-format-arg)))
	   (format-require-type arg 'list)))))
    ((#\/)
     (let* ((string *format-control-string*)
	    (ipos (1+ *format-index*))
	    (epos (format-find-char #\/ ipos *format-length*)))
       (when (not epos) (format-error "Unmatched ~~/"))
       (let* ((cpos (format-find-char #\: ipos epos))
	      (name (if cpos
		      (prog1
			  (string-upcase (%substr string ipos cpos))
			(when (eql #\: (schar string (%i+ 1 cpos)))
			  (setq cpos (%i+ cpos 1)))
			(setq ipos (%i+ cpos 1)))
		      "CL-USER"))
	      (package (find-package name))
	      (sym (and package (find-symbol (string-upcase (%substr string ipos epos)) package)))
	      (arg (pop-format-arg)))
	 (setq *format-index* epos) ; or 1+ epos?
	 ;; TODO: should we complain if the symbol doesn't exit?  Perhaps it will be defined
	 ;; later, and to detect that would need to intern it.  What if the package doesn't exist?
	 ;; Would need to extend :undefined-function warnings to handle previously-undefined package.
	 (when sym
	   (when (nx1-check-typed-call sym (list* '*standard-output* arg colon atsign parms))
	     ;; Whined, just get out now.
	     (throw 'format-error nil))))))
    ((#\[)
     (when (and colon atsign) (format-error  "~~:@[ undefined"))
     (format-nextchar)
     (cond (colon
	    (format-scan-boolean-condition parms))
	   (atsign
	    (format-scan-funny-condition parms))
	   (t (format-scan-untagged-condition parms))))
    ((#\()
     (with-format-parameters parms ()
       (format-nextchar)
       (multiple-value-bind (prev tilde parms colon atsign) (format-find-command '(#\)))
	 (with-format-parameters parms () (format-no-flags colon atsign))
	 (sub-format-scan prev tilde))))
    ((#\^)
     (format-no-flags nil atsign)
     (with-format-parameters parms ((p1 nil) (p2 nil) (p3 nil))
       (let ((val (nx-transform (cond (p3
				       (if (every (lambda (p) (nx-could-be-type p 'real)) parms)
					 ;; If the params could also be chars, don't know enough to constant fold
					 ;; anyway, so this test will do.
					 `(< ,p1 ,p2 ,p3)
					 (if (every (lambda (p) (nx-could-be-type p 'character)) parms)
					   `(char< ,p1 ,p2 ,p3)
					   ;; At least one can't be real, at least one can't be char.
					   (format-error "Wrong type of parameters for three-way comparison"))))
				      (p2 `(equal ,p1 ,p2))
				      (p1 `(eq ,p1 0))
				      (t (null (if colon *format-colon-rest* *format-arguments*)))))))
	 (when val
	   (note-format-scan-option *format-escape-options*)
	   (unless (nx-could-be-type val 'null t)
	     (throw 'format-escape t))))))
    ((#\{)
     (with-format-parameters parms ((max-iter -1))
       (format-require-type max-iter 'integer "max-iter parameter")
       (format-nextchar)
       (multiple-value-bind (prev tilde end-parms end-colon end-atsign) (format-find-command '(#\}))
	 (declare (ignore end-colon))
	 (with-format-parameters end-parms () (format-no-flags nil end-atsign))
	 (when (= prev tilde)
	   ;; Use an argument as the control string if ~{~} is empty
	   (let ((string (pop-format-arg)))
	     (unless (nx-could-be-type string '(or string function))
	       (format-error "Control string is not a string or function"))))
	 ;; Could try to actually scan the iteration if string is a compile-time string,
	 ;; by that seems unlikely.
	 (if atsign
	   (setq *format-arguments-variance* (length *format-arguments*))
	   (format-require-type (pop-format-arg) 'list)))))
    ((#\<)
     (multiple-value-bind (start tilde eparms ecolon eatsign) (format-find-command '(#\>))
       (declare (ignore tilde eparms eatsign))
       (setq *format-index* start)
       (if ecolon
	 (format-logical-block-scan colon atsign parms)
	 (format-justification-scan colon atsign parms))))
    ))

(defun format-justification-scan (colon atsign parms)
  (declare (ignore colon atsign))
  (with-format-parameters parms ((mincol 0) (colinc 1) (minpad 0) (padchar #\space))
    (format-require-type mincol 'integer "mincol (first parameter)")
    (format-require-type colinc '(integer 1) "colinc (second parameter)")
    (format-require-type minpad 'integer "minpad (third parameter)")
    (format-require-type padchar `(or character (integer 0 #.char-code-limit)) "padchar (fourth parameter)"))
  (let ((first-parms nil) (first-colon nil) (count 0))
    (with-format-scan-options (*format-escape-options*)
      (loop
	 (format-nextchar)
	 (multiple-value-bind (prev tilde parms colon atsign cmd)
	     (format-find-command '(#\; #\>) nil T)
	   (if (and (eql count 0) (eql cmd #\;) colon)
	     (progn
	       (format-no-flags nil atsign)
	       (setq first-colon t)
	       (setq *format-index* tilde)
	       (setq first-parms (nth-value 2 (format-find-command '(#\; #\>) t T))))
	     (with-format-parameters parms ()
	       (format-no-flags colon atsign)))
	   (when (catch 'format-escape
		   (sub-format-scan prev tilde)
		   nil)
	     (unless (eq cmd #\>) (format-find-command '(#\>) nil t))
	     (return))
	   (incf count)
	   (when (eq cmd #\>)
	     (return))))
      (note-format-scan-option *format-escape-options*))
    (when first-colon
      (when *format-pprint*
	(format-error "Justification illegal in this context"))
      (setq *format-justification-semi* t)
      (with-format-parameters first-parms ((spare 0) (linel 0))
	(format-require-type spare 'integer "spare (first parameter)")
	(format-require-type linel 'integer "line length (second parameter)")))))
      


(defun format-logical-block-scan (colon atsign params)
  (declare (ignore colon))
  (with-format-parameters params ()
    (format-no-semi #\<))
    ;; First section can be termined by ~@;
  (let ((format-string *format-control-string*)
	(prefix "")
	(suffix "")
	(body-string nil))
    (multiple-value-bind (start1 tilde parms1 colon1 atsign1 cmd) (format-find-command  '(#\; #\>))
      (setq body-string (%substr format-string (1+ start1) tilde))
      (with-format-parameters parms1 ())
      (when (eq cmd #\;)
	(format-no-flags colon1 nil)
	(setq prefix body-string)
	(multiple-value-setq (start1 tilde parms1 colon1 atsign1 cmd) (format-find-command '(#\; #\>)))
	(with-format-parameters parms1 ())
	(setq body-string (%substr format-string (1+ start1) tilde))
	(when (eq cmd #\;)
	  (format-no-flags colon1 atsign1)
	  (multiple-value-setq (start1 tilde parms1 colon1 atsign1 cmd) (format-find-command  '(#\; #\>)))
	  (with-format-parameters parms1 ())
	  (setq suffix (%substr format-string (1+ start1) tilde))
	  (when (eq cmd #\;)
	    (format-error "Too many sections")))))
    (flet ((format-check-simple (str where)
	     (when (and str (or (%str-member #\~ str) (%str-member #\newline str)))
	       (format-error "~A must be simple" where))))
      (format-check-simple prefix "Prefix")
      (format-check-simple suffix "Suffix"))
    (if atsign
      (let ((*logical-block-p* t))
	(format-scan body-string *format-arguments* *format-arguments-variance*)
	(setq *format-arguments* nil *format-arguments-variance* 0))
      ;; If no atsign, we just use up an arg.  Don't bother trying to scan it, unlikely to be a constant.
      (when *format-arguments*
	(pop-format-arg)))))


(defun format-scan-untagged-condition (parms)
  (with-format-parameters parms ((index nil))
    (unless index (setq index (pop-format-arg)))
    (format-require-type index 'integer)
    (with-format-scan-options (cond-options)
      (loop with default = nil do
	   (multiple-value-bind (prev tilde parms colon atsign cmd)
	       (format-find-command '(#\; #\]))
	     (when (and default (eq cmd #\;))
	       (format-error "~:; must be the last clause"))
	     (with-format-parameters parms ()
	       (format-no-flags (if (eq cmd #\]) colon) atsign)
	       (when colon (setq default t)))
	     (format-scan-optional-clause prev tilde cond-options)
	     (when (eq cmd #\])
	       (unless default 	  ;; Could just skip the whole thing
		 (note-format-scan-option cond-options))
	       (return))
	     (format-nextchar))))))

(defun format-scan-funny-condition (parms)
  (with-format-parameters parms ())
  (multiple-value-bind (prev tilde parms colon atsign) (format-find-command '(#\]))
    (with-format-parameters parms ()
      (format-no-flags colon atsign))
    (when (null *format-arguments*) (pop-format-arg)) ;; invoke std error
    (with-format-scan-options (cond-options)
      (let ((arg (nx-transform (car *format-arguments*))))
	(when (nx-could-be-type arg 'null t)
	  (let ((*format-arguments* *format-arguments*)
		(*format-arguments-variance* *format-arguments-variance*))
	    (when (eql *format-arguments-variance* (length *format-arguments*))
	      (decf *format-arguments-variance*))
	    (pop *format-arguments*)
	    (note-format-scan-option cond-options)))
	(when arg
	  (format-scan-optional-clause prev tilde cond-options))))))


(defun format-scan-boolean-condition (parms)
  (with-format-parameters parms ())
  (multiple-value-bind (prev tilde parms colon atsign cmd) (format-find-command '(#\; #\]))
    (when (eq cmd #\])
      (format-error "Two clauses separated by ~~; are required for ~~:["))
    (with-format-parameters parms () (format-no-flags colon atsign))
    (format-nextchar)
    (with-format-scan-options (cond-options)
      (let ((arg (nx-transform (pop-format-arg))))
	(when (nx-could-be-type arg 'null t)
	  (format-scan-optional-clause prev tilde cond-options))
	(multiple-value-bind (prev tilde parms colon atsign) (format-find-command '(#\]))
	  (with-format-parameters parms () (format-no-flags colon atsign))
	  (when arg
	    (format-scan-optional-clause prev tilde cond-options)))))))


(defun format-scan-optional-clause (start end cond-option)
  (let ((*format-arguments* *format-arguments*)
	(*format-arguments-variance* *format-arguments-variance*))
    ;; Let the branch points collect in outer *format-escape-options*, but don't
    ;; throw there because need to consider the other clauses.
    (catch 'format-escape
      (sub-format-scan start end)
      (note-format-scan-option cond-option)
      nil)))

(defun format-scan-goto (colon atsign count)
  (if atsign 
    (let* ((orig *format-original-arguments*)
           (orig-pos (- (length orig) (length *format-arguments*)))
           (new-pos (or count 0)))
      (format-no-flags colon nil)
      ;; After backing up, we may not use up all the arguments we backed over,
      ;; so even though real variance here is 0, increase variance so we don't
      ;; complain.
      (setq *format-arguments-variance* (max 0 (- orig-pos new-pos)))
      (setq *format-arguments* (nthcdr-no-overflow new-pos orig)))
    (progn
      (when (null count)(setq count 1))
      (when colon (setq count (- count)))
      (cond ((> count 0)
	     (when (> count (length *format-arguments*))
	       (format-error "Target position for ~~* out of bounds"))
	     (setq *format-arguments* (nthcdr count *format-arguments*))
	     (when *format-arguments-variance*
	       (setq *format-arguments-variance*
		     (min *format-arguments-variance* (length *format-arguments*)))))
	    ((< count 0)
	     (let* ((orig *format-original-arguments*)
		    (orig-pos (- (length orig) (length *format-arguments*)))
		    (pos (+ orig-pos count))
		    (max-pos (+ pos (or *format-arguments-variance* 0))))
	       (when (< max-pos 0)
		 (format-error "Target position for ~~* out of bounds"))
	       ;; After backing up, we may not use up all the arguments we backed over.
	       ;; Increase the variance allowed to cover those arguments, so we don't
	       ;; complain about not using them.  E.g. (format t "~a ~a ~2:*~a" 1 2) should
	       ;; be ok, (format t "~a ~a ~2:*" 1 2) should warn.
	       (setq max-pos (1- (- max-pos count)))
	       (if (< pos 0)
		 (setq *format-arguments* orig
		       *format-arguments-variance* max-pos)
		 (setq *format-arguments* (nthcdr pos orig)
		       *format-arguments-variance* (- max-pos pos)))))))))
