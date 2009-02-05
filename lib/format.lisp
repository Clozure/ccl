;;; -*- Mode: Lisp; Package: CCL -*-
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


(def-standard-initial-binding *format-stream-stack* nil "A stack of string streams for collecting FORMAT output")

(defvar *format-pprint* nil
  "Has a pprint format directive (~W ~I ~_ ~:T) or logical-block directive been seen?")

(defvar *format-justification-semi* nil
  "Has a ~<...~:;...~> been seen?")

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
  "Might someday want to add proper format error checking for negative 
      parameters"
  (let (neg-parm)
    (when (eq ch #\-)(setq neg-parm ch)
          (setq ch (format-nextchar)))
    (case ch
      (#\# (format-nextchar) (length *format-arguments*))
      ((#\V #\v)
       (prog1 (pop-format-arg) (format-nextchar)))
      (#\' (prog1 (format-nextchar) (format-nextchar)))
      (t (cond ((setq ch (digit-char-p ch))
                (do ((number ch (%i+ ch (%i* number 10))))
                    ((not (setq ch (digit-char-p (format-nextchar))))
                     (if neg-parm (- number) number))))
               (t nil))))))

(defun format-skip-parameter (ch) ; only caller is parse-format-operation
  "Might someday want to add proper format error checking for negative 
      parameters"
  (let ()
    (case ch
      ((#\V #\v #\#)
       (format-nextchar))
      (#\' (format-nextchar) (format-nextchar))
      (#\,)
      (t (cond (T ;(or (eq ch #\-)(digit-char-p ch)) ; t
                (while (digit-char-p (format-nextchar))))
               (t nil))))))


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
    ; allow either order - (also allows :: or @@)
    (case ch
      (#\: (setq colon t))
      (#\@ (setq atsign t)))
    (when (or colon atsign)
      (case (setq ch (format-nextchar))
        (#\: (setq colon t)
         (setq ch (format-nextchar)))
        (#\@ (setq atsign t)
         (setq ch (format-nextchar)))))
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
          ((#\} #\> #\) #\])
           (format-error "No matching bracket")))))))

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
	(when (xp-structure-p stream)(setq stream (xp-stream-stream stream))) ; for xp tests only! They call format on a structure
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
		 (declare (special *format-original-arguments* *format-arguments*
				   *format-control-string* *format-colon-rest*))
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
 (defun pop-format-arg (&aux (args *format-arguments*)(xp *logical-block-xp*))
   (when xp
     (if (pprint-pop-check+ args xp)    ; gets us level and length stuff in logical block
       (throw 'logical-block nil)))           
   (if (and (null args)(null xp))       ; what if its 3?
     (format-error "Missing argument")
     (progn
       (setq *format-arguments* (cdr args))
       (%car args)))))

; SUB-FORMAT is now defined in L1-format.lisp
; DEFFORMAT is also defined there.

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pretty-printing stuff
;;; 

(defformat #\W format-write (stream colon atsign)
  (if *format-justification-semi*
      (format-error "~~W illegal in this context"))
  (setq *format-pprint* t)
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
  (if *format-justification-semi*
      (format-error "~~I illegal in this context"))
  (setq *format-pprint* t)
  (with-format-parameters parms ((n 0))
    (pprint-indent (if colon :current :block) n stream)))

(defformat #\_ format-conditional-newline (stream colon atsign)
  (if *format-justification-semi*
      (format-error "~~_ illegal in this context"))
  (setq *format-pprint* t)
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
      (if *format-justification-semi*
          (format-error "~~:T illegal in this context"))
      (setq *format-pprint* t))
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
             (setq package (find-package (string-upcase (%substr string ipos cpos))))
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
  (multiple-value-bind
   (prev tilde end-parms end-colon end-atsign)
   (format-find-command '(#\)))
   (when (or end-parms end-colon end-atsign)
         (format-error "Flags or parameters not allowed"))
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
  (multiple-value-bind
    (prev tilde end-parms end-colon end-atsign)
    (format-find-command '(#\)))
    (when (or end-parms end-colon end-atsign)
      (format-error "Flags or parameters not allowed"))
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

(defformat #\^ format-escape (stream colon atsign &rest parms)
  (declare (special *format-colon-rest*)) ; worry about this later??
  (declare (ignore stream))
  (declare (dynamic-extent parms))
  (when atsign
    (format-error "FORMAT command ~~~:[~;:~]@^ is undefined" colon))
  (setq parms (remove-if #'null parms))
  (when
    (cond ((null parms)
           (null (if colon *format-colon-rest* *format-arguments*)))
          ((null (cdr parms))
           (let ((p (car parms)))
             (typecase p
               (number     (zerop p))
               (character  (null p))
               (t          nil))))
          ((null (cddr parms))
           (equal (car parms)(cadr parms)))
          (t (let ((first (car parms))(second (cadr parms))(third (caddr parms)))
               (typecase second
                 (integer
                  (<= first second third))
                 (character
                  (char< first second third))
                 (t nil)))))  ; shouldnt this be an error??
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
         (multiple-value-bind (prev tilde parms colon atsign cmd)
                              (format-find-command '(#\; #\]))
           (declare (ignore colon))
           (when (or atsign parms)
             (format-error "Atsign flag or parameters not allowed"))
           (sub-format stream prev tilde)
           (unless (eq cmd #\])
             (format-find-command '(#\])))))
      (multiple-value-bind (prev tilde parms colon atsign cmd)
                           (format-find-command '(#\; #\]))
        (declare (ignore prev tilde))
        (when (or atsign parms)
          (format-error "Atsign flag or parameters not allowed"))
        (when (eq cmd #\]) (return))
        (when colon
          (format-nextchar)
          (multiple-value-bind (prev tilde parms colon atsign cmd)
                               (format-find-command '(#\; #\]))
            (declare (ignore parms colon atsign))
            (sub-format stream prev tilde)
            (unless (eq cmd #\])
              (format-find-command '(#\]))))
          (return))
        (format-nextchar)))))


;;; ~@[

(defun format-funny-condition (stream)
  (multiple-value-bind (prev tilde parms colon atsign) (format-find-command '(#\]))
    (when (or colon atsign parms)
      (format-error "Flags or arguments not allowed"))
    (if *format-arguments*
      (if (car *format-arguments*)
        (sub-format stream prev tilde)
        (pop *format-arguments*))
      (format-error "Missing argument"))))


;;; ~:[ 

(defun format-boolean-condition (stream)
  (multiple-value-bind
    (prev tilde parms colon atsign command)
    (format-find-command '(#\; #\]))
    (when (or parms colon atsign)
      (format-error "Flags or parameters not allowed"))
    (when (eq command #\])
      (format-error "Two clauses separated by ~~; are required for ~~:["))
    (format-nextchar)
    (if (pop-format-arg)
      (multiple-value-bind (prev tilde parms colon atsign)
          (format-find-command '(#\]))
        (when (or colon atsign parms)
          (format-error "Flags or parameters not allowed"))
        (sub-format stream prev tilde))
      (progn
        (sub-format stream prev tilde)
        (format-find-command '(#\]))))))


(defformat #\[ format-condition (stream colon atsign &rest parms)
  (declare (dynamic-extent parms))
  (when parms
    (let ((p (pop parms)))
      (if p (push p *format-arguments*)))
    (unless (null parms)
      (format-error "Too many parameters to ~~[")))
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
    (multiple-value-bind (prev tilde end-parms end-colon end-atsign)
                         (format-find-command '(#\}))
      (when (or end-atsign end-parms)
        (format-error "Illegal terminator for ~~{"))
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
                           (declare (special *format-colon-rest*))
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
  (multiple-value-bind (prev tilde colon atsign parms cmd)
                       (format-find-command '(#\; #\>) nil T)
    (when colon
      (format-error "~~:; allowed only after first segment in ~~<"))
    (when (or atsign parms)
      (format-error "Flags and parameters not allowed"))
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
  (multiple-value-bind (start tilde eparms ecolon eatsign)
                       (format-find-command '(#\>)) ; bumps format-index
    (declare (ignore tilde eparms))
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
                      (format-error "Justification illegal in this context."))
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
    (if *format-justification-semi*
      (format-error "~<...~:> illegal in this context."))
    (setq *format-pprint* t)
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
                    (when *format-arguments*
                      (if  (and (listp *format-arguments*)
                                (first-block-p start))
                        (pop *format-arguments*)  ; dont count
                        (pop-format-arg))) ; unless not listp or not first
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


(defconstant long-log10-of-2 0.30103d0)

#| 
(defun scale-exponent (x)
  (if (floatp x )
      (scale-expt-aux (abs x) 0.0d0 1.0d0 1.0d1 1.0d-1 long-log10-of-2)
      (report-bad-arg x 'float)))

#|this is the slisp code that was in the place of the error call above.
  before floatp was put in place of shortfloatp.
      ;(scale-expt-aux x (%sp-l-float 0) (%sp-l-float 1) %long-float-ten
      ;                %long-float-one-tenth long-log10-of-2)))
|#

; this dies with floating point overflow (?) if fed least-positive-double-float

(defun scale-expt-aux (x zero one ten one-tenth log10-of-2)
  (let ((exponent (nth-value 1 (decode-float x))))
    (if (= x zero)
      (values zero 1)
      (let* ((e (round (* exponent log10-of-2)))
             (x (if (minusp e)		;For the end ranges.
                  (* x ten (expt ten (- -1 e)))
                  (/ x ten (expt ten (1- e))))))
        (do ((d ten (* d ten))
             (y x (/ x d))
             (e e (1+ e)))
            ((< y one)
             (do ((m ten (* m ten))
                  (z y (* z m))
                  (e e (1- e)))
                 ((>= z one-tenth) (values x e)))))))))
|#

(defun scale-exponent (n)
  (let ((exp (nth-value 1 (decode-float n))))
    (values (round (* exp long-log10-of-2)))))


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

(defun format-newline (stream colon atsign &rest parms)
  (declare (dynamic-extent parms))
  (when parms
    (format-error "Parameters not allowed"))
  (cond (colon
         (when atsign (format-error "~:@<newline> is undefined")))
        (atsign (terpri stream) (format-eat-whitespace))
        (t (format-eat-whitespace))))
  
(defformat  #\newline format-newline (stream colon atsign &rest parms)
  (apply #'format-newline stream colon atsign parms))

(defformat #\return format-newline (stream colon atsign &rest parms)
  (apply #'format-newline stream colon atsign parms))

;;; Indirection  ~?

(defformat #\? format-indirection (stream colon atsign &rest parms)
  (declare (dynamic-extent parms))
  (when (or colon parms)
    (format-error "Flags or parameters not allowed"))
  (let ((string (pop-format-arg)))
    (unless (or (stringp string)(functionp string))
      (format-error "Indirected control string is not a string or function"))
    ; fix so 3.1 doesn't make an extended-string here! for which %str-member was busted
    ; it didn't fail in 3.0 cause the setq was erroneously missing
    ; should really fix the compiler macro to not do that! - done 
    (when (AND (stringp string)(NOT (SIMPLE-STRING-P STRING)))
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
          ((< 127 code)
           (write-char char stream)
           (when (and atsign
                      (neq #\Null (setq char (code-char (logand 127 code)))))
             (princ " (Meta " stream)
             (write-char char stream)
             (write-char #\) stream)))
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
  (if (not (or w d))  ; perhaps put this back when prin1 is better
    (prin1 number stream)
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
          (setq spaceleft nil w nil)))
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
      (when (not exp) (setq exp (scale-exponent  number)))
      AGAIN
      (let* ((expt (- exp k))
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
                         (if (> k 0)(+ d 2)(+ d k 1))
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
              (when (not (minusp spaceleft))(write-char #\0 stream))
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
          (cond ((> strlen before-pt)
                 (dotimes (i (- n before-pt))
                   (write-char #\0 stream))
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

