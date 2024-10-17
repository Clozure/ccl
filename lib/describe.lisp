;;; -*- Mode:Lisp; Package:INSPECTOR -*-
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

(defpackage "INSPECTOR"
  (:use "CL" "CCL")
  (:export "MAKE-INSPECTOR"
           "COMPUTE-LINE-COUNT"
           "LINE-N"
           "INSPECTOR-OBJECT"
           "INSPECTOR-LINE-COUNT"

           "*INSPECTOR-DISASSEMBLY*"))



(in-package "INSPECTOR")

(defvar ccl::@ nil)
(defvar ccl::@@ nil)
(defvar ccl::@@@ nil)

;;; The basic inspector object.
;;; Note that this knows nothing about windows.
;;; It merely knows how to number the constituent parts of an object,
;;; How to access a constituent, and how to print a constituent to a stream.
(defclass inspector ()
  ((object :accessor inspector-object :initarg :object)
   (line-count :accessor inspector-line-count :initarg :line-count :initform nil)
   ;; so can refresh.
   (initargs :reader inspector-initargs :initform nil)))

(defmethod initialize-instance :before ((i inspector) &rest initargs)
  (setf (slot-value i 'initargs) initargs))

;;; The usual way to cons up an inspector
(defmethod make-inspector (object)
  (multiple-value-bind (class alias) (inspector-class object)
    (make-instance class :object (or alias object))))

(defmethod initialize-instance :after ((i inspector) &key update-line-count)
  (when update-line-count
    (update-line-count i)))

(defmethod refresh-inspector ((i inspector))
  (apply #'make-instance (class-of i) (slot-value i 'initargs)))

;; New protocol, used by gui inspector instead of the line-n protocol, which isn't quite right.
;; Perhaps tty inspector should use it as well.  Returns the line inspector rather than object,
;; and returns the value string rather than having the caller print it.
(defmethod inspector-line ((i inspector) index)
  (let ((line-i (multiple-value-bind (value label type) (inspector::line-n i index)
		  (and (not (eq (parse-type i type) :comment))
		       (line-n-inspector i index value label type)))))
    (multiple-value-bind (label-string value-string) (line-n-strings i index)
      (values line-i label-string value-string))))

(defmethod inspector-object-string ((i inspector))
  (let ((ob (inspector-object i))
	(*print-readably* nil)
        (*signal-printing-errors* nil)
        (*print-circle* t)
        (*print-length* 20)
        (*print-pretty* nil))
    (prin1-to-string ob)))

;; for a comment value = nil, label = "the comment" type = :comment
;;; => line-i = nil

;;;;;;;
;;;
;;; The protocol for an inspector.
;;; Change these to defgeneric's when it exists.
;;;
;;; Usually, you need to define methods only for
;;; inspector-class, compute-line-count, line-n, and (setf line-n)

;;; Return the type of inspector for an object
(defmethod inspector-class (object)
  (cond ((or (method-exists-p #'line-n object 0)
             (method-exists-p #'inspector-line 0))
         'usual-inspector)
        ((and (uvectorp object)
              (find-class 'uvector-inspector nil))
         'uvector-inspector)
        (t 'basic-inspector)))

;;; Return three values: the value, label, and type of the nth line of the object
;;; Valid types are:
;;;  :NORMAL or NIL  - a normal constituent line: changeable
;;;  :COLON          - a normal line with ": " between the label and the value
;;;  :COMMENT        - a commentary line - Print only the label
;;;  :STATIC         - a commentary line with an inspectable value: not changeable
(defmethod line-n ((i inspector) n)
  (declare (ignore n)))

; set the value of line n of the object (the label is fixed)
(defmethod (setf line-n) (value (i inspector) n)
  (declare (ignore value n)))

; Compute the number of lines in the object
(defmethod compute-line-count ((i inspector))
  0
  )

; Compute the number of lines in the object and set the line-count slot
; If the length is greater than the limit, return (list limit)
(defun update-line-count (inspector)
  (setf (inspector-line-count inspector) (compute-line-count inspector)))

; Print the nth line to a stream
(defmethod prin1-line-n ((i inspector) stream n)
  (multiple-value-call #'prin1-line i stream (line-n i n)))

(defmethod prin1-line ((i inspector) stream value &optional label type function)
  (unless function
    (setq function (inspector-print-function i type)))
  (funcall function i stream value label type))

(defvar *collect-labels-if-list* t)

(defmethod end-of-label ((stream string-output-stream))
  (when (listp *collect-labels-if-list*)
    (push (get-output-stream-string stream) *collect-labels-if-list*)))

(defmethod line-n-strings ((i inspector) n)
  (let* ((*collect-labels-if-list* ())
	 (value-string (with-output-to-string (stream)
			 (prin1-line-n i stream n)))
	 (label-string (pop *collect-labels-if-list*))
         (end (or (position-if-not #'whitespacep label-string :from-end t) -1)))
    (assert (null *collect-labels-if-list*))
    (unless (and (>= end 0) (eql (char label-string end) #\:)) (incf end))
    (setq label-string (subseq label-string 0 end))
    (values label-string value-string)))

(defmethod inspector-print-function ((i inspector) type)
  (declare (ignore type))
  'prin1-normal-line)

; Print a value to a stream.
(defmethod prin1-normal-line ((i inspector) stream value &optional label type
                              colon-p)
  (let* ((type-sym (parse-type i type)))
    (if (eq type-sym :colon) (setq colon-p t))
    (when label
      (prin1-label i stream value label type)
      (if colon-p (princ ": " stream)))
    (end-of-label stream)              ; used by cacheing code
    (unless (eq type-sym :comment)
      (prin1-value i stream value label type))))

(defun prin1-colon-line (i stream value &optional label type)
  (prin1-normal-line i stream value label type t))

(defmethod prin1-label ((i inspector) stream value &optional label type)
  (declare (ignore value type))
  (if (stringp label)
    (write-string label stream)
    (princ label stream)))

(defmethod prin1-value ((i inspector) stream value &optional label type)
  (declare (ignore label type))
  (prin1 value stream))

;;; Call function on the inspector object and its value, label, & type, for
;;; each line in the selected range (default to the whole thing).
;;; This can avoid (e.g.) doing NTH for each element of a list.
;;; This is the generic-function which the inspector-window uses to
;;; display a screenful.
(defmethod map-lines ((i inspector) function &optional 
                      (start 0) 
                      end)
  (when (null (inspector-line-count i))
    (update-line-count i))
  (unless end
    (setq end (inspector-line-count i)))
  (when (and start end)
    (let ((index start))
      (dotimes (c (- end start))
        (multiple-value-call function i index (inspector-line i index))
        (incf index)))))

;;;;;;;
;;;
;;; Dealing with unbound slots and bogus objects
;;;
(defclass unbound-marker () ())

(defvar *unbound-marker* (make-instance 'unbound-marker))
(defvar *slot-unbound-marker* (make-instance 'unbound-marker))

(defmethod print-object ((x unbound-marker) stream)
  (print-object (ccl::%unbound-marker) stream))

(defclass bogus-object-wrapper ()
  ((address :initarg :address)))

(defmethod print-object ((x bogus-object-wrapper) stream)
  (print-unreadable-object (x stream)
    (format stream "BOGUS object @ #x~x" (slot-value x 'address))))

(defvar *bogus-object-hash*
  (make-hash-table :test 'eql :weak :value :size 0))

(defun bogus-object-wrapper (x)
  (let ((address (%address-of x)))
    (or (gethash address *bogus-object-hash*)
        (setf (gethash address *bogus-object-hash*)
              (make-instance 'bogus-object-wrapper :address address)))))

(defun eliminate-unbound (x)
  (cond ((eq x (ccl::%unbound-marker))
         *unbound-marker*)
        ((eq x (ccl::%slot-unbound-marker))
         *slot-unbound-marker*)
        ((ccl::bogus-thing-p x)
         (bogus-object-wrapper x))
        (t x)))

(defun restore-unbound (x)
  (if (eq x *unbound-marker*)
    (ccl::%unbound-marker)
    (if (eq x *slot-unbound-marker*)
      (ccl::%slot-unbound-marker)
      x)))

(defmethod line-n :around ((i inspector) n)
  (declare (ignore n))
  (let ((res (multiple-value-list (call-next-method))))
    (declare (dynamic-extent res))
    (apply #'values (eliminate-unbound (car res)) (cdr res))))

(defmethod (setf line-n) :around (new-value (i inspector) n)
  (call-next-method (restore-unbound new-value) i n))


;;;;;;;
;;;
;;; describe-object
;;; Eventually, this wants to reuse a global inspector rather than
;;; consing one.
(defparameter *describe-pretty* t)

(defmacro with-errorfree-printing (&body body)
  `(let ((*print-readably* nil)
         (*signal-printing-errors* nil))
     ,@body))

(defun format-line-for-tty (stream label-string value-string)
  (when (equal label-string "") (setq label-string nil))
  (when (equal value-string "") (setq value-string nil))
  (format stream "~@[~a~]~@[~a~]~@[~a~]"
	  label-string
	  (and label-string
	       value-string 
	       (not (eql #\space (char label-string (1- (length label-string)))))
	       ": ")
	  value-string))

(defun describe (object &optional stream)
  "Print a description of the object X."
  (cond ((null stream) (setq stream *standard-output*))
        ((eq stream t) (setq stream *terminal-io*)))
  (setq stream (require-type stream 'stream))
  (let* ((*print-circle* t)
         (*print-length* 20))
    (describe-object object stream)
    (values)))

(defmethod describe-object (object stream)
  (let ((inspector (make-inspector object)))
    (with-errorfree-printing
        (let* ((*print-pretty* (or *print-pretty* *describe-pretty*))
               (temp #'(lambda (i index child &optional label-string value-string)
			 (declare (ignore i index child))
			 (format-line-for-tty stream label-string value-string)
			 (terpri stream))))
          (declare (dynamic-extent temp))
          (map-lines inspector temp))))
  (values))

;;; usual-inspector
;;; Objects that know how to inspect themselves but don't need any
;;; special info other than the object can be a usual-inspector.
;;; This class exists mostly to save consing a class for every type
;;; of object in the world.
(defclass usual-inspector (inspector)
  ())

;;;;;;;
;;
;; formatting-inspector
;; This one prints using a format string.
;; Expects line-n to return (values value label type format-string)

(defclass formatting-inspector (inspector) ())
(defclass usual-formatting-inspector (usual-inspector formatting-inspector) ())

(defmethod prin1-line ((i formatting-inspector) stream value
                       &optional label type (format-string "~s"))
  (funcall (if (listp format-string) #'apply #'funcall)
           #'format-normal-line i stream value label type format-string))

(defmethod format-normal-line ((i inspector) stream value &optional 
                               label type (format-string "~s") colon-p)
  (let* ((type-sym (parse-type i type)))
    (if (eq type-sym :colon) (setq colon-p t))
    (when label
      (prin1-label i stream value label type)
      (if colon-p (princ ": " stream)))
    (end-of-label stream)              ; used by cacheing code
    (unless (eq type-sym :comment)
      (format stream format-string value))))

;;;;;;;
;;
;; inspectors for CCL objects
;;


(defmethod parse-type ((i inspector) type &optional default1 default2)
  (declare (ignore default1 default2))
  (values (if (consp type) (car type) type)))

;;; Used by the cache-entry-stream class to save the column where the label ends.
(defmethod end-of-label (stream)
  (declare (ignore stream)))



;;;;;
;;
;; The default inspector class
;; Used when we don't know what else to do
;;

(defclass basic-inspector (inspector) ())

(defmethod compute-line-count ((i basic-inspector))
  3)                                    ; type, class, value

(defun line-n-out-of-range (i n)
  (error "~s is not a valid index for line-n of ~s" n i))

(defun setf-line-n-out-of-range (i n)
  (error "~s is not a valid index for setf-line-n of ~s" n i))

(defmethod line-n ((i basic-inspector) n)
  (let ((object (inspector-object i)))
    (case n
      (0 (values object nil :static))
      (1 (values (type-of object) "Type: " :static))
      (2 (values (class-of object) "Class: " :static))
      (t (line-n-out-of-range i n)))))

;;;;;;;
;;
;; Automate the object being the first line
;;
(defclass object-first-mixin () ())
(defclass object-first-inspector (object-first-mixin inspector) ())

(defmethod compute-line-count :around ((i object-first-mixin))
  (1+ (call-next-method)))

(defmethod line-n :around ((i object-first-mixin) n)
  (if (eql 0 n)
    (values (inspector-object i) nil)
    (call-next-method i (1- n))))

(defmethod (setf line-n) :around (value (i object-first-mixin) n)
  (if (eql n 0)
    (replace-object i value)
    (call-next-method value i (1- n))))

(defun replace-object (inspector new-object)
  (declare (ignore inspector))
  (make-inspector new-object))


; A mixin that displays the object, its type, and its class as the first three lines.
(defclass basics-first-mixin () ())

(defmethod compute-line-count :around ((i basics-first-mixin))
  (+ 3 (call-next-method)))

(defmethod line-n :around ((i basics-first-mixin) n)
  (let ((object (inspector-object i)))
    (case n
      (0 (values object nil))
      (1 (values (type-of object) "Type: " :static))
      (2 (values (class-of object) "Class: " :static))
      (t (call-next-method i (- n 3))))))

(defmethod line-n-inspector :around ((i basics-first-mixin) n value label type)
  (if (< n 3)
    (make-inspector value)
    (call-next-method i (- n 3) value label type)))

(defmethod (setf line-n) :around (new-value (i basics-first-mixin) n)
  (case n
    (0 (replace-object i new-value))
    ((1 2) (setf-line-n-out-of-range i n))
    (t (call-next-method new-value i (- n 3)))))

;;;;;;;
;;
(defclass usual-object-first-inspector (object-first-mixin usual-inspector)
  ())
(defclass usual-basics-first-inspector (basics-first-mixin usual-inspector)
  ())

(defvar *inspector*)

(defmethod compute-line-count ((i usual-inspector))
  (let ((*inspector* i))
    (compute-line-count (inspector-object i))))

(defmethod line-n ((i usual-inspector) n)
  (let ((*inspector* i))
    (line-n (inspector-object i) n)))

(defmethod (setf line-n) (value (i usual-inspector) n)
  (let ((*inspector* i))
    (setf (line-n (inspector-object i) n) value)))

(defmethod inspector-commands ((i usual-inspector))
  (let ((*inspector* i))
    (inspector-commands (inspector-object i))))

(defmethod inspector-commands (random)
  (declare (ignore random))
  nil)

;;;;;;;
;;
;; Bogus objects
;;

(defclass bogus-object-inspector (object-first-inspector)
  ())

(defmethod compute-line-count ((i bogus-object-inspector))
  3)

(defmethod line-n ((i bogus-object-inspector) n)
  (values
   nil
   (case n
     (0 "One cause of a bogus object is when a stack consed object is stored")
     (1 "in a register and then control exits the dynamic-extent of the object.")
     (2 "The compiler doesn't bother to clear the register since it won't be used again."))
   '(:comment :plain :plain)))

(defmethod inspector-class :around (object)
  (if (ccl::bogus-thing-p object)
    'bogus-object-inspector
    (call-next-method)))

;;;;;;;
;;
;; A general sequence inspector
;;
(defclass sequence-inspector (inspector)
  ((print-function :initarg :print-function :initform #'prin1 :reader print-function)
   (commands :initarg :commands :initform nil :accessor inspector-commands)
   (line-n-inspector :initform nil :initarg :line-n-inspector
                     :accessor line-n-inspector-function)
   (replace-object-p :initform nil :initarg :replace-object-p
                     :reader replace-object-p)
   (resample-function :initform nil :initarg :resample-function
                      :reader resample-function)
   (line-n-function :initform nil :initarg :line-n-function
                    :reader line-n-function)
   (setf-line-n-p :initform t :initarg :setf-line-n-p
                  :reader setf-line-n-p))
  (:default-initargs :update-line-count t))



(defmethod compute-line-count ((i sequence-inspector))
  (let ((resample-function (resample-function i)))
    (when resample-function
      (setf (inspector-object i) (funcall resample-function i))))
  (length (inspector-object i)))

(defmethod line-n ((i sequence-inspector) n)
  (let ((f (line-n-function i)))
    (if f
      (funcall f i n)
      (values (elt (inspector-object i) n) nil (unless (setf-line-n-p i) :static)))))

(defmethod (setf line-n) (new-value (i sequence-inspector) n)
  (if (setf-line-n-p i)
    (setf (elt (inspector-object i) n) new-value)
    (setf-line-n-out-of-range i n)))

(defmethod prin1-value ((inspector sequence-inspector) stream value
                        &optional label type)
  (declare (ignore label type))
  (funcall (print-function inspector) value stream))

(defmethod line-n-inspector ((i sequence-inspector) n value label type)
  (let ((f (line-n-inspector-function i)))
    (or (and f (funcall f i n value label type)) (call-next-method))))

;;;;;;;
;;
;; standard-object
;; This should be redone to use the exported class query functions
;; (as soon as they exist)
;;
(defclass standard-object-inspector (object-first-inspector)
  ())

(defmethod inspector-class ((o standard-object))
  'standard-object-inspector)

(defmethod compute-line-count ((i standard-object-inspector))
  (standard-object-compute-line-count i))

(defun standard-object-compute-line-count (i)  
  (let* ((object (ccl::maybe-update-obsolete-instance (inspector-object i)))
         (class (class-of object))
         (all-slots (ccl::class-slots class)))
    (multiple-value-bind (instance-slots class-slots other-slots) (ccl::extract-instance-class-and-other-slotds all-slots)
      (let* ((ninstance-slots (length instance-slots))
             (nclass-slots (length class-slots))
             (nother-slots (length other-slots)))
        (+ 2                                ; class, wrapper
           (if (eql 0 ninstance-slots)
             0
             (1+ ninstance-slots))
           (if (eql 0 nclass-slots)
             0
             (1+ nclass-slots))
           (if (eql 0 nother-slots)
             0
             (1+ nother-slots))
           (if (eql 0 (+ nclass-slots ninstance-slots nother-slots))
             1
             0))))))

(defun slot-value-or-unbound (instance slot-name)
  (eliminate-unbound (ccl::slot-value-if-bound instance slot-name
					       (ccl::%slot-unbound-marker))))

(defparameter *standard-object-type* (list nil))
(defparameter *standard-object-static-type*
  (cons :static (cdr *standard-object-type*)))
(defparameter *standard-object-comment-type* 
  (list :comment))

(defmethod line-n ((i standard-object-inspector) n)
  (standard-object-line-n i n))

(defmethod prin1-label ((i standard-object-inspector) stream value &optional label type)
  (declare (ignore value type))
  (if (symbolp label)
    (prin1 label stream)
    (call-next-method)))

; Looks like
; Class:
; Wrapper:
; [Instance slots:
;  slots...]
; [Class slots:
;  slots...]
; [Other slots:
;  slots...]

(defun standard-object-line-n (i n)
  (let* ((instance (inspector-object i))
         (class (class-of instance))
         (all-slots (class-slots class))
         (wrapper (or (ccl::standard-object-p instance)
                      (if (typep instance 'ccl::funcallable-standard-object)
                        (ccl::gf.instance.class-wrapper instance))))
	 (instance-start 2))
    (if (< n instance-start)
      (if (eql n 0)
	(values class "Class: " :normal)
	(values wrapper "Wrapper: " :static))
      (multiple-value-bind (instance-slotds class-slotds other-slotds)
          (ccl::extract-instance-class-and-other-slotds all-slots)
        (let* ((instance-count (length instance-slotds))
               (shared-start (+ instance-start instance-count
                                (if (eql 0 instance-count) 0 1))))
          (if (< n shared-start)
            (if (eql n instance-start)
              (values nil "Instance slots" :comment)
              (let ((slot-name (slot-definition-name
                                (elt instance-slotds (- n instance-start 1)))))
                (values (slot-value-or-unbound instance slot-name)
                        slot-name
                        :colon)))
            (let* ((shared-count (length class-slotds))
                   (shared-end (+ shared-start shared-count
                                  (if (eql shared-count 0) 0 1))))
              (if (< n shared-end)
                (if (eql n shared-start)
                  (values nil "Class slots" :comment)
                  (let ((slot-name (slot-definition-name 
                                    (elt class-slotds (- n shared-start 1)))))
                    (values (slot-value-or-unbound instance slot-name)
                            slot-name
                            :colon)))
                (let* ((other-start shared-end)
                       (other-end (+ other-start (if other-slotds (1+ (length other-slotds)) 0))))
                  (if (< n other-end)
                    (if (eql n other-start)
                      (values nil "Other slots" :comment)
                      (let ((slot-name (slot-definition-name 
                                        (elt other-slotds (- n other-start 1)))))
                        (values (slot-value-or-unbound instance slot-name)
                                slot-name
                                :colon)))
                    (if (and (eql 0 instance-count) (eql 0 shared-count) (null other-slotds) (eql n other-end))
                      (values nil "No Slots" :comment)
                      (line-n-out-of-range i n))))))))))))

(defmethod (setf line-n) (value (i standard-object-inspector) n)
  (standard-object-setf-line-n value i n))

(defun standard-object-setf-line-n (value i n)
  (let* ((instance (inspector-object i))
         (class (class-of instance))
         (instance-start 2))
    (if (< n instance-start)
      (cond
       ((eql n 0) (change-class instance value)
         (update-line-count i))
        (t (setf-line-n-out-of-range i n)))
      (let* ((slotds (ccl::extract-instance-effective-slotds class))
             (instance-count (length slotds))
             (shared-start (+ instance-start instance-count
                              (if (eql 0 instance-count) 0 1))))
        (if (< n shared-start)
          (if (eql n instance-start)
            (setf-line-n-out-of-range i n)
            (let ((slot-name (slot-definition-name
                              (elt slotds (- n instance-start 1)))))
              (setf (slot-value instance slot-name) (restore-unbound value))))
          (let* ((slotds (ccl::extract-class-effective-slotds class))
                 (shared-count (length slotds))
                 (shared-end (+ shared-start shared-count
                                (if (eql shared-count 0) 0 1))))
            (if (< n shared-end)
              (if (eql n shared-start)
                (setf-line-n-out-of-range i n)
                (let ((slot-name (slot-definition-name 
                                  (elt slotds (- n shared-start 1)))))
                  (setf (slot-value instance slot-name)
                        (restore-unbound value))))
              (setf-line-n-out-of-range i n))))))))


;;;;;;;;;;;  Inspector objects for common classes.

(defparameter *plain-comment-type* '(:comment (:plain)))
(defparameter *bold-comment-type* '(:comment (:bold)))

(defun resample-it ()
  )

;;;;;;;
;;
;; Lists
;;
(defclass cons-inspector (basics-first-mixin inspector) ())

(defclass list-inspector (basics-first-mixin inspector)
  ((length :accessor list-inspector-length)
   (dotted-p :accessor list-inspector-dotted-p)
   (nthcdr :accessor list-inspector-nthcdr)
   (n :accessor list-inspector-n)))

(defmethod inspector-class ((o list))
  (if (listp (cdr o))
    'list-inspector
    'cons-inspector))

; Same as list-length-and-final-cdr, but computes the real length of the list
(defun real-list-length (list)
  (multiple-value-bind (len final-cdr max-circ-len)
      (ccl::list-length-and-final-cdr list)
    (if (null max-circ-len)
      (values len final-cdr nil)
      (let ((middle (nthcdr max-circ-len list))
            (n 1))
        (loop (when (eq list middle) (return))
          (pop list)
          (incf n))
        (pop list)
        (loop (when (eq list middle) (return))
          (pop list)
          (incf n))
        (values nil nil n)))))        

(defmethod compute-line-count ((i list-inspector))
  (multiple-value-bind (len final-cdr circ-len) (real-list-length (inspector-object i))
    (setf (list-inspector-dotted-p i) final-cdr)
    (setf (list-inspector-nthcdr i) (inspector-object i))
    (setf (list-inspector-n i) 0)
    (+ 1                                ; regular, dotted, or circular
       1                                ; length
       (abs (setf (list-inspector-length i)
                  (or len (- circ-len))))   ; the elements
       (if final-cdr 2 0))))            ; the final-cdr and it's label

(defmethod compute-line-count ((i cons-inspector))
  2)                                    ; car & cdr

(defmethod line-n ((i list-inspector) en &aux (n en))
  (let* ((circ? (list-inspector-length i))
         (length (abs circ?)))
    (cond ((eql 0 n)
           (values nil (cond ((list-inspector-dotted-p i) "Dotted List")
                             ((< circ? 0) "Circular List")
                             (t "Normal List"))
                   *plain-comment-type*))
          ((eql 0 (decf n)) (values length "Length: "))
          ((>= (decf n) (setq length length))   ; end of dotted list
           (let ((final-cdr (list-inspector-dotted-p i)))
             (unless final-cdr (line-n-out-of-range i en))
             (if (eql n length)
               (values nil "Non-nil final cdr" *plain-comment-type*)
               (values final-cdr (- length 0.5) :colon))))
          (t (let* ((saved-n (list-inspector-n i))
                    (nthcdr (if (>= n saved-n)
                              (nthcdr (- n saved-n) (list-inspector-nthcdr i))
                              (nthcdr n (inspector-object i)))))
               (setf (list-inspector-nthcdr i) nthcdr
                     (list-inspector-n i) n)
               (values (car nthcdr) n :colon))))))

(defmethod line-n ((i cons-inspector) n)
  (let ((object (inspector-object i)))
    (ecase n
           (0 (values (car object) "Car: "))
           (1 (values (cdr object) "Cdr: ")))))

(defmethod (setf line-n) (value (i list-inspector) n)
  (when (< n 2)
    (setf-line-n-out-of-range i n))
  (decf n 2)
  (setf (elt (inspector-object i) n) value)
  (resample-it))

(defmethod (setf line-n) (value (i cons-inspector) n)
  (let ((object (inspector-object i)))
    (ecase n
           (0 (setf (car object) value))
           (1 (setf (cdr object) value))))
  (resample-it))

;;;;;;;
;;
;; General uvector's
;;
(defclass uvector-inspector (basics-first-mixin inspector)
  ((name-list :initarg :name-list :initform nil :accessor name-list)))

(defmethod uvector-name-list (object) 
  (let* ((type (type-of object))
         (names (cdr (assq type ccl::*def-accessor-types*)))
         (names-size (length names))
         res)
    (when names
      (dotimes (i (uvsize object))
        (declare (fixnum i))
        (let ((name (and (> names-size i) (aref names i))))
          (if name
            (push (if (listp name) (car name) name) res)
            (if (and (eql i 0) (typep object 'ccl::internal-structure))
              (push 'type res)
              (push i res)))))
      (nreverse res))))

(defmethod compute-line-count ((i uvector-inspector))
  (setf (name-list i) (uvector-name-list (inspector-object i)))
  (uvsize (inspector-object i)))

(defmethod line-n ((i uvector-inspector) n)
  (values (uvref (inspector-object i) n)
          (or (let ((name-list (name-list i))) (and name-list (nth n (name-list i))))
              n)
          :colon))

(defmethod (setf line-n) (new-value (i uvector-inspector) n)
  (setf (uvref (inspector-object i) n) new-value))

(defmethod inspector-commands ((i uvector-inspector))
  (let ((object (inspector-object i)))
    (if (method-exists-p #'inspector-commands object)
      (inspector-commands object))))

;;;;;;;
;;
;; Vectors & Arrays
;;
(defmethod inspector-class ((v ccl::simple-1d-array))
  'usual-basics-first-inspector)

(defmethod compute-line-count ((v ccl::simple-1d-array))
  (+ 1 (length v)))

(defmethod line-n ((v ccl::simple-1d-array) n)
  (cond ((eql 0 n) (values (length v) "Length" :static 'prin1-colon-line))
        (t (decf n 1)
           (values (aref v n) n :colon))))

(defmethod (setf line-n) (value (v ccl::simple-1d-array) n)
  (when (<= n 0)
    (setf-line-n-out-of-range v n))
  (decf n 1)
  (prog1 (setf (aref v n) value)
    (resample-it)))

(defclass array-inspector (uvector-inspector) ())

(defmethod inspector-class ((v array))
  'array-inspector)

(defmethod uvector-name-list ((a array))
  (if (eql 1 (array-rank a))
    (if (array-has-fill-pointer-p a)
      '("Fill Pointer" "Physical size" "Data vector" "Displacement" "Flags")
      '("Logical size" "Physical size" "Data vector" "Displacement" "Flags"))
    `("Rank" "Physical size" "Data vector" "Displacement" "Flags" "Dim0" "Dim1" "Dim2" "Dim3")))

(defmethod compute-line-count ((i array-inspector))
  (let* ((a (inspector-object i))
         (rank (array-rank a)))
    (call-next-method)                  ; calculate name list
    (+ (if (eql rank 1) (1+ (uvsize a))  7)
       (apply #'* (array-dimensions a)))))

(defmethod line-n ((i array-inspector) n)
  (let* ((v (inspector-object i))
         (rank (array-rank v))
         (uvsize (if (eql rank 1)
                   (+ (uvsize v) 1)
                   7)))
    (cond ((eql 0 n) (values (array-element-type v)
                             (if (adjustable-array-p v)
                               "Adjustable, Element type"
                               "Element type")
                             :static 'prin1-colon-line))
          ((eql  5 n)
           (values  (uvref v target::vectorH.flags-cell)
                   "Flags: "
                   :static
                   #'(lambda (i s v l type)
                       (format-normal-line i s v l type "#x~x"))))
          ((and (eql  6 n) (not (eql rank 1)))
           (values (array-dimensions v) "Dimensions: " :static))
          ((< n uvsize) (call-next-method i (1- n)))
          (t (let ((index (- n uvsize)))
               (values (row-major-aref v index) (array-indices v index) :colon))))))

(defmethod (setf line-n) (new-value (i array-inspector) n)
  (let* ((v (inspector-object i))
         (rank (array-rank v))
         (uvsize (if (eql rank 1)
                   (+ (uvsize v) 1)
                   7)))
    (prog1
      (cond ((or (eql 0 n) (eql 1 n) (and (eql 4 n) (not (eql rank 1))))
             (setf-line-n-out-of-range i n))
            ((< n uvsize)
             (if (eql 3 n)
               (setq new-value (require-type new-value 'array))
               (setq new-value (require-type new-value 'fixnum)))
             (call-next-method new-value i (1- n)))
          (t (let ((index (- n uvsize)))
               (setf (row-major-aref v index) new-value))))
      (resample-it))))

(defun array-indices (a row-major-index)
  (let ((rank (array-rank a)))
    (if (eql 1 rank)
      row-major-index
      (let ((res nil)
            dim
            (dividend row-major-index)
            remainder)
        (loop
          (when (zerop rank) (return res))
          (setq dim (array-dimension a (decf rank)))
          (multiple-value-setq (dividend remainder) (floor dividend dim))
          (push remainder res))))))
  
(defmethod prin1-line ((i array-inspector) stream value &optional
                       label type function)
  (declare (ignore stream value type function))
  (if (or (numberp label) (listp label))   ; First line or contents lines
    (call-next-method)
    (let ((*print-array* nil))
      (call-next-method))))

;;;;;;;
;;
;; Numbers
;;
(defmethod inspector-class ((num number)) 'usual-formatting-inspector)

; floats
(defmethod compute-line-count ((num float)) 5)

(defmethod line-n ((num float) n)
  (let ((type :static))
    (ecase n
      (0 (values num "Float:           " type))
      (1 (values num "Scientific:      " type
                 (if (< num 0) "~8,2e" "~7,2e")))
      (2 (values (if (zerop num) "illegal" (log num 2))
                     "Log base 2:      " type "~d"))
      (3 (values (rationalize num)
                     "Ratio equiv:     " type))
      (4 (values (round num)
                     "Nearest integer: " type)))))

; complex numbers
(defmethod compute-line-count ((num complex)) 3)

(defmethod line-n ((num complex) n)
  (let ((type :static))
    (ecase n
      (0 (values num            "Complex num:    " type))
      (1 (values (realpart num) "Real part:      " type))
      (2 (values (imagpart num) "Imaginary part: " type)))))

; ratios
(defmethod compute-line-count ((num ratio)) 6)

(defmethod line-n ((num ratio) n)
  (let ((type :static))
    (ecase n
      (0 (values num               "Ratio:           " type))
      (1 (values (float num)       "Scientific:      " type 
                 (if (< num 0) "~8,2e" "~7,2E")))
      (2 (values (if (zerop num) "illegal" (log num 2))
                                   "Log base 2:      " type "~d"))
      (3 (values (round num)       "Nearest integer: " type))
      (4 (values (numerator num)   "Numerator:       " type))
      (5 (values (denominator num) "Denominator:     " type)))))

; integers
(defmethod compute-line-count ((num integer)) 
  (let ((res 12))
    (unless (< 0 num 4000) (decf res))   ; not a roman number
    (unless (<= 0 num 255) (decf res))   ; not a character
    res))

(defmethod line-n ((num integer) n)
  (if (and (>= n 7) (not (< 0 num 4000))) (incf n))   ; maybe skip roman.
  (if (and (>= n 8) (not (<= 0 num 255))) (incf n))   ; maybe skip character.
  (let* ((type :static)
         (neg? (< num 0))
         (norm (if neg? 
                 (+ num (expt 2 (max 32 (* 4 (round (+ (integer-length num) 4) 4)))))
                 num)))
    (ecase n
      (0  (values num
                (if (fixnump num)
                  "Fixnum:      "
                  "Bignum:      ")
                type "~s"))
      (1  (let ((num (ignore-errors (float num))))
            (values num "Scientific:  " type
                    (cond ((null num) "FLOATING-POINT-OVERFLOW")
                          ((< num 0) "~8,2e")
                          (t "~7,2e")))))
      (2  (values (if (zerop num) "illegal" (log num 2)) 
                  "Log base 2:  " type "~d"))
      (3  (values norm
                  "Binary:      " type
                  (if neg? "#b...~b" "#b~b")))
      (4  (values norm
                  "Octal:       " type
                  (if neg? "#o...~o" "#o~o")))
      (5  (values num
                  "Decimal:     " type "~d."))
      (6  (values norm
                  "Hex:         " type
                  (if neg? "#x...~x" "#x~x")))
      (7  (values (format nil "~@r" num)
                  "Roman:       " type "~a"))
      (8  (values (code-char num)
                  "Character:   " type "~s"))
      (9 (values (ccl::ensure-simple-string (prin1-to-string num))
                  "Abbreviated: "
                  type #'format-abbreviated-string))
      (10 (values (or (ignore-errors (universal-time-string num)) "#<error>")
                  "As time:     " type "~a"))
      (11 (if (< num 0)
            (values most-negative-fixnum 'most-negative-fixnum type '("~d." t))
            (values most-positive-fixnum 'most-positive-fixnum type '("~d." t)))))))

(defun format-abbreviated-string (stream string)
  (setq string (require-type string 'simple-string))
  (let ((length (length string)))
    (if (< length 7)
      (princ string stream)
      (format stream "~a <- ~s digits -> ~a"
              (subseq string 0 3)
              (- length 6)
              (subseq string (- length 3) length)))))

(defun universal-time-string (num)
  (multiple-value-bind (second minute hour date month year day)
                       (decode-universal-time num)
    (with-output-to-string (s)
      (format s "~d:~2,'0d:~2,'0d " hour minute second)
      (princ (nth day '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday"
                        "Saturday" "Sunday"))
             s)
      (format s ", ~d " date)
      (princ (nth month '("" "January" "February" "March" "April" "May" "June" "July"
                          "August" "September" "October" "November" "December"))
             s)
      (format s ", ~d" year))))

; Characters
(defmethod compute-line-count ((ch character)) 2)

(defmethod line-n ((ch character) n)
  (let ((type :static))
    (ecase n
      (0 (values ch             "Character: " type))
      (1 (values (char-code ch) "char-code: " type)))))

;;;;;;;
;;
;; Symbols
;;
(defun symbol-has-bindings-p (sym)
  (or (constantp sym) (proclaimed-special-p sym) (boundp sym)
      (special-operator-p sym) (macro-function sym) (fboundp sym)
      (type-specifier-p sym) (record-type-p sym nil)
      (find-class sym nil)
      (compiler-macro-function sym)))

(defmethod inspector-class ((sym symbol)) 'usual-basics-first-inspector)

(defmethod compute-line-count ((sym symbol))
  (+ (if (symbol-has-bindings-p sym) 1 0)
     1                                  ; package
     1                                  ; symbol-name
     1                                  ; symbol-value
     1                                  ; symbol-function
     (if (fboundp sym) 1 0)             ; arglist
     1                                  ; plist
     (if (find-class sym nil) 1 0)      ; class
     ))


(defmethod normalize-line-number ((sym symbol) n)
  (if (and (>= n 0) (not (symbol-has-bindings-p sym))) (incf n))
  (if (and (>= n 5) (not (fboundp sym))) (incf n))
  n)

(defmethod line-n ((sym symbol) n)
  (setq n (normalize-line-number sym n))
  (let ((type :normal)
        (comment '(:comment (:bold)))
        (static :static))
    (ecase n
      (0 (values nil (symbol-type-line sym) comment))
      (1 (let ((p (symbol-package sym)))
           (if (null p)
             (values nil "No home package." comment)
             (multiple-value-bind (found kind) (find-symbol (symbol-name sym) p)
               (values p 
                       (if (or (null kind) (neq found sym))
                         "NOT PRESENT in home package: "
                         (format nil "~a in package: " kind))
                       static)))))
      (2 (values (symbol-name sym) "Print name: " static))
      (3 (values (if (boundp sym) (symbol-value sym) *unbound-marker*)
                 "Value: " type))
      (4 (values (if (fboundp sym)
                   (cond ((macro-function sym))
                         ((special-operator-p sym) sym)
                         (t (symbol-function sym)))
                   *unbound-marker*)
                 "Function: " type))
      (5 (values (and (fboundp sym) (arglist sym))
                 "Arglist: " static))
      (6 (values (symbol-plist sym) "Plist: " type))
      (7 (values (find-class sym) "Class: " static)))))

(defmethod (setf line-n) (value (sym symbol) n)
  (let (resample-p)
    (setq n (normalize-line-number sym n))
    (setq value (restore-unbound value))
    (ecase n
      ((0 1 2 5) (setf-line-n-out-of-range sym n))
      (3 (setf resample-p (not (boundp sym))
               (symbol-value sym) value))
      (4 (setf resample-p (not (fboundp sym))
               (symbol-function sym) value))
      (6 (setf (symbol-plist sym) value)))
    (when resample-p (resample-it))
    value))

(defun record-type-p (name &optional check-database)
  (declare (ignore check-database))
  (and (keywordp name)
       (ignore-errors (ccl::%foreign-type-or-record name))))

; Add arglist here.
(defun symbol-type-line (sym)
  (let ((types (list
                (cond ((constantp sym)
                       "Constant")
                      ((proclaimed-special-p sym)
                       "Special Variable")
                      ((boundp sym)
                       "Non-special Variable")
                      (t nil))
                (cond ((special-operator-p sym)
                       "Special Operator")
                      ((macro-function sym)
                       "Macro")
                      ((fboundp sym)
                       "Function")
                      (t nil))
                (if (type-specifier-p sym) "Type Specifier")
                (if (record-type-p sym nil) "Record Type")
                (if (find-class sym nil) "Class Name")
                (if (compiler-macro-function sym) "Compiler Macro")))
        flag)
    (with-output-to-string (s)
      (dolist (type types)
        (when type
          (if flag (write-string ", " s))
          (setq flag t)
          (write-string type s))))))
    

(defmethod inspector-commands ((sym symbol))
  (let ((res nil))
    '(push (list "Documentation" #'(lambda () (show-documentation sym)))
          res)
    (let ((class (find-class sym nil)))
      (if class
        (push (list "Inspect Class" #'(lambda () (inspect class))) res)))
    (if (boundp sym)
      (push (list "MAKUNBOUND" #'(lambda () (when (y-or-n-p (format nil "~s?" `(makunbound ',sym)))
                                              (makunbound sym) (resample-it))))
            res))
    (if (fboundp sym)
      (push (list "FMAKUNBOUND" #'(lambda () (when (y-or-n-p (format nil "~s?" `(fmakunbound ',sym)))
                                               (fmakunbound sym) (resample-it))))
            res))
    '(if (record-type-p sym)
      (push (list "Inspect Record Type" #'(lambda () (inspect-record-type sym)))
            res))
    (nreverse res)))


(defmethod line-n-inspector ((sym symbol) n value label type)
  (declare (ignore label type))
  (setq n (normalize-line-number sym n))
  (if (eql n 6)
    (make-instance 'plist-inspector :symbol sym :object value)
    (call-next-method)))

(defclass plist-inspector (inspector)
  ((symbol :initarg :symbol :reader plist-symbol)))

(defmethod inspector-window-title ((i plist-inspector))
  (format nil "~a of ~s" 'plist (plist-symbol i)))

(defmethod compute-line-count ((i plist-inspector))
  (+ 3 (/ (length (inspector-object i)) 2)))

(defmethod line-n ((i plist-inspector) n)
  (let* ((plist (inspector-object i)))
    (cond ((eql 0 n) (values plist "Plist: "))
          ((eql 1 n) (values (plist-symbol i) "Symbol: " :static))
          ((eql 2 n) (values nil nil :comment))
          (t (let ((rest (nthcdr (* 2 (- n 3)) plist)))
               (values (cadr rest) (car rest) :colon))))))

(defmethod (setf line-n) (new-value (i plist-inspector) n)
  (let* ((plist (inspector-object i)))
    (if (eql n 0)
      (replace-object i new-value)
      (if (< n 3)
        (setf-line-n-out-of-range i n)
        (let ((rest (nthcdr (* 2 (- n 3)) plist)))
          (setf (cadr rest) new-value)
          (resample-it))))))

(defparameter *inspector-disassembly* nil)

;;;;;;;
;;
;; Functions
;;
(defclass function-inspector (inspector)
  ((header-lines :initform nil :reader header-lines)
   (disasm-p :accessor disasm-p :initform *inspector-disassembly*)
   (disasm-info :accessor disasm-info)
   (pc-width :accessor pc-width)
   (pc :initarg :pc :initform nil :accessor pc)))

(defmethod standard-header-count ((f function-inspector)) (length (header-lines f)))

(defmethod header-count ((f function-inspector)) (standard-header-count f))

(defclass closure-inspector (function-inspector)
  ((n-closed :accessor closure-n-closed)))

(defmethod inspector-class ((f function)) 'function-inspector)
(defmethod inspector-class ((f compiled-lexical-closure)) 'closure-inspector)

(defmethod compute-line-count :before ((f function-inspector))
  (let* ((o (inspector-object f))
         (doc (documentation o t))
         (sn (ccl::function-source-note o))
         (lines (nconc (list (list o ""))
                       (list (list (function-name o) "Name" :colon))
                       (list (multiple-value-bind (arglist type) (arglist o)
                               (let ((label (if type
                                              (format nil "Arglist (~(~a~))" type)
                                              "Arglist unknown")))
                                 (list arglist label (if type :colon '(:comment (:plain)))))))
                       (list (list (ccl::lfun-bits o) "Bits" :colon))
                       (list (list (ccl::%lfun-info o) "Plist" :colon))
                       (when doc (list (list (substitute #\space #\newline doc) "Documentation" :colon)))
                       (when sn (list (list sn "Source Location" :colon))))))
    (setf (slot-value f 'header-lines) lines)))

(defmethod compute-line-count ((f function-inspector))
  (+ (header-count f) (compute-disassembly-lines f)))

(defmethod line-n-strings ((f function-inspector) n)
  (if (< (decf n (header-count f)) 0)
    (call-next-method)
    (disassembly-line-n-strings f n)))

(defmethod line-n-inspector ((f function-inspector) n value label type)
  (declare (ignore value label type))
  (if (< (decf n (header-count f)) 0)
    (call-next-method)
    (disassembly-line-n-inspector f n)))

(defmethod line-n ((f function-inspector) n)
  (let* ((lines (header-lines f))
         (nlines (length lines)))
    (if (< n nlines)
      (apply #'values (nth n lines))
      (disassembly-line-n f (- n nlines)))))

(defmethod compute-line-count :before ((f closure-inspector))
  (let* ((o (inspector-object f))
	 (nclosed (nth-value 8 (function-args (ccl::closure-function o)))))
    (setf (closure-n-closed f) nclosed)))

(defmethod header-count ((f closure-inspector))
  (+ (standard-header-count f)
     1                              ; the function we close over
     1                              ; "Closed over values"
     (closure-n-closed f)))

(defmethod line-n ((f closure-inspector) n)
  (let ((o (inspector-object f))
        (nclosed (closure-n-closed f)))
    (if (< (decf n (standard-header-count f)) 0)
      (call-next-method)
      (cond ((< (decf n) 0)
             (values (ccl::closure-function o) "Inner lfun: " :static))
            ((< (decf n) 0)
             (values nclosed "Closed over values" :comment))
            ((< n nclosed)
             (let* ((value (ccl::nth-immediate o (1+ (- nclosed n))))
                    (map (car (ccl::function-symbol-map (ccl::closure-function o))))
                    (label (or (and map (aref map (+ n (- (length map) nclosed))))
                               n))
                    (cellp (ccl::closed-over-value-p value)))
               (when cellp
                 (setq value (ccl::closed-over-value value)
                       label (format nil "(~a)" label)))
               (values value label (if cellp :normal :static) #'prin1-colon-line)))
            (t (disassembly-line-n f (- n nclosed)))))))

(defmethod (setf line-n) (new-value (f function-inspector) n)
  (let ((o (inspector-object f))
        (standard-header-count (standard-header-count f)))
    (if (< n standard-header-count)
      (case n
        (0 (replace-object f new-value))
        (1 (ccl::lfun-name o new-value) (resample-it))
        (t (setf-line-n-out-of-range f n)))
      (set-disassembly-line-n f (- n standard-header-count) new-value)))
  new-value)

(defmethod (setf line-n) (new-value (f closure-inspector) en &aux (n en))
  (let ((o (inspector-object f))
        (nclosed (closure-n-closed f)))
    (if (< (decf n (standard-header-count f)) 0)
      (call-next-method)
      (cond ((< (decf n 2) 0)          ; inner-lfun or "Closed over values"
             (setf-line-n-out-of-range f en))
            ((< n nclosed)       ; closed-over variable
             (let* ((value (ccl::nth-immediate o (1+ (- nclosed n))))
                    (cellp (ccl::closed-over-value-p value)))
               (unless cellp (setf-line-n-out-of-range f en))
               (ccl::set-closed-over-value value new-value)))
            (t (set-disassembly-line-n f (- n nclosed) new-value))))))

(defun compute-disassembly-lines (f &optional (function (inspector-object f)))
  (if (and (functionp function) (disasm-p f))
    (let* ((lines (ccl::disassemble-lines function)) ;; list of (object label instr)
           (length (length lines))
           (last-label (loop for n from (1- length) downto 0 as line = (aref lines n)
                             thereis (and (consp line) (cadr line))))
           (max-pc (if (consp last-label) (cadr last-label) last-label)))
      (setf (pc-width f) (length (format nil "~d" max-pc)))
      (setf (disasm-info f) lines)
      (1+ length))
    0))

(defun disassembly-line-n (f n)
  (if (< (decf n) 0)
    (values nil "Disassembly:" :comment)
    (let ((line (aref (disasm-info f) n)))
      (if (consp line)
        (destructuring-bind (object label instr) line
          (values object (cons label instr) :static))
        (values nil (cons nil line) :static)))))

(defun disassembly-line-n-inspector (f n)
  (unless (< (decf n) 0)
    (let ((line (aref (disasm-info f) n)))
      (and (consp line)
	   (car line)
	   (make-inspector (car line))))))

(defun disassembly-line-n-strings (f n)
  (if (< (decf n) 0)
    (values "Disassembly:" nil)
    (let ((line (aref (disasm-info f) n)))
      (if (consp line)
        (destructuring-bind (object label instr) line
          (declare (ignore object))
          (unless (stringp label)
            (setq label (with-output-to-string (stream)
                          (prin1-disassembly-label f stream label))))
          (values label instr))
        (values nil line)))))

(defun set-disassembly-line-n (f n new-value &optional 
                                 (function (inspector-object f)))
  (declare (ignore new-value function))
  (setf-line-n-out-of-range f n))

(defmethod prin1-label ((f function-inspector) stream value &optional data type)
  (declare (ignore value type))
  (if (atom data)                      ; not a disassembly line
    (call-next-method)
    (prin1-disassembly-label f stream (car data))))

(defun prin1-disassembly-label (f stream label)
  (let* ((pc label)
         (label-p (and (consp pc) (setq pc (cadr pc))))
         (pc-mark (pc f))
         (pc-width (pc-width f)))
    (when pc
      (write-char (if (eql pc pc-mark) #\* #\Space) stream)
      (format stream "~@[L~d~]~vT~v<[~d]~> " label-p (+ pc-width 3) (+ pc-width 2) pc))))

#+x86-target
(defmethod prin1-value ((f function-inspector) stream value &optional data type)
  (declare (ignore value type))
  (if (atom data) ;; not a disassembly line
    (call-next-method)
    (princ (cdr data) stream)))


#+ppc-target
(defmethod prin1-value ((f function-inspector) stream value &optional label type)
  (if (atom label)                      ; not a disassembly line
    (unless (eq (if (consp type) (car type) type) :comment)
      (call-next-method))
    (let ((q (cdr label)))
      (write-char #\( stream)
      (loop (if (null q) (return))
        (ccl::disasm-prin1 (pop q) stream)
        (if q (write-char #\space stream)))
      (write-char #\) stream)))
  value)

;; Generic-functions
;; Display the list of methods on a line of its own to make getting at them faster
;; (They're also inside the dispatch-table which is the first immediate in the disassembly).
(defclass gf-inspector (function-inspector)
  ((method-count :accessor method-count)))

(defmethod inspector-class ((f standard-generic-function))
  (if (functionp f) 
    'gf-inspector
    'standard-object-inspector))

(defmethod compute-line-count :before ((f gf-inspector))
  (let* ((gf (inspector-object f))
         (count (length (generic-function-methods gf))))
    (setf (method-count f) count)))

(defmethod header-count ((f gf-inspector))
  (+ (standard-header-count f) 1 (method-count f)))

(defmethod line-n ((f gf-inspector) n)
  (let* ((count (method-count f))
	 (methods (generic-function-methods (inspector-object f))))
    (cond ((< (decf n  (standard-header-count f)) 0)
           (call-next-method))
          ((< (decf n) 0)
	   (values methods "Methods: " :comment))
          ((< n count)
	   (values (nth n methods) nil :static))
          (t (disassembly-line-n f (- n count))))))

(defmethod (setf line-n) (new-value (f gf-inspector) n)
  (let* ((count (method-count f))
         (en n))
    (cond ((< (decf n (standard-header-count f)) 0)
           (call-next-method))
          ((< (decf n) count)
           (setf-line-n-out-of-range f en))
          (t (set-disassembly-line-n f (- n count) new-value)))))

#|
(defmethod inspector-commands ((f gf-inspector))
  (let* ((function (inspector-object f))
         (method (selected-object (inspector-view f))))
    (if (typep method 'method)
      (nconc
       (call-next-method)
       `(("Remove method"
         ,#'(lambda ()
              (remove-method function method)
              (resample-it)))))
      (call-next-method))))
|#

;;;;;;;
;;
;; Structures
;;
(defmethod inspector-class ((s structure-object))
  'usual-basics-first-inspector)

(defun structure-slots (s)
  (let ((slots (ccl::sd-slots (ccl::struct-def s))))
    (if (symbolp (caar slots))
      slots
      (cdr slots))))

(defmethod compute-line-count ((s structure-object))
  (length (structure-slots s)))

(defmethod line-n ((s structure-object) n)
  (let ((slot (nth n (structure-slots s))))
    (if slot
      (values (uvref s (ccl::ssd-offset slot)) (ccl::ssd-name slot) :colon)
      (line-n-out-of-range s n))))

(defmethod (setf line-n) (new-value (s structure-object) n)
  (let ((slot (nth n (structure-slots s))))
    (if slot
      (setf (uvref s (ccl::ssd-offset slot)) new-value)
      (setf-line-n-out-of-range s n))))


(defclass basic-stream-inspector (uvector-inspector) ())

(defmethod inspector-class ((bs ccl::basic-stream)) 'basic-stream-inspector)
  
;;;;;;;
;;
;; packages
;;
(defclass package-inspector (uvector-inspector) ())

(defmethod inspector-class ((p package)) 'package-inspector)

(defmethod compute-line-count ((i package-inspector))
  (+ 2 (call-next-method)))

(defmethod line-n ((i package-inspector) n)
  (cond ((eql n 0) (values (ccl::%pkgtab-count (ccl::pkg.itab (inspector-object i)))
                           "Internal Symbols: " :static))
        ((eql n 1) (values (ccl::%pkgtab-count (ccl::pkg.etab (inspector-object i)))
                           "External Symbols: " :static))
        (t (call-next-method i (- n 2)))))

(defmethod (setf line-n) (new-value (i package-inspector) n)
  (if (< n 2)
    (setf-line-n-out-of-range i n)
    (call-next-method new-value i (- n 2))))

(defmethod inspector-commands ((i package-inspector))
  `(("Inspect all packages" ,#'(lambda () (inspect (list-all-packages))))
    (,(format nil "(setq *package* '~a" (inspector-object i))
     ,#'(lambda () (setq *package* (inspector-object i))))))

;;;;;;;
;;
;; Records
;;
(defclass record-inspector (object-first-inspector)
  ((record-type :accessor record-type)
   (field-names :accessor field-names)
   (unlock :initform nil :accessor unlock)))

(defmethod inspector-class ((o macptr))
  'record-inspector)


;;; Still needs work.
;;; Lots of work.
(defclass thread-inspector (uvector-inspector) ())

(defmethod inspector-class ((thread ccl::lisp-thread))
  'thread-inspector)

(defmethod compute-line-count :before ((i thread-inspector))
)

(defmethod line-n ((thread thread-inspector) n)
  (declare (ignorable n))
  (call-next-method)
)

#|
(defmethod line-n-inspector ((i thread-inspector) n value label type)
  (declare (ignore n type))
  (or (and value
           (macptrp value)
           (not (%null-ptr-p value)))
      (call-next-method)))
|#


(defmethod line-n-inspector (i n value label type)
  (declare (ignore i n label type))
  (make-inspector value))

(defmethod line-n-inspector ((i usual-inspector) n value label type)
  (let ((object (inspector-object i)))
    (if (typep object 'usual-inspector)
      (make-inspector value)
      (line-n-inspector (inspector-object i) n value label type))))




;;;;;;;
;;
;; an ERROR-FRAME stores the stack addresses that the backtrace window displays
;;

;; set to list of function you don't want to see
;; Functions can be symbols, nil for kernel, or #'functions
(defparameter *backtrace-internal-functions*  
  (list :kernel))

(defvar *backtrace-hide-internal-functions-p* t)

(defclass error-frame ()
  ((addresses :accessor addresses)
   (restart-info :accessor restart-info)
   (stack-start :initarg :stack-start  :reader stack-start)
   (stack-end :initarg :stack-end :reader stack-end)
   (tcr :initarg :tcr :initform (ccl::%current-tcr) :reader tcr)
   (context :initarg :context :reader context)
   (frame-count :accessor frame-count)
   (ignored-functions :accessor ignored-functions
                      :initform (and *backtrace-hide-internal-functions-p*
                                     *backtrace-internal-functions*))
   (break-condition :accessor break-condition
                    :initarg :break-condition)
   (unavailable-value-marker :initform (cons nil nil)
                             :accessor unavailable-value-marker)))
  


(defmethod initialize-instance ((f error-frame) &key)
  (call-next-method)
  (initialize-addresses f))

(defmethod initialize-addresses ((f error-frame))
  (let* ((addresses (coerce (ccl::%stack-frames-in-context (context f)) 'vector)))
      (setf (frame-count f) (length addresses)
            (addresses f) addresses)))

(defmethod compute-frame-info ((f error-frame) n)
  (let* ((frame (aref (addresses f) n))
         (context (context f))
         (marker (unavailable-value-marker f)))
    
    (multiple-value-bind (lfun pc) (ccl::cfp-lfun frame)
      (multiple-value-bind (args locals) (ccl::arguments-and-locals context frame lfun pc marker)
        (list (ccl::arglist-from-map lfun) args locals)))))

(defun print-error-frame-limits (f stream)
  (format stream "#x~x - #x~x" (stack-start f) (stack-end f)))

(defmethod print-object ((f error-frame) stream)
  (print-unreadable-object (f stream :type 'frame-ptr)
    (print-error-frame-limits f stream)))



;;;;;;;
;;
;; The inspector for error-frame objects
;;



;;; The "vsp-range" and "tsp-range" slots have to do with
;;; recognizing/validating stack-allocated objects
(defclass stack-inspector (inspector)
  ((vsp-range :accessor vsp-range :initarg :vsp-range)
   (tsp-range :accessor tsp-range :initarg :tsp-range)
   (csp-range :accessor csp-range :initarg :csp-range)))



                           
(defmethod initialize-instance ((i stack-inspector) &rest initargs &key context)
  (declare (dynamic-extent initargs))
  (let* ((start (ccl::child-frame (ccl::parent-frame (ccl::bt.youngest context) context) context))
         (end (ccl::child-frame (ccl::parent-frame (ccl::bt.oldest context) context) context))
         (tcr (ccl::bt.tcr context)))
    (apply #'call-next-method
           i
           :object 
           (make-instance 'error-frame
             :stack-start start
             :stack-end end
             :tcr tcr
             :context context
             :break-condition (ccl::bt.break-condition context))
           #-arm-target
           :tsp-range #-arm-target (ccl::make-tsp-stack-range tcr context)
           :vsp-range (ccl::make-vsp-stack-range tcr context)
           :csp-range (ccl::make-csp-stack-range tcr context)
           initargs)))

(defmethod print-object ((i stack-inspector) stream)
  (print-unreadable-object (i stream :type 'stack-inspector)
    (print-error-frame-limits (inspector-object i) stream)))

(defmethod addresses ((f stack-inspector))
  (addresses (inspector-object f)))

(defmethod compute-line-count ((f stack-inspector))
  (frame-count (inspector-object f)))

(defmethod line-n ((f stack-inspector) n)
  (let* ((frame (aref (addresses (inspector-object f)) n)))
    (ccl::cfp-lfun frame)))



 


;;; inspecting a single stack frame
;;; The inspector-object is expected to be an error-frame
(defclass stack-frame-inspector (inspector)
  ((frame-number :initarg :frame-number :initform nil :reader frame-number)
   (frame-info :initform nil :accessor frame-info)))


(defmethod initialize-instance ((i stack-frame-inspector) &rest initargs &key
                                object frame-number)
  (declare (dynamic-extent initargs))
  (setq object (require-type object 'error-frame))
  (apply #'call-next-method i 
         :object object
         initargs)
  (setf (frame-number i) frame-number))

    

(defmethod compute-line-count ((i stack-frame-inspector))
  (let ((frame-number (frame-number i)))
    (if (null frame-number)
      0
      (let* ((error-frame (inspector-object i))
             (frame-info (or (frame-info i)
                             (setf (frame-info i) (compute-frame-info error-frame frame-number)))))
        (destructuring-bind (args locals) (cdr frame-info)
          (+ 1 (length args) 1 (length locals)))))))

(defmethod line-n ((i stack-frame-inspector) n)
  (unless (< -1 n (inspector-line-count i))
    (line-n-out-of-range i n))
  (destructuring-bind (arglist args locals) (frame-info i)
    (if (zerop n)
      (values arglist nil :static)
      (let* ((nargs (length args)))
        (decf n)
        (if (< n nargs)
          (cons :arg (nth n args))
          (progn
            (decf n nargs)
            (if (zerop n)
              nil
              (cons :local (nth (1- n) locals)))))))))

(defmethod (setf line-n) (value (i stack-frame-inspector) n)
  (declare (ignorable value n))
  (error "not yet!"))

        



(defmethod prin1-value ((i stack-frame-inspector) stream value &optional label type)
  (declare (ignore label type))
  (when value
    (if (or (atom value) (not (typep (car value) 'keyword)))
      (prin1 value stream)
      (progn
        (if (eq (car value) :arg)
          (format stream "   ")
          (format stream "  "))
        (when (cdr value)
          (destructuring-bind (label . val) (cdr value)
            (format stream "~a: " label)
            (if (eq val *unbound-marker*)
              (format stream "??")
              (prin1 val stream))))))))

(defmethod (setf frame-number) (frame-number (i stack-frame-inspector))
  (let ((max (1- (frame-count (inspector-object i)))))
    (unless (or (null frame-number)
                (and (<= 0 frame-number max)))
      (setq frame-number (require-type frame-number `(or null (integer 0 ,max))))))
  (unless (eql frame-number (frame-number i))
    (setf (slot-value i 'frame-number) frame-number)
    (setf (inspector-line-count i) nil)
    frame-number))

;;; Inspector


(defvar *inspector-ui* ())
(defvar *previous-inspector-ui* nil)

(defclass inspector-ui ()
    ((inspector :initarg :inspector :accessor inspector-ui-inspector)
     (level :initarg :level :accessor inspector-ui-level)))

(defclass inspector-tty-ui (inspector-ui)
    ((origin :initarg :origin :initform 0 :accessor inspector-tty-ui-origin)
     (pagesize :initarg :pagesize :initform 20 :accessor
	       inspector-tty-ui-pagesize)))

(defmethod ui-initialize ((ui inspector-tty-ui)))

(defmethod ui-present ((ui inspector-tty-ui))
  (let* ((inspector (inspector-ui-inspector ui)))
    (with-errorfree-printing
	(let* ((stream *debug-io*)
	       (origin (inspector-tty-ui-origin ui))
	       (pagesize (inspector-tty-ui-pagesize ui))
	       (page-end (+ origin pagesize))
	       (n (compute-line-count inspector))
	       (end (min page-end n))
	       (tag -1)
	       (*print-pretty* (or *print-pretty* *describe-pretty*))
	       (*print-length* 5)
	       (*print-level* 5)
	       (func #'(lambda (i index child &optional label-string value-string)
			 (declare (ignore i))
			 (when child (incf tag))
			 (unless (< index origin)
			   (format stream "~@[[~d]~]~8t" (and child tag))
			   (format-line-for-tty stream label-string value-string)
			   (terpri stream)))))
	  (declare (dynamic-extent func))
	  (map-lines inspector func 0 end)))
    (values)))

(ccl::define-toplevel-command
    :tty-inspect i (n)
    "inspect <n>th item"
    (inspector-ui-inspect-nth *inspector-ui* n))

(ccl::define-toplevel-command
    :tty-inspect pop ()
    "exit current inspector level"
    (invoke-restart 'exit-inspector))

(ccl::define-toplevel-command
    :tty-inspect q ()
    "exit inspector"
  (invoke-restart 'end-inspect))

(ccl::define-toplevel-command
    :tty-inspect show ()
    "re-show currently inspected object (the value of CCL:@)"
    (ui-present *inspector-ui*))

(defmethod inspector-ui-next-page ((ui inspector-tty-ui))
  (let* ((nlines (compute-line-count (inspector-ui-inspector ui)))
	 (origin (inspector-tty-ui-origin ui))
	 (page-size (inspector-tty-ui-pagesize ui))
	 (new-origin (+ origin page-size)))
    (if (< new-origin nlines)
      (setf (inspector-tty-ui-origin ui) new-origin))
    (ui-present ui)))
    
(ccl::define-toplevel-command
    :tty-inspect next ()
    "show next page of object data"
    (inspector-ui-next-page *inspector-ui*))

(defmethod inspector-ui-prev-page ((ui inspector-tty-ui))
  (let* ((origin (inspector-tty-ui-origin ui))
	 (page-size (inspector-tty-ui-pagesize ui))
	 (new-origin (max 0 (- origin page-size))))
    (setf (inspector-tty-ui-origin ui) new-origin)
    (ui-present ui)))

(ccl::define-toplevel-command
    :tty-inspect prev ()
    "show previous page of object data"
    (inspector-ui-prev-page *inspector-ui*))

(ccl::define-toplevel-command
    :tty-inspect home ()
    "show first page of object data"
    (progn
      (setf (inspector-tty-ui-origin *inspector-ui*) 0)
      (ui-present *inspector-ui*)))

(ccl::define-toplevel-command
    :tty-inspect s (n v)
    "set the <n>th line of object data to value <v>"
    (let* ((ui *inspector-ui*))
      (setf (line-n (inspector-ui-inspector ui) n) v)
      (ui-present ui)))


(defmethod ui-interact ((ui inspector-tty-ui))
  (let* ((level (inspector-ui-level ui))
         (ccl::*default-integer-command* `(:i 0 ,(1- (compute-line-count (inspector-ui-inspector ui))))))
    (declare (special ccl::*default-integer-command*))
    (restart-case
        (ccl:with-terminal-input
          (ccl::with-toplevel-commands :tty-inspect
            (ccl::read-loop
             :prompt-function #'(lambda (stream)
                                  (if (eql level 0)
                                    (format stream "~&Inspect> ")
                                    (format stream "~&Inspect ~d> " level))))))
      (exit-inspector ()
        (if *previous-inspector-ui*
          (ui-present *previous-inspector-ui*)
          (terpri *debug-io*))))))

(defmethod inspector-ui-inspect-nth ((ui inspector-tty-ui) n)
  (let* ((inspector (inspector-ui-inspector ui))
	 (new-inspector (block nil
			  (let* ((tag -1)
				 (func #'(lambda (i index child &rest strings)
					   (declare (ignore i index strings))
					   (when (and child (eql (incf tag) n)) (return child)))))
			    (declare (dynamic-extent func))
			    (map-lines inspector func))))
	 (ccl::@ (inspector-object new-inspector)))
    (inspector-ui-inspect
     (make-instance 'inspector-tty-ui
       :level (1+ (inspector-ui-level ui))
       :inspector new-inspector))))

(defparameter *default-inspector-ui-class-name* 'inspector-tty-ui)

(defmethod inspector-ui-inspect ((ui inspector-ui))
  (let* ((*previous-inspector-ui* *inspector-ui*)
         (*inspector-ui* ui))
    (ui-initialize ui)
    (ui-present ui)
    (ui-interact ui)
    (values)))

(defun tty-inspect (thing)
  (inspector-ui-inspect (make-instance *default-inspector-ui-class-name*
                                       :inspector (make-inspector thing)
					 :level 0)))

(defparameter *default-inspector-ui-creation-function* (lambda (thing)
                                                         (ccl::application-ui-operation *application* :inspect thing)))

(defmethod ccl::ui-object-do-operation ((o ccl::ui-object) (operation (eql :inspect)) &rest args)
  (apply #'tty-inspect args))

(defun inspect (thing)
  (let* ((ccl::@@@ nil)
         (ccl::@@ nil)
         (ccl::@ thing))
    (restart-case (funcall *default-inspector-ui-creation-function* thing)
      (end-inspect () thing))))

(defmethod note-inspecting-item ((i inspector))
  (setq ccl::@@@ ccl::@@
        ccl::@@ ccl::@
        ccl::@ (inspector-object i)))
