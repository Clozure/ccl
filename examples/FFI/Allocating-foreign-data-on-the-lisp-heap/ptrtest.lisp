(defun message (string)
  (format t "~a~%~%" string)
  (force-output))

;; Setup
(message "*** Building the shared library")
(run-program (namestring
	      (translate-logical-pathname #P"ccl:examples;FFI;Allocating-foreign-data-on-the-lisp-heap;ptrtest-compile.sh"))
	     (list
	      (namestring
	       (translate-logical-pathname #P"ccl:examples;FFI;Allocating-foreign-data-on-the-lisp-heap")))
	     :output t)

;; make-heap-ivector courtesy of Gary Byers
; This is now predefined by OpenMCL
#|(defun make-heap-ivector (element-count element-type)
  (let* ((subtag (ccl::element-type-subtype element-type)))
    (unless (= (logand subtag target::fulltagmask)
	       target::fulltag-immheader)
      (error "~s is not an ivector subtype." element-type))
    (let* ((size-in-bytes (ccl::subtag-bytes subtag element-count)))
      (ccl::%make-heap-ivector subtag size-in-bytes element-count))))|#

;; dispose-heap-ivector created for symmetry
; This is now predefined by OpenMCL but the example uses a different definition so we'll change the name
(defmacro my-dispose-heap-ivector (a mp)
  `(progn
     (ccl::%dispose-heap-ivector ,a)
     ;; Demolish the arguments for safety
     (setf ,a nil)
     (setf ,mp nil)))

;; Create an array of 3 4-byte-long integers
(multiple-value-bind (la lap)
    (make-heap-ivector 3 '(unsigned-byte 32))
  (setq a la)
  (setq ap lap))

(message (format nil "a: ~a~%" a))
(message (format nil "ap: ~a~%" ap))
(message (format nil "(aref a 2): ~a~%" (aref a 2)))
(message "Setting values of a to #(3 4 5)")
(setf (aref a 0) 3)
(setf (aref a 1) 4)
(setf (aref a 2) 5)
(message (format nil "a: ~a~%" a))

(setq *byte-length-of-long* 4)
(message (format nil
		 "(%get-signed-long ap (* 2 *byte-length-of-long*)): ~a~%"
		 (%get-signed-long ap (* 2 *byte-length-of-long*))))
(message (format nil
		 "(%get-signed-long ap (* 0 *byte-length-of-long*)): ~a~%"
		 (%get-signed-long ap (* 0 *byte-length-of-long*))))
(message "Setting values of ap to (setf (%get-signed-long ap (* 0 *byte-length-of-long*)) 6) and (setf (%get-signed-long ap (* 2 *byte-length-of-long*)) 7)~%")
(setf (%get-signed-long ap (* 0 *byte-length-of-long*)) 6)
(setf (%get-signed-long ap (* 2 *byte-length-of-long*)) 7)
;; Show that a actually got changed through ap
(message (format nil "a: ~a~%" a))

;; Insert the full path to your copy of libptrtest.dylib
(message "*** Loading the shared library")
(open-shared-library (namestring
		      (translate-logical-pathname #P"ccl:examples;FFI;Allocating-foreign-data-on-the-lisp-heap;libptrtest.dylib")))

(message (format nil "a: ~a~%" a))
(message (format nil "ap: ~a~%" ap))

(message "Calling: (external-call \"_reverse_int_array\" :address ap :unsigned-int (length a) :address)")
(external-call "_reverse_int_array" :address ap :unsigned-int (length a) :address)

(message (format nil "a: ~a~%" a))
(message (format nil "ap: ~a~%" ap))

(message "Calling: (my-dispose-heap-ivector a ap)")
(my-dispose-heap-ivector a ap)

(message (format nil "a: ~a~%" a))
(message (format nil "ap: ~a~%" ap))

#|
(defclass wrapper (whatever)
  ((element-type :initarg :element-type)
   (element-count :initarg :element-count)
   (ivector)
   (macptr)))

(defmethod initialize-instance ((wrapper wrapper) &rest initargs)
  (declare (ignore initargs))
  (call-next-method)
  (ccl:terminate-when-unreachable wrapper)
  (with-slots (ivector macptr element-type element-count) wrapper
    (multiple-value-bind (new-ivector new-macptr)
	(make-heap-ivector element-count element-type)
      (setq ivector new-ivector
	    macptr new-macptr))))

(defmethod ccl:terminate ((wrapper wrapper))
  (with-slots (ivector macptr) wrapper
    (when ivector
      (dispose-heap-ivector ivector macptr)
      (setq ivector nil
	    macptr nil))))
|#