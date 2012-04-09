;; -*- Mode:Lisp; tab-width:2; indent-tabs-mode:nil -*-

;; Run tests and generate code coverage results 

#-ccl (error "This code uses the Clozure CL code coverage tool")

(in-package :code-cover-test)

;; Output files

(defparameter *output-directory-path* #P"~/tmp/code-cover-test/")
(defparameter *state-file-name*  #P"covstate.dat")
(defparameter *index-file-path* #P"html/index.html")

(defun output-path (filename)
  (let ((path (merge-pathnames filename *output-directory-path*)))
    (ensure-directories-exist path)
    path))

(defun state-file-path ()
  (output-path *state-file-name*))

(defun index-file-path ()
  (output-path *index-file-path*))

(defvar *code-coverage-table* nil "Collect incremental data from compiler code coverage tool")

;; Wild guess at expected # of table entries (??)

(defvar *code-coverage-entry-size* 1800.)

(defun init-code-coverage-table (&key (size *code-coverage-entry-size*))
  (make-hash-table :size size))

;; Base class for tests

(defclass code-cover-test ()
  ((systems :initform nil :initarg :systems :accessor systems-of)))

;; Compile unit tests with code coverage analysis (maybe) enabled

(defmethod compile-code-coverage ((test code-cover-test) &key (compile-code-coverage-p t))
  (let ((ccl:*compile-code-coverage* compile-code-coverage-p)
        (*load-verbose* t)
        (*compile-verbose* t))
    (with-slots (systems) test
      (if (and systems (atom systems))
          (setq systems (list systems)))
      (asdf:operate 'asdf:compile-op (first systems) :force systems)
      (ccl:save-coverage-in-file (state-file-path))
      t)))

(defmethod init-code-coverage ((test code-cover-test) &key
                               (compile-p t) (reset-p compile-p) (restore-p (not compile-p)))
  (setf *code-coverage-table* (init-code-coverage-table))
  ;; Maybe reset code coverage data
  (if reset-p
      (ccl:reset-coverage))
  ;; Maybe restore coverage data from file
  (if restore-p
      (ccl:restore-coverage-from-file (state-file-path)))
  ;; Maybe compile source files
  (if compile-p
      (compile-code-coverage test))
  ;; Returns
  nil)

;; Tags for results display

(defvar *verbose-tag-names* nil)

(let ((counter 0.))
  (defun make-code-coverage-tag (sym &key (verbose *verbose-tag-names*))
    (intern (format nil "~:[~*~;CODE-COVER-TEST-~d-~]~a"
                    verbose (incf counter)
                    sym) ':keyword)))

;; Running tests

(defvar *current-test*)

(defmacro do-test (tag &body body)
  (let ((tag-form
         (etypecase tag
           (null (gentemp "TEST"))
           (symbol (list 'quote tag))
           (t tag))))
    `(do-test-body *current-test* ,tag-form
                   (lambda () ,@body))))

(defmethod do-test-body ((test code-cover-test) tag fcn)
  (funcall fcn)
  (setf (gethash (make-code-coverage-tag tag)
                 *code-coverage-table*)
        (ccl:get-incremental-coverage)))

(defmethod do-tests :around ((test code-cover-test) &rest args)
  (let ((*current-test* test)
        (*compile-code-coverage* t))
    (apply #'call-next-method test args)))

(defmethod do-tests :before ((test code-cover-test) &rest args)
  (apply #'init-code-coverage test args))

;; Generating formatted results

(defun report-code-coverage-test (&optional (state *code-coverage-table*))
  ;; Delete code coverage report output files *.html, *.js
  (dolist (type '("js" "html"))
    (dolist (file
              (directory
               (output-path
                (make-pathname :directory '(:relative "html") :name ':wild :type type))))
      (delete-file file)))
  (ccl:report-coverage (index-file-path) :tags state))
