;; -*- Mode:Lisp; tab-width:2; indent-tabs-mode:nil -*-

#-ccl (error "This code uses the Clozure CL code coverage tool")

(in-package :code-cover-test)

(require :cl-ppcre-test)

(defparameter *output-directory-path* #P"~/tmp/code-cover-test/")
(defparameter *state-file-name*  #P"covstate.dat")
(defparameter *index-file-path* #P"html/index.html")

(defun output-path (filename)
  (merge-pathnames filename *output-directory-path*))

(defun state-file-path ()
  (output-path *state-file-name*))

(defun index-file-path ()
  (output-path *index-file-path*))

(defvar *code-coverage-table* nil "Collect incremental data from compiler code coverage tool")

;; Wild guess at expected # of table entries (??)
(defvar *code-coverage-entry-size* 1800.)

(defun init-code-coverage-table (&key (size *code-coverage-entry-size*))
  (make-hash-table :size size))

(defun compile-code-coverage (&optional undo)
  (let ((ccl:*compile-code-coverage* (not undo))
        (*load-verbose* t)
        (*compile-verbose* t))
    (asdf:oos 'asdf:load-op ':cl-ppcre-test :force '(:cl-ppcre-test :cl-ppcre))
    (unless undo (ccl:save-coverage-in-file (state-file-path)))
    t))

(let ((tests-compile-p-default t))
  (defun init-code-coverage (&key (compile-p tests-compile-p-default))
    (setf tests-compile-p-default compile-p) ;save flag for next time
    (setf *code-coverage-table* (init-code-coverage-table))
    ;; Compile sources files or restore coverage data from file
    (if compile-p
        (compile-code-coverage)
        (ccl:restore-coverage-from-file (state-file-path)))
    ;; Returns
    t))

(let ((counter 0.))
  (defun make-code-coverage-tag (sym)
    (intern (format nil "CODE-COVER-TEST-~d-~a"  (incf counter) sym) ':keyword)))

(defun run-all-tests-with-code-coverage (&key (compile-p nil compile-p-supplied-p) verbose (iterations 25.))
  (apply #'init-code-coverage
         (and compile-p-supplied-p (list :compile-p compile-p)))
  (let ((successp t))
    (macrolet ((run-test-suite (&body body)
                 (let ((tag (caar body)))
                   `(prog1
                        (unless (progn ,@body)
                          (setq successp nil))
                      (setf (gethash (make-code-coverage-tag ',tag)
                                     *code-coverage-table*)
                            (ccl:get-incremental-coverage))))))
      ;; run the automatically generated Perl tests
      (run-test-suite (perl-test :verbose verbose))
      (run-test-suite (test-optimized-test-functions :verbose verbose))
      (dotimes (n iterations)
        (run-test-suite (simple-tests :verbose verbose)))
      ;; Returns
      successp)))

(defun report-code-coverage-test (&optional (state *code-coverage-table*))
  ;; Delete code coverage report output files *.html, *.js
  (dolist (type '("js" "html"))
    (dolist (file (directory (output-path (make-pathname :name ':wild :type type))))
      (delete-file file)))
  (ccl:report-coverage (index-file-path) :tags state))
