;;; This is asdf: Another System Definition Facility. 
;;; hash - $Format:%H$
;;;
;;; Local Variables:
;;; mode: lisp
;;; End:
;;;
;;; Feedback, bug reports, and patches are all welcome: please mail to
;;; <asdf-devel@common-lisp.net>.  But note first that the canonical
;;; source for asdf is presently on common-lisp.net at
;;; <URL:http://common-lisp.net/project/asdf/>
;;;
;;; If you obtained this copy from anywhere else, and you experience
;;; trouble using it, or find bugs, you may want to check at the
;;; location above for a more recent version (and for documentation
;;; and test files, if your copy came without them) before reporting
;;; bugs.  There are usually two "supported" revisions - the git HEAD
;;; is the latest development version, whereas the revision tagged
;;; RELEASE may be slightly older but is considered `stable'

;;; -- LICENSE START
;;; (This is the MIT / X Consortium license as taken from 
;;;  http://www.opensource.org/licenses/mit-license.html on or about
;;;  Monday; July 13, 2009)
;;;
;;; Copyright (c) 2001-2009 Daniel Barlow and contributors
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; -- LICENSE END

;;; the problem with writing a defsystem replacement is bootstrapping:
;;; we can't use defsystem to compile it.  Hence, all in one file

#+xcvb (module ())

(defpackage #:asdf
  (:documentation "Another System Definition Facility")
  (:export #:defsystem #:oos #:operate #:find-system #:run-shell-command
           #:system-definition-pathname #:find-component ; miscellaneous
	   #:compile-system #:load-system #:test-system
           #:compile-op #:load-op #:load-source-op
           #:test-op
           #:operation		 ; operations
           #:feature		 ; sort-of operation
           #:version		 ; metaphorically sort-of an operation

           #:input-files #:output-files #:perform ; operation methods
           #:operation-done-p #:explain

           #:component #:source-file
           #:c-source-file #:cl-source-file #:java-source-file
           #:static-file
           #:doc-file
           #:html-file
           #:text-file
           #:source-file-type
           #:module                     ; components
           #:system
           #:unix-dso

           #:module-components          ; component accessors
           #:component-pathname
           #:component-relative-pathname
           #:component-name
           #:component-version
           #:component-parent
           #:component-property
           #:component-system

           #:component-depends-on

           #:system-description
           #:system-long-description
           #:system-author
           #:system-maintainer
           #:system-license
           #:system-licence
           #:system-source-file
           #:system-relative-pathname
	   #:map-systems

           #:operation-on-warnings
           #:operation-on-failure

					;#:*component-parent-pathname*
           #:*system-definition-search-functions*
           #:*central-registry*         ; variables
           #:*compile-file-warnings-behaviour*
           #:*compile-file-failure-behaviour*
           #:*asdf-revision*
	   #:*resolve-symlinks*

           #:operation-error #:compile-failed #:compile-warned #:compile-error
           #:error-component #:error-operation
           #:system-definition-error
           #:missing-component
	   #:missing-component-of-version
           #:missing-dependency
           #:missing-dependency-of-version
           #:circular-dependency        ; errors
           #:duplicate-names

	   #:try-recompiling
           #:retry
           #:accept                     ; restarts
	   #:coerce-entry-to-directory
	   #:remove-entry-from-registry

           #:standard-asdf-method-combination
           #:around                     ; protocol assistants
	   
	   #:*source-to-target-mappings*
	   #:*default-toplevel-directory*
	   #:*centralize-lisp-binaries*
	   #:*include-per-user-information*
	   #:*map-all-source-files*
	   #:output-files-for-system-and-operation
	   #:*enable-asdf-binary-locations*
	   #:implementation-specific-directory-name)
  (:use :cl))


#+nil
(error "The author of this file habitually uses #+nil to comment out ~
        forms. But don't worry, it was unlikely to work in the New ~
        Implementation of Lisp anyway")

(in-package #:asdf)

(defvar *asdf-revision* 
  ;; the 1+ hair is to ensure that we don't do an inadvertant find and replace
  (subseq "REVISION:1.366" (1+ (length "REVISION"))))
  

(defvar *resolve-symlinks* t
  "Determine whether or not ASDF resolves symlinks when defining systems.

Defaults to `t`.")

(defvar *compile-file-warnings-behaviour* :warn)

(defvar *compile-file-failure-behaviour* #+sbcl :error #-sbcl :warn)

(defvar *verbose-out* nil)

(defparameter +asdf-methods+
  '(perform explain output-files operation-done-p))

(define-method-combination standard-asdf-method-combination ()
  ((around-asdf (around))
   (around (:around))
   (before (:before))
   (primary () :required t)
   (after (:after)))
  (flet ((call-methods (methods)
           (mapcar #'(lambda (method)
                       `(call-method ,method))
                   methods)))
    (let* ((form (if (or before after (rest primary))
                     `(multiple-value-prog1
                          (progn ,@(call-methods before)
                                 (call-method ,(first primary)
                                              ,(rest primary)))
                        ,@(call-methods (reverse after)))
                     `(call-method ,(first primary))))
           (standard-form (if around
                              `(call-method ,(first around)
                                            (,@(rest around)
                                               (make-method ,form)))
                              form)))
      (if around-asdf
          `(call-method ,(first around-asdf)
                        (,@(rest around-asdf) (make-method ,standard-form)))
          standard-form))))

(setf (documentation 'standard-asdf-method-combination 
		     'method-combination)
      "This method combination is based on the standard method combination,
but defines a new method-qualifier, `asdf:around`.  `asdf:around`
methods will be run *around* any `:around` methods, so that the core
protocol may employ around methods and those around methods will not
be overridden by around methods added by a system developer.")

(defgeneric perform (operation component)
  (:method-combination standard-asdf-method-combination))
(defgeneric operation-done-p (operation component)
  (:method-combination standard-asdf-method-combination))
(defgeneric explain (operation component)
  (:method-combination standard-asdf-method-combination))
(defgeneric output-files (operation component)
  (:method-combination standard-asdf-method-combination))
(defgeneric input-files (operation component)
  (:method-combination standard-asdf-method-combination))

(defgeneric system-source-file (system)
  (:documentation "Return the source file in which system is defined."))

(defgeneric component-system (component)
  (:documentation "Find the top-level system containing COMPONENT"))

(defgeneric component-pathname (component)
  (:documentation "Extracts the pathname applicable for a particular component."))

(defgeneric component-relative-pathname (component)
  (:documentation "Extracts the relative pathname applicable for a particular component."))

(defgeneric component-property (component property))

(defgeneric (setf component-property) (new-value component property))

(defgeneric version-satisfies (component version))

(defgeneric find-component (module name &optional version)
  (:documentation "Finds the component with name NAME present in the
MODULE module; if MODULE is nil, then the component is assumed to be a
system."))

(defgeneric source-file-type (component system))

(defgeneric operation-ancestor (operation)
  (:documentation
   "Recursively chase the operation's parent pointer until we get to
the head of the tree"))

(defgeneric component-visited-p (operation component))

(defgeneric visit-component (operation component data))

(defgeneric (setf visiting-component) (new-value operation component))

(defgeneric component-visiting-p (operation component))

(defgeneric component-depends-on (operation component)
  (:documentation
   "Returns a list of dependencies needed by the component to perform
    the operation.  A dependency has one of the following forms:

      (<operation> <component>*), where <operation> is a class
        designator and each <component> is a component
        designator, which means that the component depends on
        <operation> having been performed on each <component>; or

      (FEATURE <feature>), which means that the component depends
        on <feature>'s presence in *FEATURES*.

    Methods specialized on subclasses of existing component types
    should usually append the results of CALL-NEXT-METHOD to the
    list."))

(defgeneric component-self-dependencies (operation component))

(defgeneric traverse (operation component)
  (:documentation 
"Generate and return a plan for performing `operation` on `component`.

The plan returned is a list of dotted-pairs. Each pair is the `cons`
of ASDF operation object and a `component` object. The pairs will be 
processed in order by `operate`."))

(defgeneric output-files-using-mappings (source possible-paths path-mappings)
  (:documentation 
"Use the variable \\*source-to-target-mappings\\* to find
an output path for the source. The algorithm transforms each
entry in possible-paths as follows: If there is a mapping
whose source starts with the path of possible-path, then
replace possible-path with a pathname that starts with the
target of the mapping and continues with the rest of
possible-path. If no such mapping is found, then use the
default mapping.

If \\*centralize-lisp-binaries\\* is false, then the default
mapping is to place the output in a subdirectory of the
source. The subdirectory is named using the Lisp
implementation \(see
implementation-specific-directory-name\). If
\\*centralize-lisp-binaries\\* is true, then the default
mapping is to place the output in subdirectories of
\\*default-toplevel-directory\\* where the subdirectory
structure will mirror that of the source."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility stuff

(defmacro aif (test then &optional else)
  `(let ((it ,test)) (if it ,then ,else)))

(defun pathname-sans-name+type (pathname)
  "Returns a new pathname with same HOST, DEVICE, DIRECTORY as PATHNAME,
and NIL NAME and TYPE components"
  (make-pathname :name nil :type nil :defaults pathname))

(define-modify-macro appendf (&rest args)
  append "Append onto list")

(defun asdf-message (format-string &rest format-args)
  (declare (dynamic-extent format-args))
  (apply #'format *verbose-out* format-string format-args))

(defun split-path-string (s &optional force-directory)
  (check-type s string)
  (let* ((components (split s nil "/"))
         (last-comp (car (last components))))
    (multiple-value-bind (relative components)
        (if (equal (first components) "")
          (values :absolute (cdr components))
          (values :relative components))
      (cond
        ((equal last-comp "")
         (values relative (butlast components) nil))
        (force-directory
         (values relative components nil))
        (t
         (values relative (butlast components) last-comp))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; classes, condiitons

(define-condition system-definition-error (error) ()
  ;; [this use of :report should be redundant, but unfortunately it's not.
  ;; cmucl's lisp::output-instance prefers the kernel:slot-class-print-function
  ;; over print-object; this is always conditions::%print-condition for
  ;; condition objects, which in turn does inheritance of :report options at
  ;; run-time.  fortunately, inheritance means we only need this kludge here in
  ;; order to fix all conditions that build on it.  -- rgr, 28-Jul-02.]
  #+cmu (:report print-object))

(define-condition formatted-system-definition-error (system-definition-error)
  ((format-control :initarg :format-control :reader format-control)
   (format-arguments :initarg :format-arguments :reader format-arguments))
  (:report (lambda (c s)
             (apply #'format s (format-control c) (format-arguments c)))))

(define-condition circular-dependency (system-definition-error)
  ((components :initarg :components :reader circular-dependency-components)))

(define-condition duplicate-names (system-definition-error)
  ((name :initarg :name :reader duplicate-names-name)))

(define-condition missing-component (system-definition-error)
  ((requires :initform "(unnamed)" :reader missing-requires :initarg :requires)
   (parent :initform nil :reader missing-parent :initarg :parent)))

(define-condition missing-component-of-version (missing-component)
  ((version :initform nil :reader missing-version :initarg :version)))

(define-condition missing-dependency (missing-component)
  ((required-by :initarg :required-by :reader missing-required-by)))

(define-condition missing-dependency-of-version (missing-dependency
						 missing-component-of-version)
  ())

(define-condition operation-error (error)
  ((component :reader error-component :initarg :component)
   (operation :reader error-operation :initarg :operation))
  (:report (lambda (c s)
             (format s "~@<erred while invoking ~A on ~A~@:>"
                     (error-operation c) (error-component c)))))
(define-condition compile-error (operation-error) ())
(define-condition compile-failed (compile-error) ())
(define-condition compile-warned (compile-error) ())

(defclass component ()
  ((name :accessor component-name :initarg :name :documentation
         "Component name: designator for a string composed of portable pathname characters")
   (version :accessor component-version :initarg :version)
   (in-order-to :initform nil :initarg :in-order-to)
   ;; XXX crap name
   (do-first :initform nil :initarg :do-first)
   ;; methods defined using the "inline" style inside a defsystem form:
   ;; need to store them somewhere so we can delete them when the system
   ;; is re-evaluated
   (inline-methods :accessor component-inline-methods :initform nil)
   (parent :initarg :parent :initform nil :reader component-parent)
   ;; no direct accessor for pathname, we do this as a method to allow
   ;; it to default in funky ways if not supplied
   (relative-pathname :initarg :pathname)
   (operation-times :initform (make-hash-table )
                    :accessor component-operation-times)
   ;; XXX we should provide some atomic interface for updating the
   ;; component properties
   (properties :accessor component-properties :initarg :properties
               :initform nil)))

;;;; methods: conditions

(defmethod print-object ((c missing-dependency) s)
  (format s "~@<~A, required by ~A~@:>"
          (call-next-method c nil) (missing-required-by c)))

(defun sysdef-error (format &rest arguments)
  (error 'formatted-system-definition-error :format-control 
	 format :format-arguments arguments))

;;;; methods: components

(defmethod print-object ((c missing-component) s)
   (format s "~@<component ~S not found~
             ~@[ in ~A~]~@:>"
          (missing-requires c)
          (when (missing-parent c)
            (component-name (missing-parent c)))))

(defmethod print-object ((c missing-component-of-version) s)
  (format s "~@<component ~S does not match version ~A~
              ~@[ in ~A~]~@:>"
           (missing-requires c)
           (missing-version c)
	   (when (missing-parent c)
	     (component-name (missing-parent c)))))

(defmethod component-system ((component component))
  (aif (component-parent component)
       (component-system it)
       component))

(defmethod print-object ((c component) stream)
  (print-unreadable-object (c stream :type t :identity t)
    (ignore-errors
      (prin1 (component-name c) stream))))

(defclass module (component)
  ((components :initform nil :accessor module-components :initarg :components)
   ;; what to do if we can't satisfy a dependency of one of this module's
   ;; components.  This allows a limited form of conditional processing
   (if-component-dep-fails :initform :fail
                           :accessor module-if-component-dep-fails
                           :initarg :if-component-dep-fails)
   (default-component-class :accessor module-default-component-class
     :initform 'cl-source-file :initarg :default-component-class)))

(defun component-parent-pathname (component)
  (aif (component-parent component)
       (component-pathname it)
       *default-pathname-defaults*))

(defmethod component-relative-pathname ((component module))
  (or (slot-value component 'relative-pathname)
      (multiple-value-bind (relative path)
	  (split-path-string (component-name component) t)
        (make-pathname
         :directory `(,relative ,@path)
         :host (pathname-host (component-parent-pathname component))))))

(defmethod component-pathname ((component component))
  (let ((*default-pathname-defaults* (component-parent-pathname component)))
    (merge-pathnames (component-relative-pathname component))))

(defmethod component-property ((c component) property)
  (cdr (assoc property (slot-value c 'properties) :test #'equal)))

(defmethod (setf component-property) (new-value (c component) property)
  (let ((a (assoc property (slot-value c 'properties) :test #'equal)))
    (if a
        (setf (cdr a) new-value)
        (setf (slot-value c 'properties)
              (acons property new-value (slot-value c 'properties))))))

(defclass system (module)
  ((description :accessor system-description :initarg :description)
   (long-description
    :accessor system-long-description :initarg :long-description)
   (author :accessor system-author :initarg :author)
   (maintainer :accessor system-maintainer :initarg :maintainer)
   (licence :accessor system-licence :initarg :licence
            :accessor system-license :initarg :license)
   (source-file :reader system-source-file :initarg :source-file
		:writer %set-system-source-file)))

;;; version-satisfies

;;; with apologies to christophe rhodes ...
(defun split (string &optional max (ws '(#\Space #\Tab)))
  (flet ((is-ws (char) (find char ws)))
    (nreverse
     (let ((list nil) (start 0) (words 0) end)
       (loop
         (when (and max (>= words (1- max)))
           (return (cons (subseq string start) list)))
         (setf end (position-if #'is-ws string :start start))
         (push (subseq string start end) list)
         (incf words)
         (unless end (return list))
         (setf start (1+ end)))))))

(defmethod version-satisfies ((c component) version)
  (unless (and version (slot-boundp c 'version))
    (return-from version-satisfies t))
  (let ((x (mapcar #'parse-integer
                   (split (component-version c) nil '(#\.))))
        (y (mapcar #'parse-integer
                   (split version nil '(#\.)))))
    (labels ((bigger (x y)
               (cond ((not y) t)
                     ((not x) nil)
                     ((> (car x) (car y)) t)
                     ((= (car x) (car y))
                      (bigger (cdr x) (cdr y))))))
      (and (= (car x) (car y))
           (or (not (cdr y)) (bigger (cdr x) (cdr y)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; finding systems

(defun make-defined-systems-table ()
  (make-hash-table :test 'equal))

(defvar *defined-systems* (make-defined-systems-table))

(defun coerce-name (name)
  (typecase name
    (component (component-name name))
    (symbol (string-downcase (symbol-name name)))
    (string name)
    (t (sysdef-error "~@<invalid component designator ~A~@:>" name))))

(defun system-registered-p (name)
  (gethash (coerce-name name) *defined-systems*))

(defun map-systems (fn)
  "Apply `fn` to each defined system.

`fn` should be a function of one argument. It will be
called with an object of type asdf:system."
  (maphash (lambda (_ datum)
	     (declare (ignore _))
	     (destructuring-bind (_ . def) datum
	       (declare (ignore _))
	       (funcall fn def)))
	   *defined-systems*))

;;; for the sake of keeping things reasonably neat, we adopt a
;;; convention that functions in this list are prefixed SYSDEF-

(defvar *system-definition-search-functions*
  '(sysdef-central-registry-search))

(defun system-definition-pathname (system)
  (let ((system-name (coerce-name system)))
    (or
     (some (lambda (x) (funcall x system-name))
	   *system-definition-search-functions*)
     (let ((system-pair (system-registered-p system-name)))
       (and system-pair
	    (system-source-file (cdr system-pair)))))))

(defvar *central-registry*
  `((directory-namestring *default-pathname-defaults*))
"A list of 'system directory designators' ASDF uses to find systems.

A 'system directory designator' is a pathname or a function 
which evaluates to a pathname. For example:

    (setf asdf:*central-registry*
          (list '*default-pathname-defaults*
                #p\"/home/me/cl/systems/\"
                #p\"/usr/share/common-lisp/systems/\"))
")

(defun directory-pathname-p (pathname)
  "Does `pathname` represent a directory?

A directory-pathname is a pathname _without_ a filename. The three
ways that the filename components can be missing are for it to be `nil`, 
`:unspecific` or the empty string.

Note that this does _not_ check to see that `pathname` points to an
actually-existing directory."
  (flet ((check-one (x)
	   (not (null (member x '(nil :unspecific "")
			      :test 'equal)))))
    (and (check-one (pathname-name pathname))
	 (check-one (pathname-type pathname)))))

#+(or)
;;test
;;?? move into testsuite sometime soon
(every (lambda (p)
	  (directory-pathname-p p))
	(list 
	 (make-pathname :name "." :type nil :directory '(:absolute "tmp"))
	 (make-pathname :name "." :type "" :directory '(:absolute "tmp"))
	 (make-pathname :name nil :type "" :directory '(:absolute "tmp"))
	 (make-pathname :name "" :directory '(:absolute "tmp"))
	 (make-pathname :type :unspecific :directory '(:absolute "tmp"))
	 (make-pathname :name :unspecific :directory '(:absolute "tmp"))
	 (make-pathname :name :unspecific :directory '(:absolute "tmp"))
	 (make-pathname :type "" :directory '(:absolute "tmp"))
	 ))

(defun ensure-directory-pathname (pathname)
  (if (directory-pathname-p pathname)
      pathname
      (make-pathname :defaults pathname
		     :directory (append
				 (pathname-directory pathname)
				 (list (file-namestring pathname)))
		     :name nil :type nil :version nil)))

(defun sysdef-central-registry-search (system)
  (let ((name (coerce-name system))
	(to-remove nil)
	(to-replace nil))
    (block nil
      (unwind-protect
	   (dolist (dir *central-registry*)
	     (let ((defaults (eval dir)))
	       (when defaults
		 (cond ((directory-pathname-p defaults)
			(let ((file (and defaults
					 (make-pathname
					  :defaults defaults :version :newest
					  :name name :type "asd" :case :local)))
                               #+(and (or win32 windows) (not :clisp))
                               (shortcut (make-pathname
                                          :defaults defaults :version :newest
                                          :name name :type "asd.lnk" :case :local)))
			  (if (and file (probe-file file))
			      (return file))
                          #+(and (or win32 windows) (not :clisp))
                          (when (probe-file shortcut)
                            (let ((target (parse-windows-shortcut shortcut)))
                              (when target
                                (return (pathname target)))))))
		       (t
			(restart-case 
			    (let* ((*print-circle* nil)
				   (message 
				    (format nil 
					    "~@<While searching for system `~a`: `~a` evaluated ~
to `~a` which is not a directory.~@:>" 
					    system dir defaults)))
			      (error message))
			  (remove-entry-from-registry ()
			    :report "Remove entry from *central-registry* and continue"
			    (push dir to-remove))
			  (coerce-entry-to-directory ()
			    :report (lambda (s)
				      (format s "Coerce entry to ~a, replace ~a and continue."
					      (ensure-directory-pathname defaults) dir))
			    (push (cons dir (ensure-directory-pathname defaults)) to-replace))))))))
	;; cleanup
	(dolist (dir to-remove)
	  (setf *central-registry* (remove dir *central-registry*)))
	(dolist (pair to-replace)
	  (let* ((current (car pair))
		 (new (cdr pair))
		 (position (position current *central-registry*)))
	    (setf *central-registry*
		  (append (subseq *central-registry* 0 position)
			  (list new)
			  (subseq *central-registry* (1+ position))))))))))

(defun make-temporary-package ()
  (flet ((try (counter)
           (ignore-errors
             (make-package (format nil "~a~D" 'asdf counter)
                           :use '(:cl :asdf)))))
    (do* ((counter 0 (+ counter 1))
          (package (try counter) (try counter)))
         (package package))))

(defun find-system (name &optional (error-p t))
  (let* ((name (coerce-name name))
         (in-memory (system-registered-p name))
         (on-disk (system-definition-pathname name)))
    (when (and on-disk
               (or (not in-memory)
                   (< (car in-memory) (file-write-date on-disk))))
      (let ((package (make-temporary-package)))
        (unwind-protect
             (let ((*package* package))
	       (asdf-message
                "~&~@<; ~@;loading system definition from ~A into ~A~@:>~%"
                ;; FIXME: This wants to be (ENOUGH-NAMESTRING
                ;; ON-DISK), but CMUCL barfs on that.
                on-disk
                *package*)
               (load on-disk))
          (delete-package package))))
    (let ((in-memory (system-registered-p name)))
      (if in-memory
          (progn (if on-disk (setf (car in-memory) (file-write-date on-disk)))
                 (cdr in-memory))
          (if error-p (error 'missing-component :requires name))))))

(defun register-system (name system)
  (asdf-message "~&~@<; ~@;registering ~A as ~A~@:>~%" system name)
  (setf (gethash (coerce-name name) *defined-systems*)
        (cons (get-universal-time) system)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; finding components

(defmethod find-component ((module module) name &optional version)
  (if (slot-boundp module 'components)
      (let ((m (find name (module-components module)
                     :test #'equal :key #'component-name)))
        (if (and m (version-satisfies m version)) m))))


;;; a component with no parent is a system
(defmethod find-component ((module (eql nil)) name &optional version)
  (let ((m (find-system name nil)))
    (if (and m (version-satisfies m version)) m)))

;;; component subclasses

(defclass source-file (component) ())

(defclass cl-source-file (source-file) ())
(defclass c-source-file (source-file) ())
(defclass java-source-file (source-file) ())
(defclass static-file (source-file) ())
(defclass doc-file (static-file) ())
(defclass html-file (doc-file) ())

(defmethod source-file-type ((c cl-source-file) (s module)) "lisp")
(defmethod source-file-type ((c c-source-file) (s module)) "c")
(defmethod source-file-type ((c java-source-file) (s module)) "java")
(defmethod source-file-type ((c html-file) (s module)) "html")
(defmethod source-file-type ((c static-file) (s module)) nil)

(defmethod component-relative-pathname ((component source-file))
  (multiple-value-bind (relative path name)
      (split-path-string (component-name component))
    (let ((type (source-file-type component (component-system component)))
          (relative-pathname (slot-value component 'relative-pathname))
          (*default-pathname-defaults* (component-parent-pathname component)))
      (if relative-pathname
	(merge-pathnames
         relative-pathname
         (if type
           (make-pathname :name name :type type)
           name))
        (make-pathname :directory `(,relative ,@path) :name name :type type)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; operations

;;; one of these is instantiated whenever (operate ) is called

(defclass operation ()
  ((forced :initform nil :initarg :force :accessor operation-forced)
   (original-initargs :initform nil :initarg :original-initargs
                      :accessor operation-original-initargs)
   (visited-nodes :initform nil :accessor operation-visited-nodes)
   (visiting-nodes :initform nil :accessor operation-visiting-nodes)
   (parent :initform nil :initarg :parent :accessor operation-parent)))

(defmethod print-object ((o operation) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (ignore-errors
      (prin1 (operation-original-initargs o) stream))))

(defmethod shared-initialize :after ((operation operation) slot-names
                                     &key force
                                     &allow-other-keys)
  (declare (ignore slot-names force))
  ;; empty method to disable initarg validity checking
  )

(defun node-for (o c)
  (cons (class-name (class-of o)) c))

(defmethod operation-ancestor ((operation operation))
  (aif (operation-parent operation)
       (operation-ancestor it)
       operation))


(defun make-sub-operation (c o dep-c dep-o)
  (let* ((args (copy-list (operation-original-initargs o)))
         (force-p (getf args :force)))
    ;; note explicit comparison with T: any other non-NIL force value
    ;; (e.g. :recursive) will pass through
    (cond ((and (null (component-parent c))
                (null (component-parent dep-c))
                (not (eql c dep-c)))
           (when (eql force-p t)
             (setf (getf args :force) nil))
           (apply #'make-instance dep-o
                  :parent o
                  :original-initargs args args))
          ((subtypep (type-of o) dep-o)
           o)
          (t
           (apply #'make-instance dep-o
                  :parent o :original-initargs args args)))))


(defmethod visit-component ((o operation) (c component) data)
  (unless (component-visited-p o c)
    (push (cons (node-for o c) data)
          (operation-visited-nodes (operation-ancestor o)))))

(defmethod component-visited-p ((o operation) (c component))
  (assoc (node-for o c)
         (operation-visited-nodes (operation-ancestor o))
         :test 'equal))

(defmethod (setf visiting-component) (new-value operation component)
  ;; MCL complains about unused lexical variables
  (declare (ignorable new-value operation component)))

(defmethod (setf visiting-component) (new-value (o operation) (c component))
  (let ((node (node-for o c))
        (a (operation-ancestor o)))
    (if new-value
        (pushnew node (operation-visiting-nodes a) :test 'equal)
        (setf (operation-visiting-nodes a)
              (remove node  (operation-visiting-nodes a) :test 'equal)))))

(defmethod component-visiting-p ((o operation) (c component))
  (let ((node (node-for o c)))
    (member node (operation-visiting-nodes (operation-ancestor o))
            :test 'equal)))

(defmethod component-depends-on ((op-spec symbol) (c component))
  (component-depends-on (make-instance op-spec) c))

(defmethod component-depends-on ((o operation) (c component))
  (cdr (assoc (class-name (class-of o))
              (slot-value c 'in-order-to))))

(defmethod component-self-dependencies ((o operation) (c component))
  (let ((all-deps (component-depends-on o c)))
    (remove-if-not (lambda (x)
                     (member (component-name c) (cdr x) :test #'string=))
                   all-deps)))

(defmethod input-files ((operation operation) (c component))
  (let ((parent (component-parent c))
        (self-deps (component-self-dependencies operation c)))
    (if self-deps
        (mapcan (lambda (dep)
                  (destructuring-bind (op name) dep
                    (output-files (make-instance op)
                                  (find-component parent name))))
                self-deps)
        ;; no previous operations needed?  I guess we work with the
        ;; original source file, then
        (list (component-pathname c)))))

(defmethod input-files ((operation operation) (c module)) nil)

(defmethod operation-done-p ((o operation) (c component))
  (flet ((fwd-or-return-t (file)
           ;; if FILE-WRITE-DATE returns NIL, it's possible that the
           ;; user or some other agent has deleted an input file.  If
           ;; that's the case, well, that's not good, but as long as
           ;; the operation is otherwise considered to be done we
           ;; could continue and survive.
           (let ((date (file-write-date file)))
             (cond
               (date)
               (t
                (warn "~@<Missing FILE-WRITE-DATE for ~S: treating ~
                       operation ~S on component ~S as done.~@:>"
                      file o c)
                (return-from operation-done-p t))))))
    (let ((out-files (output-files o c))
          (in-files (input-files o c)))
      (cond ((and (not in-files) (not out-files))
             ;; arbitrary decision: an operation that uses nothing to
             ;; produce nothing probably isn't doing much
             t)
            ((not out-files)
             (let ((op-done
                    (gethash (type-of o)
                             (component-operation-times c))))
               (and op-done
                    (>= op-done
                        (apply #'max
                               (mapcar #'fwd-or-return-t in-files))))))
            ((not in-files) nil)
            (t
             (and
              (every #'probe-file out-files)
              (> (apply #'min (mapcar #'file-write-date out-files))
                 (apply #'max (mapcar #'fwd-or-return-t in-files)))))))))

;;; So you look at this code and think "why isn't it a bunch of
;;; methods".  And the answer is, because standard method combination
;;; runs :before methods most->least-specific, which is back to front
;;; for our purposes.  

(defmethod traverse ((operation operation) (c component))
  (let ((forced nil))
    (labels ((%do-one-dep (required-op required-c required-v)
               (let* ((dep-c (or (find-component
                                  (component-parent c)
                                  ;; XXX tacky.  really we should build the
                                  ;; in-order-to slot with canonicalized
                                  ;; names instead of coercing this late
                                  (coerce-name required-c) required-v)
				 (if required-v
				     (error 'missing-dependency-of-version
					    :required-by c
					    :version required-v
					    :requires required-c)
				     (error 'missing-dependency
					    :required-by c
					    :requires required-c))))
                      (op (make-sub-operation c operation dep-c required-op)))
                 (traverse op dep-c)))
	     (do-one-dep (required-op required-c required-v)
               (loop
		  (restart-case
		      (return (%do-one-dep required-op required-c required-v))
		    (retry ()
		      :report (lambda (s)
				(format s "~@<Retry loading component ~S.~@:>"
					required-c))
		      :test
		      (lambda (c)
#|
			(print (list :c1 c (typep c 'missing-dependency)))
			(when (typep c 'missing-dependency)
			  (print (list :c2 (missing-requires c) required-c
				       (equalp (missing-requires c)
					       required-c))))
|#
			(and (typep c 'missing-dependency)
			     (equalp (missing-requires c)
				     required-c)))))))
             (do-dep (op dep)
               (cond ((eq op 'feature)
                      (or (member (car dep) *features*)
                          (error 'missing-dependency
                                 :required-by c
                                 :requires (car dep))))
                     (t
                      (dolist (d dep)
                        (cond ((consp d)
			       (cond ((string-equal
				       (symbol-name (first d))
				       "VERSION")
				      (appendf
				       forced
				       (do-one-dep op (second d) (third d))))
				     ((and (string-equal
					    (symbol-name (first d))
					    "FEATURE")
					   (find (second d) *features*
						 :test 'string-equal))
				      (appendf
				       forced
				       (do-one-dep op (second d) (third d))))
				     (t
				      (error "Bad dependency ~a.  Dependencies must be (:version <version>), (:feature <feature>), or a name" d))))
                              (t
                               (appendf forced (do-one-dep op d nil)))))))))
      (aif (component-visited-p operation c)
           (return-from traverse
             (if (cdr it) (list (cons 'pruned-op c)) nil)))
      ;; dependencies
      (if (component-visiting-p operation c)
          (error 'circular-dependency :components (list c)))
      (setf (visiting-component operation c) t)
      (unwind-protect
	   (progn
	     (loop for (required-op . deps) in
		  (component-depends-on operation c)
		  do (do-dep required-op deps))
	     ;; constituent bits
	     (let ((module-ops
		    (when (typep c 'module)
		      (let ((at-least-one nil)
			    (forced nil)
			    (error nil))
			(loop for kid in (module-components c)
			   do (handler-case
				  (appendf forced (traverse operation kid ))
				(missing-dependency (condition)
				  (if (eq (module-if-component-dep-fails c)
					  :fail)
				      (error condition))
				  (setf error condition))
				(:no-error (c)
				  (declare (ignore c))
				  (setf at-least-one t))))
			(when (and (eq (module-if-component-dep-fails c)
				       :try-next)
				   (not at-least-one))
			  (error error))
			forced))))
	       ;; now the thing itself
	       (when (or forced module-ops
			 (not (operation-done-p operation c))
			 (let ((f (operation-forced
				   (operation-ancestor operation))))
			   (and f (or (not (consp f))
				      (member (component-name
					       (operation-ancestor operation))
					      (mapcar #'coerce-name f)
					      :test #'string=)))))
		 (let ((do-first (cdr (assoc (class-name (class-of operation))
					     (slot-value c 'do-first)))))
		   (loop for (required-op . deps) in do-first
		      do (do-dep required-op deps)))
		 (setf forced (append (delete 'pruned-op forced :key #'car)
				      (delete 'pruned-op module-ops :key #'car)
				      (list (cons operation c)))))))
	(setf (visiting-component operation c) nil))
      (visit-component operation c (and forced t))
      forced)))


(defmethod perform ((operation operation) (c source-file))
  (sysdef-error
   "~@<required method PERFORM not implemented ~
    for operation ~A, component ~A~@:>"
   (class-of operation) (class-of c)))

(defmethod perform ((operation operation) (c module))
  nil)

(defmethod explain ((operation operation) (component component))
  (asdf-message "~&;;; ~A on ~A~%" operation component))

;;; compile-op

(defclass compile-op (operation)
  ((proclamations :initarg :proclamations :accessor compile-op-proclamations :initform nil)
   (on-warnings :initarg :on-warnings :accessor operation-on-warnings
                :initform *compile-file-warnings-behaviour*)
   (on-failure :initarg :on-failure :accessor operation-on-failure
               :initform *compile-file-failure-behaviour*)))

(defmethod perform :before ((operation compile-op) (c source-file))
  (map nil #'ensure-directories-exist (output-files operation c)))

(defmethod perform :after ((operation operation) (c component))
  (setf (gethash (type-of operation) (component-operation-times c))
        (get-universal-time)))

;;; perform is required to check output-files to find out where to put
;;; its answers, in case it has been overridden for site policy
(defmethod perform ((operation compile-op) (c cl-source-file))
  #-:broken-fasl-loader
  (let ((source-file (component-pathname c))
        (output-file (car (output-files operation c))))
    (multiple-value-bind (output warnings-p failure-p)
        (compile-file source-file :output-file output-file)
      (when warnings-p
        (case (operation-on-warnings operation)
          (:warn (warn
                  "~@<COMPILE-FILE warned while performing ~A on ~A.~@:>"
                  operation c))
          (:error (error 'compile-warned :component c :operation operation))
          (:ignore nil)))
      (when failure-p
        (case (operation-on-failure operation)
          (:warn (warn
                  "~@<COMPILE-FILE failed while performing ~A on ~A.~@:>"
                  operation c))
          (:error (error 'compile-failed :component c :operation operation))
          (:ignore nil)))
      (unless output
        (error 'compile-error :component c :operation operation)))))

(defmethod output-files ((operation compile-op) (c cl-source-file))
  #-:broken-fasl-loader (list (compile-file-pathname (component-pathname c)))
  #+:broken-fasl-loader (list (component-pathname c)))

(defmethod perform ((operation compile-op) (c static-file))
  nil)

(defmethod output-files ((operation compile-op) (c static-file))
  nil)

(defmethod input-files ((op compile-op) (c static-file))
  nil)


;;; load-op

(defclass basic-load-op (operation) ())

(defclass load-op (basic-load-op) ())

(defmethod perform ((o load-op) (c cl-source-file))
  (mapcar #'load (input-files o c)))

(defmethod perform around ((o load-op) (c cl-source-file))
  (let ((state :initial))
    (loop until (or (eq state :success)
		    (eq state :failure)) do
	 (case state
	   (:recompiled
	    (setf state :failure)
	    (call-next-method)
	    (setf state :success))
	   (:failed-load
	    (setf state :recompiled)
	    (perform (make-instance 'asdf:compile-op) c))
	   (t
	    (with-simple-restart
		(try-recompiling "Recompile ~a and try loading it again"
				  (component-name c))
	      (setf state :failed-load)
	      (call-next-method)
	      (setf state :success)))))))

(defmethod perform around ((o compile-op) (c cl-source-file))
  (let ((state :initial))
    (loop until (or (eq state :success)
		    (eq state :failure)) do
	 (case state
	   (:recompiled
	    (setf state :failure)
	    (call-next-method)
	    (setf state :success))
	   (:failed-compile
	    (setf state :recompiled)
	    (perform (make-instance 'asdf:compile-op) c))
	   (t
	    (with-simple-restart
		(try-recompiling "Try recompiling ~a"
				  (component-name c))
	      (setf state :failed-compile)
	      (call-next-method)
	      (setf state :success)))))))

(defmethod perform ((operation load-op) (c static-file))
  nil)

(defmethod operation-done-p ((operation load-op) (c static-file))
  t)

(defmethod output-files ((o operation) (c component))
  nil)

(defmethod component-depends-on ((operation load-op) (c component))
  (cons (list 'compile-op (component-name c))
        (call-next-method)))

;;; load-source-op

(defclass load-source-op (basic-load-op) ())

(defmethod perform ((o load-source-op) (c cl-source-file))
  (let ((source (component-pathname c)))
    (setf (component-property c 'last-loaded-as-source)
          (and (load source)
               (get-universal-time)))))

(defmethod perform ((operation load-source-op) (c static-file))
  nil)

(defmethod output-files ((operation load-source-op) (c component))
  nil)

;;; FIXME: we simply copy load-op's dependencies.  this is Just Not Right.
(defmethod component-depends-on ((o load-source-op) (c component))
  (let ((what-would-load-op-do (cdr (assoc 'load-op
                                           (slot-value c 'in-order-to)))))
    (mapcar (lambda (dep)
              (if (eq (car dep) 'load-op)
                  (cons 'load-source-op (cdr dep))
                  dep))
            what-would-load-op-do)))

(defmethod operation-done-p ((o load-source-op) (c source-file))
  (if (or (not (component-property c 'last-loaded-as-source))
          (> (file-write-date (component-pathname c))
             (component-property c 'last-loaded-as-source)))
      nil t))

(defclass test-op (operation) ())

(defmethod perform ((operation test-op) (c component))
  nil)

(defmethod operation-done-p ((operation test-op) (c system))
  "Testing a system is _never_ done."
  nil)

(defmethod component-depends-on :around ((o test-op) (c system))
  (cons `(load-op ,(component-name c)) (call-next-method)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; invoking operations

(defun operate (operation-class system &rest args &key (verbose t) version force
                &allow-other-keys)
  (declare (ignore force))
  (let* ((*package* *package*)
         (*readtable* *readtable*)
         (op (apply #'make-instance operation-class
                    :original-initargs args
                    args))
         (*verbose-out* (if verbose *standard-output* (make-broadcast-stream)))
         (system (if (typep system 'component) system (find-system system))))
    (unless (version-satisfies system version)
      (error 'missing-component-of-version :requires system :version version))
    (let ((steps (traverse op system)))
      (with-compilation-unit ()
        (loop for (op . component) in steps do
                 (loop
                   (restart-case
                       (progn (perform op component)
                              (return))
                     (retry ()
                       :report
                       (lambda (s)
                         (format s "~@<Retry performing ~S on ~S.~@:>"
                                 op component)))
                     (accept ()
                       :report
                       (lambda (s)
                         (format s "~@<Continue, treating ~S on ~S as ~
                                   having been successful.~@:>"
                                 op component))
                       (setf (gethash (type-of op)
                                      (component-operation-times component))
                             (get-universal-time))
                       (return)))))))
    op))

(defun oos (operation-class system &rest args &key force (verbose t) version
	    &allow-other-keys)
  (declare (ignore force verbose version))
  (apply #'operate operation-class system args))

(let ((operate-docstring
  "Operate does three things:

1. It creates an instance of `operation-class` using any keyword parameters
as initargs.
2. It finds the  asdf-system specified by `system` (possibly loading
it from disk).
3. It then calls `traverse` with the operation and system as arguments

The traverse operation is wrapped in `with-compilation-unit` and error
handling code. If a `version` argument is supplied, then operate also
ensures that the system found satisfies it using the `version-satisfies`
method.

Note that dependencies may cause the operation to invoke other
operations on the system or its components: the new operations will be
created with the same initargs as the original one.
"))
  (setf (documentation 'oos 'function)
	(format nil
		"Short for _operate on system_ and an alias for the [operate][] function. ~&~&~a"
		operate-docstring))
  (setf (documentation 'operate 'function)
	operate-docstring))

(defun load-system (system &rest args &key force (verbose t) version)
  "Shorthand for `(operate 'asdf:load-op system)`. See [operate][] for details."
  (declare (ignore force verbose version))
  (apply #'operate 'load-op system args))

(defun compile-system (system &rest args &key force (verbose t) version)
  "Shorthand for `(operate 'asdf:compile-op system)`. See [operate][] for details."
  (declare (ignore force verbose version))
  (apply #'operate 'compile-op system args))

(defun test-system (system &rest args &key force (verbose t) version)
  "Shorthand for `(operate 'asdf:test-op system)`. See [operate][] for details."
  (declare (ignore force verbose version))
  (apply #'operate 'test-op system args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; syntax

(defun remove-keyword (key arglist)
  (labels ((aux (key arglist)
             (cond ((null arglist) nil)
                   ((eq key (car arglist)) (cddr arglist))
                   (t (cons (car arglist) (cons (cadr arglist)
                                                (remove-keyword
                                                 key (cddr arglist))))))))
    (aux key arglist)))

(defun resolve-symlinks (path)
  #-allegro (truename path)
  #+allegro (excl:pathname-resolve-symbolic-links path)
  )

(defun determine-system-pathname (pathname pathname-supplied-p)
  ;; called from the defsystem macro.
  ;; the pathname of a system is either
  ;; 1. the one supplied, 
  ;; 2. derived from the *load-truename* (see below), or
  ;; 3. taken from *default-pathname-defaults*
  ;;
  ;; if using *load-truename*, then we also deal with whether or not
  ;; to resolve symbolic links. If not resolving symlinks, then we use
  ;; *load-pathname* instead of *load-truename* since in some
  ;; implementations, the latter has *already resolved it.
  (or (and pathname-supplied-p pathname)
      (when *load-truename*
	(pathname-sans-name+type 
	 (if *resolve-symlinks*
	     (resolve-symlinks *load-truename*)
	     *load-pathname*)))
      *default-pathname-defaults*))

(defmacro defsystem (name &body options)
  (destructuring-bind (&key (pathname nil pathname-arg-p) (class 'system)
                            &allow-other-keys)
      options
    (let ((component-options (remove-keyword :class options)))
      `(progn
         ;; system must be registered before we parse the body, otherwise
         ;; we recur when trying to find an existing system of the same name
         ;; to reuse options (e.g. pathname) from
         (let ((s (system-registered-p ',name)))
           (cond ((and s (eq (type-of (cdr s)) ',class))
                  (setf (car s) (get-universal-time)))
                 (s
                  (change-class (cdr s) ',class))
                 (t
                  (register-system (quote ,name)
                                   (make-instance ',class :name ',name))))
           (%set-system-source-file *load-truename* 
				    (cdr (system-registered-p ',name))))
         (parse-component-form 
	  nil (apply
	       #'list
	       :module (coerce-name ',name)
	       :pathname
	       ,(determine-system-pathname pathname pathname-arg-p)
	       ',component-options))))))


(defun class-for-type (parent type)
  (let* ((extra-symbols (list (find-symbol (symbol-name type) *package*)
                              (find-symbol (symbol-name type)
                                           (load-time-value
                                            (package-name :asdf)))))
         (class (dolist (symbol (if (keywordp type)
                                    extra-symbols
                                    (cons type extra-symbols)))
                  (when (and symbol
                             (find-class symbol nil)
                             (subtypep symbol 'component))
                    (return (find-class symbol))))))
    (or class
        (and (eq type :file)
             (or (module-default-component-class parent)
                 (find-class 'cl-source-file)))
        (sysdef-error "~@<don't recognize component type ~A~@:>" type))))

(defun maybe-add-tree (tree op1 op2 c)
  "Add the node C at /OP1/OP2 in TREE, unless it's there already.
Returns the new tree (which probably shares structure with the old one)"
  (let ((first-op-tree (assoc op1 tree)))
    (if first-op-tree
        (progn
          (aif (assoc op2 (cdr first-op-tree))
               (if (find c (cdr it))
                   nil
                   (setf (cdr it) (cons c (cdr it))))
               (setf (cdr first-op-tree)
                     (acons op2 (list c) (cdr first-op-tree))))
          tree)
        (acons op1 (list (list op2 c)) tree))))

(defun union-of-dependencies (&rest deps)
  (let ((new-tree nil))
    (dolist (dep deps)
      (dolist (op-tree dep)
        (dolist (op  (cdr op-tree))
          (dolist (c (cdr op))
            (setf new-tree
                  (maybe-add-tree new-tree (car op-tree) (car op) c))))))
    new-tree))


(defun remove-keys (key-names args)
  (loop for ( name val ) on args by #'cddr
        unless (member (symbol-name name) key-names
                       :key #'symbol-name :test 'equal)
        append (list name val)))

(defvar *serial-depends-on*)

(defun sysdef-error-component (msg type name value)
  (sysdef-error (concatenate 'string msg
                             "~&The value specified for ~(~A~) ~A is ~W")
                type name value))

(defun check-component-input (type name weakly-depends-on 
			      depends-on components in-order-to)
  "A partial test of the values of a component."
  (unless (listp depends-on)
    (sysdef-error-component ":depends-on must be a list."
                            type name depends-on))
  (unless (listp weakly-depends-on)
    (sysdef-error-component ":weakly-depends-on must be a list."
                            type name weakly-depends-on))
  (unless (listp components)
    (sysdef-error-component ":components must be NIL or a list of components."
                            type name components))
  (unless (and (listp in-order-to) (listp (car in-order-to)))
    (sysdef-error-component ":in-order-to must be NIL or a list of components."
                            type name in-order-to)))

(defun %remove-component-inline-methods (component)
  (loop for name in +asdf-methods+
        do (map 'nil
                ;; this is inefficient as most of the stored
                ;; methods will not be for this particular gf n
                ;; But this is hardly performance-critical
                (lambda (m)
                  (remove-method (symbol-function name) m))
                (component-inline-methods component)))
  ;; clear methods, then add the new ones
  (setf (component-inline-methods component) nil))

(defun %define-component-inline-methods (ret rest)
  (loop for name in +asdf-methods+ do
       (let ((keyword (intern (symbol-name name) :keyword)))
	 (loop for data = rest then (cddr data)
	      for key = (first data)
	      for value = (second data)
              while data
	      when (eq key keyword) do
	      (destructuring-bind (op qual (o c) &body body) value
	      (pushnew
		 (eval `(defmethod ,name ,qual ((,o ,op) (,c (eql ,ret)))
				   ,@body))
		 (component-inline-methods ret)))))))

(defun %refresh-component-inline-methods (component rest)
  (%remove-component-inline-methods component)
  (%define-component-inline-methods component rest))
  
(defun parse-component-form (parent options)

  (destructuring-bind
        (type name &rest rest &key
              ;; the following list of keywords is reproduced below in the
              ;; remove-keys form.  important to keep them in sync
              components pathname default-component-class
              perform explain output-files operation-done-p
              weakly-depends-on
              depends-on serial in-order-to
              ;; list ends
              &allow-other-keys) options
    (declare (ignorable perform explain output-files operation-done-p))
    (check-component-input type name weakly-depends-on depends-on components in-order-to)

    (when (and parent
               (find-component parent name)
               ;; ignore the same object when rereading the defsystem
               (not
                (typep (find-component parent name)
                       (class-for-type parent type))))
      (error 'duplicate-names :name name))

    (let* ((other-args (remove-keys
                        '(components pathname default-component-class
                          perform explain output-files operation-done-p
                          weakly-depends-on
                          depends-on serial in-order-to)
                        rest))
           (ret
            (or (find-component parent name)
                (make-instance (class-for-type parent type)))))
      (when weakly-depends-on
        (setf depends-on (append depends-on (remove-if (complement #'find-system) weakly-depends-on))))
      (when (boundp '*serial-depends-on*)
        (setf depends-on
              (concatenate 'list *serial-depends-on* depends-on)))
      (apply #'reinitialize-instance ret
             :name (coerce-name name)
             :pathname pathname
             :parent parent
             other-args)
      (when (typep ret 'module)
        (setf (module-default-component-class ret)
              (or default-component-class
                  (and (typep parent 'module)
                       (module-default-component-class parent))))
        (let ((*serial-depends-on* nil))
          (setf (module-components ret)
                (loop for c-form in components
                      for c = (parse-component-form ret c-form)
                      collect c
                      if serial
                      do (push (component-name c) *serial-depends-on*))))

        ;; check for duplicate names
        (let ((name-hash (make-hash-table :test #'equal)))
          (loop for c in (module-components ret)
                do
                (if (gethash (component-name c)
                             name-hash)
                    (error 'duplicate-names
                           :name (component-name c))
                    (setf (gethash (component-name c)
                                   name-hash)
                          t)))))

      (setf (slot-value ret 'in-order-to)
            (union-of-dependencies
             in-order-to
             `((compile-op (compile-op ,@depends-on))
               (load-op (load-op ,@depends-on))))
            (slot-value ret 'do-first) `((compile-op (load-op ,@depends-on))))

      (%refresh-component-inline-methods ret rest)

      ret)))

;;; optional extras

;;; run-shell-command functions for other lisp implementations will be
;;; gratefully accepted, if they do the same thing.  If the docstring
;;; is ambiguous, send a bug report

(defun run-shell-command (control-string &rest args)
  "Interpolate `args` into `control-string` as if by `format`, and
synchronously execute the result using a Bourne-compatible shell, with
output to `*verbose-out*`.  Returns the shell's exit code."
  (let ((command (apply #'format nil control-string args)))
    (asdf-message "; $ ~A~%" command)
    #+sbcl
    (sb-ext:process-exit-code
     (apply #'sb-ext:run-program
	    #+win32 "sh" #-win32 "/bin/sh"
	    (list  "-c" command)
	    :input nil :output *verbose-out*
	    #+win32 '(:search t) #-win32 nil))

    #+(or cmu scl)
    (ext:process-exit-code
     (ext:run-program
      "/bin/sh"
      (list  "-c" command)
      :input nil :output *verbose-out*))

    #+allegro
    ;; will this fail if command has embedded quotes - it seems to work
    (multiple-value-bind (stdout stderr exit-code)
        (excl.osi:command-output 
	 (format nil "~a -c \"~a\"" 
		 #+mswindows "sh" #-mswindows "/bin/sh" command)
	 :input nil :whole nil
	 #+mswindows :show-window #+mswindows :hide)
      (format *verbose-out* "~{~&; ~a~%~}~%" stderr)
      (format *verbose-out* "~{~&; ~a~%~}~%" stdout)
      exit-code)

    #+lispworks
    (system:call-system-showing-output
     command
     :shell-type "/bin/sh"
     :output-stream *verbose-out*)

    #+clisp                     ;XXX not exactly *verbose-out*, I know
    (ext:run-shell-command  command :output :terminal :wait t)

    #+openmcl
    (nth-value 1
               (ccl:external-process-status
                (ccl:run-program "/bin/sh" (list "-c" command)
                                 :input nil :output *verbose-out*
                                 :wait t)))

    #+ecl ;; courtesy of Juan Jose Garcia Ripoll
    (si:system command)

    #-(or openmcl clisp lispworks allegro scl cmu sbcl ecl)
    (error "RUN-SHELL-PROGRAM not implemented for this Lisp")
    ))

(defmethod system-source-file ((system-name t))
  (system-source-file (find-system system-name)))

(defun system-source-directory (system-name)
  (make-pathname :name nil
                 :type nil
                 :defaults (system-source-file system-name)))

(defun system-relative-pathname (system pathname &key name type)
  ;; you're not allowed to muck with the return value of pathname-X
  (let ((directory (copy-list (pathname-directory pathname))))
    (when (eq (car directory) :absolute)
      (setf (car directory) :relative))
    (merge-pathnames
     (make-pathname :name (or name (pathname-name pathname))
                    :type (or type (pathname-type pathname))
                    :directory directory)
     (system-source-directory system))))

;;; ---------------------------------------------------------------------------
;;; asdf-binary-locations
;;;
;;; this bit of code was stolen from Bjorn Lindberg and then it grew!
;;; see http://www.cliki.net/asdf%20binary%20locations
;;; and http://groups.google.com/group/comp.lang.lisp/msg/bd5ea9d2008ab9fd
;;; ---------------------------------------------------------------------------
;;; Portions of this code were once from SWANK / SLIME

(defparameter *centralize-lisp-binaries*
  nil "
If true, compiled lisp files without an explicit mapping (see
\\*source-to-target-mappings\\*) will be placed in subdirectories of
\\*default-toplevel-directory\\*. If false, then compiled lisp files
without an explicitly mapping will be placed in subdirectories of
their sources.")

(defparameter *enable-asdf-binary-locations* nil
  "
If true, then compiled lisp files will be placed into a directory 
computed from the Lisp version, Operating System and computer archetecture.
See [implementation-specific-directory-name][] for details.")


(defparameter *default-toplevel-directory*
  (merge-pathnames
   (make-pathname :directory '(:relative ".fasls"))
   (truename (user-homedir-pathname)))
  "If \\*centralize-lisp-binaries\\* is true, then compiled lisp files without an explicit mapping \(see \\*source-to-target-mappings\\*\) will be placed in subdirectories of \\*default-toplevel-directory\\*.")

(defparameter *include-per-user-information*
  nil
  "When \\*centralize-lisp-binaries\\* is true this variable controls whether or not to customize the output directory based on the current user. It can be nil, t or a string. If it is nil \(the default\), then no additional information will be added to the output directory. If it is t, then the user's name \(as taken from the return value of #'user-homedir-pathname\) will be included into the centralized path (just before the lisp-implementation directory). Finally, if \\*include-per-user-information\\* is a string, then this string will be included in the output-directory.")

(defparameter *map-all-source-files*
  nil
  "If true, then all subclasses of source-file will have their output locations mapped by ASDF-Binary-Locations. If nil (the default), then only subclasses of cl-source-file will be mapped.")

(defvar *source-to-target-mappings* 
  #-sbcl
  nil
  #+sbcl
  (list (list (princ-to-string (sb-ext:posix-getenv "SBCL_HOME")) nil))
  "The \\*source-to-target-mappings\\* variable specifies mappings from source to target. If the target is nil, then it means to not map the source to anything. I.e., to leave it as is. This has the effect of turning off ASDF-Binary-Locations for the given source directory. Examples:

    ;; compile everything in .../src and below into .../cmucl
    '((\"/nfs/home/compbio/d95-bli/share/common-lisp/src/\" 
       \"/nfs/home/compbio/d95-bli/lib/common-lisp/cmucl/\"))

    ;; leave SBCL innards alone (SBCL specific)
    (list (list (princ-to-string (sb-ext:posix-getenv \"SBCL_HOME\")) nil))
")

(defparameter *implementation-features*
  '(:allegro :lispworks :sbcl :ccl :openmcl :cmu :clisp
    :corman :cormanlisp :armedbear :gcl :ecl :scl))

(defparameter *os-features*
  '(:windows :mswindows :win32 :mingw32
    :solaris :sunos
    :macosx :darwin :apple
    :freebsd :netbsd :openbsd :bsd
    :linux :unix))

(defparameter *architecture-features*
  '(:amd64 (:x86-64 :x86_64 :x8664-target) :i686 :i586 :pentium3 
    :i486 (:i386 :pc386 :iapx386) (:x86 :x8632-target) :pentium4
    :hppa64 :hppa :ppc64 :ppc32 :powerpc :ppc :sparc64 :sparc))

;; note to gwking: this is in slime, system-check, and system-check-server too
(defun lisp-version-string ()
  #+cmu       (substitute #\- #\/ 
			  (substitute #\_ #\Space 
				      (lisp-implementation-version)))
  #+scl       (lisp-implementation-version)
  #+sbcl      (lisp-implementation-version)
  #+ecl       (reduce (lambda (x str) (substitute #\_ str x))
		      '(#\Space #\: #\( #\)) 
		      :initial-value (lisp-implementation-version))
  #+gcl       (let ((s (lisp-implementation-version))) (subseq s 4))
  #+openmcl   (format nil "~d.~d~@[-~d~]"
                      ccl::*openmcl-major-version* 
                      ccl::*openmcl-minor-version*
                      #+ppc64-target 64 
                      #-ppc64-target nil)
  #+lispworks (format nil "~A~@[~A~]"
                      (lisp-implementation-version)
                      (when (member :lispworks-64bit *features*) "-64bit"))
  #+allegro   (format nil
                      "~A~A~A~A"
                      excl::*common-lisp-version-number*
					; ANSI vs MoDeRn
		      ;; thanks to Robert Goldman and Charley Cox for
		      ;; an improvement to my hack
		      (if (eq excl:*current-case-mode* 
			      :case-sensitive-lower) "M" "A")
		      ;; Note if not using International ACL
		      ;; see http://www.franz.com/support/documentation/8.1/doc/operators/excl/ics-target-case.htm
		      (excl:ics-target-case
			(:-ics "8")
			(:+ics ""))
                      (if (member :64bit *features*) "-64bit" ""))
  #+clisp     (let ((s (lisp-implementation-version)))
                (subseq s 0 (position #\space s)))
  #+armedbear (lisp-implementation-version)
  #+cormanlisp (lisp-implementation-version)
  #+digitool   (subseq (lisp-implementation-version) 8))


(defparameter *implementation-specific-directory-name* nil)

(defun implementation-specific-directory-name ()
  "Return a name that can be used as a directory name that is
unique to a Lisp implementation, Lisp implementation version,
operating system, and hardware architecture."
  (and *enable-asdf-binary-locations*
       (list 
	(or *implementation-specific-directory-name*
	    (setf *implementation-specific-directory-name*
		  (labels 
		      ((fp (thing)
			 (etypecase thing
			   (symbol
			    (let ((feature (find thing *features*)))
			      (when feature (return-from fp feature))))
			   ;; allows features to be lists of which the first
			   ;; member is the "main name", the rest being aliases
			   (cons
			    (dolist (subf thing)
			      (let ((feature (find subf *features*)))
				(when feature (return-from fp (first thing))))))))
		       (first-of (features)
			 (loop for f in features
			    when (fp f) return it))
		       (maybe-warn (value fstring &rest args)
			 (cond (value)
			       (t (apply #'warn fstring args)
				  "unknown"))))
		    (let ((lisp (maybe-warn (first-of *implementation-features*)
					    "No implementation feature found in ~a." 
					    *implementation-features*))
			  (os   (maybe-warn (first-of *os-features*)
					    "No os feature found in ~a." *os-features*))
			  (arch (maybe-warn (first-of *architecture-features*)
					    "No architecture feature found in ~a."
					    *architecture-features*))
			  (version (maybe-warn (lisp-version-string)
					       "Don't know how to get Lisp ~
                                          implementation version.")))
		      (format nil "~(~@{~a~^-~}~)" lisp version os arch))))))))

(defun pathname-prefix-p (prefix pathname)
  (let ((prefix-ns (namestring prefix))
        (pathname-ns (namestring pathname)))
    (= (length prefix-ns)
       (mismatch prefix-ns pathname-ns))))

(defgeneric output-files-for-system-and-operation
  (system operation component source possible-paths)
  (:documentation "Returns the directory where the componets output files should be placed. This may depends on the system, the operation and the component. The ASDF default input and outputs are provided in the source and possible-paths parameters."))

(defun source-to-target-resolved-mappings ()
  "Answer `*source-to-target-mappings*` with additional entries made
by resolving sources that are symlinks.

As ASDF sometimes resolves symlinks to compute source paths, we must
follow that.  For example, if SBCL is installed under a symlink, and
SBCL_HOME is set through that symlink, the default rule above
preventing SBCL contribs from being mapped elsewhere will not be
applied by the plain `*source-to-target-mappings*`."
  (loop for mapping in asdf:*source-to-target-mappings*
	for (source target) = mapping
	for true-source = (and source (resolve-symlinks source))
	if (equal source true-source)
	  collect mapping
	else append (list mapping (list true-source target))))

(defmethod output-files-for-system-and-operation
           ((system system) operation component source possible-paths)
  (declare (ignore operation component))
  (output-files-using-mappings
   source possible-paths (source-to-target-resolved-mappings)))

(defmethod output-files-using-mappings (source possible-paths path-mappings)
  (mapcar 
   (lambda (path) 
     (loop for (from to) in path-mappings 
	when (pathname-prefix-p from source) 
	do (return 
	     (if to
		 (merge-pathnames 
		  (make-pathname :type (pathname-type path)) 
		  (merge-pathnames (enough-namestring source from) 
				   to))
		 path))
		  
	finally
	  (return 
	    ;; Instead of just returning the path when we 
	    ;; don't find a mapping, we stick stuff into 
	    ;; the appropriate binary directory based on 
	    ;; the implementation
	    (if *centralize-lisp-binaries*
		(merge-pathnames
		 (make-pathname
		  :type (pathname-type path)
		  :directory `(:relative
			       ,@(cond ((eq *include-per-user-information* t)
					(cdr (pathname-directory
					      (user-homedir-pathname))))
				       ((not (null *include-per-user-information*))
					(list *include-per-user-information*)))
			       ,@(implementation-specific-directory-name)
			       ,@(rest (pathname-directory path)))
		  :defaults path)
		 *default-toplevel-directory*)
		(make-pathname 
		 :type (pathname-type path)
		 :directory (append
			     (pathname-directory path)
			     (implementation-specific-directory-name))
		 :defaults path))))) 
	  possible-paths))

(defmethod output-files 
    :around ((operation compile-op) (component source-file)) 
  (if (or *map-all-source-files*
	    (typecase component 
	      (cl-source-file t)
	      (t nil)))
    (let ((source (component-pathname component )) 
	  (paths (call-next-method))) 
      (output-files-for-system-and-operation 
       (component-system component) operation component source paths))
    (call-next-method)))

;;;; -----------------------------------------------------------------
;;;; Windows shortcut support.  Based on:
;;;;
;;;; Jesse Hager: The Windows Shortcut File Format.
;;;; http://www.wotsit.org/list.asp?fc=13
;;;; -----------------------------------------------------------------

(defparameter *link-initial-dword* 76)
(defparameter *link-guid* #(1 20 2 0 0 0 0 0 192 0 0 0 0 0 0 70))

(defun read-null-terminated-string (s)
  (with-output-to-string (out)
    (loop
	for code = (read-byte s)
	until (zerop code)
	do (write-char (code-char code) out))))

(defun read-little-endian (s &optional (bytes 4))
  (let ((result 0))
    (loop
	for i from 0 below bytes
	do
	  (setf result (logior result (ash (read-byte s) (* 8 i)))))
    result))

(defun parse-windows-shortcut (pathname)
  (with-open-file (s pathname :element-type '(unsigned-byte 8))
    (handler-case
	(when (and (= (read-little-endian s) *link-initial-dword*)
		   (let ((header (make-array (length *link-guid*))))
		     (read-sequence header s)
		     (equalp header *link-guid*)))
	  (let ((flags (read-little-endian s)))
	    (file-position s 76)	;skip rest of header
	    (when (logbitp 0 flags)
	      ;; skip shell item id list
	      (let ((length (read-little-endian s 2)))
		(file-position s (+ length (file-position s)))))
	    (cond
	      ((logbitp 1 flags)
		(parse-file-location-info s))
	      (t
		(when (logbitp 2 flags)
		  ;; skip description string
		  (let ((length (read-little-endian s 2)))
		    (file-position s (+ length (file-position s)))))
		(when (logbitp 3 flags)
		  ;; finally, our pathname
		  (let* ((length (read-little-endian s 2))
			 (buffer (make-array length)))
		    (read-sequence buffer s)
		    (map 'string #'code-char buffer)))))))
      (end-of-file ()
	nil))))

(defun parse-file-location-info (s)
  (let ((start (file-position s))
	(total-length (read-little-endian s))
	(end-of-header (read-little-endian s))
	(fli-flags (read-little-endian s))
	(local-volume-offset (read-little-endian s))
	(local-offset (read-little-endian s))
	(network-volume-offset (read-little-endian s))
	(remaining-offset (read-little-endian s)))
    (declare (ignore total-length end-of-header local-volume-offset))
    (unless (zerop fli-flags)
      (cond
	((logbitp 0 fli-flags)
	  (file-position s (+ start local-offset)))
	((logbitp 1 fli-flags)
	  (file-position s (+ start
			      network-volume-offset
			      #x14))))
      (concatenate 'string
	(read-null-terminated-string s)
	(progn
	  (file-position s (+ start remaining-offset))
	  (read-null-terminated-string s))))))


(pushnew :asdf *features*)

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (sb-ext:posix-getenv "SBCL_BUILDING_CONTRIB")
    (pushnew :sbcl-hooks-require *features*)))

#+(and sbcl sbcl-hooks-require)
(progn
  (defun module-provide-asdf (name)
    (handler-bind ((style-warning #'muffle-warning))
      (let* ((*verbose-out* (make-broadcast-stream))
             (system (asdf:find-system name nil)))
        (when system
          (asdf:operate 'asdf:load-op name)
          t))))

  (defun contrib-sysdef-search (system)
    (let ((home (sb-ext:posix-getenv "SBCL_HOME")))
      (when (and home (not (string= home "")))
        (let* ((name (coerce-name system))
               (home (truename home))
               (contrib (merge-pathnames
                         (make-pathname :directory `(:relative ,name)
                                        :name name
                                        :type "asd"
                                        :case :local
                                        :version :newest)
                         home)))
          (probe-file contrib)))))

  (pushnew
   '(let ((home (sb-ext:posix-getenv "SBCL_HOME")))
      (when (and home (not (string= home "")))
        (merge-pathnames "site-systems/" (truename home))))
   *central-registry*)

  (pushnew
   '(merge-pathnames ".sbcl/systems/"
     (user-homedir-pathname))
   *central-registry*)

  (pushnew 'module-provide-asdf sb-ext:*module-provider-functions*)
  (pushnew 'contrib-sysdef-search *system-definition-search-functions*))

(if *asdf-revision*
    (asdf-message ";; ASDF, revision ~a" *asdf-revision*)
    (asdf-message ";; ASDF, revision unknown; possibly a development version"))

(provide 'asdf)


#+(or)
;;?? ignore -- so how will ABL get "installed"
;; should be unnecessary with newer versions of ASDF
;; load customizations
(eval-when (:load-toplevel :execute)
  (let* ((*package* (find-package :common-lisp)))
    (load
     (merge-pathnames
      (make-pathname :name "asdf-binary-locations"
		     :type "lisp"
		     :directory '(:relative ".asdf"))
      (truename (user-homedir-pathname)))
     :if-does-not-exist nil)))
