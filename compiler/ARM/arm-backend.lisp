;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2009 Clozure Associates
;;;   Copyright (C) 1994-2001 Digitool, Inc
;;;   This file is part of Clozure CL.  
;;;
;;;   Clozure CL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with Clozure CL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with Clozure CL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   Clozure CL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

(in-package "CCL")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "NXENV")
  (require "ARMENV"))

(next-nx-defops)
(defvar *arm2-specials* nil)
(let* ((newsize (%i+ (next-nx-num-ops) 10))
       (old *arm2-specials*)
       (oldsize (length old)))
  (declare (fixnum newsize oldsize))
  (unless (>= oldsize newsize)
    (let* ((v (make-array newsize :initial-element nil)))
      (dotimes (i oldsize (setq *arm2-specials* v))
        (setf (svref v i) (svref old i))))))

;;; This defines a template.  All expressions in the body must be
;;; evaluable at macroexpansion time.
(defun %define-arm-vinsn (backend vinsn-name results args temps body)
  (let* ((arch-name (backend-target-arch-name backend))
	 (template-hash (backend-p2-template-hash-name backend))
	 (name-list ())
	 (attrs 0)
         (nhybrids 0)
         (local-labels ())
         (referenced-labels ())
	 (source-indicator (form-symbol arch-name "-VINSN"))
         (opcode-alist ()))
    (flet ((valid-spec-name (x)
	     (or (and (consp x) 
		      (consp (cdr x)) 
		      (null (cddr x)) 
		      (atom (car x))
		      (or (assoc (cadr x) *vreg-specifier-constant-constraints* :test #'eq)
			  (assoc (cadr x) *spec-class-storage-class-alist* :test #'eq)
			  (eq (cadr x) :label)
			  (and (consp (cadr x))
			       (or 
				(assoc (caadr x) *vreg-specifier-constant-constraints* :test #'eq)
				(assoc (caadr x) *spec-class-storage-class-alist* :test #'eq))))
		      (car x))
		 (error "Invalid vreg spec: ~s" x)))
           (add-spec-name (vname) 
             (if (member vname name-list :test #'eq)
               (error "Duplicate name ~s in vinsn ~s" vname vinsn-name)
               (push vname name-list))))
      (declare (dynamic-extent #'valid-spec-name #'add-spec-name))
      (when (consp vinsn-name)
        (setq attrs (encode-vinsn-attributes (cdr vinsn-name))
              vinsn-name (car vinsn-name)))
      (unless (and (symbolp vinsn-name) (eq *CCL-PACKAGE* (symbol-package vinsn-name)))
        (setq vinsn-name (intern (string vinsn-name) *CCL-PACKAGE*)))
      (dolist (n (append args temps))
        (add-spec-name (valid-spec-name n)))
      #+no
      (dolist (form body)
        (if (atom form)
          (add-spec-name form)))
      (setq name-list (nreverse name-list))
      ;; We now know that "args" is an alist; we don't know if
      ;; "results" is.  First, make sure that there are no duplicate
      ;; result names (and validate "results".)
      (do* ((res results tail)
            (tail (cdr res) (cdr tail)))
           ((null res))
        (let* ((name (valid-spec-name (car res))))
          (if (assoc name tail :test #'eq)
            (error "Duplicate result name ~s in ~s." name results))))
      (let* ((non-hybrid-results ()) 
             (match-args args))
        (dolist (res results)
          (let* ((res-name (car res)))
            (if (not (assoc res-name args :test #'eq))
              (if (not (= nhybrids 0))
                (error "result ~s should also name an argument. " res-name)
                (push res-name non-hybrid-results))
              (if (eq res-name (caar match-args))
                (setf nhybrids (1+ nhybrids)
                      match-args (cdr match-args))
                (error "~S - hybrid results should appear in same order as arguments." res-name)))))
        (dolist (name non-hybrid-results)
          (add-spec-name name)))
      (let* ((k -1))
        (declare (fixnum k))
        (let* ((name-alist (mapcar #'(lambda (n) (cons n (list (incf k)))) name-list)))
          (flet ((find-name (n)
                   (let* ((pair (assoc n name-alist :test #'eq)))
                     (declare (list pair))
                     (if pair
                       (cdr pair)
                       (or (subprim-name->offset n backend)
                           (error "Unknown name ~s" n))))))
            (labels ((simplify-operand (op)
                       (if (atom op)
                         (if (typep op 'fixnum)
                           op
                           (if (constantp op)
                             (progn
                               (if (keywordp op)
                                 (pushnew op referenced-labels))
                               (eval op))
                             (find-name op)))
                         (if (eq (car op) :apply)
                           `(,(cadr op) ,@(mapcar #'simplify-operand (cddr op)))
                           (simplify-operand (eval op)))))) ; Handler-case this?         
              (labels ((simplify-constraint (guard)
                         ;; A constraint is one of

                         ;; (:eq|:lt|:gt vreg-name constant)

                         ;; value" of vreg relop constant

                         ;; (:pred <function-name> <operand>* ;
                         ;; <function-name> unquoted, each <operand>
                         ;; is a vreg-name or constant expression.

                         ;; (:type vreg-name typeval) ; vreg is of
                         ;; "type" typeval
                         ;;
                         ;;(:not <constraint>) ; constraint is false
                         ;; (:and <constraint> ...)        ;  conjuntion
                         ;; (:or <constraint> ...)         ;  disjunction
                         ;; There's no "else"; we'll see how ugly it
                         ;; is without one.
                         (destructuring-bind (guardname &rest others) guard
                           (ecase guardname
                             (:not 
                              (destructuring-bind (negation) others
                                `(:not ,(simplify-constraint negation))))
                             (:pred
                              (destructuring-bind (predicate &rest operands) others
                                `(:pred ,predicate ,@(mapcar #'simplify-operand operands))))
                             ((:eq :lt :gt :type)
                              (destructuring-bind (vreg constant) others
                                (unless (constantp constant)
                                  (error "~S : not constant in constraint ~s ." constant guard))
                                `(,guardname ,(find-name vreg) ,(eval constant))))
                             ((:or :and)
                              (unless others (error "Missing constraint list in ~s ." guard))
                              `(,guardname ,(mapcar #'simplify-constraint others))))))
                       (simplify-form (form)
                         (if (atom form)
                           (progn 
                             (if (keywordp form) (push form local-labels) )
                             form)
                           (destructuring-bind (&whole w opname &rest opvals) form
                             (if (consp opname) ; A constraint, we presume ...
                               (cons (simplify-constraint opname)
                                     (mapcar #'simplify-form opvals))
                               (if (keywordp opname)
                                 (ecase opname
                                   ((:code :data)  form)
                                   (:word (destructuring-bind (val) opvals
                                            (list opname
                                                  (let* ((p (position val name-list)))
                                                    (if p (list p) (eval val)))))))
                                 (arm::vinsn-simplify-instruction form name-list)))))))
                (let* ((template (make-vinsn-template
                                  :name vinsn-name
                                  :result-vreg-specs results
                                  :argument-vreg-specs args
                                  :temp-vreg-specs temps
                                  :nhybrids nhybrids
                                  :results&args (append results (nthcdr nhybrids args))
                                  :nvp (- (+ (length results) (length args) (length temps))
                                          nhybrids)
                                  :body (prog1 (mapcar #'simplify-form body)
                                          (dolist (ref referenced-labels)
                                            (unless (memq ref local-labels)
                                              (error 
                                               "local label ~S was referenced but never defined in VINSN-TEMPLATE definition for ~s" ref vinsn-name))))
                                  :local-labels local-labels :attributes attrs :opcode-alist
                                  opcode-alist)))
                  `(progn (set-vinsn-template ',vinsn-name ,template
                           ,template-hash) (record-source-file ',vinsn-name ',source-indicator)
                    ',vinsn-name))))))))))



(defvar *arm-vinsn-templates* (make-hash-table :test #'eq))




(defvar *known-arm-backends* ())


#+(or linuxarm-target (not arm-target))
(defvar *linuxarm-backend*
  (make-backend :lookup-opcode #'arm::lookup-arm-instruction
		:lookup-macro #'false
		:lap-opcodes arm::*arm-instruction-table*
                :define-vinsn '%define-arm-vinsn
                :platform-syscall-mask (logior platform-os-linux platform-cpu-arm)
		:p2-dispatch *arm2-specials*
		:p2-vinsn-templates *arm-vinsn-templates*
		:p2-template-hash-name '*arm-vinsn-templates*
		:p2-compile 'arm2-compile
		:target-specific-features
		'(:arm :arm-target :eabi-target :linux-target :linuxarm-target  :32-bit-target :little-endian-target)
		:target-fasl-pathname (make-pathname :type "lafsl")
		:target-platform (logior platform-word-size-32
                                         platform-cpu-arm
                                         platform-os-linux)
		:target-os :linuxarm
		:name :linuxarm
		:target-arch-name :arm
		:target-foreign-type-data nil
                :target-arch arm::*arm-target-arch*))


#+darwinarm-target
(defvar *darwinarm-backend*
  (make-backend :lookup-opcode #'arm::lookup-arm-instruction
		:lookup-macro #'false
		:lap-opcodes arm::*arm-instruction-table*
                :define-vinsn '%define-arm-vinsn
                :platform-syscall-mask (logior platform-os-darwin platform-cpu-arm)                
		:p2-dispatch *arm2-specials*
		:p2-vinsn-templates *arm-vinsn-templates*
		:p2-template-hash-name '*arm-vinsn-templates*
		:p2-compile 'arm2-compile
		:target-specific-features
		'(:arm :arm-target :darwin-target :darwinarm-target :arm-target :32-bit-target :little-endian-target)
		:target-fasl-pathname (make-pathname :type "dafsl")
		:target-platform (logior platform-word-size-32
                                         platform-cpu-arm
                                         platform-os-darwin)
		:target-os :darwinarm
		:name :darwinarm
		:target-arch-name :arm
		:target-foreign-type-data nil
                :target-arch arm::*arm-target-arch*))

#+(or linuxarm-target (not arm-target))
(pushnew *linuxarm-backend* *known-arm-backends* :key #'backend-name)


#+darwinarm-target
(pushnew *darwinarm-backend* *known-arm-backends* :key #'backend-name)

(defvar *arm-backend* (car *known-arm-backends*))

(defun fixup-arm-backend ()
  (dolist (b *known-arm-backends*)
    (setf (backend-lap-opcodes b) arm::*arm-instruction-table*
	  (backend-p2-dispatch b) *arm2-specials*
	  (backend-p2-vinsn-templates b)  *arm-vinsn-templates*)
    (or (backend-lap-macros b) (setf (backend-lap-macros b)
                                     (make-hash-table :test #'equalp)))))



(fixup-arm-backend)

#+arm-target
(setq *host-backend* *arm-backend* *target-backend* *arm-backend*)

(defun setup-arm-ftd (backend)
  (or (backend-target-foreign-type-data backend)
      (let* ((name (backend-name backend))
             (ftd
              (case name
                (:darwinarm
                 (make-ftd :interface-db-directory "ccl:darwin-arm-headers;"
			   :interface-package-name "ARM-DARWIN"
                           :attributes '(:bits-per-word  32
                                         :signed-char t
                                         :struct-by-value t
                                         :prepend-underscore nil)
                           :ff-call-expand-function
                           (intern "EXPAND-FF-CALL" "ARM-DARWIN")
			   :ff-call-struct-return-by-implicit-arg-function
                           (intern "RECORD-TYPE-RETURNS-STRUCTURE-AS-FIRST-ARG"
                                   "ARM-DARWIN")
                           :callback-bindings-function
                           (intern "GENERATE-CALLBACK-BINDINGS" "ARM-DARWIN")
                           :callback-return-value-function
                           (intern "GENERATE-CALLBACK-RETURN-VALUE" "ARM-DARWIN")))
                (:linuxarm
                 (make-ftd :interface-db-directory "ccl:arm-headers;"
			   :interface-package-name "ARM-LINUX"
                           :attributes '(:bits-per-word  32
                                         :signed-char nil
                                         :struct-by-value t)
                           :ff-call-expand-function
                           (intern "EXPAND-FF-CALL" "ARM-LINUX")
			   :ff-call-struct-return-by-implicit-arg-function
                           (intern "RECORD-TYPE-RETURNS-STRUCTURE-AS-FIRST-ARG"
                                   "ARM-LINUX")
                           :callback-bindings-function
                           (intern "GENERATE-CALLBACK-BINDINGS" "ARM-LINUX")
                           :callback-return-value-function
                           (intern "GENERATE-CALLBACK-RETURN-VALUE" "ARM-LINUX"))))))
        (install-standard-foreign-types ftd)
        (use-interface-dir :libc ftd)
        (setf (backend-target-foreign-type-data backend) ftd))))

(pushnew *arm-backend* *known-backends* :key #'backend-name)


(defmacro make-fake-stack-frame (sp next-sp fn lr vsp xp)
  `(ccl::%istruct 'arm::fake-stack-frame ,sp ,next-sp ,fn ,lr ,vsp ,xp))

(require "ARM-VINSNS")




	      


  
