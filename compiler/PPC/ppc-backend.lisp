;;;-*- Mode: Lisp; Package: CCL -*-
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

(in-package "CCL")

(next-nx-defops)
(defvar *ppc2-specials* nil)
(let* ((newsize (%i+ (next-nx-num-ops) 10))
       (old *ppc2-specials*)
       (oldsize (length old)))
  (declare (fixnum newsize oldsize))
  (unless (>= oldsize newsize)
    (let* ((v (make-array newsize :initial-element nil)))
      (dotimes (i oldsize (setq *ppc2-specials* v))
        (setf (svref v i) (svref old i))))))

;;; This defines a template.  All expressions in the body must be
;;; evaluable at macroexpansion time.
(defun define-ppc-vinsn (backend vinsn-name results args temps body)
  (let* ((opcode-vector (backend-lap-opcodes backend))
	 (opcode-lookup (backend-lookup-opcode backend))
	 (opcode-expander (backend-lookup-macro backend))
	 (backend-name (backend-name backend))
         (arch-name (backend-target-arch-name backend))
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
      (declare (dynamic-extent valid-spec-name add-spec-name))
      (when (consp vinsn-name)
        (setq attrs (encode-vinsn-attributes (cdr vinsn-name))
              vinsn-name (car vinsn-name)))
      (unless (and (symbolp vinsn-name) (eq *CCL-PACKAGE* (symbol-package vinsn-name)))
        (setq vinsn-name (intern (string vinsn-name) *CCL-PACKAGE*)))
      (dolist (n (append args temps))
        (add-spec-name (valid-spec-name n)))
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
                                 form
                                 (let* ((name (string opname))
                                        (opnum (funcall opcode-lookup name)))
                                   (if (and (not opnum) opcode-expander)
                                     (let* ((expander (funcall opcode-expander name)))
                                       (if expander
                                         (simplify-form (funcall expander form nil))
                                         (error "Unknown ~A instruction in ~s" backend-name form)))
                                     (let* ((opcode (if (< -1 opnum (length opcode-vector))
                                                      (svref opcode-vector opnum)
                                                      (error "~& Invalid ~A opcode: ~s" backend-name name)))
                                            (opvals (mapcar #'simplify-operand opvals)))
                                       (setf (assq opnum opcode-alist) name)
                                       (let* ((operands (opcode-vinsn-operands opcode))
                                              (nmin (opcode-min-vinsn-args opcode))
                                              (nmax (opcode-max-vinsn-args opcode))
                                              (nhave (length opvals)))
                                         (declare (fixnum nreq nhave))
                                         (if (= nhave nmax)
                                           `(,opnum ,@opvals)
                                           (if (> nhave nmax)
                                             (error "Too many operands in ~s (~a accepts at most ~d)"
                                                    (cdr w) name nmax)
                                             (if (= nhave nmin)
                                               (let* ((newops ()))
                                                 (dolist (op operands `(,opnum ,@(nreverse newops)))
                                                   (let* ((flags (operand-flags op)))
                                                     (unless (logbitp operand-fake flags)
                                                       (push (if (logbitp operand-optional flags)
                                                               0
                                                               (pop opvals))
                                                             newops)))))
                                               (error "Too few operands in ~s : (~a requires at least ~d)"
                                                      (cdr w) name nmin))))))))))))))
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

#+ppc32-target
(require "PPC32-BACKEND")
#+ppc64-target
(require "PPC64-BACKEND")

(defparameter *ppc-backend*
  #+ppc32-target *ppc32-backend*
  #+ppc64-target *ppc64-backend*
  #-(or ppc32-target ppc64-target)
  nil)


	      
(defun fixup-ppc-backend (&rest args)
  #+ppc32-target (apply #'fixup-ppc32-backend args)
  #+ppc64-target (apply #'fixup-ppc64-backend args))

  