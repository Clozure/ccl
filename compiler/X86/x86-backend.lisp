;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2005, Clozure Associates
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
(defvar *x862-specials* nil)
(let* ((newsize (%i+ (next-nx-num-ops) 10))
       (old *x862-specials*)
       (oldsize (length old)))
  (declare (fixnum newsize oldsize))
  (unless (>= oldsize newsize)
    (let* ((v (make-array newsize :initial-element nil)))
      (dotimes (i oldsize (setq *x862-specials* v))
        (setf (svref v i) (svref old i))))))

(defun x86-encode-vinsn-operand-type (thing backend)
  (when thing
    (if (atom thing)
      (x86::encode-operand-type :label)
      (ecase (car thing)
        (:% (ecase (arch::target-lisp-node-size (backend-target-arch backend))
              (8 (x86::encode-operand-type :reg64))
              (4 (x86::encode-operand-type :reg32))))
	(:%acc (ecase (arch::target-lisp-node-size (backend-target-arch backend))
              (8 (x86::encode-operand-type :reg64 :acc))
              (4 (x86::encode-operand-type :reg32 :acc))))
        (:%q (x86::encode-operand-type :reg64))
        (:%accq (x86::encode-operand-type :reg64 :acc))
        (:%l (x86::encode-operand-type :reg32))
        (:%accl (x86::encode-operand-type :reg32 :acc))
        (:%w (x86::encode-operand-type :reg16))
        (:%accw (x86::encode-operand-type :reg16 :acc))
        (:%b (x86::encode-operand-type :reg8))
        (:%accb (x86::encode-operand-type :reg8 :acc))
        (:%xmm (x86::encode-operand-type :regxmm))
        (:%mmx (x86::encode-operand-type :regmmx))
        (:@ (x86::encode-operand-type :anymem))
        (:$1 (x86::encode-operand-type :imm1) )
        (:$b (x86::encode-operand-type :imm8s ))
        (:$ub (x86::encode-operand-type :imm8))
        (:$w (x86::encode-operand-type :imm16))
        (:$l (x86::encode-operand-type :imm32s))
        (:$ul  (x86::encode-operand-type :imm32))
        (:$q (x86::encode-operand-type :imm64))
        (:%shift (x86::encode-operand-type :shiftcount :reg8))
	(:$self (x86::encode-operand-type :self))))))

(defun lookup-x86-opcode (form backend)
  (when (consp form)
    (let* ((name (string (car form)))
           (templates (gethash name x86::*x86-opcode-template-lists*)))
      (when templates
        (flet ((optype (thing)
                 (x86-encode-vinsn-operand-type thing backend)))
          (let* ((operands (cdr form))
                 (type0 (optype (pop operands)))
                 (type1 (optype (pop operands)))
                 (type2 (optype (car operands))))
            (dolist (template templates)
              (when (x86::match-template-types template type0 type1 type2)
                (collect ((types))
                  (if type0 (types type0))
                  (if type1 (types type1))
                  (if type2 (types type2))
                  (return (values (x86::x86-opcode-template-ordinal template)
                                  (types))))))))))))

(defun fixup-opcode-ordinals (vinsn-template opcode-templates)
  (let* ((changed ()))
    (dolist (vinsn-opcode (vinsn-template-opcode-alist vinsn-template))
      (destructuring-bind (old-ordinal name &optional type0 type1 type2) vinsn-opcode
        (let* ((opcode-templates (gethash name opcode-templates)))
          (unless opcode-templates
            (error "Unknown X86 instruction - ~a.  Odd, because it was once a known instruction." name))
        (let* ((new-ordinal (dolist (template opcode-templates)
                              (when (x86::match-template-types template type0 type1 type2)
                                (return (x86::x86-opcode-template-ordinal template))))))
          (unless new-ordinal
            (error "No match for opcode ~s in ~s" vinsn-opcode vinsn-template))
          (unless (eql old-ordinal new-ordinal)
            (setf (car vinsn-opcode) new-ordinal)
            (push (cons old-ordinal new-ordinal) changed))))))
    (when changed
      ;;(format t "~& opcode ordinals changed in ~s: ~s" vinsn-template changed)
      (flet ((update-instruction (i)
               (when (consp i)
                 ;; An :ANCHORED-UUO directive contains a real
                 ;; (vinsn-encoded) instruction (typically a UUO) in
                 ;; its cadr.  Other directives won't contain embedded
                 ;; instructions and whatever's in their CARs won't
                 ;; match in the assoc below.
                 (when (eq (car i) :anchored-uuo)
                   (setq i (cadr i)))
                 (let* ((pair (assoc (car i) changed :test #'eq)))
                   (when pair
                     (setf (car i) (cdr pair)))))))
        (labels ((fixup-form (form)
                   (unless (atom form)
                     (if (atom (car form))
                       (update-instruction form)
                       (dolist (f (cdr form))
                         (fixup-form f))))))
          (dolist (form (vinsn-template-body vinsn-template))
            (fixup-form form)))))))

(defparameter *report-missing-vinsns* nil)

(defun fixup-x86-vinsn-templates (template-hash opcode-templates)
  (maphash #'(lambda (name vinsn-template)
               (if (not (cdr vinsn-template))
                 (when *report-missing-vinsns*
                   (warn "Reference to undefined vinsn ~s" name))
                 (fixup-opcode-ordinals (cdr vinsn-template) opcode-templates)))
           template-hash))



;;; This defines a template.  All expressions in the body must be
;;; evaluable at macroexpansion time.
(defun define-x86-vinsn (backend vinsn-name results args temps body)
  (let* ((opcode-lookup (backend-lookup-opcode backend))
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
                          (and (consp (cadr x)) (eq (caadr x) :label) (consp (cdadr x)) (null (cddadr x)))
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
            (labels ((simplify-simple-operand (op)
                       (if (atom op)
                         (if (typep op 'fixnum)
                           op
                           (if (eq op :rcontext)
                             op
                             (if (constantp op)
                               (progn
                                 (if (keywordp op)
                                   (pushnew op referenced-labels))
                                 (eval op))
                               (find-name op))))
                         (if (eq (car op) :^)
                           (list :^ (simplify-simple-operand (cadr op)))
                           (if (eq (car op) :apply)
                             `(,(cadr op) ,@(mapcar #'simplify-operand (cddr op)))
                             (if (member (car op)
                                         '(:tra :align :byte :word :long :quad :talign))
                               `(,(car op) ,(simplify-operand (cadr op)))
                               (simplify-operand (eval op))))))) ; Handler-case this?
                     (simplify-memory-operand (op)
                       ;; This happens to be the only place that
                       ;; we allow segment registers.
                       (let* ((seg nil)
                              (disp nil)
                              (base nil)
                              (index nil)
                              (scale nil))
                         (do* ((form op (cdr form)))
                              ((null form) (list seg disp base index scale))
                           (let* ((head (car form)))
                             (if (consp head)
                               (case (car head)
                                 (:%seg
                                  (if (eq form op)
                                    (setq seg (simplify-operand (cadr head)))
                                    (error "Bad :%seg in ~s" op)))
                                 ((:%q :% :%l)
                                  (let* ((r (simplify-operand head)))
                                    (if base
                                      (if index
                                        (error "Extra register ~s in ~s"
                                               head op)
                                        (setq index r))
                                      (setq base r))))
                                 (t
                                  (if (and (null (cdr form))
                                           (or disp base index))
                                    (progn
                                      (setq scale (simplify-simple-operand head))
                                      (if (and base (not index))
                                        (setq index base base nil)))
                                    (if (not (or disp base index))
                                      (setq disp (simplify-simple-operand head))
                                      (error "~s not expected in ~s"  op)))))
                               (if (and (null (cdr form))
                                        (or disp base index))
                                 (progn
                                   (setq scale (simplify-simple-operand head))
                                   (if (and base (not index))
                                     (setq index base base nil)))
                                 (if (not (or disp base index))
                                   (setq disp (simplify-simple-operand head))
                                   (error "~s not expected in ~s"  op))))))))
                     (simplify-operand (op)
                       (cond ((atom op)
                              (simplify-simple-operand op))
                             ((eq (car op) :@)
                              (cons :@
                                    (simplify-memory-operand (cdr op))))
                             ((member (car op)
                                      '(:% :%q :%l :%w :%b
					:%acc :%accq :%accl :%accw :%accb
					:$ :$1 :$b :$ub :$w :$l
                                        :$ul :$q :%mmx :%xmm :%shift :$self))
                              (simplify-simple-operand (cadr op)))
                             (t
                              (simplify-simple-operand op)))))
              (labels ((simplify-constraint (guard)
                         ;; A constraint is one of

                         ;; (:eq|:lt|:gt vreg-name constant) ; "value"
                         ;; of vreg relop constant

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
                                 (progn
                                   (list opname
                                         (if (eq opname :anchored-uuo)
                                           (simplify-form (car opvals))
                                           (simplify-operand (car opvals)))))
                                 (let* ((name (string opname)))
                                   (multiple-value-bind (opnum types)
                                       (funcall opcode-lookup form backend)
                                     (if (not opnum)
                                       (error "Unknown ~A instruction in ~s" backend-name form)
                                       (let* ((opvals (mapcar #'simplify-operand opvals)))
                                         (setf (assq opnum opcode-alist) (cons name types))
                                         `(,opnum ,@opvals)))))))))))
                (let* ((template (make-vinsn-template :name vinsn-name
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
                                                                   "local-label ~S was referenced but ~
                                                                    never defined in VINSN-TEMPLATE definition for ~s"
                                                                   ref vinsn-name))))
                                                      :local-labels local-labels
                                                      :attributes attrs
                                                      :opcode-alist opcode-alist)))
                  
                  `(progn
                    (set-vinsn-template ',vinsn-name ,template ,template-hash)
                    (record-source-file ',vinsn-name ',source-indicator)
                    ',vinsn-name))))))))))



#+x8632-target
(require "X8632-BACKEND")
#+x8664-target
(require "X8664-BACKEND")

(defparameter *x86-backend*
  #+x8632-target *x8632-backend*
  #+x8664-target *x8664-backend*
  #-x86-target nil)

	      
(defun fixup-x86-backend (&rest args)
  #+x8632-target (apply #'fixup-x8632-backend args)
  #+x8664-target (apply #'fixup-x8664-backend args)
  #-x86-target (declare (ignore args))
  )

(provide "X86-BACKEND")
