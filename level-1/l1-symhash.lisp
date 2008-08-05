;;;-*-Mode: LISP; Package: CCL -*-
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

(declaim (special %all-packages%))
(declaim (list %all-package%))
(declaim (type package *package*))



(defun dereference-base-string-or-symbol (s)
  (if (symbolp s)
    (dereference-base-string (symbol-name s))
    (dereference-base-string s)))

(defun dereference-base-string-or-symbol-or-char (s)
  (if (typep s 'character)
    (values (make-string 1 :element-type 'base-char :initial-element s) 0 1)
    (dereference-base-string-or-symbol s)))


(defun %string= (string1 string2 start1 end1)
  (declare (optimize (speed 3) (safety 0))
           (fixnum start1 end1))
  (when (eq (length string2) (%i- end1 start1))
    (do* ((i start1 (1+ i))
          (j 0 (1+ j)))
         ((>= i end1))
      (declare (fixnum i j))
      (when (not (eq (%scharcode string1 i)(%scharcode string2 j)))
        (return-from %string= nil)))
    t))




(defun export (sym-or-syms &optional (package *package*))
  "Exports SYMBOLS from PACKAGE, checking that no name conflicts result."
  (setq package (pkg-arg package))
  (if (atom sym-or-syms)
    (let* ((temp (cons sym-or-syms nil)))
      (declare (dynamic-extent temp))
      (export temp package))
    (progn
      (dolist (sym sym-or-syms)
        (unless (symbolp sym) (return (setq sym-or-syms  (mapcar #'(lambda (s) (require-type s 'symbol)) sym-or-syms)))))
      ;; First, see if any packages used by the package being
      ;; "exported from" already contain a distinct non-shadowing
      ;; symbol that conflicts with one of those that we're trying to
      ;; export.
      (let* ((conflicts (check-export-conflicts sym-or-syms package)))
        (if conflicts
          (progn 
            (resolve-export-conflicts conflicts package)
            (export sym-or-syms package))
          (let* ((missing nil) (need-import nil))
            (dolist (s sym-or-syms) 
              (multiple-value-bind (foundsym foundp) (%findsym (symbol-name s) package)
                (if (not (and foundp (eq s foundsym)))
                  (push s missing)
                  (if (eq foundp :inherited)
                    (push s need-import)))))
            (when missing
              (cerror "Import missing symbols before exporting them from ~S."
                      'export-requires-import
                      :package  package
                      :to-be-imported missing)
              (import missing package))
            (if need-import (import need-import package))
            ; Can't lose now: symbols are all directly present in package.
            ; Ensure that they're all external; do so with interrupts disabled
            (without-interrupts
             (let* ((etab (pkg.etab package))
                    (ivec (car (pkg.itab package))))
               (dolist (s sym-or-syms t)
                 (multiple-value-bind (foundsym foundp internal-offset)
                                      (%findsym (symbol-name s) package)
                   (when (eq foundp :internal)
                     (setf (%svref ivec internal-offset) (package-deleted-marker))
                     (let* ((pname (symbol-name foundsym)))
                       (%htab-add-symbol foundsym etab (nth-value 2 (%get-htab-symbol pname (length pname) etab)))))))))))))))

(defun check-export-conflicts (symbols package)
  (let* ((conflicts nil))
    (with-package-lock (package)
      (dolist (user (pkg.used-by package) conflicts)
        (with-package-lock (user)
          (dolist (s symbols)
            (multiple-value-bind (foundsym foundp) (%findsym (symbol-name s) user)
              (if (and foundp (neq foundsym s) (not (memq foundsym (pkg.shadowed user))))
                (push (list (eq foundp :inherited) s user foundsym) conflicts)))))))))
  


(defun keywordp (x)
  "Return true if Object is a symbol in the \"KEYWORD\" package."
  (and (symbolp x) (eq (symbol-package x) *keyword-package*)))

;;;No type/range checking.  For DO-SYMBOLS and friends.
(defun %htab-symbol (array index)
  (let* ((sym (%svref array index)))
    (if (symbolp sym)
      (values (%symptr->symbol sym) t)
      (values nil nil))))

(defun find-all-symbols (name)
  "Return a list of all symbols in the system having the specified name."
  (let* ((syms ())
         (pname (ensure-simple-string (string name)))
         (len (length pname)))
    (with-package-list-read-lock
        (dolist (p %all-packages% syms)
          (with-package-lock (p)
            (multiple-value-bind (sym foundp) (%find-package-symbol pname p len)
              (if foundp (pushnew sym syms :test #'eq))))))))
      

(defun list-all-packages ()
  "Return a list of all existing packages."
  (with-package-list-read-lock (copy-list %all-packages%)))

(defun rename-package (package new-name &optional new-nicknames)
  "Changes the name and nicknames for a package."
  (setq package (pkg-arg package)
        new-name (ensure-simple-string (string new-name)))
  (with-package-lock (package)
    (let* ((names (pkg.names package)))
      (declare (type cons names))
      (dolist (n names)
        (let* ((ref (register-package-ref n)))
          (setf (package-ref.pkg ref) nil)))
      (rplaca names (new-package-name new-name package))
      (let* ((ref (register-package-ref (car names))))
        (setf (package-ref.pkg ref) package))
      (rplacd names nil))
    (%add-nicknames new-nicknames package)))

;;; Someday, this should become LISP:IN-PACKAGE.
(defun old-in-package (name &key 
                        nicknames 
                        (use nil use-p) 
                        (internal-size 60)
                        (external-size 10))
  (let ((pkg (find-package (setq name (string name)))))
    (if pkg
      (progn
        (use-package use pkg)
        (%add-nicknames nicknames pkg))
      (setq pkg
            (make-package name 
                          :nicknames nicknames
                          :use (if use-p use *make-package-use-defaults*)
                          :internal-size internal-size
                          :external-size external-size)))
    (setq *package* pkg)))


(defvar *make-package-use-defaults* '("COMMON-LISP" "CCL"))

;;; On principle, this should get exported here.  Unfortunately, we
;;; can't execute calls to export quite yet.


(defun make-package (name &key
                          nicknames
                          (use *make-package-use-defaults*)
                          (internal-size 60)
                          (external-size 10))
  "Make a new package having the specified NAME, NICKNAMES, and 
  USE list. :INTERNAL-SYMBOLS and :EXTERNAL-SYMBOLS are
  estimates for the number of internal and external symbols which
  will ultimately be present in the package. The default value of
  USE is implementation-dependent, and in this implementation
  it is NIL."
  (setq internal-size (require-type internal-size 'fixnum)
        external-size (require-type external-size 'fixnum))
  (let ((pkg (gvector :package 
                      (%new-package-hashtable internal-size)
                      (%new-package-hashtable external-size)
                      nil
                      nil
                      (list (new-package-name name))
                      nil
                      (make-read-write-lock)
                      nil)))
    (let* ((ref (register-package-ref name)))
      (setf (package-ref.pkg ref) pkg))
    (use-package use pkg)
    (%add-nicknames nicknames pkg)
    (with-package-list-write-lock
        (push pkg %all-packages%))
    pkg))

(defun new-package-name (name &optional package)
  (do* ((prompt "Enter package name to use instead of ~S ."))
       ((let* ((found (find-package (setq name (ensure-simple-string (string name))))))
          (or (not found)
              (eq package found)))
        name)
    (restart-case (%error "Package name ~S is already in use." (list name) (%get-frame-ptr))
      (new-name (new-name)
                :report (lambda (s) (format s prompt name))
                :interactive 
                (lambda () 
                  (list (block nil (catch-cancel (return (get-string-from-user
                                                          (format nil prompt name))))
                               nil)))
                (if new-name (setq name new-name))))))
       
(defun new-package-nickname (name package)
  (setq name (string name))
  (let* ((other (find-package name))
         (prompt "Enter package name to use instead of ~S ."))
    (if other
      (unless (eq other package)
        (let* ((conflict-with-proper-name (string= (package-name other) name))
               (condition (make-condition 'package-name-conflict-error
                                          :package package
                                          :format-arguments (list name other)
                                          :format-control (%str-cat "~S is already "
                                                                   (if conflict-with-proper-name
                                                                     "the "
                                                                     "a nick")
                                                                   "name of ~S."))))
          (restart-case (%error condition nil (%get-frame-ptr))
            (continue ()
                      :report (lambda (s) (format s "Don't make ~S a nickname for ~S" name package)))
            (new-name (new-name)
                      :report (lambda (s) (format s prompt name))
                      :interactive 
                      (lambda () 
                        (list (block nil (catch-cancel (return (get-string-from-user
                                                                (format nil prompt name))))
                                     nil)))
                      (if new-name (new-package-nickname new-name package)))
            (remove-conflicting-nickname ()
                                         :report (lambda (s)
                                                   (format s "Remove conflicting-nickname ~S from ~S." name other))
                                         :test (lambda (&rest ignore) (declare (ignore ignore)) (not conflict-with-proper-name))
                                         (rplacd (pkg.names other)
                                                 (delete name (cdr (pkg.names other)) :test #'string=))
                                         name))))
      name)))

(defun %add-nicknames (nicknames package)
  (let ((names (pkg.names package)))
    (dolist (name nicknames package)
      (let* ((ok-name (new-package-nickname name package)))
        (when ok-name
          (let* ((ref (register-package-ref ok-name)))
            (setf (package-ref.pkg ref) package)
            (push ok-name (cdr names))))))))

(defun find-symbol (string &optional package)
  "Return the symbol named STRING in PACKAGE. If such a symbol is found
  then the second value is :INTERNAL, :EXTERNAL or :INHERITED to indicate
  how the symbol is accessible. If no symbol is found then both values
  are NIL."
  (multiple-value-bind (sym flag)
      (%findsym (ensure-simple-string string) (pkg-arg (or package *package*)))
    (values sym flag)))

(defun %pkg-ref-find-symbol (string ref)
  (multiple-value-bind (sym flag)
      (%findsym (ensure-simple-string string)
                (or (package-ref.pkg ref)
                    (setf (package-ref.pkg ref)
                          (%find-pkg (package-ref.name ref)))))
    (values sym flag)))
    
;;; Somewhat saner interface to %find-symbol
(defun %findsym (string package)
  (%find-symbol string (length string) package))

(eval-when (:compile-toplevel)
  (declaim (inline %intern)))

(defun %intern (str package)
  (setq str (ensure-simple-string str))
  (with-package-lock (package)
   (multiple-value-bind (symbol where internal-offset external-offset) 
                        (%find-symbol str (length str) package)
     (if where
       (values symbol where)
       (values (%add-symbol str package internal-offset external-offset) nil)))))


(defun intern (str &optional (package *package*))
  "Return a symbol in PACKAGE having the specified NAME, creating it
  if necessary."
  (%intern str (pkg-arg package)))

(defun %pkg-ref-intern (str ref)
  (%intern str (or (package-ref.pkg ref)
                   (setf (package-ref.pkg ref)
                         (%find-pkg (package-ref.name ref))))))

(defun unintern (symbol &optional (package *package*))
  "Makes SYMBOL no longer present in PACKAGE. If SYMBOL was present
  then T is returned, otherwise NIL. If PACKAGE is SYMBOL's home
  package, then it is made uninterned."
  (setq package (pkg-arg package))
  (setq symbol (require-type symbol 'symbol))
  (multiple-value-bind (foundsym table index) (%find-package-symbol (symbol-name symbol) package)
    (when (and table (eq symbol foundsym))
      (when (memq symbol (pkg.shadowed package))
        ;; A conflict is possible if more than one distinct
        ;; similarly-named external symbols exist in the packages used
        ;; by this one.  Grovel around looking for such conflicts; if
        ;; any are found, signal an error (via %kernel-restart) which
        ;; offers to either shadowing-import one of the conflicting
        ;; symbols into the current package or abandon the attempt to
        ;; unintern in the first place.
        (let* ((first nil)
               (first-p nil)
               (name (symbol-name symbol))
               (len (length name))
               (others nil))
          (declare (dynamic-extent first))
          (with-package-lock (package)
            (dolist (pkg (pkg.used package))
              (with-package-lock (pkg)
                (multiple-value-bind (found conflicting-sym) (%get-htab-symbol name len (pkg.etab pkg))
                  (when found
                    (if first-p
                      (unless (or (eq conflicting-sym first)
                                  (memq conflicting-sym others))
                        (push conflicting-sym others))
                      (setq first-p t first conflicting-sym)))))))
          (when others
            ;;If this returns, it will have somehow fixed things.
            (return-from unintern (%kernel-restart $xunintc symbol package (cons first others)))))
        ;; No conflicts found, but symbol was on shadowing-symbols list.  Remove it atomically.
        (do* ((head (cons nil (pkg.shadowed package)))
              (prev head next)
              (next (cdr prev) (cdr next)))
             ((null next))              ; Should never happen
          (declare (dynamic-extent head) 
                   (list head prev next)
                   (optimize (speed 3) (safety 0)))
          (when (eq (car next) symbol)
            (setf (cdr prev) (cdr next)
                  (pkg.shadowed package) (cdr head))
            (return))))
      ;; Now remove the symbol from package; if package was its home
      ;; package, set its package to NIL.  If we get here, the "table"
      ;; and "index" values returned above are still valid.
      (%svset (car table) index (package-deleted-marker))
      (when (eq (symbol-package symbol) package)
        (%set-symbol-package symbol nil))
      t)))

(defun import-1 (package sym)
  (multiple-value-bind (conflicting-sym type internal-offset external-offset) (%findsym (symbol-name sym) package)
    (if (and type (neq conflicting-sym sym))
      (let* ((external-p (eq type :inherited))
             (condition (make-condition 'import-conflict-error 
                                        :package package
                                        :imported-sym sym
                                        :conflicting-sym conflicting-sym
                                        :conflict-external external-p)))
        (restart-case (error condition)
          (continue ()
                    :report (lambda (s) (format s "Ignore attempt to import ~S to ~S." sym package)))
          (resolve-conflict ()
                            :report (lambda (s)
                                      (let* ((package-name (package-name package)))
                                        (if external-p 
                                          (format s "~A ~s in package ~s ." 'shadowing-import sym package-name)
                                          (format s "~A ~s from package ~s ." 'unintern conflicting-sym package-name))))
                            (if external-p 
                              (shadowing-import-1 package sym)
                              (progn
                                (unintern conflicting-sym package)
                                (import-1 package sym))))))
      (unless (or (eq type :external) (eq type :internal))
        (%insert-symbol sym package internal-offset external-offset)))))


(defun import (sym-or-syms &optional package)
  "Make SYMBOLS accessible as internal symbols in PACKAGE. If a symbol
  is already accessible then it has no effect. If a name conflict
  would result from the importation, then a correctable error is signalled."
  (setq package (pkg-arg (or package *package*)))
  (if (listp sym-or-syms)
    (dolist (sym sym-or-syms)
      (import-1 package sym))
    (import-1 package sym-or-syms))
  t)

(defun shadow-1 (package sym)
  (let* ((pname (ensure-simple-string (string sym)))
         (len (length pname)))
    (without-interrupts
     (multiple-value-bind (symbol where internal-idx external-idx) (%find-symbol pname len package)
       (if (or (eq where :internal) (eq where :external))
         (pushnew symbol (pkg.shadowed package))
         (push (%add-symbol pname package internal-idx external-idx) (pkg.shadowed package)))))
    nil))

(defun shadow (sym-or-symbols-or-string-or-strings &optional package)
  "Make an internal symbol in PACKAGE with the same name as each of
  the specified SYMBOLS. If a symbol with the given name is already
  present in PACKAGE, then the existing symbol is placed in the
  shadowing symbols list if it is not already present."
  (setq package (pkg-arg (or package *package*)))
  (if (listp sym-or-symbols-or-string-or-strings)
    (dolist (s sym-or-symbols-or-string-or-strings)
      (shadow-1 package s))
    (shadow-1 package sym-or-symbols-or-string-or-strings))
  t)

(defun unexport (sym-or-symbols &optional package)
  "Makes SYMBOLS no longer exported from PACKAGE."
  (setq package (pkg-arg (or package *package*)))
  (if (listp sym-or-symbols)
    (dolist (sym sym-or-symbols)
      (unexport-1 package sym))
    (unexport-1 package sym-or-symbols))
  t)

(defun unexport-1 (package sym)
  (when (eq package *keyword-package*)
    (error "Can't unexport ~S from ~S ." sym package))
  (multiple-value-bind (foundsym foundp internal-offset external-offset)
                       (%findsym (symbol-name sym) package)
    (unless foundp
      (error 'symbol-name-not-accessible
             :symbol-name (symbol-name sym)
             :package package))
    (when (eq foundp :external)
      (let* ((evec (car (pkg.etab package)))
             (itab (pkg.itab package))
             (ivec (car itab))
             (icount&limit (cdr itab)))
        (declare (type cons itab icount&limit))
        (setf (svref evec external-offset) (package-deleted-marker))
        (setf (svref ivec internal-offset) (%symbol->symptr foundsym))
        (if (eql (setf (car icount&limit)
                       (the fixnum (1+ (the fixnum (car icount&limit)))))
                 (the fixnum (cdr icount&limit)))
          (%resize-htab itab)))))
  nil)

;;; Both args must be packages.
(defun %use-package-conflict-check (using-package package-to-use)
  (let ((already-used (pkg.used using-package)))
    (unless (or (eq using-package package-to-use)
                (memq package-to-use already-used))
      ;; There are two types of conflict that can potentially occur:
      ;;   1) An external symbol in the package being used conflicts
      ;;        with a symbol present in the using package
      ;;   2) An external symbol in the package being used conflicts
      ;;        with an external symbol in some other package that's
      ;;        already used.
      (let* ((ext-ext-conflicts nil)
             (used-using-conflicts nil)
             (shadowed-in-using (pkg.shadowed using-package))
             (to-use-etab (pkg.etab package-to-use)))
        (without-interrupts
         (dolist (already already-used)
           (let ((user (if (memq package-to-use (pkg.used-by already))
                         package-to-use
                         (if (memq package-to-use (pkg.used already))
                           already))))
             (if user
               (let* ((used (if (eq user package-to-use) already package-to-use))
                      (user-etab (pkg.etab user))
                      (used-etab (pkg.etab used)))
                 (dolist (shadow (pkg.shadowed user))
                   (let ((sname (symbol-name shadow)))
                     (unless (member sname shadowed-in-using :test #'string=)
                       (let ((len (length sname)))
                         (when (%get-htab-symbol sname len user-etab)   ; external in user
                           (multiple-value-bind (external-in-used used-sym) (%get-htab-symbol sname len used-etab)
                             (when (and external-in-used (neq used-sym shadow))
                               (push (list shadow used-sym) ext-ext-conflicts)))))))))
               ;; Remember what we're doing here ?
               ;; Neither of the two packages use the other.  Iterate
               ;; over the external symbols in the package that has
               ;; the fewest external symbols and note conflicts with
               ;; external symbols in the other package.
               (let* ((smaller (if (%i< (%cadr to-use-etab) (%cadr (pkg.etab already)))
                                 package-to-use
                                 already))
                      (larger (if (eq smaller package-to-use) already package-to-use))
                      (larger-etab (pkg.etab larger))
                      (smaller-v (%car (pkg.etab smaller))))
                 (dotimes (i (uvsize smaller-v))
                   (declare (fixnum i))
                   (let ((symptr (%svref smaller-v i)))
                     (when (symbolp symptr)
                       (let* ((sym (%symptr->symbol symptr))
                              (symname (symbol-name sym)))
                         (unless (member symname shadowed-in-using :test #'string=)
                           (multiple-value-bind (found-in-larger sym-in-larger)
                                                (%get-htab-symbol symname (length symname) larger-etab)
                             (when (and found-in-larger (neq sym-in-larger sym))
                               (push (list sym sym-in-larger) ext-ext-conflicts))))))))))))
         ;; Now see if any non-shadowed, directly present symbols in
         ;; the using package conflicts with an external symbol in the
         ;; package being used.  There are two ways of doing this; one
         ;; of them -may- be much faster than the other.
         (let* ((to-use-etab-size (%cadr to-use-etab))
                (present-symbols-size (%i+ (%cadr (pkg.itab using-package)) (%cadr (pkg.etab using-package)))))
           (unless (eql 0 present-symbols-size)
             (if (%i< present-symbols-size to-use-etab-size)
               ;; Faster to look up each present symbol in to-use-etab.
               (let ((htabvs (list (%car (pkg.etab using-package)) (%car (pkg.itab using-package)))))
                 (declare (dynamic-extent htabvs))
                 (dolist (v htabvs)
                   (dotimes (i (the fixnum (uvsize v)))
                     (declare (fixnum i))
                     (let ((symptr (%svref v i)))
                       (when (symbolp symptr)
                         (let* ((sym (%symptr->symbol symptr)))
                           (unless (memq sym shadowed-in-using)
                             (let* ((name (symbol-name symptr)))
                               (multiple-value-bind (found-p to-use-sym) (%get-htab-symbol name (length name) to-use-etab)
                                 (when (and found-p (neq to-use-sym sym))
                                   (push (list sym to-use-sym) used-using-conflicts)))))))))))
               ;; See if any external symbol present in the package
               ;; being used conflicts with any symbol present in the
               ;; using package.
               (let ((v (%car to-use-etab)))
                 (dotimes (i (uvsize v))
                   (declare (fixnum i))
                   (let ((symptr (%svref v i)))
                     (when (symbolp symptr)
                       (let* ((sym (%symptr->symbol symptr)))
                         (multiple-value-bind (using-sym found-p) (%find-package-symbol (symbol-name sym) using-package)
                           (when (and found-p
                                      (neq sym using-sym)
                                      (not (memq using-sym shadowed-in-using)))
                             (push (list using-sym sym) used-using-conflicts))))))))))))
        (values ext-ext-conflicts used-using-conflicts)))))

(defun use-package-1 (using-package package-to-use)
  (if (eq (setq package-to-use (pkg-arg package-to-use))
          *keyword-package*)
    (error "~S can't use ~S." using-package package-to-use))
  (do* ((used-external-conflicts nil)
        (used-using-conflicts nil))
       ((and (null (multiple-value-setq (used-external-conflicts used-using-conflicts)
                     (%use-package-conflict-check using-package package-to-use)))
             (null used-using-conflicts)))
    (if used-external-conflicts
      (%kernel-restart $xusecX package-to-use using-package used-external-conflicts)
      (if used-using-conflicts
        (%kernel-restart $xusec package-to-use using-package used-using-conflicts))))
  (unless (memq using-package (pkg.used-by package-to-use))   ;  Not already used in break loop/restart, etc.
    (push using-package (pkg.used-by package-to-use))
    (push package-to-use (pkg.used using-package))))

(defun use-package (packages-to-use &optional package)
  "Add all the PACKAGES-TO-USE to the use list for PACKAGE so that
  the external symbols of the used packages are accessible as internal
  symbols in PACKAGE."
  (setq package (pkg-arg (or package *package*)))
  (if (listp packages-to-use)
    (dolist (to-use packages-to-use)
      (use-package-1 package to-use))
    (use-package-1 package packages-to-use))
  t)

(defun shadowing-import-1 (package sym)
  (let* ((pname (symbol-name sym))
         (len (length pname))
         (need-add t))
    (without-interrupts
     (multiple-value-bind (othersym htab offset) (%find-package-symbol pname package)
       (if htab
         (if (eq othersym sym)
           (setq need-add nil)
           (progn                       ; Delete conflicting symbol
             (if (eq (symbol-package othersym) package)
               (%set-symbol-package othersym nil))
             (setf (%svref (car htab) offset) (package-deleted-marker))
             (setf (pkg.shadowed package) (delete othersym (pkg.shadowed package) :test #'eq)))))
       (if need-add                   ; No symbols with same pname; intern & shadow
         (multiple-value-bind (xsym foundp internal-offset external-offset) 
                              (%find-symbol pname len package)
           (declare (ignore xsym foundp))
           (%insert-symbol sym package internal-offset external-offset)))
       (pushnew sym (pkg.shadowed package))
       nil))))

(defun shadowing-import (sym-or-syms &optional (package *package*))
  "Import SYMBOLS into package, disregarding any name conflict. If
  a symbol of the same name is present, then it is uninterned."
  (setq package (pkg-arg package))
  (if (listp sym-or-syms)
    (dolist (sym sym-or-syms)
      (shadowing-import-1 package sym))
    (shadowing-import-1 package sym-or-syms))
  t)

(defun unuse-package (packages-to-unuse &optional package)
  "Remove PACKAGES-TO-UNUSE from the USE list for PACKAGE."
  (let ((p (pkg-arg (or package *package*))))
    (flet ((unuse-one-package (unuse)
            (setq unuse (pkg-arg unuse))
            (setf (pkg.used p) (nremove unuse (pkg.used p))
                  (pkg.used-by unuse) (nremove p (pkg.used-by unuse)))))
      (declare (dynamic-extent #'unuse-one-package))
      (if (listp packages-to-unuse)
        (dolist (u packages-to-unuse) (unuse-one-package u))
        (unuse-one-package packages-to-unuse))
      t)))

(defun delete-package (package)
  "Delete the package designated by PACKAGE-DESIGNATOR from the package
  system data structures."
  (unless (packagep package)
    (setq package (or (find-package package)
                      (progn
                        (cerror "Do nothing" 'no-such-package :package package)
                        (return-from delete-package nil)))))
  (unless (memq package %all-packages%)
    (return-from delete-package nil))
  (when (pkg.used-by package)
    (cerror "unuse ~S" 'package-is-used-by :package package
            :using-packages (pkg.used-by package)))
  (while (pkg.used-by package)
    (unuse-package package (car (pkg.used-by package))))
  (while (pkg.used package)
    (unuse-package (car (pkg.used package)) package))
  (setf (pkg.shadowed package) nil)
  (setq %all-packages% (nremove package %all-packages%))
  (dolist (n (pkg.names package))
    (let* ((ref (register-package-ref n)))
      (setf (package-ref.pkg ref) nil)))
  (setf (pkg.names package) nil)
  (let* ((ivec (car (pkg.itab package)))
         (evec (car (pkg.etab package)))
         (deleted (package-deleted-marker)))
    (dotimes (i (the fixnum (length ivec)))
      (let* ((sym (%svref ivec i)))
        (setf (%svref ivec i) deleted)          ; in case it's in STATIC space
        (when (symbolp sym)
          (if (eq (symbol-package sym) package)
            (%set-symbol-package sym nil)))))
    (dotimes (i (the fixnum (length evec)))
      (let* ((sym (%svref evec i)))
        (setf (%svref evec i) deleted)          ; in case it's in STATIC space
        (when (symbolp sym)
          (if (eq (symbol-package sym) package)
            (%set-symbol-package sym nil))))))
  (let ((itab (pkg.itab package)) (etab (pkg.etab package)) (v '#(nil nil nil)))
    (%rplaca itab v) (%rplaca etab v)
    (%rplaca (%cdr itab) 0) (%rplaca (%cdr etab) 0)
    (%rplacd (%cdr itab) #x4000) (%rplacd (%cdr etab) #x4000))
  t)

(defun %find-package-symbol (string package &optional (len (length string)))
  (let* ((etab (pkg.etab package))
         (itab (pkg.itab package)))
    (multiple-value-bind (foundp sym offset) (%get-htab-symbol string len itab)
      (if foundp
        (values sym itab offset)
        (progn
          (multiple-value-setq (foundp sym offset)
          (%get-htab-symbol string len etab))
          (if foundp
            (values sym etab offset)
            (values nil nil nil)))))))

;;;For the inspector, number of symbols in pkg.
(defun %pkgtab-count (pkgtab)
  (let* ((n 0))
    (declare (fixnum n))
    (dovector (x (pkgtab-table pkgtab) n)
       (when (symbolp x)
         (incf n)))))


(defun %resize-package (pkg)
  (%resize-htab (pkg.itab pkg))
  (%resize-htab (pkg.etab pkg))
  pkg)

;These allow deleted packages, so can't use pkg-arg which doesn't.
;Of course, the wonderful world of optional arguments comes in handy.
(defun pkg-arg-allow-deleted (pkg)
  (pkg-arg pkg t))


(defun package-name (pkg) (%car (pkg.names (pkg-arg-allow-deleted pkg))))
;;>> Shouldn't these copy-list their result so that the user
;;>>  can't cause a crash through evil rplacding?
;Of course that would make rplacding less evil, and then how would they ever learn?
(defun package-nicknames (pkg) (%cdr (pkg.names (pkg-arg-allow-deleted pkg))))
(defun package-use-list (pkg) (pkg.used (pkg-arg-allow-deleted pkg)))
(defun package-used-by-list (pkg) (pkg.used-by (pkg-arg-allow-deleted pkg)))
(defun package-shadowing-symbols (pkg) (pkg.shadowed (pkg-arg-allow-deleted pkg)))

;;; This assumes that all symbol-names and package-names are strings.
(defun %define-package (name size 
                             external-size ; extension (may be nil.)
                             nicknames
                             shadow
                             shadowing-import-from-specs
                             use
                             import-from-specs
                             intern
                             export
			     &optional doc)
  (if (eq use :default) (setq use *make-package-use-defaults*))
  (let* ((pkg (find-package name)))
    (if pkg
      ;; Restarts could offer several ways of fixing this.
      (unless (string= (package-name pkg) name)
        (cerror "Redefine ~*~S"
                "~S is already a nickname for ~S" name pkg))
      (setq pkg (make-package name
                              :use nil
                              :internal-size (or size 60)
                              :external-size (or external-size
                                                 (max (length export) 1)))))
    (unuse-package (package-use-list pkg) pkg)
    (rename-package pkg name nicknames)
    (flet ((operation-on-all-specs (function speclist)
             (let ((to-do nil))
               (dolist (spec speclist)
                 (let ((from (pop spec)))
                   (dolist (str spec)
                     (multiple-value-bind (sym win) (find-symbol str from)
                       (if win
                         (push sym to-do)
                         ; This should (maybe) be a PACKAGE-ERROR.
                         (cerror "Ignore attempt to ~s ~s from package ~s"
                                 "Cannot ~s ~s from package ~s" function str from))))))
               (when to-do (funcall function to-do pkg)))))
      
      (dolist (sym shadow) (shadow sym pkg))
      (operation-on-all-specs 'shadowing-import shadowing-import-from-specs)
      (use-package use pkg)
      (operation-on-all-specs 'import import-from-specs)
      (dolist (str intern) (intern str pkg))
      (when export
        (let* ((syms nil))
          (dolist (str export)
            (multiple-value-bind (sym found) (find-symbol str pkg)
              (unless found (setq sym (intern str pkg)))
              (push sym syms)))
          (export syms pkg)))
      (when (and doc *save-doc-strings*)
        (set-documentation pkg t doc))
      pkg)))

(defun %setup-pkg-iter-state (pkg-list types)
  (collect ((steps))
    (flet ((cons-pkg-iter-step (package type table &optional shadowed)
             (steps (vector package type table shadowed nil nil))))
      (let* ((pkgs (if (listp pkg-list)
                     (mapcar #'pkg-arg pkg-list)
                     (list (pkg-arg pkg-list)))))
        (dolist (pkg pkgs)
          (dolist (type types)
            (case type
              (:internal (cons-pkg-iter-step pkg type (pkg.itab pkg)))
              (:external (cons-pkg-iter-step pkg type (pkg.etab pkg)))
              (:inherited
               (let* ((shadowed (pkg.shadowed pkg))
                      (used (pkg.used pkg)))
                 (dolist (u used)
                   (cons-pkg-iter-step pkg type (pkg.etab u) shadowed)))))))))
    (vector nil (steps))))

(defun %pkg-iter-next (state)
  (flet ((get-step ()
           (let* ((step (pkg-iter.step state)))
             (loop
               (if (and step (> (pkg-iter-step.index step) 0))
                 (return step))
               (when (setq step (pop (pkg-iter.remaining-steps state)))
                 (setf (pkg-iter.step state) step)
                 (setf (pkg-iter-step.index step)
                       (length (setf (pkg-iter-step.vector step)
                                     (pkgtab-table  (pkg-iter-step.table step))))))
               (unless step
                 (return))))))
    (loop
      (let* ((step (get-step)))
        (when (null step) (return))
        (multiple-value-bind (symbol found)
            (%htab-symbol (pkg-iter-step.vector step)
                          (decf (pkg-iter-step.index step)))
          (when (and found
                     (not (member symbol (pkg-iter-step.shadowed step)
                                  :test #'string=)))
            (return (values t
                            symbol
                            (pkg-iter-step.type step)
                            (pkg-iter-step.pkg step)))))))))


;;; For do-symbols and with-package-iterator
;;; string must be a simple string
;;; package must be a package
;;; Wouldn't it be nice if this distinguished "not found" from "found NIL" ?
(defun %name-present-in-package-p (string package)
  (values (%find-package-symbol string package)))

;;; This is supposed to be (somewhat) like the lisp machine's MAKE-PACKAGE.
;;; Accept and ignore some keyword arguments, accept and process some others.

(defun lispm-make-package (name &key 
                                (use *make-package-use-defaults*)
                                nicknames
                                ;prefix-name
                                ;invisible
                                (shadow nil shadow-p)
                                (export nil export-p)
                                (shadowing-import nil shadowing-import-p)
                                (import nil import-p)
                                (import-from nil import-from-p)
                                ;relative-names
                                ;relative-names-for-me
                                ;size
                                ;hash-inherited-symbols
                                ;external-only
                                ;include
                                ;new-symbol-function
                                ;colon-mode
                                ;prefix-intern-function
                                &allow-other-keys)
  ;  (declare (ignore prefix-name invisible relative-names relative-names-for-me
  ;                   size hash-inherited-symbols external-only include
  ;                   new-symbol-function colon-mode prefix-intern-function))
  (let ((pkg (make-package name :use NIL :nicknames nicknames)))
    (when shadow-p (shadow shadow pkg))
    (when shadowing-import-p (shadowing-import shadowing-import pkg))
    (use-package use pkg)
    (when import-from-p
      (let ((from-pkg (pop import-from)))
        (dolist (name import-from)
          (multiple-value-bind (sym win) (find-symbol (string name) from-pkg)
            (when win (import-1 pkg sym))))))
    (when import-p (import import pkg))
    (when export-p
      (let* ((syms nil))
        (dolist (name export)
          (multiple-value-bind (sym win) (find-symbol (string name) pkg)
            (unless win (setq sym (intern (string name) pkg)))
            (push sym syms)))
        (export syms pkg)))
    pkg))

