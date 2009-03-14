;;; -*- Mode: Lisp; Package: CCL; indent-tabs-mode: nil -*-
;;;
;;;   Copyright (C) 2003 Oliver Markovic <entrox@entrox.org>
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*RECORD-XREF-INFO*
            *LOAD-XREF-INFO*
            XREF-ENTRY
            XREF-ENTRY-NAME
            XREF-ENTRY-TYPE
            XREF-ENTRY-METHOD-QUALIFIERS
            XREF-ENTRY-METHOD-SPECIALIZERS
            XREF-ENTRY-P
            XREF-ENTRY-EQUAL
            DISCARD-ALL-XREF-INFO
            GET-RELATION
            MACROS-CALLED-BY
            START-XREF
            STOP-XREF
            WHO-BINDS
            WHO-CALLS
            WHO-DIRECTLY-CALLS
            WHO-INDIRECTLY-CALLS
            WHO-REFERENCES
            WHO-SETS
            WHO-USES
            WITH-XREF
            XREF-DESCRIBE)))

(defpackage "CROSS-REFERENCE"
  (:use "CL")
  (:nicknames "XREF")
  (:import-from "CCL"
                "*RECORD-XREF-INFO*"
                "*LOAD-XREF-INFO*"
                "XREF-ENTRY"
                "XREF-ENTRY-NAME"
                "XREF-ENTRY-TYPE"
                "XREF-ENTRY-METHOD-QUALIFIERS"
                "XREF-ENTRY-METHOD-SPECIALIZERS"
                "XREF-ENTRY-P"
                "XREF-ENTRY-EQUAL"
                "DISCARD-ALL-XREF-INFO"
                "GET-RELATION"
                "MACROS-CALLED-BY"
                "START-XREF"
                "STOP-XREF"
                "WHO-BINDS"
                "WHO-CALLS"
                "WHO-DIRECTLY-CALLS"
                "WHO-INDIRECTLY-CALLS"
                "WHO-REFERENCES"
                "WHO-SETS"
                "WHO-USES"
                "WITH-XREF"
                "XREF-DESCRIBE")
  (:export "*RECORD-XREF-INFO*"
           "*LOAD-XREF-INFO*"
           "XREF-ENTRY"
           "XREF-ENTRY-NAME"
           "XREF-ENTRY-TYPE"
           "XREF-ENTRY-METHOD-QUALIFIERS"
           "XREF-ENTRY-METHOD-SPECIALIZERS"
           "XREF-ENTRY-P"
           "XREF-ENTRY-EQUAL"
           "DISCARD-ALL-XREF-INFO"
           "GET-RELATION"
           "MACROS-CALLED-BY"
           "START-XREF"
           "STOP-XREF"
           "WHO-BINDS"
           "WHO-CALLS"
           "WHO-DIRECTLY-CALLS"
           "WHO-INDIRECTLY-CALLS"
           "WHO-REFERENCES"
           "WHO-SETS"
           "WHO-USES"
           "WITH-XREF"
           "XREF-DESCRIBE"))


;; *RECORD-XREF-INFO* -- external
;;
;; Cross-referencing information will only be recorded if this flag
;; is set. It is usually set/unset by START-XREF/STOP-XREF
(defvar *record-xref-info* nil
  "Flag indicating wether cross-referencing information should be recorded.")

;; *LOAD-XREF-INFO* -- external
;;
;; FIXME: We don't save any information yet...
(defvar *load-xref-info* nil
  "Flag indicating wether cross-referencing information should be loaded
from FASLs.")



;; START-XREF -- external
;;
(defun start-xref ()
  "Start recording cross-referencing information while compiling."
  (setf *record-xref-info* t)
  (setf *load-xref-info* t)
  t)

;; STOP-XREF -- external
;;
(defun stop-xref ()
  "Stop recording cross-referencing information while compiling."
  (setf *record-xref-info* nil)
  (setf *load-xref-info* nil)
  nil)

;; WITH-XREF -- external
;;
(defmacro with-xref (&body body)
  "Execute BODY with cross-referencing turned on."
  (let ((return-value (gensym "RETURN-VALUE")))
    `(let ((*record-xref-info* t)
           (*load-xref-info* t)
           (,return-value nil))
       (setf ,return-value (progn ,@body))
       ,return-value)))


;; XREF-ENTRY -- external
;;
(defstruct (xref-entry
            (:constructor %make-xref-entry)
            (:print-function %print-xref-entry))
  name
  type
  (method-qualifiers nil)
  (method-specializers nil))

;; %PRINT-XREF-ENTRY -- internal
;;
(defun %print-xref-entry (struct stream d)
  (declare (ignore d))
  (if *print-readably*
      (format stream "#S(xref::xref-entry :name '~A :type '~A :method-qualifiers ~A :method-specializers ~A)"
              (xref-entry-name struct)
              (xref-entry-type struct)
              (xref-entry-method-qualifiers struct)
              (xref-entry-method-specializers struct))
    (print-unreadable-object (struct stream :type t)
      (format stream "~A ~A~@[ ~A~]~@[ ~A~]"
              (xref-entry-name struct)
              (xref-entry-type struct)
              (xref-entry-method-qualifiers struct)
              (xref-entry-method-specializers struct)))))

;; MAKE-XREF-ENTRY -- internal
;;
;; Takes a simple input form and makes a XREF-ENTRY from it. The input is
;; assumed to be a function, macro or variable when a simple symbol is passed,
;; or a method when it is a cons. Since this needs to also handle the ouput
;; from CCL::CALLERS, there is additional hackery trying to do the right thing.
(defun make-xref-entry (input relation)
  (etypecase input
    (symbol
     (let ((type (ecase relation
                   ((:direct-calls :indirect-calls) 'function)
                   ((:binds :sets :references) 'variable)
                   ((:macro-calls) 'macro))))
       (%make-xref-entry :name input :type type)))
    (method
     (let ((name (method-name input))
           (qualifiers (method-qualifiers input))
           (specializers (canonicalize-specializers (method-specializers input))))
       (%make-xref-entry :name name :type 'method
                         :method-qualifiers (unless (eql qualifiers t) qualifiers)
                         :method-specializers specializers)))
    (cons
     (case (car input)
       ((ppc-lap-macro compiler-macro-function)
        (%make-xref-entry :name (cadr input) :type (car input)))
       ((:internal)
        (make-xref-entry (car (last input)) relation))
       (t
        (multiple-value-bind (type name specializers qualifiers)
            (parse-definition-spec input)
          (%make-xref-entry :name name :type type
                            :method-qualifiers (unless (eql qualifiers t) qualifiers)
                            :method-specializers specializers)))))))

(defun parse-definition-spec (form)
  (let ((type t)
        name classes qualifiers)
    (cond
     ((consp form)
      (cond ((eq (car form) 'setf)
             (setq name form))
            (t (setq name (car form))
               (let ((last (car (last (cdr form)))))
                 (cond ((and (listp last)(or (null last)(neq (car last) 'eql)))
                        (setq classes last)
                        (setq qualifiers (butlast (cdr form))))
                       (t (setq classes (cdr form)))))                   
               (cond ((null qualifiers)
                      (setq qualifiers t))
                     ((equal qualifiers '(:primary))
                      (setq qualifiers nil))))))
     (t (setq name form)))
    (when (and (consp name)(eq (car name) 'setf))
        (setq name (or (%setf-method (cadr name)) name))) ; e.g. rplacd
    (when (not (or (symbolp name)
                   (setf-function-name-p name)))
      (return-from parse-definition-spec))
    (when (consp qualifiers)
      (mapc #'(lambda (q)
                (when (listp q)
                  (return-from parse-definition-spec)))
          qualifiers))
    (when classes
      (mapc #'(lambda (c)
                (when (not (and c (or (symbolp c)(and (consp c)(eq (car c) 'eql)))))
                  (return-from parse-definition-spec)))
            classes))            
    (when (or (consp classes)(consp qualifiers))(setq type 'method))
    (values type name classes qualifiers)))

;; XREF-ENTRY-EQUAL -- external
;;
;; Simply compares all slots.
(defun xref-entry-equal (entry1 entry2)
  (and (eql (xref-entry-name entry1) (xref-entry-name entry2))
       (eql (xref-entry-type entry1) (xref-entry-type entry2))
       (equal (xref-entry-method-qualifiers entry1)
              (xref-entry-method-qualifiers entry2))
       (equal (xref-entry-method-specializers entry1)
              (xref-entry-method-specializers entry2))))

;; %DB-KEY-FROM-XREF-ENTRY -- internal
;;
;; This is mostly the inverse to MAKE-XREF-ENTRY, since it takes an entry
;; and returns either a symbol (for functions, macros and variables) or a
;; list in the form (METHOD-NAME QUALIFIERS (SPECIALIZERS)) for a method.
;; These are used as keys in the database hash-tables.
(defun %db-key-from-xref-entry (entry)
  (if (eql (xref-entry-type entry) 'method)
      `(,(xref-entry-name entry)
        ,@(xref-entry-method-qualifiers entry)
        ,(xref-entry-method-specializers entry))
    (xref-entry-name entry)))

;; edit-definition-p needs this - what is it for?
(defvar *direct-methods-only* nil)

;; %SOURCE-FILE-FOR-XREF-ENTRY -- internal
;;
(defun %source-file-for-xref-entry (entry)
  (multiple-value-bind (files name type specializers qualifiers)
      (edit-definition-p (%db-key-from-xref-entry entry)
                         (if (eql (xref-entry-type entry) 'macro)
                             'function
                           (xref-entry-type entry)))
    (declare (ignore name type specializers qualifiers))
    (let ((filename (if (consp files) (cdar files) files)))
      (when filename
        (truename filename)))))


;; MAKE-XREF-DATABASE -- internal
;;
;; This returns a fresh cross-referencing "database". It's a simple association
;; list with two hash-tables per entry. The CAR hash holds the direct entries
;; e.g. KEY calls/references/etc VALUE, while the CDR holds inverse hash (KEY
;; is called/referenced/etc by VALUE.
(defun make-xref-database ()
  (list :binds (cons (make-hash-table :test #'equal)
                     (make-hash-table :test #'equal))
        :references (cons (make-hash-table :test #'equal)
                          (make-hash-table :test #'equal))
        :sets (cons (make-hash-table :test #'equal)
                    (make-hash-table :test #'equal))
        :direct-calls (cons (make-hash-table :test #'equal)
                            (make-hash-table :test #'equal))
        :indirect-calls (cons (make-hash-table :test #'equal)
                              (make-hash-table :test #'equal))
        :macro-calls (cons (make-hash-table :test #'equal)
                           (make-hash-table :test #'equal))))

;; *XREF-DATABASE* -- internal
;;
;; The one and only cross-referencing database.
(defvar *xref-database* (make-xref-database))


;; %XREF-TABLE -- internal
;;
;; Returns the appropriate table for a given relation.
(defun %xref-table (relation inversep)
  (if inversep
      (cdr (getf *xref-database* relation))
    (car (getf *xref-database* relation))))


;; DISCARD-ALL-XREF-INFO -- external
;;
(defun discard-all-xref-info ()
  "Clear the cross-referencing database."
  (setf *xref-database* (make-xref-database))
  t)


;; %ADD-XREF-ENTRY -- internal
;;
;; The compiler adds cross-referencing information by calling this
;; (see NX-RECORD-XREF-INFO).
(defun %add-xref-entry (relation name1 name2)
  (when (and *record-xref-info* relation name1 name2)
    (pushnew (make-xref-entry name2 relation)
             (gethash name1 (%xref-table relation nil))
             :test #'xref-entry-equal)
    (pushnew (make-xref-entry name1 relation)
             (gethash name2 (%xref-table relation t))
             :test #'xref-entry-equal)
    t))




;; %DISCARD-XREF-INFO-FOR-FUNCTION -- internal
;;
;; This rather expensive operation removes all traces of a given function
;; from the cross-referencing database. It needs to be called whenever a
;; function gets redefined, so we don't pick up stale xref entries.
(defun %discard-xref-info-for-function (func)
  ;; need to go through every possible relation
  (dolist (relation '(:direct-calls :indirect-calls :macro-calls
                      :binds :references :sets))
    ;; get a list of the places to which the func points to...
    (dolist (entry (gethash func (%xref-table relation nil)))
      (let ((key (%db-key-from-xref-entry entry)))
        ;; ... and remove it from there
        (setf (gethash key (%xref-table relation t))
              (delete func (gethash key (%xref-table relation t))))))
    ;; the non-inverse case is easy
    (remhash func (%xref-table relation nil))))


;; GET-RELATION -- external
;;
;; FIXME: Implement filtering by files.
;;        And what the heck should errorp do?
(defun get-relation (relation name1 name2 &key in-files in-functions exhaustive errorp)
  "Returns a list of matches for RELATION between NAME1 and NAME2. Results can
be filtered by passing a list of files in IN-FILES or functions in IN-FUNCTIONS.
If EXHAUSTIVE is true, it will also look for callers for which no xref information
is present by looping through all defined functions in memory."
  (when (and (eql name1 :wild) (eql name2 :wild))
    (error "Only one wildcard allowed in a cross-reference query"))
  (ecase relation
    ((:binds :references :sets :direct-calls :indirect-calls :macro-calls)
     (let ((lookup-table (%xref-table relation nil))
           (inverse-lookup-table (%xref-table relation t)))
       (let ((matches (if (eql name1 :wild)
                          (%do-wild-xref-lookup name2 inverse-lookup-table
                                                in-files in-functions)
                        (if (eql name2 :wild)
                            (%do-wild-xref-lookup name1 lookup-table
                                                  in-files in-functions)
                          (%do-simple-xref-lookup name1 name2 lookup-table
                                                  in-files in-functions)))))
         ;; search all lfuns if exhaustive is t
         (when (and exhaustive (eql name1 :wild) (or (eql relation :direct-calls)
                                                     (eql relation :indirect-calls)))
           (dolist (caller (callers name2))
             (pushnew (make-xref-entry caller relation)
                      matches
                      :test #'xref-entry-equal)))
         matches)))
    (:calls
     (let ((direct-calls (get-relation :direct-calls name1 name2
                                       :in-files in-files :in-functions in-functions
                                       :exhaustive exhaustive :errorp errorp))
           (indirect-calls (get-relation :indirect-calls name1 name2
                                         :in-files in-files :in-functions in-functions
                                         :exhaustive exhaustive :errorp errorp))
           (macro-calls (get-relation :macro-calls name1 name2
                                      :in-files in-files :in-functions in-functions
                                      :exhaustive exhaustive :errorp errorp)))
       (if (or (eql name1 :wild) (eql name2 :wild))
           ;; need to weed out possible duplicates here
           (let ((matches nil))
             (dolist (c direct-calls) (pushnew c matches))
             (dolist (c indirect-calls) (pushnew c matches))
             (dolist (c macro-calls) (pushnew c matches))
             matches)
         (when (or direct-calls indirect-calls macro-calls)
           name2))))
    (:uses
     (let ((binds (get-relation :binds name1 name2 :in-files in-files
                                :in-functions in-functions :errorp errorp
                                :exhaustive exhaustive))
           (references (get-relation :binds name1 name2 :in-files in-files
                                     :in-functions in-functions :errorp errorp
                                     :exhaustive exhaustive))
           (sets (get-relation :sets name1 name2 :in-files in-files
                               :in-functions in-functions :errorp errorp
                               :exhaustive exhaustive)))
       (if (or (eql name1 :wild) (eql name2 :wild))
           (concatenate 'list binds references sets)
         (when (or binds references sets)
           name2))))))

;; %DO-WILD-XREF-LOOKUP -- internal
;;
;; Does a wild lookup into the xref database and returns a list of matches.
;;
;; FIXME: implement filtering by files
(defun %do-wild-xref-lookup (name table in-files in-functions)
  (declare (ignore in-files))
  (multiple-value-bind (value foundp) (gethash name table)
    (declare (ignore foundp))
    (if in-functions
        (remove-if (lambda (x) (not (find x in-functions))) value)
      value)))

;; %DO-SIMPLE-XREF-LOOKUP -- internal
;;
;; Does a simple lookup into the xref database and returns NAME2 if a relation
;; between NAME1 and NAME2 exists.
;;
;; FIXME: implement filtering by files
(defun %do-simple-xref-lookup (name1 name2 table in-files in-functions)
  (declare (ignore in-files))
  (when (some (lambda (x)
                (when in-functions
                  (find x in-functions))
                (eql x name2))
              (gethash name1 table))
    name2))


(defun %print-xref-entries (entries stream verbose)
  (dolist (entry entries)
    (if (eql (xref-entry-type entry) 'method)
        ;; print qualifiers and specializers if it's a method
        (format stream "~5,5T~A ~@[~A ~]~A~%"
                (xref-entry-name entry)
                (xref-entry-method-qualifiers entry)
                (xref-entry-method-specializers entry))
      (format stream "~5,5T~A~%" (xref-entry-name entry)))
    ;; print extra information when verbose
    (when verbose
      (format stream "~5,5T  Type: ~A~%" (xref-entry-type entry))
      (let ((file (%source-file-for-xref-entry entry)))
        (format stream "~5,5T  File: ~A~%~%" (if file file "not recorded"))))))


;; WHO-DIRECTLY-CALLS -- external
;;
(defun who-directly-calls (name &key inverse in-files in-functions verbose
                                (stream *standard-output*))
  "Prints information about direct callers of NAME. If INVERSE is true,
it will print direct callees of NAME instead."
  (let ((callers/callees (if inverse
                             (get-relation :direct-calls name :wild 
                                           :in-files in-files
                                           :in-functions in-functions)
                           (get-relation :direct-calls :wild name
                                         :in-files in-files
                                         :in-functions in-functions
                                         :exhaustive t))))
    (format stream "~%~T")
    (if callers/callees
        (progn
          (format stream "~A ~:[is directly called by~;directly calls~]:~%"
                  name inverse)
          (%print-xref-entries callers/callees stream verbose))
      (format stream "No direct ~:[callers~;callees~] of ~A were found in the database~%"
              inverse name)))
  (values))

;; WHO-INDIRECTLY-CALLS -- external
;;
;; FIXME: Implement this (we can't currently detect indirect calls).
(defun who-indirectly-calls (name &key inverse in-files in-functions verbose
                                  (stream *standard-output*))
  "Prints information about indirect callers of NAME. If INVERSE is true,
it will print indirect callees of NAME instead."
  (let ((callers/callees (if inverse
                             (get-relation :indirect-calls name :wild 
                                           :in-files in-files
                                           :in-functions in-functions)
                           (get-relation :indirect-calls :wild name
                                         :in-files in-files
                                         :in-functions in-functions))))
    (format stream "~%~T")
    (if callers/callees
        (progn
          (format stream "~A ~:[is indirectly called by~;indirectly calls~]:~%"
                  name inverse)
          (%print-xref-entries callers/callees stream verbose))
      (format stream "No indirect ~:[callers~;callees~] of ~A were found in the database~%"
              inverse name)))
  (values))

;; MACROS-CALLED-BY -- external
;;
(defun macros-called-by (name &key inverse in-files in-functions verbose
                              (stream *standard-output*))
  "Prints information about macros which get called by NAME. If INVERSE is true,
it will list all functions which macroexpand NAME instead."
    (let ((callers/callees (if (not inverse)
                             (get-relation :macro-calls name :wild 
                                           :in-files in-files
                                           :in-functions in-functions)
                           (get-relation :macro-calls :wild name
                                         :in-files in-files
                                         :in-functions in-functions))))
    (format stream "~%~T")
    (if callers/callees
        (progn
          (format stream "~A ~:[is macro called by~;macro calls~]:~%"
                name (not inverse))
          (%print-xref-entries callers/callees stream verbose))
      (format stream "No macro ~:[callers~;callees~] of ~A were found in the database~%"
              (not inverse) name)))
    (values))

;; WHO-CALLS -- external
;;
(defun who-calls (name &key inverse in-files in-functions verbose
                       (stream *standard-output*))
  "Shorthand for WHO-DIRECTLY-CALLS, WHO-INDIRECTLY-CALLS and
MACROS-CALLED-BY."
  (who-directly-calls name :inverse inverse :stream stream :verbose verbose
                           :in-files in-files :in-functions in-functions)
  (who-indirectly-calls name :inverse inverse :stream stream :verbose verbose
                             :in-files in-files :in-functions in-functions)
  (macros-called-by name :inverse (not inverse) :stream stream :verbose verbose
                         :in-files in-files :in-functions in-functions)
  (values))


;; WHO-BINDS -- external
;;
(defun who-binds (name &key inverse in-files in-functions verbose
                       (stream *standard-output*))
  "Prints a list of functions which bind NAME. If INVERSE is true, it will
print a list of variables bound by NAME instead."
  (let ((bindings (if inverse
                      (get-relation :binds name :wild :in-files in-files
                                    :in-functions in-functions)
                    (get-relation :binds :wild name :in-files in-files
                                  :in-functions in-functions))))
    (format stream "~%~T")
    (if bindings
        (progn
          (format stream "~A ~:[is bound by~;binds~]:" name inverse)
          (%print-xref-entries bindings stream verbose))
      (format stream "No ~:[bindings of~;symbols bound by~] ~A were found in the database~%"
              inverse name)))
  (values))

;; WHO-REFERENCES -- external
;;
(defun who-references (name &key inverse in-files in-functions verbose
                            (stream *standard-output*))
  "Prints a list of functions which reference NAME. If INVERSE is true, it will
print a list of variables referenced by NAME instead."
  (let ((references (if inverse
                        (get-relation :references name :wild :in-files in-files
                                      :in-functions in-functions)
                      (get-relation :references :wild name :in-files in-files
                                    :in-functions in-functions))))
    (format stream "~%~T")
    (if references
        (progn
          (format stream "~A ~:[is referenced by~;references~]:~%" name inverse)
          (%print-xref-entries references stream verbose))
      (format stream "No ~:[references to~;symbols referenced by~] ~A were found in the database~%"
              inverse name)))
  (values))

;; WHO-SETS -- external
;;
(defun who-sets (name &key inverse in-files in-functions verbose
                      (stream *standard-output*))
    "Prints a list of functions which set NAME. If INVERSE is true, it will
print a list of variables set by NAME instead."
  (let ((sets (if inverse
                  (get-relation :sets name :wild :in-files in-files
                                :in-functions in-functions)
                (get-relation :sets :wild name :in-files in-files
                              :in-functions in-functions))))
    (format stream "~%~T")
    (if sets
        (progn
          (format stream "~A ~:[is set by~;sets~]:~%" name inverse)
          (%print-xref-entries sets stream verbose))
      (format stream "No ~:[settings of~;symbols set by~] ~A were found in the database~%"
              inverse name)))
  (values))

;; WHO-USES -- external
;;
(defun who-uses (name &key inverse in-files in-functions verbose
                      (stream *standard-output*))
  "Shorthand for WHO-BINDS, WHO-REFERENCES and WHO-SETS."
  (who-binds name :inverse inverse :stream stream :verbose verbose
                  :in-files in-files :in-functions in-functions)

  (who-references name :inverse inverse :stream stream :verbose verbose
                       :in-files in-files :in-functions in-functions)

  (who-sets name :inverse inverse :stream stream :verbose verbose
                 :in-files in-files :in-functions in-functions)
  (values))


;; XREF-DESCRIBE -- external
;;
(defun xref-describe (name &key verbose)
  "Prints relevant cross-referencing information about NAME."
  (if (fboundp name)
      (progn
        (who-calls name :stream *terminal-io* :verbose verbose)
        (who-calls name :inverse t :stream *terminal-io* :verbose verbose)
        (who-uses name :inverse t :stream *terminal-io* :verbose verbose))
      (who-uses name :stream *terminal-io* :verbose verbose))
  (values))


;;; Hook into the OpenMCL compiler frontend, by pointing a couple
;;; of its variables at our functions.
(setq ccl::*nx-discard-xref-info-hook* #'%discard-xref-info-for-function)
(setq ccl::*nx-add-xref-entry-hook* #'%add-xref-entry)

(provide :xref)
