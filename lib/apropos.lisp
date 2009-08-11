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

;; Apropos.lisp

(in-package "CCL")

(eval-when (:execute :compile-toplevel)
   (require :level-2)
   )

(defun apropos-list (string &optional package &aux list)
  "Like APROPOS, except that it returns a list of the symbols found instead
  of describing them."
  (setq string (string-arg string))
  (if package
    (do-symbols (sym package)
      (when (%apropos-substring-p string (symbol-name sym))
        (push sym list)))
    (do-all-symbols (sym)
      (when (%apropos-substring-p string (symbol-name sym))
        (push sym list))))
  (let* ((last 0)                      ; not a symbol
         (junk #'(lambda (item)
                   (declare (debugging-function-name nil))
                   (or (eq item last) (progn (setq last item) nil)))))
    (declare (dynamic-extent junk))
    (setq list (delete-if junk (sort list #'string-lessp))))
  list)

(defvar *apropos-indent-to-search-string* nil)
(defun apropos-list-aux (theString package indent-to-search-string &aux theList)
    (setq theString (string-arg theString))
    (if package
      (do-symbols (sym package)
        (when (%apropos-substring-p theString (symbol-name sym))
          (pushnew sym theList)))
      (do-all-symbols (sym)
        (when (%apropos-substring-p theString (symbol-name sym))
          (pushnew sym theList))))
    (let* ((last 0)                      ; not a symbol
           (junk #'(lambda (item)
                     (declare (debugging-function-name nil))
                     (or (eq item last) (progn (setq last item) nil)))))
      (declare (dynamic-extent junk))
      (sort-symbol-list (delete-if junk theList) (if indent-to-search-string
                                                   theString
                                                   nil))))
  
(defun apropos-string-indented (symTuple indent)
    (let ((pr-string     (prin1-to-string (aref symTuple 0)))
          (displayOffset (aref symTuple 3)))
      (format nil "~v@a~a"
              indent
              (subseq pr-string 0 displayOffset)
              (subseq pr-string displayOffset))))
  

(defun apropos-aux (theString symtuple indent)
  (declare (ignore theString))
  (let ((sym (aref symtuple 0))
        val)
    (format t "~a" (apropos-string-indented symtuple indent))
    (when (setq val (fboundp sym))
      (cond ((functionp val)
             (princ ", Def: ")
             (prin1 (type-of val)))
            ((setq val (macro-function sym))
             (princ ", Def: MACRO ")
             (prin1 (type-of val)))
            (t (princ ", Special form"))))
    (when (boundp sym)
      (princ ",  Value: ")
      (prin1 (symbol-value sym)))
    (terpri)))

  
(defun apropos (theString &optional package)
    (multiple-value-bind (symVector indent) (apropos-list-aux theString package *apropos-indent-to-search-string*)
      (loop for symtuple across symVector
        do (apropos-aux theString symtuple indent))
      (values)))
  
#|
(defun apropos (string &optional package)
  "Briefly describe all symbols which contain the specified STRING.
  If PACKAGE is supplied then only describe symbols present in
  that package. If EXTERNAL-ONLY then only describe
  external symbols in the specified package."
  (setq string (string-arg string))
  (if package
    (do-symbols (sym package) (apropos-aux string sym))
    (do-all-symbols (sym) (apropos-aux string sym)))
  (values))

(defun apropos-aux (string sym &aux val)
  (when (%apropos-substring-p string (symbol-name sym))
    (prin1 sym)
    (when (setq val (fboundp sym))
      (cond ((functionp val)
             (princ ", Def: ")
             (prin1 (type-of val)))
            ((setq val (macro-function sym))
             (princ ", Def: MACRO ")
             (prin1 (type-of val)))
            (t (princ ", Special form"))))
    (when (boundp sym)
       (princ ",  Value: ")
       (prin1 (symbol-value sym)))
    (terpri)))
|#

; (%apropos-substring-p a b)
; Returns true iff a is a substring (case-sensitive) of b.
; Internal subroutine of apropos, does no type-checking.  Assumes strings no
; longer than 64K...




(defun %apropos-substring-p (a b)
  (let ((charA0 (%schar a 0))
        (alen (length a))
        (blen (length b)))
    (declare (fixnum alen blen) (optimize (speed 3)(safety 0)))
    (if (= alen 0)  ; "" is substring of every string
        t
        (if *apropos-case-sensitive-p*
            (dotimes (i (the fixnum (%imin blen (%i+ 1 (%i- blen alen)))))
              (declare (fixnum i))
              (when (eq (%schar b i) chara0)
                (when
                    (do ((j 1 (1+ j)))
                        ((>= j alen) t)
                      (declare (fixnum j))
                      (when (neq (%schar a j)(%schar b (%i+ j i)))
                        (return nil)))
                  (return  (%i- blen i alen)))))
            (dotimes (i (the fixnum (%imin blen (%i+ 1 (%i- blen alen)))))
              (declare (fixnum i))
              (when (eq (char-upcase (%schar b i)) (char-upcase chara0))
                (when
                    (do ((j 1 (1+ j)))
                        ((>= j alen) t)
                      (declare (fixnum j))
                      (unless (eq (char-upcase (%schar a j)) (char-upcase (%schar b (%i+ j i))))
                        (return nil)))
                  (return  (%i- blen i alen)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; from Dave Yost
(defun find-sym-alpha-part (sym)
  (let* ((str (prin1-to-string sym))
         (sortOffset (let ((sym-start (if (find #\: str)
                                      (loop for ind from (1- (length str)) downto 0
                                            when (eql #\: (char str ind))
                                            return (1+ ind))
                                      0)))
                     (+ sym-start (find-alpha-char (subseq str sym-start))))))
    (values str sortOffset sortOffset)))

(defun find-str-in-sym (str sym)
  (let* ((symStr (string-arg (prin1-to-string sym)))
         (sortOffset (let ((sym-start (if (find #\: str)
                                      (loop for ind from (1- (length str)) downto 0
                                            when (eql #\: (char str ind))
                                            return (1+ ind))
                                      0)))
                     (+ sym-start (find-alpha-char (subseq str sym-start)))))
         (displayOffset (let ((sym-start (if (find #\: symStr)
                                       (or (loop for ind from (1- (length symStr)) downto 0
                                             when (eql #\| (schar symStr ind))
                                             do (setf ind (loop for ind2 from (1- ind) downto 0
                                                                when (eql #\| (schar symStr ind2))
                                                                return ind2))
                                             when (eql #\: (char symStr ind))
                                             return (1+ ind))
                                           0)
                                       0)))
                      (+ sym-start (search (string-upcase str) (string-upcase (subseq symStr sym-start)))))))
    (values symStr sortOffset displayOffset)))

(defun find-alpha-char (str)
  "returns the character position of the first
alphabetic character in str, or the length of str
if it contains no alphabetic characters."
  (setq str (string-arg str))
  (dotimes (ind (length str)  ind)
    (when (alpha-char-p (schar str ind))
       (return ind))))

(defun sort-symbol-list (theList search-string)
  ;;; First precompute the stylized string form of the symbols as they will be compared
  ;;; and calculate the maximum indent
  (multiple-value-bind (tmpVector indentation)
      (let (sortOffset
            displayOffset
            str)
        (loop for x in thelist do
              (multiple-value-setq (str sortOffset displayOffset)
                (if search-string
                  (find-str-in-sym search-string x)
                  (find-sym-alpha-part           x)))
                           
                           
              maximize displayOffset into indentation1
              collect `#(,x ,(string-arg (subseq str sortOffset)) ,sortOffset ,displayOffset) into tmpList1
              finally  (return (values `#(,@tmpList1) indentation1))))
    (setq TMPVECTor (sort tmpVector #'(lambda (symPair1 symPair2)
                                         (string-lessp (aref symPair1 1) (aref symPair2 1)))))
    (values tmpVector ; each element is a vector of `#(,sym sortable-string-for-sym)
            indentation)))


#|
(defun %apropos-substring-p (a b &aux (alen (length a))
                                     (xlen (%i- (length b) alen)))
  (if (%iminusp xlen) nil
    (if (eq alen 0) alen
      (let ((a0 (schar a 0)) (i 0) j)
        (tagbody loop
          (when (eq (schar b i) a0)
            (setq j 1)
            (tagbody subloop
              (when (eq j alen) (return-from %apropos-substring-p i))
              (when (eq (schar b (%i+ i j)) (schar a j))
                 (setq j (%i+ j 1))
                 (go subloop))))
          (unless (eq i xlen)
            (setq i (%i+ i 1))
            (go loop)))
        nil))))
|#
