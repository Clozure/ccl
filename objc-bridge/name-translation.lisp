;;;; -*- Mode: Lisp; Package: CCL -*-
;;;; name-translation.lisp
;;;;
;;;; Handles the translation between ObjC and Lisp names
;;;;
;;;; Copyright (c) 2003 Randall D. Beer
;;;; 
;;;; This software is licensed under the terms of the Lisp Lesser GNU Public
;;;; License , known as the LLGPL.  The LLGPL consists of a preamble and 
;;;; the LGPL. Where these conflict, the preamble takes precedence.  The 
;;;; LLGPL is available online at http://opensource.franz.com/preamble.html.
;;;;
;;;; Please send comments and bug reports to <beer@eecs.cwru.edu>

;;; Temporary package stuff 

(in-package "CCL")

(require "SEQUENCE-UTILS")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                         Special ObjC Words                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Special character sequences that should be treated as words in ObjC
;;; names even though they do not follow the normal naming conventions

(defvar *special-objc-words* nil)


;;; Add a special word to *SPECIAL-OBJC-WORDS*, keeping the words sorted
;;; from longest to shortest

(defmacro define-special-objc-word (str)
  `(setf *special-objc-words* 
         (sort (pushnew ,str *special-objc-words* :test #'equal)
               #'>
               :key #'length)))


;;; Known special words used in Cocoa names

(define-special-objc-word "AB")
(define-special-objc-word "AE")
(define-special-objc-word "ATS")
(define-special-objc-word "BMP")
(define-special-objc-word "CA")
(define-special-objc-word "CCL")
(define-special-objc-word "CF")
(define-special-objc-word "CG")
(define-special-objc-word "CMYK")
(define-special-objc-word "MIME")
(define-special-objc-word "DR")
(define-special-objc-word "EPS")
(define-special-objc-word "FTP")
(define-special-objc-word "GMT")
(define-special-objc-word "objC")
(define-special-objc-word "OpenGL")
(define-special-objc-word "HTML")
(define-special-objc-word "HTTP")
(define-special-objc-word "HTTPS")
(define-special-objc-word "IB")
(define-special-objc-word "ID")
(define-special-objc-word "INT64")
(define-special-objc-word "NS")
(define-special-objc-word "MIME")
(define-special-objc-word "PDF")
(define-special-objc-word "PICT")
(define-special-objc-word "PNG")
(define-special-objc-word "QD")
(define-special-objc-word "RGB")
(define-special-objc-word "RTFD")
(define-special-objc-word "RTF")
(define-special-objc-word "TCP")
(define-special-objc-word "TIFF")
(define-special-objc-word "UI")
(define-special-objc-word "UID")
(define-special-objc-word "UTF8")
(define-special-objc-word "URL")
(define-special-objc-word "XOR")
(define-special-objc-word "XML")
(define-special-objc-word "1970")
#+gnu-objc
(define-special-objc-word "GS")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                              Utilities                                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Concatenate all of the simple strings STRS 

(defun string-cat (&rest strs)
  (apply #'concatenate 'simple-string strs))

;;; Collapse all prefixes of L that correspond to known special ObjC words

(defun collapse-prefix (l)
  (unless (null l)
    (multiple-value-bind (newpre skip) (check-prefix l)
      (cons newpre (collapse-prefix (nthcdr skip l))))))

(defun check-prefix (l)
  (let ((pl (prefix-list l)))
    (loop for w in *special-objc-words*
          for p = (position-if #'(lambda (s) (string= s w)) pl)
          when p do (return-from check-prefix (values (nth p pl) (1+ p))))
    (values (first l) 1)))

(defun prefix-list (l)
  (loop for i from (1- (length l)) downto 0
        collect (apply #'string-cat (butlast l i))))


;;; Concatenate a list of strings with optional separator into a symbol 

(defun symbol-concatenate (slist &optional (sep "") (package *package*))
  (values 
   (intern 
    (reduce #'(lambda (s1 s2) (string-cat s1 sep s2))
             (mapcar #'string-upcase slist))
    package)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             Implementation                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Convert an ObjC name to a corresponding Lisp name 
;;; Example: "NSURLHandleClient" ==> ns-url-handle-client 
;;;
;;; 1) Break the string at each uppercase letter
;;;    e.g., "NSWindow" ==> ("N" "S" "Window")
;;; 2) Collapse known sequences of letters 
;;;    e.g., ("N" "S" "Window") ==> ("NS" "Window")
;;; 3) Uppercase and concatenate with hyphens into a symbol
;;;    e.g., ("NS" "Window") ==> NS-WINDOW

(defun compute-lisp-name (str &optional (package *package*))
  (symbol-concatenate
    (collapse-prefix 
      (split-if #'(lambda (ch) (or (upper-case-p ch) (digit-char-p ch))) str))
    "-"
    package))


;;; Convert a Lisp classname into a corresponding ObjC classname
;;; Example: ns-url-handle-client ==> "NSURLHandleClient" 

(defun compute-objc-classname (sym)
  (apply #'string-cat
         (loop for str in (split-if-char #\- (string sym) :elide)
               for e = (member str *special-objc-words* 
                               :test #'equal 
                               :key #'string-upcase)
               collect (if e (first e) (string-capitalize str)))))


;;; Convert an ObjC method selector to a set of Lisp keywords
;;; Example: "nextEventMatchingMask:untilDate:inMode:dequeue:" ==>
;;;          (:next-event-matching-mask :until-date :in-mode :dequeue)

(defun compute-objc-to-lisp-message (str)
  (mapcar #'(lambda (s) (compute-lisp-name s (find-package "KEYWORD")))
          (split-if-char #\: str :elide)))


(defparameter *objc-colon-replacement-character* #\.)


(defun compute-objc-to-lisp-function-name (str &optional (package "NSFUN"))
  #-nil
  (intern str package)
  #+nil
  (let* ((n (length str))
         (i 0)
         (trailing t))
      (let* ((subs (if (not (position #\: str))
                     (progn (setq trailing nil)
                            (list str))
                     (collect ((substrings))
                       (do* ()
                            ((= i n) (substrings))
                         (let* ((pos (position #\: str :start i)))
                           (unless pos
                             (break "Huh?"))
                           (substrings (subseq str i pos))
                           (setq i (1+ pos)))))))
             (split 
              (mapcar #'(lambda (s)
                    (collapse-prefix
                     (split-if #'(lambda (ch)
                                   (or (upper-case-p ch) (digit-char-p ch)))
                               s)))
                
                subs))
             (namelen (+ (if trailing (length split) 0)
                           (let* ((c 0))
                             (dolist (s split c)
                               (if s (incf c (1- (length s))))))
                           (let* ((c 0))
                             (dolist (s split c)
                               (dolist (sub s)
                                 (incf c (length sub)))))))
             (name (make-string namelen)))
        (declare (dynamic-extent name))
        (let* ((p 0))
          (flet ((out-ch (ch)
                   (setf (schar name p) ch)
                   (incf p)))
            (dolist (sub split)
              (when sub
                (do* ((string (pop sub) (pop sub)))
                     ((null string))
                  (dotimes (i (length string))
                    (out-ch (char-upcase (schar string i))))
                  (when sub
                    (out-ch #\-))))
              (when trailing (out-ch *objc-colon-replacement-character*)))))
        (values
         (or (find-symbol name package)
             (intern (copy-seq name) package))))))

        
;;; Convert a Lisp list of keywords into an ObjC method selector string
;;; Example: (:next-event-matching-mask :until-date :in-mode :dequeue) ==>
;;;          "nextEventMatchingMask:untilDate:inMode:dequeue:"

(defun compute-lisp-to-objc-message (klist)
  (flet ((objcify (sym)
           (apply 
            #'string-cat
            (loop for str in (split-if-char #\- (string sym) :elide)
                  for first-word-flag = t then nil
                  for e = (member str *special-objc-words* 
                                  :test #'equal 
                                  :key #'string-upcase)
                  collect 
                  (cond (e (first e))
                        (first-word-flag (string-downcase str))
                        (t (string-capitalize str)))))))
    (if (and (= (length klist) 1) 
             (neq (symbol-package (first klist)) (find-package :keyword)))
      (objcify (first klist))
      (apply #'string-cat
             (mapcar #'(lambda (sym) (string-cat (objcify sym) ":")) klist)))))


;;; Convert an ObjC initializer to a list of corresponding initargs,
;;; stripping off any initial "init"
;;; Example: "initWithCString:length:" ==> (:with-c-string :length)

(defun compute-objc-to-lisp-init (init)
  (cond 
   ((= (length init) 0) nil)
   ((and (> (length init) 3) (string= init "init" :start1 0 :end1 4))
    (mapcar #'(lambda (s) (compute-lisp-name s (find-package "KEYWORD")))
          (split-if-char #\: (subseq init 4 (length init)) :elide)))
   (t (error "~S is not a valid initializer" init))))


;;; Convert a list of initargs into an ObjC initilizer, adding an "init"
;;; prefix if necessary
;;; Example: (:with-c-string :length) ==> "initWithCString:length:"

(defun compute-lisp-to-objc-init (initargs)
  (if (null initargs) 
    "init"
    (let ((str (compute-lisp-to-objc-message initargs)))
      (if (string/= (first (split-if-char #\- (string (first initargs)))) 
                    "INIT")
        (string-cat "init" (nstring-upcase str :start 0 :end 1))
        str))))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                         Class Name Translation                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Hash tables for caching class name translations

(defvar *lisp-classname-table* (make-hash-table :test #'equal))
(defvar *objc-classname-table* (make-hash-table :test #'eq))

  
;;; Define a hard-wired ObjC class name translation (if the automatic
;;; translation doesn't apply) 

(defmacro define-classname-translation (str sym)
  (let ((str-temp (gensym))
        (sym-temp (gensym))
        (old-str-temp (gensym))
        (old-sym-temp (gensym)))
    `(let* ((,str-temp ',str)
            (,sym-temp ',sym)
            (,old-sym-temp (gethash ,str-temp *lisp-classname-table*))
            (,old-str-temp (gethash ,sym-temp *objc-classname-table*)))
       (remhash ,old-str-temp *lisp-classname-table*)
       (remhash ,old-sym-temp *objc-classname-table*)
       (setf (gethash ,str-temp *lisp-classname-table*) ,sym-temp)
       (setf (gethash ,sym-temp *objc-classname-table*) ,str-temp)
       (values))))


;;; Translate an ObjC class name to a Lisp class name

(defun objc-to-lisp-classname (str &optional (package *package*))
  (let ((sym 
         (or (gethash str *lisp-classname-table*)
             (compute-lisp-name str package))))
    (setf (gethash sym *objc-classname-table*) str)
    (setf (gethash str *lisp-classname-table*) sym)))


;;; Translate a Lisp class name to an ObjC class name

(defun lisp-to-objc-classname (sym)
  (let ((str 
         (or (gethash sym *objc-classname-table*)
             (compute-objc-classname sym))))
    (setf (gethash str *lisp-classname-table*) sym)
    (setf (gethash sym *objc-classname-table*) str)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                      Message Keyword Translation                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Hash tables for caching initializer translations

(defvar *lisp-message-table* (make-hash-table :test #'equal))
(defvar *objc-message-table* (make-hash-table :test #'equal))


;;; Define a hard-wired message-keyword translation (if the automatic
;;; translation doesn't apply) 

(defmacro define-message-translation (message msg-keywords)
  (let ((message-temp (gensym))
        (msg-keywords-temp (gensym))
        (old-message-temp (gensym))
        (old-msg-keywords-temp (gensym)))
    `(let* ((,message-temp ',message)
            (,msg-keywords-temp ',msg-keywords)
            (,old-message-temp 
             (gethash ,message-temp *lisp-message-table*))
            (,old-msg-keywords-temp 
             (gethash ,msg-keywords-temp *objc-message-table*)))
       (remhash ,old-message-temp *lisp-message-table*)
       (remhash ,old-msg-keywords-temp *objc-message-table*)
       (setf (gethash ,message-temp *lisp-message-table*) ,msg-keywords-temp)
       (setf (gethash ,msg-keywords-temp *objc-message-table*) ,message-temp)
       (values))))


;;; Translate an ObjC message to a list of Lisp message keywords

(defun objc-to-lisp-message (message)
  (let ((msg-keywords 
         (or (gethash message *lisp-message-table*)
             (compute-objc-to-lisp-message message))))
    (setf (gethash msg-keywords *objc-message-table*) message)
    (setf (gethash message *lisp-message-table*) msg-keywords)))


;;; Translate a set of Lisp message keywords to an ObjC message 

(defun lisp-to-objc-message (msg-keywords)
  (let ((message 
         (or (gethash msg-keywords *objc-message-table*)
             (compute-lisp-to-objc-message msg-keywords))))
    (setf (gethash message *lisp-message-table*) msg-keywords)
    (setf (gethash msg-keywords *objc-message-table*) message)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                        Initializer Translation                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Hash tables for caching initializer translations

(defvar *lisp-initializer-table* (make-hash-table :test #'equal))
(defvar *objc-initializer-table* (make-hash-table :test #'equal))


;;; Define a hard-wired init-keyword translation (if the automatic
;;; translation doesn't apply) 

(defmacro define-init-translation (initmsg initargs)
  (let ((initmsg-temp (gensym))
        (initargs-temp (gensym))
        (old-initmsg-temp (gensym))
        (old-initargs-temp (gensym)))
    `(let* ((,initmsg-temp ',initmsg)
            (,initargs-temp ',initargs)
            (,old-initmsg-temp 
             (gethash ,initmsg-temp *lisp-initializer-table*))
            (,old-initargs-temp 
             (gethash ,initargs-temp *objc-initializer-table*)))
       (remhash ,old-initmsg-temp *lisp-initializer-table*)
       (remhash ,old-initargs-temp *objc-initializer-table*)
       (setf (gethash ,initmsg-temp *lisp-initializer-table*) ,initargs-temp)
       (setf (gethash ,initargs-temp *objc-initializer-table*) ,initmsg-temp)
       (values))))


;;; Translate an ObjC initializer to a list of Lisp initargs

(defun objc-to-lisp-init (initmsg)
  (let ((initargs 
         (or (gethash initmsg *lisp-initializer-table*)
             (compute-objc-to-lisp-init initmsg))))
    (setf (gethash initargs *objc-initializer-table*) initmsg)
    (setf (gethash initmsg *lisp-initializer-table*) initargs)))


;;; Translate a set of Lisp initargs to an ObjC initializer 

(defun lisp-to-objc-init (initargs)
  (let ((initmsg 
         (or (gethash initargs *objc-initializer-table*)
             (compute-lisp-to-objc-init initargs))))
    (setf (gethash initmsg *lisp-initializer-table*) initargs)
    (setf (gethash initargs *objc-initializer-table*) initmsg)))
