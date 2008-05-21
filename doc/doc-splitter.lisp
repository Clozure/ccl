;;; Copyright (c) 2008 Clozure Associates.  All Rights Reserved.

;;;
;;; (doc-splitter:split-doc-file "ccl:doc;ccl-documentation.html" "ccl:doc;manual;")
;;;

(eval-when (eval compile load)
  (defpackage doc-splitter
    (:use common-lisp ccl)
    (:export #:split-doc-file)))

(in-package doc-splitter)

(defparameter *output-template*
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />
    <title>Clozure CL Documentation</title>
  </head>
  <body>

<table width=\"100%\" cellspacing=\"1\" cellpadding=\"1\" border=\"1\">
<tr><td align=\"center\" bgcolor=\"((BGCOLOR))\" width=\"20%\">((PREVIOUS))</td>
    <td align=\"center\" bgcolor=\"((BGCOLOR))\" width=\"20%\">((NEXT))</td>
    <td align=\"center\" bgcolor=\"((BGCOLOR))\" width=\"0%\"></td>
    <td align=\"center\" bgcolor=\"((BGCOLOR))\" width=\"20%\">((HOME))</td>
    <td align=\"center\" bgcolor=\"((BGCOLOR))\" width=\"20%\">((GLOSSARY))</td>
    <td align=\"center\" bgcolor=\"((BGCOLOR))\" width=\"20%\">((INDEX))</td>
</tr></table>
<hr>
 ((BODY))
<hr>
<table width=\"100%\" cellspacing=\"1\" cellpadding=\"1\" border=\"1\">
<tr><td align=\"center\" bgcolor=\"((BGCOLOR))\" width=\"20%\">((PREVIOUS))</td>
    <td align=\"center\" bgcolor=\"((BGCOLOR))\" width=\"20%\">((NEXT))</td>
    <td align=\"center\" bgcolor=\"((BGCOLOR))\" width=\"0%\"></td>
    <td align=\"center\" bgcolor=\"((BGCOLOR))\" width=\"20%\">((HOME))</td>
    <td align=\"center\" bgcolor=\"((BGCOLOR))\" width=\"20%\">((GLOSSARY))</td>
    <td align=\"center\" bgcolor=\"((BGCOLOR))\" width=\"20%\">((INDEX))</td>
</tr></table>
</body>
")

(defparameter *links-bgcolor* "lightgray")

(defparameter *link-names* '((:previous . "Previous")
                             (:next . "Next")
                             (:up . "Up")
                             (:home . "Table of Contents")
                             (:glossary . "Glossary")
                             (:index . "Index")))

(defun output-split-doc-header-link (stream sf link)
  (let ((name (cdr (assq link *link-names*))))
    (if sf
      (format stream "<a href=\"~a\"><b>~a~@[ ~a~]</b></a>"
              (split-file-name sf)
              name
              (and (memq link '(:previous :next))
                   (if (eq (split-file-type sf) :sect1) "Section" "Chapter")))
      (format stream "~:(~a~)" name))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct node
  start
  end)

;; Text node
(defstruct (tnode (:include node))
  )

;; Compound node
(defstruct (cnode (:include node))
  tag
  tag-end
  children)

(defmethod print-object ((node cnode) stream)
  (print-unreadable-object (node stream :type t) 
    (format stream "~s ~s:~s:~s~a"
	    (cnode-tag node) (cnode-start node) (cnode-tag-end node) (cnode-end node)
	    (cond ((null (cnode-children node)) "")
		  ((null (cdr (cnode-children node))) " 1 child")
		  (t (format nil " ~s children" (length (cnode-children node))))))))

(defun node-tag (node)
  (and (cnode-p node) (cnode-tag node)))

;; Toplevel node
(defstruct html
  string
  node)

(defmethod print-object ((node html) stream)
  (print-unreadable-object (node stream :type t)
    (let ((*print-string-length* 400))
      (format stream ":STRING ~s :NODE ~s" (html-string node) (html-node node)))))

(defstruct split-file
  type
  name
  up
  nodes)

(defvar *cur-html* nil)

(defun split-doc-file (html directory)
  (unless (html-p html)
    (setq html (read-html-file html)))
  (ensure-directories-exist directory)
  (let* ((*cur-html* html)
         (splits (doc-file-splits html))
         (id-table (make-hash-table :test #'equal))
         (top (find :book splits :key #'split-file-type))
         (glossary (find :glossary splits :key #'split-file-type))
         (index (find :symbol-index splits :key #'split-file-type)))
    (loop for sf in splits as name = (split-file-name sf)
      do (loop for node in (split-file-nodes sf)
           do (doc-file-register-ids node name id-table)))
    (loop
      for prev = nil then sf
      for prev-chap = nil then (if (eq (split-file-type sf) :sect1) prev-chap sf)
      for sfs on splits
      for sf = (car sfs)
      do (with-open-file (stream (merge-pathnames (split-file-name sf) directory)
                                 :direction :output
                                 :if-exists :supersede)
           (output-split-doc-file sf stream id-table
                                  :previous (if (eq (split-file-type sf) :sect1) prev prev-chap)
                                  :next (if (eq (split-file-type sf) :sect1)
                                          (cadr sfs)
                                          (find :sect1 (cdr sfs) :key #'split-file-type :test #'neq))
                                  :top top
                                  :glossary glossary
                                  :index index)))))

(defun output-split-doc-file (sf stream id-table &key previous next top glossary index)
  (loop with template = *output-template*
    for start = 0 then (+ epos 2)
    as bpos = (search "((" template :start2 start) while bpos
    as epos = (search "))" template :start2 bpos)
    do (write-string template stream :start start :end bpos)
    do (ecase (intern (subseq template (+ bpos 2) epos) :keyword)
         (:previous
          (output-split-doc-header-link stream previous :previous))
         (:next
          (output-split-doc-header-link stream next :next))
         (:home
          (output-split-doc-header-link stream top :home))
         (:glossary
          (output-split-doc-header-link stream glossary :glossary))
         (:index
          (output-split-doc-header-link stream index :index))
         (:bgcolor
          (write-string *links-bgcolor* stream))
         (:body
          (output-split-doc-file-body stream sf id-table)))
    finally (write-string template stream :start start)))

;; (setq *print-string-length* 400 *print-length* 100 *print-level* 50)
(defun read-html-file (pathname)
  (with-open-file (stream pathname)
    (let ((str (make-string (file-length stream))))
      (read-sequence str stream)
      (make-html :string str
                 :node (read-html-form str (search "<html" str :test #'char-equal) (length str))))))


(defun output-split-doc-file-body (stream sf id-table)
  (let* ((up (split-file-up sf))
         (up-title (and up (split-file-title up))))
    (when up-title
      (format stream "<a href=\"~a\">~a</a>" (split-file-name up) up-title)))
  (loop with string = (html-string *cur-html*)
    for node in (split-file-nodes sf)
    do (let ((hrefs (doc-file-collect-hrefs node id-table)))
         (setq hrefs (sort hrefs #'< :key #'car))
         (assert (or (null hrefs) (<= (node-start node) (caar hrefs))))
         (loop as start = (node-start node) then pos
           for (pos . name) in hrefs
           do (write-string string stream :start start :end pos)
           do (write-string name stream)
           finally (write-string string stream :start start :end (node-end node)))
         (fresh-line stream))))

(defun doc-file-register-ids (node name hash)
  (when (cnode-p node)
    (let ((id (and (eq (cnode-tag node) :a)
                   (cnode-attribute-value node :id))))
      (when id
        (let ((old (gethash id hash)))
          (when old
            (warn "~s already registered in file ~s" id old)))
        (setf (gethash id hash) name)))
    (loop for subnode in (cnode-children node)
      do (doc-file-register-ids subnode name hash))))

(defun doc-file-collect-hrefs (node hash)
  (when (cnode-p node)
    (let* ((hrefs (loop for subnode in (cnode-children node)
                    nconc (doc-file-collect-hrefs subnode hash)))
           (href (and (eq (cnode-tag node) :a)
                      (cnode-attribute-value node :href))))
      (when (and href (position #\# href))
        (assert (eql (char href 0) #\#))
        (let ((name (gethash (subseq href 1) hash)))
          (unless name
             (warn "Couldn't find the split file id for href ~s" href))
          (when name
            (let ((pos (search (format nil "href=~s" href) (html-string *cur-html*)
                               :start2 (cnode-start node) :end2 (cnode-tag-end node))))
              (assert pos)
              (push (cons (+ pos 6) name) hrefs)))))
      hrefs)))

(defparameter *times* 0)
(defun split-file-title (sf)
  (labels ((title (node)
             (when (cnode-p node)
               (if (and (eq (cnode-tag node) :h2)
                        (equal (cnode-attribute-value node :class) "title"))
                 (labels ((text (node)
                            (if (tnode-p node)
                              (subseq (html-string *cur-html*) (node-start node) (node-end node))
                              (apply #'concatenate 'string
                                     (loop for sub in (cnode-children node) collect (text sub))))))
                   (text node))
                 (loop for sub in (cnode-children node) thereis (title sub))))))
    (loop for node in (split-file-nodes sf) thereis (title node))))

(defun doc-file-splits (html)
  (let* ((*cur-html* html)
         (node (html-node html)))
    (assert (eq (node-tag node) :html))
    (setq node (find :body (cnode-children node) :key #'node-tag))
    (assert node)
    (setq node (find :div (cnode-children node) :key #'node-tag))
    (assert node)
    (assert (equal (cnode-attribute-value node :class) "book"))
    (loop with nchapters = 0
      for subnode in (cnode-children node)
      as class = (and (eq (node-tag subnode) :div) (cnode-attribute-value subnode :class))
      if (member class '("chapter" "glossary" "index") :test #'equal)
      nconc (doc-file-chapter-splits subnode (incf nchapters)) into sections
      else collect subnode into nodes
      finally (let ((sf (make-split-file :name "index.html" :type :book :nodes nodes)))
                (loop for sub in sections
                  unless (eq (split-file-type sub) :sect1) do (setf (split-file-up sub) sf))
                (return (cons sf sections))))))

(defun doc-file-chapter-splits (node num)
  (let* ((class (and (eq (node-tag node) :div) (cnode-attribute-value node :class))))
    (cond ((equal class "chapter")
           (loop with nsect = 0
             for subnode in (cnode-children node)
             as class = (and (eq (node-tag subnode) :div) (cnode-attribute-value subnode :class))
             if (equal class "sect1")
             collect (make-split-file :name (format nil "chapter~d.~d.html" num (incf nsect))
                                      :type :sect1 :nodes (list subnode)) into sections
             else collect subnode into nodes
             finally (let ((sf (make-split-file :name (format nil "chapter~d.html" num)
                                                    :type :chapter :nodes nodes)))
                       (loop for sub in sections do (setf (split-file-up sub) sf))
                       (return (cons sf sections)))))
          ((equal class "glossary")
           (list (make-split-file :name "glossary.html" :type :glossary :nodes (list node))))
          ((equal class "index")
           (list (make-split-file :name "symbol-index.html" :type :symbol-index :nodes (list node))))
          (t (error "expected a chapter, glossary or index: ~s" class)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns NIL for </tag> case.
(defun read-html-tag (str s e &aux (s1 (1+ s)))
  (and (< s e)
       (eq (char str s) #\<)
       (let* ((te (or (position-if #'(lambda (ch) (or (whitespacep ch)
                                                      (char= ch #\>)
                                                      (char= ch #\/)))
                                   str :start s1 :end e)
                      e)))
	 (and (< s1 te)
	      (intern (nstring-upcase (subseq str s1 te)) ccl::*keyword-package*)))))

;; Returns NIL if at end of buffer or if looking at "</..."
(defun read-html-form (str s e &optional (tag (read-html-tag str s e)))
  (cond (tag
	 (let* ((te (1+ (position-ignoring-strings #\> str s e)))
		(node (make-cnode :tag tag
				  :start s
				  :tag-end te
				  :end e
				  :children nil)))
	   (if (eq (char str (- te 2)) #\/)
	     (setf (node-end node) te)
	     (read-html-children-into-cnode str node))
	   node))
	((>= s e) NIL)
	((eq (char str s) #\<)
	 (assert (and (< (1+ s) e) (eq (char str (1+ s)) #\/)))
	 NIL)
	(t (make-tnode :start s :end (or (position #\< str :start s :end e) e)))))

(defun position-ignoring-strings (ch str start end)
  (let* ((p (position ch str :start start :end end)))
    (and p
	 (let ((q (position #\" str :start start :end p)))
	   (if (null q)
	     p
	     (let ((qe (position #\" str :start (1+ q) :end end)))
	       (and qe
		    (position-ignoring-strings ch str (1+ qe) end))))))))

(defun read-html-children-into-cnode (str node)
  ;; This is entered with node-end = end of region, and it updates both
  ;; cnode-children and node-end.  Eats up the ending tag if it matches
  ;; the node tag, otherwise leaves it to be re-read.
  (let* ((s (cnode-tag-end node))
	 (e (cnode-end node)))
    (loop
      (assert (< s e) () "Unended tag ~S" (subseq str (cnode-start node) e))
      (when (string= "</" str :start2 s :end2 (min (+ s 2) e))
	(let* ((te (1+ (position #\> str :start s :end e))))
	  (setf (cnode-end node)
		(if (string-equal str (symbol-name (cnode-tag node))
				   :start1 (+ s 2) :end1 (1- te))
		  te s))
	  (return)))
      (let* ((ntag (read-html-tag str s e))
             (child (read-html-form str s e ntag)))
        (setq s (node-end child))
        (push child (cnode-children node))))
    (setf (cnode-children node) (nreverse (cnode-children node)))))

(defun cnode-attributes (node &optional string-or-html &aux string)
  (setq string-or-html (or string-or-html *cur-html*))
  (setq string (if (html-p string-or-html) (html-string string-or-html) string-or-html))
  (multiple-value-bind (start end)
      (let* ((start (1+ (node-start node)))
             (end (cnode-tag-end node))
             (word-end (position-if #'(lambda (ch) (or (whitespacep ch)
                                                       (char= ch #\>)
                                                       (char= ch #\/)))
                                    string :start start :end end)))
        (assert word-end)
        (values word-end (1- end)))
    (flet ((next-token (type)
             (when (setq start (position-if-not #'whitespacep string :start start :end end))
               (let ((ch (char string start)))
                 (incf start)
                 (case ch
                   ((#\" #\')
                    (assert (eq type :value))
                    (let ((tend (position ch string :start start :end end)))
                      (prog1
                          (subseq string start tend)
                        (setq start (1+ tend)))))
                   ((#\=)
                    (assert (eq type :separator))
                    t)
                   ((nil)
                    (assert (or (eq type :attribute) (eq type :separator)))
                    nil)
                   (t
                    (assert (or (eq type :value) (eq type :attribute)))
                    (let ((tend (or (position-if #'(lambda (ch) (or (whitespacep ch) (eql ch #\=)))
                                                 string :start start :end end) end)))
                      (prog1
                          (subseq string (1- start) tend)
                        (setq start tend)))))))))
      (loop
        as attribute = (next-token :attribute) while attribute
        collect (cons (intern (string-upcase attribute) :keyword)
                      (if (next-token :separator) (next-token :value) t))))))

(defun cnode-attribute-value (node attribute &optional string-or-html)
  (cdr (assoc attribute (cnode-attributes node string-or-html) :test #'eq)))

#+debugging
(defun debug-print-html (str node &key (stream t) (depth nil))
  (when (html-p str) (setq str (html-string str)))
  (if (null stream)
    (with-output-to-string (s) (debug-print-html str node :stream s :depth depth))
    (labels ((print (node cur-depth)
               (etypecase node
                 (tnode (format stream "~A" (subseq str (node-start node) (node-end node))))
                 (cnode (format stream "~A" (subseq str (node-start node) (cnode-tag-end node)))
                        (if (or (null depth) (< cur-depth depth))
                          (dolist (child (cnode-children node))
                            (print child (1+ cur-depth)))
                          (format stream "..."))
                        (format stream "</~A>" (node-tag node))))))
      (print node 0))))

#+debugging
(defun debug-outline-html (str node &key (stream t) (depth nil))
  (if (null stream)
    (with-output-to-string (s) (debug-outline-html str node s depth))
    (labels ((outline (node cur-depth idx)
               (etypecase node
                 (tnode (unless (loop for i from (node-start node) below (node-end node)
                                  always (whitespacep (char str i)))
                          (if idx (format stream "[~a]..." idx) (format stream "..."))))
                 (cnode (fresh-line stream)
                        (if idx (format stream "~&[~a]" idx) (format stream "~&"))
                        (dotimes (i cur-depth) (write-char #\Space stream))
                        (format stream "<~A ~:a>" (cnode-tag node) (cnode-attributes node str))
                        (when (or (null depth) (< cur-depth depth))
                          (loop for i upfrom 0 as child in  (cnode-children node)
                            do (outline child (1+ cur-depth) (if idx (format nil "~a.~d" idx i)
                                                               (format nil "~d" i)))))
                        (format stream "</~A>" (node-tag node))))))
      (outline node 0 nil))))
