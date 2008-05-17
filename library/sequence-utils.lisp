;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: cl-user -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          split-if.lisp
;;;; Version:       0.1
;;;; Project:       utilities
;;;; Purpose:       utilities for splitting sequences
;;;;
;;;; ***********************************************************************

(in-package "CCL")


;;; Split a sequence SEQ at each point where TEST is true 
;;; DIR should be one of :BEFORE, :AFTER or :ELIDE

(defun split-if (test seq &optional (dir :before))
  (remove-if
   #'(lambda (x) (equal x (subseq seq 0 0)))
   (loop for start fixnum = 0 
         then (if (eq dir :before) stop (the fixnum (1+ (the fixnum stop))))
         while (< start (length seq))
         for stop = (position-if 
                     test seq 
                     :start (if (eq dir :elide) start (the fixnum (1+ start))))
         collect (subseq 
                  seq start 
                  (if (and stop (eq dir :after)) 
                    (the fixnum (1+ (the fixnum stop))) 
                    stop))
         while stop)))
  
(defun split-if-char (char seq &optional dir)
  (split-if #'(lambda (ch) (eq ch char)) seq dir))

(defmethod split-lines ((text string))
  (delete-if (lambda (x) (string= x ""))
             (mapcar (lambda (s)
                       (string-trim '(#\return #\newline) s))
                     (split-if (lambda (c) (member c '(#\return #\newline) :test #'char=))
                               text))))
