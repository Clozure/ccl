;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: cl-user -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          sequence-utils.lisp
;;;; Version:       0.2
;;;; Project:       utilities
;;;; Purpose:       utilities for working with sequences
;;;;
;;;; ***********************************************************************

(in-package "CCL")

;;; -----------------------------------------------------------------
;;; splitting sequences
;;; -----------------------------------------------------------------

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

;;; -----------------------------------------------------------------
;;; matching subsequences
;;; -----------------------------------------------------------------

(defun match-subsequence (subseq seq &key (test #'eql) (start 0))
  (let ((max-index (1- (length seq))))
    (block matching
      ;; search for mismatches
      (dotimes (i (length subseq))
        (let ((pos (+ start i)))
          (when (or (> pos max-index)
                    (not (funcall test (elt seq pos)
                                  (elt subseq i))))
            (return-from matching nil))))
      ;; no mismatches found; return true
      (return-from matching t))))

(defun %find-matching-subsequence-backward (subseq seq &key (test #'eql) (start 0) end)
  (let ((end (or end (length seq)))
        (pos end)
        (min-index (or start 0)))
    (block finding
      (dotimes (i (- (length seq) start))
        (setf pos (- end i))
        (if (<= pos min-index)
            (return-from finding nil)
            (when (match-subsequence subseq seq :test test :start pos)
              (return-from finding pos))))
      nil)))

(defun %find-matching-subsequence-forward (subseq seq &key (test #'eql) (start 0) end)
  (let ((pos start)
        (max-index (or end (length seq))))
    (block finding
      (dotimes (i (- (length seq) start))
        (setf pos (+ start i))
        (if (>= pos max-index)
            (return-from finding nil)
            (when (match-subsequence subseq seq :test test :start pos)
              (return-from finding pos))))
      nil)))

(defun find-matching-subsequence (subseq seq &key (test #'eql) (start 0) end from-end)
  (if from-end
      (%find-matching-subsequence-backward subseq seq :test test :start start :end end)
      (%find-matching-subsequence-forward subseq seq :test test :start start :end end)))