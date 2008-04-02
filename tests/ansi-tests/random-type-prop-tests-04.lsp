;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Mar  6 21:44:41 2005
;;;; Contains: Test that invoke the random type prop infrastructure, part 4

(in-package :cl-test)

(defun char-or-same (c &rest args)
  (declare (ignore args))
  (if (coin) `(eql ,c) 'character))

(eval-when (:load-toplevel :execute) (compile 'char-or-same))

(def-type-prop-test char=.1 'char= nil 2 :rest-type 'base-char :maxargs 5)
(def-type-prop-test char=.2 'char= '(character character) 2)
(def-type-prop-test char=.3 'char= (list 'character #'char-or-same) 2)
(def-type-prop-test char=.4 'char= (list 'character #'char-or-same #'char-or-same) 3)
(def-type-prop-test char=.5 'char= '(character) 3 :rest-type #'char-or-same :maxargs 6)

(def-type-prop-test char/=.1 'char/= nil 2 :rest-type 'base-char :maxargs 5)
(def-type-prop-test char/=.2 'char/= '(character character) 2)
(def-type-prop-test char/=.3 'char/= (list 'character #'char-or-same) 2)
(def-type-prop-test char/=.4 'char/= (list 'character 'character #'char-or-same) 3)
(def-type-prop-test char/=.5 'char/= nil 2 :rest-type 'character :maxargs 6)

(def-type-prop-test char<=.1 'char<= nil 2 :rest-type 'base-char :maxargs 5)
(def-type-prop-test char<=.2 'char<= '(character character) 2)
(def-type-prop-test char<=.3 'char<= (list 'character #'char-or-same) 2)
(def-type-prop-test char<=.4 'char<= (list 'character #'char-or-same #'char-or-same) 3)
(def-type-prop-test char<=.5 'char<= '(character) 3 :rest-type #'char-or-same :maxargs 6)

(def-type-prop-test char>=.1 'char>= nil 2 :rest-type 'base-char :maxargs 5)
(def-type-prop-test char>=.2 'char>= '(character character) 2)
(def-type-prop-test char>=.3 'char>= (list 'character #'char-or-same) 2)
(def-type-prop-test char>=.4 'char>= (list 'character #'char-or-same #'char-or-same) 3)
(def-type-prop-test char>=.5 'char>= '(character) 3 :rest-type #'char-or-same :maxargs 6)

(def-type-prop-test char<.1 'char< nil 2 :rest-type 'base-char :maxargs 5)
(def-type-prop-test char<.2 'char< '(character character) 2)
(def-type-prop-test char<.3 'char< (list 'character #'char-or-same) 2)
(def-type-prop-test char<.4 'char< (list 'character 'character #'char-or-same) 3)
(def-type-prop-test char<.5 'char< nil 2 :rest-type 'character :maxargs 6)

(def-type-prop-test char>.1 'char> nil 2 :rest-type 'base-char :maxargs 5)
(def-type-prop-test char>.2 'char> '(character character) 2)
(def-type-prop-test char>.3 'char> (list 'character #'char-or-same) 2)
(def-type-prop-test char>.4 'char> (list 'character 'character #'char-or-same) 3)
(def-type-prop-test char>.5 'char> nil 2 :rest-type 'character :maxargs 6)


(def-type-prop-test char-equal.1 'char-equal nil 2 :rest-type 'base-char :maxargs 5)
(def-type-prop-test char-equal.2 'char-equal '(character character) 2)
(def-type-prop-test char-equal.3 'char-equal (list 'character #'char-or-same) 2)
(def-type-prop-test char-equal.4 'char-equal (list 'character #'char-or-same #'char-or-same) 3)
(def-type-prop-test char-equal.5 'char-equal '(character) 3 :rest-type #'char-or-same :maxargs 6)

(def-type-prop-test char-not-equal.1 'char-not-equal nil 2 :rest-type 'base-char :maxargs 5)
(def-type-prop-test char-not-equal.2 'char-not-equal '(character character) 2)
(def-type-prop-test char-not-equal.3 'char-not-equal (list 'character #'char-or-same) 2)
(def-type-prop-test char-not-equal.4 'char-not-equal (list 'character 'character #'char-or-same) 3)
(def-type-prop-test char-not-equal.5 'char-not-equal nil 2 :rest-type 'character :maxargs 6)

(def-type-prop-test char-not-greaterp.1 'char-not-greaterp nil 2 :rest-type 'base-char :maxargs 5)
(def-type-prop-test char-not-greaterp.2 'char-not-greaterp '(character character) 2)
(def-type-prop-test char-not-greaterp.3 'char-not-greaterp (list 'character #'char-or-same) 2)
(def-type-prop-test char-not-greaterp.4 'char-not-greaterp (list 'character #'char-or-same #'char-or-same) 3)
(def-type-prop-test char-not-greaterp.5 'char-not-greaterp '(character) 3 :rest-type #'char-or-same :maxargs 6)

(def-type-prop-test char-not-lessp.1 'char-not-lessp nil 2 :rest-type 'base-char :maxargs 5)
(def-type-prop-test char-not-lessp.2 'char-not-lessp '(character character) 2)
(def-type-prop-test char-not-lessp.3 'char-not-lessp (list 'character #'char-or-same) 2)
(def-type-prop-test char-not-lessp.4 'char-not-lessp (list 'character #'char-or-same #'char-or-same) 3)
(def-type-prop-test char-not-lessp.5 'char-not-lessp '(character) 3 :rest-type #'char-or-same :maxargs 6)

(def-type-prop-test char-lessp.1 'char-lessp nil 2 :rest-type 'base-char :maxargs 5)
(def-type-prop-test char-lessp.2 'char-lessp '(character character) 2)
(def-type-prop-test char-lessp.3 'char-lessp (list 'character #'char-or-same) 2)
(def-type-prop-test char-lessp.4 'char-lessp (list 'character 'character #'char-or-same) 3)
(def-type-prop-test char-lessp.5 'char-lessp nil 2 :rest-type 'character :maxargs 6)

(def-type-prop-test char-greaterp.1 'char-greaterp nil 2 :rest-type 'base-char :maxargs 5)
(def-type-prop-test char-greaterp.2 'char-greaterp '(character character) 2)
(def-type-prop-test char-greaterp.3 'char-greaterp (list 'character #'char-or-same) 2)
(def-type-prop-test char-greaterp.4 'char-greaterp (list 'character 'character #'char-or-same) 3)
(def-type-prop-test char-greaterp.5 'char-greaterp nil 2 :rest-type 'character :maxargs 6)

(defun length1-p (seq) (= (length seq) 1))
  
(def-type-prop-test character 'character '((or character (and (string 1) (satisfies length1-p)))) 1)
(def-type-prop-test characterp 'characterp '(t) 1)
(def-type-prop-test alpha-char-p 'alpha-char-p '(character) 1)
(def-type-prop-test alphanumericp 'alphanumericp '(character) 1)
(def-type-prop-test digit-char 'digit-char '((or (integer 0 36) (integer 0)) (integer 2 36)) 1 :maxargs 2)
(def-type-prop-test digit-char-p 'digit-char-p '(character) 1)
(def-type-prop-test graphic-char-p 'graphic-char-p '(character) 1)
(def-type-prop-test standard-char-p 'standard-char-p '(character) 1)
(def-type-prop-test char-upcase 'char-upcase '(character) 1)
(def-type-prop-test char-downcase 'char-downcase '(character) 1)
(def-type-prop-test upper-case-p 'upper-case-p '(character) 1)
(def-type-prop-test lower-case-p 'lower-case-p '(character) 1)
(def-type-prop-test both-case-p 'both-case-p '(character) 1)
(def-type-prop-test char-code 'char-code '(character) 1)
(def-type-prop-test char-int 'char-int '(character) 1)
(def-type-prop-test code-char 'code-char '((integer 0 #.char-code-limit)) 1)
(def-type-prop-test char-name 'char-name '(character) 1)
(def-type-prop-test name-char 'name-char '(string) 1)

