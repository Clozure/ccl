;;; -*- Log: hemlock.log; Package: Hemlock -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
#+CMU (ext:file-comment
  "$Header$")
;;;
;;; **********************************************************************
;;;
;;; This file contains definitions of various character attributes.
;;;
(in-package :hemlock)

(defattribute "Whitespace"
  "A value of 1 for this attribute indicates that the corresponding character
  should be considered as whitespace.  This is used by the Blank-Line-P
  function.")

(setf (character-attribute :whitespace #\space) 1)
(setf (character-attribute :whitespace #\linefeed) 1)
(setf (character-attribute :whitespace #\tab) 1)
(setf (character-attribute :whitespace #\newline) 1)
(setf (character-attribute :whitespace #\return) 1)

(defattribute "Word Delimiter"
  "A value of 1 for this attribute indicates that the corresponding character
  separates words.  This is used by the word manipulating commands.")

(setf (character-attribute :word-delimiter nil) 1)
(setf (character-attribute :word-delimiter #\!) 1)
(setf (character-attribute :word-delimiter #\@) 1)
(setf (character-attribute :word-delimiter #\#) 1)
(setf (character-attribute :word-delimiter #\$) 1)
(setf (character-attribute :word-delimiter #\%) 1)
(setf (character-attribute :word-delimiter #\^) 1)
(setf (character-attribute :word-delimiter #\&) 1)
(setf (character-attribute :word-delimiter #\*) 1)
(setf (character-attribute :word-delimiter #\() 1)
(setf (character-attribute :word-delimiter #\)) 1)
(setf (character-attribute :word-delimiter #\-) 1)
(setf (character-attribute :word-delimiter #\_) 1)
(setf (character-attribute :word-delimiter #\=) 1)
(setf (character-attribute :word-delimiter #\+) 1)
(setf (character-attribute :word-delimiter #\[) 1)
(setf (character-attribute :word-delimiter #\]) 1)
(setf (character-attribute :word-delimiter #\\) 1)
(setf (character-attribute :word-delimiter #\|) 1)
(setf (character-attribute :word-delimiter #\;) 1)
(setf (character-attribute :word-delimiter #\:) 1)
(setf (character-attribute :word-delimiter #\') 1)
(setf (character-attribute :word-delimiter #\") 1)
(setf (character-attribute :word-delimiter #\{) 1)
(setf (character-attribute :word-delimiter #\}) 1)
(setf (character-attribute :word-delimiter #\,) 1)
(setf (character-attribute :word-delimiter #\.) 1)
(setf (character-attribute :word-delimiter #\<) 1)
(setf (character-attribute :word-delimiter #\>) 1)
(setf (character-attribute :word-delimiter #\/) 1)
(setf (character-attribute :word-delimiter #\?) 1)
(setf (character-attribute :word-delimiter #\`) 1)
(setf (character-attribute :word-delimiter #\~) 1)
(setf (character-attribute :word-delimiter #\space) 1)
(setf (character-attribute :word-delimiter #\linefeed) 1)
(setf (character-attribute :word-delimiter
                           #+CMU #\formfeed
                           #+(or EXCL sbcl CLISP Clozure) #\page) 1)
(setf (character-attribute :word-delimiter #\tab) 1)
(setf (character-attribute :word-delimiter #\return) 1)

(shadow-attribute :word-delimiter #\. 0 "Fundamental")
(shadow-attribute :word-delimiter #\' 0 "Text")
(shadow-attribute :word-delimiter #\backspace 0 "Text")
(shadow-attribute :word-delimiter #\_ 0 "Text")

;; These aren't generally used to separate words in a Lisp symbol
(shadow-attribute :word-delimiter #\* 0 "Lisp")
(shadow-attribute :word-delimiter #\! 0 "Lisp")
(shadow-attribute :word-delimiter #\$ 0 "Lisp")
(shadow-attribute :word-delimiter #\+ 0 "Lisp")
(shadow-attribute :word-delimiter #\% 0 "Lisp")
(shadow-attribute :word-delimiter #\^ 0 "Lisp")
(shadow-attribute :word-delimiter #\& 0 "Lisp")
(shadow-attribute :word-delimiter #\? 0 "Lisp")
(shadow-attribute :word-delimiter #\_ 0 "Lisp")
(shadow-attribute :word-delimiter #\= 0 "Lisp")
(shadow-attribute :word-delimiter #\[ 0 "Lisp")
(shadow-attribute :word-delimiter #\] 0 "Lisp")
(shadow-attribute :word-delimiter #\\ 0 "Lisp")
(shadow-attribute :word-delimiter #\| 0 "Lisp")
(shadow-attribute :word-delimiter #\{ 0 "Lisp")
(shadow-attribute :word-delimiter #\} 0 "Lisp")
(shadow-attribute :word-delimiter #\< 0 "Lisp")
(shadow-attribute :word-delimiter #\> 0 "Lisp")
(shadow-attribute :word-delimiter #\/ 0 "Lisp")
(shadow-attribute :word-delimiter #\~ 0 "Lisp")


(defattribute "Page Delimiter"
  "This attribute is 1 for characters that separate pages, 0 otherwise.")
(setf (character-attribute :page-delimiter nil) 1)
(setf (character-attribute :page-delimiter #\page) 1)


(defattribute "Lisp Syntax"
  "These character attribute is used by the lisp mode commands, and possibly
  other people.  The value of ths attribute is always a symbol.  Currently
  defined values are:
   NIL - No interesting properties.
   :space - Acts like whitespace, should not include newline.
   :newline - Newline, man.
   :open-paren - An opening bracket.
   :close-paren - A closing bracket.
   :prefix - A character that is a part of any form it appears before.
   :string-quote - The character that quotes a string.
   :char-quote - The character that escapes a single character.
   :comment - The character that comments out to end of line.
   :constituent - Things that make up symbols."
  'symbol nil)

(setf (character-attribute :lisp-syntax #\space) :space)
(setf (character-attribute :lisp-syntax #\tab) :space)

(setf (character-attribute :lisp-syntax #\() :open-paren)
(setf (character-attribute :lisp-syntax #\)) :close-paren)
(setf (character-attribute :lisp-syntax #\') :prefix)
(setf (character-attribute :lisp-syntax #\`) :prefix)  
(setf (character-attribute :lisp-syntax #\#) :prefix)
(setf (character-attribute :lisp-syntax #\,) :prefix)
(setf (character-attribute :lisp-syntax #\") :string-quote)
(setf (character-attribute :lisp-syntax #\\) :char-quote)
(setf (character-attribute :lisp-syntax #\;) :comment)
(setf (character-attribute :lisp-syntax #\newline) :newline)
(setf (character-attribute :lisp-syntax nil) :newline)

(do-alpha-chars (ch :both)
  (setf (character-attribute :lisp-syntax ch) :constituent))

(setf (character-attribute :lisp-syntax #\0) :constituent)
(setf (character-attribute :lisp-syntax #\1) :constituent)
(setf (character-attribute :lisp-syntax #\2) :constituent)
(setf (character-attribute :lisp-syntax #\3) :constituent)
(setf (character-attribute :lisp-syntax #\4) :constituent)
(setf (character-attribute :lisp-syntax #\5) :constituent)
(setf (character-attribute :lisp-syntax #\6) :constituent)
(setf (character-attribute :lisp-syntax #\7) :constituent)
(setf (character-attribute :lisp-syntax #\8) :constituent)
(setf (character-attribute :lisp-syntax #\9) :constituent)

(setf (character-attribute :lisp-syntax #\!) :constituent)
(setf (character-attribute :lisp-syntax #\{) :constituent)
(setf (character-attribute :lisp-syntax #\}) :constituent)
(setf (character-attribute :lisp-syntax #\[) :constituent)
(setf (character-attribute :lisp-syntax #\]) :constituent)
(setf (character-attribute :lisp-syntax #\/) :constituent)
(setf (character-attribute :lisp-syntax #\@) :constituent)
(setf (character-attribute :lisp-syntax #\-) :constituent)
(setf (character-attribute :lisp-syntax #\_) :constituent)
(setf (character-attribute :lisp-syntax #\+) :constituent)
(setf (character-attribute :lisp-syntax #\%) :constituent)
(setf (character-attribute :lisp-syntax #\*) :constituent)
(setf (character-attribute :lisp-syntax #\$) :constituent)
(setf (character-attribute :lisp-syntax #\^) :constituent)
(setf (character-attribute :lisp-syntax #\&) :constituent)
(setf (character-attribute :lisp-syntax #\~) :constituent)
(setf (character-attribute :lisp-syntax #\=) :constituent)
(setf (character-attribute :lisp-syntax #\<) :constituent)
(setf (character-attribute :lisp-syntax #\>) :constituent)
(setf (character-attribute :lisp-syntax #\?) :constituent)
(setf (character-attribute :lisp-syntax #\.) :constituent)
(setf (character-attribute :lisp-syntax #\:) :constituent)


(defattribute "Sentence Terminator"
  "Used for terminating sentences -- ., !, ?.
   Possibly could make type (mod 3) and use the value of 2 and 1 for spaces
   to place after chacter."
  '(mod 2)
  0)

(setf (character-attribute :sentence-terminator #\.) 1)
(setf (character-attribute :sentence-terminator #\!) 1)
(setf (character-attribute :sentence-terminator #\?) 1)
