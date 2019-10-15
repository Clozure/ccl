;;; Tue Dec 25 19:59:50 1990 by Mark Kantrowitz <mkant@GLINDA.OZ.CS.CMU.EDU>
;;; source-compare.lisp

;;; ****************************************************************
;;; Source Compare: A 'diff' Program for Lisp **********************
;;; ****************************************************************
;;; 
;;; Source Compare is a common-lisp portable tool for comparing 
;;; lisp source files, similar to the unix program 'diff'. Like diff
;;; it can ignore case, whitespace, and blank lines. In addition,
;;; it can also ignore certain classes of lisp comments. It runs in
;;; average-case O(m+n) time.
;;;
;;; Written by Mark Kantrowitz, December 1990.
;;; Address:   School of Computer Science
;;;            Carnegie Mellon University
;;;            Pittsburgh, PA 15213
;;;
;;; Copyright (c) 1990. All rights reserved.
;;;
;;; See general license below.
;;;

;;; ****************************************************************
;;; General License Agreement and Lack of Warranty *****************
;;; ****************************************************************
;;;
;;; This software is distributed in the hope that it will be useful (both
;;; in and of itself and as an example of lisp programming), but WITHOUT
;;; ANY WARRANTY. The author(s) do not accept responsibility to anyone for
;;; the consequences of using it or for whether it serves any particular
;;; purpose or works at all. No warranty is made about the software or its
;;; performance. 
;;; 
;;; Use and copying of this software and the preparation of derivative
;;; works based on this software are permitted, so long as the following
;;; conditions are met:
;;; 	o  The copyright notice and this entire notice are included intact
;;; 	   and prominently carried on all copies and supporting documentation.
;;; 	o  No fees or compensation are charged for use, copies, or
;;; 	   access to this software. You may charge a nominal
;;; 	   distribution fee for the physical act of transferring a
;;; 	   copy, but you may not charge for the program itself. 
;;; 	o  If you modify this software, you must cause the modified
;;; 	   file(s) to carry prominent notices (a Change Log)
;;; 	   describing the changes, who made the changes, and the date
;;; 	   of those changes.
;;; 	o  Any work distributed or published that in whole or in part
;;; 	   contains or is a derivative of this software or any part 
;;; 	   thereof is subject to the terms of this agreement. The 
;;; 	   aggregation of another unrelated program with this software
;;; 	   or its derivative on a volume of storage or distribution
;;; 	   medium does not bring the other program under the scope
;;; 	   of these terms.
;;; 	o  Permission is granted to manufacturers and distributors of
;;; 	   lisp compilers and interpreters to include this software
;;; 	   with their distribution. 
;;; 
;;; This software is made available AS IS, and is distributed without 
;;; warranty of any kind, either expressed or implied.
;;; 
;;; In no event will the author(s) or their institutions be liable to you
;;; for damages, including lost profits, lost monies, or other special,
;;; incidental or consequential damages arising out of or in connection
;;; with the use or inability to use (including but not limited to loss of
;;; data or data being rendered inaccurate or losses sustained by third
;;; parties or a failure of the program to operate as documented) the 
;;; program, even if you have been advised of the possibility of such
;;; damanges, or for any claim by any other party, whether in an action of
;;; contract, negligence, or other tortious action.
;;; 
;;; The current version of this software and a variety of related
;;; utilities may be obtained by anonymous ftp from a.gp.cs.cmu.edu
;;; (128.2.242.7) or any other CS machine in the directory 
;;;       /afs/cs.cmu.edu/user/mkant/Public/Lisp-Utilities/
;;; You must cd to this directory in one fell swoop, as the CMU
;;; security mechanisms prevent access to other directories from an
;;; anonymous ftp. For users accessing the directory via an anonymous
;;; ftp mail server, the file README contains a current listing and
;;; description of the files in the directory. The file UPDATES describes
;;; recent updates to the released versions of the software in the directory.
;;; The file COPYING contains the current copy of this license agreement.
;;; Of course, if your site runs the Andrew File System and you have
;;; afs access, you can just cd to the directory and copy the files directly.
;;; 
;;; Please send bug reports, comments, questions and suggestions to
;;; mkant@cs.cmu.edu. We would also appreciate receiving any changes
;;; or improvements you may make. 
;;; 
;;; If you wish to be added to the CL-Utilities@cs.cmu.edu mailing list, 
;;; send email to CL-Utilities-Request@cs.cmu.edu with your name, email
;;; address, and affiliation. This mailing list is primarily for
;;; notification about major updates, bug fixes, and additions to the lisp
;;; utilities collection. The mailing list is intended to have low traffic.
;;;

;;; ********************************
;;; Change Log *********************
;;; ********************************
;;;
;;;  16-DEC-90  mk   File created.
;;;  25-DEC-90  mk   First released version.
;;;  24-JAN-91  mk   Added average-case running time analysis.

;;; ********************************
;;; To Do **************************
;;; ********************************
;;;
;;; Extend so it can ignore documentation strings?
;;;
;;; Extend so it can ignore ALL whitespace (even within line)?
;;;
;;; Cache start and end positions for each line? [Modify line-start and
;;; line-end to first check the cache before redoing the computation.]
;;;    run the profiler on this code first, though.
;;;
;;; The file cache could be flushed after each loop in so-co. Possibly
;;; worth doing to save space and/or reduce consing.
;;;
;;; Implement diff's O(p log n) algorithm using 2-3 trees and k-candidates.
;;;
;;; Given that in diff's find-merge-split algorithm the merge is on two
;;; sets, one with elements less than the others, can we find a way to
;;; do the find-find-split-merge in constant time? At least keep a table
;;; of whether r-1,r are in the same k-candidate set. 
;;;
;;; Fancy indexing, div&conq, straight-line dist to TR corner metric.
;;; Hierarchical LCS (i.e., abstract level first)?
;;;
;;; Make it aware of which definition it is in to aid matching. (Problem,
;;; then, of function and variable renaming.)
;;;

;;; ********************************
;;; Notes **************************
;;; ********************************
;;;
;;;    SOURCE-COMPARE has been tested (successfully) in the following lisps:
;;;       CMU Common Lisp (M2.9 15-Aug-90, Compiler M1.8 15-Aug-90)
;;;       Macintosh Allegro Common Lisp (1.3.2)
;;;       ExCL (Franz Allegro CL 3.1.12 [DEC 3100] 3/30/90)
;;;       Lucid CL (Version 2.1 6-DEC-87)
;;;
;;;    SOURCE-COMPARE needs to be tested in the following lisps:
;;;       Symbolics Common Lisp (8.0)
;;;       Lucid Common Lisp (3.0, 4.0)
;;;       KCL (June 3, 1987 or later)
;;;       AKCL (1.86, June 30, 1987 or later)
;;;       TI (Release 4.1 or later)
;;;       Ibuki Common Lisp (01/01, October 15, 1987)
;;;       Golden Common Lisp (3.1 IBM-PC)
;;;       VAXLisp (2.0, 3.1)
;;;       HP Common Lisp (same as Lucid?)
;;;       Procyon Common Lisp

;;; ****************************************************************
;;; Documentation **************************************************
;;; ****************************************************************
;;;
;;; Source Compare is a common-lisp portable tool for comparing 
;;; lisp source files, similar to the unix program 'diff'. 
;;;
;;; It uses a greedy variation of the usual dynamic programming 
;;; implementation of LCS (longest common substring) to do the 
;;; comparison. It tries to maintain the two files being compared
;;; in sync, and when a difference is encountered, uses the closest
;;; next match, where distance is minimized against some metric.
;;; Note that since this is a greedy algorithm, it is possible that it
;;; will not find the optimum global match sequence. However, this case
;;; hardly ever occurs in practice, and if it does ever occur, the
;;; algorithm errs on the side of safety.
;;;
;;; Metrics should be chosen so that minimizing distance is
;;; equivalent to minimizing the edits necessary to bring the two
;;; files into agreement. Two such metrics include
;;;     x + y    = *total* additions and deletions from both files
;;;     max(x,y) = length of largest addition or deletion from either file
;;; where x is a line number from the first file and y is a line number
;;; from the second file. Both of these metrics are appropriate to the
;;; problem, since the former tries to minimize the total changes and
;;; the latter gives a preference to small changes.
;;; 
;;; While neither metric actually builds the dynamic programming table,
;;; they can be considered as exploring the table in successive
;;; rectilinear and diagonal manners, respectively.
;;;                        #####          #
;;;                            #           #
;;;                            #            #
;;;                            #             #
;;; Both metrics have been implemented.
;;;
;;; Both of these metrics lead to a worst-case O(n^2) algorithm
;;; (actually, O(nm), where n is the length of the first file and
;;; m is the length of the second file). In practice, however, the
;;; algorithm seems to have linear running time. This could be a function
;;; of its use to compare source files, since comparing two completely
;;; different files would lead to worst-case behavior. The diagonal
;;; metric seems to be slightly faster and less of a space hog than
;;; the rectilinear metric, so it has been made the default.
;;; 
;;; We show below that the average case running time is O(n+m).
;;;

;;; ********************************
;;; Average Case Analysis **********
;;; ********************************
;;;
;;; Thanks to Neil J Calkin (CMU Math Department) for the idea that led to
;;; this proof.
;;;
;;; Let a(i) and b(i) be the ith distances between matches in files A and B,
;;; respectively. Let k, an integer between 1 and n (inclusive), be the
;;; number of matches. Then
;;;	  Sum[i = 1 to k; a(i)] = m and Sum[i = 1 to k; b(i)] = n
;;; where m is the length in lines of file A, and n is the corresponding
;;; length for file B. The running time of the algorithm is proportional
;;; to Sum[i = 1 to k; a(i)b(i)].
;;;
;;; Since a(i) and b(i) are positive integers, it follows that
;;;	    Sum[i; a(i)b(i)] <= Sum[i; a(i)] Sum[i; b(i)] = m n
;;; and hence the worst-case running time is O(mn). [Best case is, of course,
;;; linear.]
;;;
;;; But the worst-case running time is atypical of the average-case behavior.
;;; As we shall show, the average-case running time is O(m+n).
;;;
;;; Combining the Cauchy-Schwartz inequality
;;;	 Sum[i;a(i)b(i)] <= Sqrt{Sum[i;a(i)^2]} sqrt{Sum[i;b(i)^2]}
;;; with the arithmetic-mean geometric-mean inequality
;;;			    Sqrt{xy} <= (x+y)/2
;;; yields
;;;	    Sum[i;a(i)b(i)] <= (Sum[i;a(i)^2] + Sum[i;b(i)^2])/2
;;;
;;; So it suffices to look at the average value of Sum[i = 1 to k; r(i)^2]
;;; over all possible ordered sequences r(i) of positive integers for k = 1
;;; to n such that Sum[i = 1 to k; r(i)] = n. Such a sequence r(k) is called a
;;; composition of n into k distinct parts. (The word distinct here
;;; signifies that permutations of a sequence r(k) are not considered
;;; identical -- the cells are distinct.)
;;;
;;; To compute this average we sum Sum[i = 1 to k; r(i)^2] over all possible
;;; compositions of n, and divide by the total number of such
;;; compositions.
;;;
;;; Clearly r(i) is an integer between 1 and n (inclusive). For a given
;;; integer i between 1 and n (inclusive), we count how often i occurs
;;; in a composition of n into k distinct parts. Call this count
;;; Comp[n,k,i]. Then the sum is equal to 
;;;	    Sum[k = 1 to n; Sum[i = 1 to n; Comp[n,k,i] i^2]]
;;;
;;; Now the number of occurrences of i in the compositions of n into k
;;; distinct parts is the same as multiplying together the number of
;;; compositions of n-i into k-1 parts and the number of positions in
;;; which i could be inserted to form a k-part composition of n. The 
;;; latter quantity is clearly k. To see that the former is 
;;; C(n-i-1,k-2), consider n-i ones separated by (n-i)-1 spaces, and
;;; choose (k-1)-1 of them to form k-1 integers. Thus Comp[n,k,i] is
;;; k C(n-i-1,k-2).
;;; 
;;; So our sum is equal to
;;;    Sum[k = 1 to n; Sum[i = 1 to n; k(i^2)C(n-i-1,k-2)]]
;;;	  = Sum[i = 1 to n; i^2 Sum[k = 1 to n; kC(n-i-1,k-2)]]
;;;	  = Sum[i = 1 to n; i^2 Sum[k = 1 to n; 
;;;                                 (k-2)C(n-i-1,k-2) + 2C(n-i-1,k-2)]]
;;;	  = Sum[i = 1 to n; i^2 Sum[k = 1 to n; 
;;;                                 (n-i-1)C(n-i-2,k-3) + 2C(n-i-1,k-2)]]
;;;	  = Sum[i = 1 to n-2; i^2 (n-i-1) 2^(n-i-2)]
;;;         + Sum[i = 1 to n; i^2 2^(n-i)]
;;; Substituting j = n-i+1 yields
;;;       = Sum[j = 3 to n; (n+1-j)^2 (j-2) 2^(j-3)] 
;;;         + Sum[j = 1 to n; (n+1-j)^2 2^(j-1)]
;;;       = Sum[j = 1 to n-2; (n-1-j)^2 j 2^(j-1)] 
;;;         + Sum[j = 1 to n; (n+1-j)^2 2^(j-1)]
;;;       = (Sum[j = 1 to n-2; 2^j (j^3 - (2n-2) j^2 + (n-1)^2 j)]
;;;          + Sum[j = 1 to n; 2^j (j^2 - (2n+2)j + (n+1)^2)])/2
;;; We substitute using the identities
;;;    Sum[j = 1 to n; 2^j]     = 2^(n+1) - 2
;;;    Sum[j = 1 to n; j 2^j]   = (n-1)2^(n+1) + 2
;;;    Sum[j = 1 to n; j^2 2^j] = (2n^2 - 4n + 6)2^n - 6
;;;    Sum[j = 1 to n; j^3 2^j] = (2n^3 - 6n^2 + 18n - 26)2^n + 26
;;; to obtain
;;;    1/2[2^(n-1)((n-2)^3 - 3(n-2)^2 + 9(n-2) - 13 
;;;                - 2(n-1)((n-2)^2 - 2(n-2) + 3)
;;;                + (n-1)^2(n-3))
;;;        2^n(2n^2 - 4n + 6
;;;            - (2n+2)(2n-2)
;;;            + (n+1)^2(2))
;;;        + (26 + 6(2n-2) + 2(n-1)^2 - 6 - 2(2n+2) - 2(n+1)^2)]
;;; Luckily the n^3 and n^2 terms cancel, simplifying the expression to
;;;    (3n-4) 2^(n-1) + 2
;;; This closed form expression has been empirically verified for n = 1 to 15.
;;;
;;; Since there are C(n-1,k-1) compositions of n into k distinct parts, the
;;; total number of compositions is Sum[k = 1 to n; C(n-1,k-1)] = 2^(n-1)
;;; by the binomial theorem.
;;;
;;; Thus the average value of Sum[n,k; r(i)^2] is the total sum divided by
;;; the total count, or
;;;	     [(3n-4) 2^(n-1) + 2]/[2^(n-1)] = 3n-4 + 1/2^(n-2)
;;; So Sum[i; a(i)b(i)] <= (Sum[i;a(i)^2] + Sum[i;b(i)^2])/2
;;;                      = (3n-4 + 1/2^(n-2) + 3m-4 + 1/2^(m-2))/2
;;;                      = 3/2(n+m) - 4 + 1/2^(n-1) + 1/2^(m-1)
;;; on average, and hence the average case running time is O(m+n).
;;;

;;; ********************************
;;; User Guide *********************
;;; ********************************
;;;
;;; SOURCE-COMPARE (filename-1 filename-2 &key                    [FUNCTION]
;;;                 (output-stream *standard-output*)
;;;                 (ignore-case *ignore-case*)
;;;                 (ignore-whitespace *ignore-whitespace*)
;;;                 (ignore-comments *ignore-comments*)
;;;                 (ignore-blank-lines *ignore-blank-lines*)
;;;                 (print-context *print-context*)
;;;                 (print-fancy-header *print-fancy-header*))
;;;    Compares the contents of the two files, outputting a report of what
;;;    lines must be changed to bring the files into agreement. The report
;;;    is similar to that generated by 'diff': Lines of the forms
;;;    n1 a n3,n4
;;;    n1,n2 d n3
;;;    n1,n2 c n3,n4
;;;    (where a is ADD, d is DELETE, and c is CHANGE) are followed by the
;;;    lines affected in the first (left) file flagged by '<' then all
;;;    the lines affected in the second (right) file flagged by '>'. If
;;;    PRINT-CONTEXT is T, will print out some additional contextual
;;;    information, such as additional lines before and after the affected
;;;    text and the definition most likely to be affected by the changes. If
;;;    PRINT-FANCY-HEADER is T, prints the file-author and file-write-date
;;;    in the header. The report is output to OUTPUT-STREAM. Returns T if
;;;    the files were "identical", NIL otherwise.
;;;    If IGNORE-CASE is T, uses a case insensitive comparison.
;;;    If IGNORE-WHITESPACE is T, ignores spaces and tabs that occur at
;;;    the beginning of the line. If IGNORE-COMMENTS is T, tries to ignore
;;;    comments at the end of the line. If *dont-ignore-major-comments*
;;;    is T, will also ignore major comments (comments with a semicolon at
;;;    char 0 of the line). If IGNORE-BLANK-LINES is T, will ignore blank
;;;    lines in both files, including lines that are effectively blank
;;;    because of ignored comments.
;;;
;;; *GREEDY-METRIC* (quote find-next-diagonal-match)              [VARIABLE]
;;;    Variable containing the name of the greedy matching function used
;;;    to minimize distance to the next match:
;;;    find-next-rectilinear-match  minimizes  max(x,y)
;;;    find-next-diagonal-match     minimizes  x+y
;;;    where x is a line number from the first file and y is a line
;;;    number from the second file.
;;;
;;; FIND-NEXT-DIAGONAL-MATCH (file-1 start-1 file-2 start-2)      [FUNCTION]
;;;    Difference detected, look ahead for a match [x+y version].
;;;
;;; FIND-NEXT-RECTILINEAR-MATCH (file-1 start-1 file-2 start-2)   [FUNCTION]
;;;    Difference detected, look ahead for a match [max(x,y) version].
;;;
;;;
;;; *** Display Parameters ***
;;;
;;; *PRINT-CONTEXT* t                                             [VARIABLE]
;;;    If T, we print the context marking lines that occur before the
;;;    difference.
;;;
;;; *PRINT-FANCY-HEADER* ()                                       [VARIABLE]
;;;    If T, prints a fancy header instead of the simple one.
;;;
;;; *CONTEXT-LINES-BEFORE-DIFFERENCE* 0                                     [VARIABLE]
;;;    Number of lines to print before a difference.
;;;
;;; *CONTEXT-LINES-AFTER-DIFFERENCE* 1                                      [VARIABLE]
;;;    Number of lines to print after a difference.
;;;
;;;
;;; *** Program Default Parameters ***
;;;
;;; *MINIMUM-MATCH-LENGTH* 2                                     [VARIABLE]
;;;    The minimum number of lines that must match for it to be considered
;;;    a match. This has the effect of collecting lots of adjacent small
;;;    differences together into one larger difference.
;;;
;;; *IGNORE-WHITESPACE* t                                         [VARIABLE]
;;;    If T, will ignore spaces and tabs that occur at the beginning of the
;;;    line before other text appears and at the end of the line after
;;;    the last text has appeared.
;;;
;;; *IGNORE-CASE* t                                               [VARIABLE]
;;;    If T, uses a case insensitive comparison. Otherwise uses a case
;;;    sensitive comparison.
;;;
;;; *IGNORE-COMMENTS* t                                           [VARIABLE]
;;;    If T, will try to ignore comments of the semicolon variety when
;;;    comparing lines. Tries to be rather intelligent about the context
;;;    to avoid ignoring something that really isn't a comment. For
;;;    example, semicolons appearing within strings, even multi-line
;;;    strings, are not considered comment characters. Uses the following
;;;    heuristics to decide if a semicolon is a comment character or not:
;;;    o  Slashification (\) works inside strings ("foo\"bar")
;;;       and symbol names (|foo\|bar|), but not balanced comments
;;;       (#|foobar\|#).
;;;    o  Balanced comments do not work inside strings ("#|") or
;;;       symbol names.
;;;    o  Strings do not work inside balanced comments (#|"|#)
;;;    o  Regular semicolon comments do not work inside strings,
;;;       symbol names, or balanced comments (#|foo;bar|#).
;;;    All this is necessary for it to correctly identify when a
;;;    semicolon indicates the beginning of a comment. Conceivably we should
;;;    consider a semicolon as a comment when it is inside a balanced
;;;    comment which isn't terminated from the semicolon to the end of the
;;;    line. However, besides being complicated and time-consuming to
;;;    implement, the lisp interpreter doesn't treat it this way, and we
;;;    like to err on the side of caution. Anyway, changes in the comments
;;;    within commented out regions of code is worth knowing about.
;;;
;;; *DONT-IGNORE-MAJOR-COMMENTS* ()                               [VARIABLE]
;;;    If T, ignoring comments does not ignore comments with a semicolon
;;;    at position 0 of the line.
;;;
;;; *IGNORE-BLANK-LINES* t                                        [VARIABLE]
;;;    If T, will ignore blank lines when doing the comparison.
;;;

;;; ****************************************************************
;;; Source Compare *************************************************
;;; ****************************************************************

(defPackage "SOURCE-COMPARE" (:nicknames "SCP") (:use :cl :ccl))
(in-package "SOURCE-COMPARE")

(export '(source-compare		; main routine
	  ;; Core function parameters used to keep files in sync.
	  *greedy-metric*		
	  *minimum-match-length*
	  ;; Program default display.
	  *print-context*		
	  *print-fancy-header*
	  *context-lines-before-difference*
	  *context-lines-after-difference*
	  ;; Program default modes.
	  *ignore-comments*
	  *dont-ignore-major-comments*
	  *ignore-case*
	  *ignore-whitespace*
	  *ignore-blank-lines*))

;;; ********************************
;;; Global Vars ********************
;;; ********************************
(defVar *print-context* t ;nil
  "If T, we print the context marking lines that occur before the difference.")
(defVar *print-fancy-header* nil ;t
  "If T, prints a fancy header instead of the simple one.")
(defVar *context-lines-before-difference* 0
  "Number of lines to print before a difference.")
(defVar *context-lines-after-difference* 1 
  "Number of lines to print after a difference.")

(defVar *greedy-metric* 'find-next-diagonal-match
  "Variable containing the name of the greedy matching function used
   to minimize distance to the next match:
      find-next-rectilinear-match  minimizes  max(x,y)
      find-next-diagonal-match     minimizes  x+y
   where x is a line number from the first file and y is a line number
   from the second file.")

(defVar *minimum-match-length* 2
  "The minimum number of lines that must match for it to be considered
   a match. This has the effect of collecting lots of adjacent small
   differences together into one larger difference.")

(defVar *ignore-whitespace* t
  "If T, will ignore spaces and tabs that occur at the beginning of the
   line before other text appears and at the end of the line after
   the last text has appeared.")
(defVar *ignore-case* t
  "If T, uses a case insensitive comparison. Otherwise uses a case
   sensitive comparison.")
(defVar *ignore-comments* t
  "If T, will try to ignore comments of the semicolon variety when
   comparing lines. Tries to be rather intelligent about the context
   to avoid ignoring something that really isn't a comment. For example, 
   semicolons appearing within strings, even multi-line strings, are not
   considered comment characters. Uses the following heuristics to decide
   if a semicolon is a comment character or not:
       o  Slashification (\\) works inside strings (\"foo\\\"bar\")
          and symbol names (\|foo\\\|bar\|), but not balanced comments
          (#|foobar\\|#).
       o  Balanced comments do not work inside strings (\"\#\|\") or
          symbol names.
       o  Strings do not work inside balanced comments (#|\"|#)
       o  Regular semicolon comments do not work inside strings, symbol
          names, or balanced comments (#|foo;bar|#).
   All this is necessary for it to correctly identify when a semicolon
   indicates the beginning of a comment. Conceivably we should consider
   a semicolon as a comment when it is inside a balanced comment which
   isn't terminated from the semicolon to the end of the line. However,
   besides being complicated and time-consuming to implement, the lisp
   interpreter doesn't treat it this way, and we like to err on the side
   of caution. Anyway, changes in the comments within commented out
   regions of code is worth knowing about.")
(defVar *dont-ignore-major-comments* nil ;t
  "If T, ignoring comments does not ignore comments with a semicolon
   at position 0 of the line.")
(defVar *ignore-blank-lines* t
  "If T, will ignore blank lines when doing the comparison.")

;;; ********************************
;;; File Cache *********************
;;; ********************************

;;; File-cache is a defstruct used to cache the lines of the file as
;;; they are read.
(defStruct (FILE-CACHE (:print-function
			(lambda (o s d)
			  (declare (ignore d))
			  (format s "#<file-cache: ~a ~d ~a>"
				  (file-cache-file-name o)
				  (file-cache-length o)
				  (file-cache-eof o)))))
  ;; LINE-TABLE is a cache of the lines of the file read so far.
  ;; INSIDE-STRING-TABLE is a table of flags which indicate whether the line
  ;; terminates while still inside a string. If so, this table indicates
  ;; what character will close the string. This is useful for parsing
  ;; multi-line strings.
  ;; BALANCED-COMMENT-COUNT-TABLE is a table of flags which indicate whether
  ;; the line terminates while still inside a balanced comment, and if so,
  ;; how many are left to be closed. This is useful for parsing multi-line
  ;; balanced comments. 
  ;; FILE-NAME is the name of the file.
  ;; FILE-STREAM is the input stream open to the file.
  ;; EOF is a flag which is true when the end of the file has been reached.
  ;; If so, it is one more than the last valid line number.
  (line-table (make-array (list 100.) 
			  :element-type t :fill-pointer 0 :adjustable t)) 
  (inside-string-table (make-array (list 100.) 
				   :element-type t
				   :initial-element nil
				   :fill-pointer 0 :adjustable t))
  (balanced-comment-count-table (make-array (list 100.) 
					    :element-type t
					    :initial-element 0
					    :fill-pointer 0 :adjustable t))
  file-name
  file-stream				
  (eof nil))

(defun file-cache-length (file)
  "The number of lines cached is simply the length of the line table.
   Note that since this table has a fill-pointer, it's length is the 
   size indicated by the fill-pointer, not the array dimensions."
  (length (file-cache-line-table file)))

(defun cached-line (file line-no)
  "Returns a cached line from the line cache, if it exists."
  (when (< line-no (file-cache-length file))
    (aref (file-cache-line-table file) line-no)))

(defun cached-comment-position-info (file line-no)
  "Returns the cached comment position (inside-string and 
   balanced-comment-count) information for the line, if it exists."
  (if (< line-no (file-cache-length file))
      (values (aref (file-cache-inside-string-table file) line-no)
	      (aref (file-cache-balanced-comment-count-table file) line-no))
    (values nil 0)))
(defun set-cached-comment-position-info (file line-no inside-string
					      balanced-comment-count)
  "Sets the cached comment position information (inside-string and
   balanced-comment-count) for the line."
  ;; We assume that get-and-cache-next-line has ensured that the
  ;; flag tables are the right length -- otherwise we're hosed.
  ;; Why doesn't CL have a defsetf with multiple values? That would
  ;; make life here so much easier. [Done 12-24-90 MK. Not installing
  ;; here to avoid clashes with other Lisps.]
  (setf (aref (file-cache-inside-string-table file) line-no) 
	inside-string)
  (setf (aref (file-cache-balanced-comment-count-table file) line-no) 
	balanced-comment-count))

(defun get-and-cache-next-line (file)
  "Gets the next line from the file, installing it in the cache."
  (let ((line (read-line (file-cache-file-stream file) nil nil)))
    (if line
	;; If there's a line, add it to the cache.
	(progn
	  (vector-push-extend line (file-cache-line-table file))
	  (vector-push-extend nil (file-cache-inside-string-table file))
	  (vector-push-extend 
	   0 (file-cache-balanced-comment-count-table file)))
      ;; If the line was null, we've reached the end of the file.
      ;; Set the eof flag to be the line number of the end of file.
      (setf (file-cache-eof file) (file-cache-length file)))
    ;; Return the line.
    line))

(defun get-line (file line-no)
  "Get the line from the file cache. If not present, get it from the stream."
  (or (cached-line file line-no)
      (when (not (file-cache-eof file))
	(get-and-cache-next-line file))))

(defMacro with-open-file-cached ((var filename &rest open-args) &body forms)
  (let ((abortp (gensym "ABORTP"))
	(stream (gensym (symbol-name var))))
    `(let* ((,stream (open ,filename ,@open-args))
	    (,var (make-file-cache :file-stream ,stream :file-name ,filename))
	    (,abortp t))
       (when ,var
         (unwind-protect
             (multiple-value-prog1
                 (progn ,@forms)
               (setq ,abortp nil))
           (close ,stream :abort ,abortp))))))

;;; ********************************
;;; Line Comparison ****************
;;; ********************************
(defun first-non-whitespace-char (line &key from-end (start 0) end)
  "Finds the position of the first character of LINE which is neither
   a space or a tab. Returns NIL if no character found."
  (position '(#\space #\tab) line
	    :test-not #'(lambda (set char)
			  (find char set :test #'char=))
	    :from-end from-end
	    :start start :end end))

(defun line-start (line &optional (start 0))
  "Returns the position of where in LINE to start the comparison."
  (if *ignore-whitespace*
      (or (first-non-whitespace-char line) start)
    start))

(defVar *slash-char* #\\
  "The character used to slashify other characters.")
(defVar *comment-char* #\;
  "The character used to begin comments.")
(defVar *string-quotes-char* #\"
  "The character used to begin and end strings.")
(defVar *string-bar-char* #\|
  "The character used to begin and end symbols.")
(defVar *splat-char* #\#
  "One of the characters used to begin balanced comments.")
(defVar *bar-char* #\|
  "One of the characters used to begin balanced comments.")

(defun find-comment-position (line &optional (start 0) end 
				   &key inside-string (splat-bar-count 0))
  "Tries to find the position of the beginning of the comment at the
   end of LINE, if there is one. START and END delimit the search. END
   defaults to the end of the line. If INSIDE-STRING is non-nil, it is
   assumed that we're inside a string before we began (if so, INSIDE-STRING
   is set to the character which will terminate the string (\#\\\" or \#\\\|).
   SPLAT-BAR-COUNT is the number of unbalanced begin balanced comments
   (\#\|'s) that have been seen so far."
  (unless end (setf end (length line)))
  (if (< start (length line))
    (do ((position start (1+ position))
	 (last-char-was-slash nil)
	 (inside-string inside-string)
	 (splat-bar-count splat-bar-count)
	 (splat-flag nil)(bar-flag nil))
	((= position end)
	 ;; If we run off the end, return nil to signify 
	 ;; that nothing was found.
	 (values nil inside-string splat-bar-count))
      (let ((char (char line position)))
	;; Slashification works inside strings but not balanced comments.
	;; Balanced comments do not work inside strings. 
	;; Strings do not work inside balanced comments.
	;; Regular comments do not work inside strings or balanced comments
	(cond (last-char-was-slash 
	       ;; If the last character was a slash, throw this one away
	       ;; and reset the flag.
	       (setf last-char-was-slash nil))
	      ((and (zerop splat-bar-count) (char= char *slash-char*))
	       ;; This is an unslashed slash occurring outside balanced
	       ;; comments, so set the slash flag.
	       (setf last-char-was-slash t))
	      ((and (not inside-string)(char= char *splat-char*))
	       ;; We saw a SPLAT which could begin/end a balanced comment.
	       (cond (bar-flag
		      ;; This is the second char of an end balanced comment.
		      (when (plusp splat-bar-count)
			;; If we see an extra end balanced comment
			;; (splat-bar-count is zero), ignore it.
			(decf splat-bar-count))
		      (setf bar-flag nil))
		     ((not bar-flag)
		      ;; This is the first char of a begin balanced comment.
		      (setf splat-flag t))))
	      ((and (not inside-string) splat-flag (char= char *bar-char*))
	       ;; We saw a BAR which could begin a balanced comment.
	       ;; This is the second char of a begin balanced comment.
	       (incf splat-bar-count)
	       (setf splat-flag nil))
	      ((and (not inside-string) (not splat-flag)
		    (plusp splat-bar-count) (char= char *bar-char*))
	       ;; We saw a BAR which could end a balanced comment.
	       ;; This is the first char of an end balanced comment.
	       (setf bar-flag t))
	      ((and (zerop splat-bar-count)
		    inside-string
		    (char= char inside-string))
	       ;; This is an unslashed end string or end symbol occurring
	       ;; outside balanced comments. So reset inside-string to nil.
	       (setf inside-string nil))
	      ((and (zerop splat-bar-count)
		    (null inside-string)
		    (or (char= char *string-quotes-char*)
			(char= char *string-bar-char*)))
	       ;; This is an unslashed start string or start symbol occurring
	       ;; outside balanced comments. So set inside-string to the
	       ;; character which will end the string or symbol.
	       (setf inside-string char))
	      ((and (zerop splat-bar-count) (not inside-string)
		    (char= char *comment-char*))
	       ;; We're not slashified or inside a string or balanced comment
	       ;; and we're a comment char, so we must begin a comment.
	       (return (values position nil 0)))
	      ((or bar-flag splat-flag)
	       ;; We last saw a BAR or SPLAT, but some other unimportant
	       ;; character was seen, so reset the flags.
	       (setf splat-flag nil
		     bar-flag nil)))))
    (values nil nil 0)))

;;; To see GNU-Emacs (and some lesser imitations) die miserably, put the
;;; cursor before the # on the next line, and try doing C-M-f or C-M-e. Ha!
#|
;;; Test find-comment-position on the various combinations of
;;; #| |#, ;, "foo", |foo|, and \. Note that this commented out
;;; region of this source file will itself serve as a good test
;;; when source-compare is run on this file! 
(find-comment-position "#| ; |# ;")
(find-comment-position "\" ; \" ;")
(find-comment-position "| ; | ;")
(find-comment-position "#\| ; | ;")
(find-comment-position "#\\| ; | ;")
(find-comment-position "| ; #\| ;")
(find-comment-position "| ; #\| \" ;")
|#

(defun get-comment-position (line file line-no &optional (start 0) end)
  "Returns the position of the beginning of the semicolon variety comment
   on this line."
  ;; Get the cached position info for the previous line. 
  (multiple-value-bind (inside-string balanced-comment-count)
      (if (zerop line-no)
	  ;; Default for first line of the file.
	  (values nil 0)
	(cached-comment-position-info file (1- line-no)))
    ;; Find the comment position for this line.
    (multiple-value-bind (end new-is new-bcc)
	(find-comment-position line start end 
			       :inside-string inside-string
			       :splat-bar-count balanced-comment-count)
      ;; Cache the position info for this line.
      (set-cached-comment-position-info file line-no new-is new-bcc)
      ;; Return the comment end.
      end)))

(defun line-end (line file line-no &optional (start 0) end)
  "Returns the position of where in LINE to end the comparison.
   If the comparison should end at the end of the line, returns NIL.
   START, if supplied, is where to start looking for the end."
  ;; Note that find-comment-position will return nil if it doesn't
  ;; find a comment, which is the default value of :end keywords
  ;; in the string comparison functions (signifying the end of the string).
  (let ((new-end (when *ignore-comments* 
		   (get-comment-position line file line-no start end))))
    (cond ((and *dont-ignore-major-comments*
		*ignore-comments*
		;; found a comment char and it's at the beginning of the line.
		new-end (zerop new-end))
	   ;; If we're not ignoring major comments (one's with the semicolon
	   ;; at char 0 of the line), return the end of the line.
	   (or end (length line)))
	  ((or *ignore-whitespace* *ignore-comments*)
	   ;; Ignoring comments means that we ignore the whitespace at the
	   ;; end of the line, no matter what we do at the beginning. Otherwise
	   ;; ignoring comments would have no affect.
	   (or (first-non-whitespace-char line :start start :end new-end
					  :from-end t)
	       new-end (length line)))
	  (t
	   new-end))))

(defun null-string (string &optional (start 0) end)
  "Returns T if STRING is the null string \"\" between START and END."
  (unless end (setf end (length string)))
  (string-equal string "" :start1 start :end1 end))

(defun compare-lines (file-1 line-no-1 file-2 line-no-2)
  "Intelligently compare two lines. If *ignore-case* is T, uses
   case-insensitive comparison. If *ignore-whitespace* is T, ignores
   spaces and tabs at the beginning of the line. If *ignore-comments* 
   is T, tries to ignore comments at the end of the line."
  (let ((string-1 (get-line file-1 line-no-1))
	(string-2 (get-line file-2 line-no-2)))
    (if (or (null string-1) (null string-2))
	;; If either of the lines is nil, both must be.
	(and (null string-1) (null string-2))
      ;; Both lines are non-nil, compare them!
      (let* ((start-1 (line-start string-1))
	     (start-2 (line-start string-2))
	     (end-1 (line-end string-1 file-1 line-no-1 start-1))
	     (end-2 (line-end string-2 file-2 line-no-2 start-2))
	     lines-same)
	(setf lines-same
	      (funcall (if *ignore-case* #'string-equal #'string=)
		       string-1 string-2
		       :start1 start-1 :start2 start-2
		       :end1 end-1 :end2 end-2))
	;; If lines-same is NIL, returns values: lines-same l1-null l2-null
	;; Otherwise returns just lines-same.
	(if *ignore-blank-lines*
	    (values lines-same
		    (null-string string-1 start-1 end-1)
		    (null-string string-2 start-2 end-2))
	  lines-same)))))

;;; ********************************
;;; Main Routine *******************
;;; ********************************
(defun source-compare (filename-1 filename-2
                                  &key (output-stream *standard-output*) 
                                  (ignore-case *ignore-case*)
				  (ignore-whitespace *ignore-whitespace*)
				  (ignore-comments *ignore-comments*)
				  (ignore-blank-lines *ignore-blank-lines*)
				  (print-context *print-context*)
				  (print-fancy-header *print-fancy-header*))
  "Compares the contents of the two files, outputting a report of what lines
   must be changed to bring the files into agreement. The report is similar
   to that generated by 'diff': Lines of the forms
      n1 a n3,n4
      n1,n2 d n3
      n1,n2 c n3,n4
   (where a is ADD, d is DELETE, and c is CHANGE) are followed by the
   lines affected in the first (left) file flagged by '<' then all the
   lines affected in the second (right) file flagged by '>'. If PRINT-CONTEXT
   is T, will print out some additional contextual information, such as 
   additional lines before and after the affected text and the definition
   most likely to be affected by the changes. If PRINT-FANCY-HEADER is T,
   prints the file-author and file-write-date in the header. The report is
   output to OUTPUT-STREAM. Returns T if the files were \"identical\",
   NIL otherwise.
   If IGNORE-CASE is T, uses a case insensitive comparison. 
   If IGNORE-WHITESPACE is T, ignores spaces and tabs that occur at the
   beginning of the line. If IGNORE-COMMENTS is T, tries to ignore
   comments at the end of the line. If *dont-ignore-major-comments* is T, will
   also ignore major comments (comments with a semicolon at char 0 of the
   line). If IGNORE-BLANK-LINES is T, will ignore blank lines in both
   files, including lines that are effectively blank because of ignored 
   comments."
  (with-open-file-cached (file-1 filename-1 :direction :input)
    (with-open-file-cached (file-2 filename-2 :direction :input)
      ;; Print the header.
      (draw-header filename-1 filename-2 
		   :stream output-stream 
		   :print-fancy-header print-fancy-header)
      ;; Do the actual comparisons.
      (let ((no-changes
	     (source-compare-internal file-1 file-2 :stream output-stream 
				      :ignore-case ignore-case
				      :ignore-whitespace ignore-whitespace
				      :ignore-comments ignore-comments
				      :ignore-blank-lines ignore-blank-lines
				      :print-context print-context)))
	;; Print the trailer.
	(format output-stream  "~&~:[Done.~;No differences found.~]~%"
		no-changes)
	no-changes))))

(defun source-compare-internal (file-1 file-2
				       &key (stream *standard-output*)
				       ignore-case ignore-whitespace
				       ignore-comments ignore-blank-lines
				       print-context)
  "A greedy implementation of LCS (longest common substring) suitably
   modified for source comparison. It is similar to the standard
   O(n^2) dynamic programming algorithm, but we don't actually keep
   distances or an explicit table. We assume that what has matched so
   far is a correct match. When we encounter a difference, we find the
   closest next match, where \"close\" is defined in terms of some
   metric. Two common metrics are max(x,y) and x+y, where x is a line number
   from file-2 and y is a line number from file-1. The former leads to 
   expanding (exploring) the table by increasing rectangles, and the
   latter by increasing triangles:
                     #####          #
                         #           #
                         #            #
                         #             #
   The average case running time of this algorithm is O(m+n), where m and n
   are the lengths of the two files. This seems to hold in practice. Worst
   case, of course, is still O(n^2), but this hardly ever occurs for source
   comparison. The metric is implemented by *greedy-metric*,
   which is either FIND-NEXT-RECTILINEAR-MATCH or FIND-NEXT-DIAGONAL-MATCH."
  (let ((*ignore-whitespace* ignore-whitespace) 
        (*ignore-case* ignore-case)
        (*ignore-comments* ignore-comments)
	(*ignore-blank-lines* ignore-blank-lines)
	(*print-context* print-context)
	(no-changes t))
    ;; Loop down both files, until a difference is encountered. Use
    ;; the function *greedy-metric* to find where they match up again,
    ;; print out the differences report for this divergence, and continue
    ;; from where they match.
    (do ((line-no-1 0 (1+ line-no-1))
	 (line-no-2 0 (1+ line-no-2)))
        ((and (file-cache-eof file-1) (>= line-no-1 (file-cache-eof file-1))
	      (file-cache-eof file-2) (>= line-no-2 (file-cache-eof file-2)))
	 ;; When we are at the end of both files, return whether the
	 ;; files are identical or not.
	 ;; use (eql (file-cache-eof file-1) (1- line-no-1)) here?
	 ;; need the 1- because of where the incrementing is happening. 
	 ;; could always have a 1+ in file-cache-eof.... 
	 ;; well, the >= is safer.
	 no-changes)
      (multiple-value-bind (lines-same line-1-blank line-2-blank)
	  (compare-lines file-1 line-no-1 file-2 line-no-2)
	(cond (lines-same
	       ;; The lines are the same. Do nothing.
	       nil)
	      ((and *ignore-blank-lines*
		    (or line-1-blank line-2-blank))
	       ;; The lines are different, but one is blank.
	       ;; Skip over the blank lines.
	       (cond ((and line-1-blank line-2-blank)
		      ;; Do nothing -- they'll be skipped automatically.
		      nil)
		     (line-1-blank
		      (decf line-no-2))
		     (line-2-blank
		      (decf line-no-1))))
	      (t
	       ;; Otherwise, a genuine difference has been encountered.
	       ;; A difference has been encountered.
	       (setq no-changes nil)
	       (multiple-value-bind (same-line-no-1 same-line-no-2)
		   ;; Find where they match up again.
		   (funcall *greedy-metric* file-1 line-no-1 file-2 line-no-2)
		 ;; Print the difference report
		 (print-differences file-1 line-no-1 same-line-no-1
				    file-2 line-no-2 same-line-no-2 stream)
		 ;; Continue from where they match.
		 (setq line-no-1 same-line-no-1
		       line-no-2 same-line-no-2))))))))

;;; ********************************
;;; The Metrics ********************
;;; ********************************

(defun find-next-diagonal-match (file-1 start-1 file-2 start-2)
  "First difference detected, look ahead for a match [x+y version]."
  (let ((sum 0)
	line-1 line-2
	eof-1 eof-2)
    ;; Starts sum (x+y) initially at zero, checks for a match on that
    ;; diagonal, and then tries the next diagonal by incrementing sum.
    (loop
     ;; Check for a diagonal match.
     (multiple-value-setq (line-1 line-2 eof-1 eof-2)
	 (find-diagonal-match sum file-1 start-1 eof-1 file-2 start-2 eof-2))
     ;; Have we found a match? If so, exit.
     (when (and line-1 line-2)
       (return (values line-1 line-2)))
     ;; Increment sum
     (incf sum))))

(defun find-diagonal-match (sum file-1 start-1 eof-1 file-2 start-2 eof-2)
  "Explores the diagonal with left-corner start-1 start-2 and index (x+y)
   equal to sum, searching for a match. Returns the match if found."
  ;; This starts at top left and works toward bottom right. This gives
  ;; a slight favoring to deletions from file-1.
  ;; For line-1 from (+ start-1 sum) downto start-1
  ;; and line-2 from start-2 upto (+ start-2 sum).
  ;; Need to ensure that the starts and ends aren't beyond the bounds
  ;; of the files, so check them against eof-1 and eof-2.
  (let ((init-1 (+ sum start-1))
	(init-2 start-2)
	(end-1 start-1)
	(end-2 (+ sum start-2)))
    ;; Ensure we have the current EOF line numbers.
    (unless (or eof-1 (get-line file-1 init-1))
      (setf eof-1 (file-cache-eof file-1)))
    (unless (or eof-2 (get-line file-2 end-2))
      (setf eof-2 (file-cache-eof file-2)))
    ;; Adjust start and end to fit EOF.
    (when (and eof-1 (> init-1 eof-1))
      (setf init-2 (- init-1 eof-1))
      (setf init-1 eof-1))
    (when (and eof-2 (> init-2 eof-2))
      (setf end-1 (- end-2 eof-2))
      (setf end-2 eof-2))
    ;; Check all the entries in the diagonal...
    (do ((line-1 init-1 (1- line-1))
	 (line-2 init-2 (1+ line-2)))
	((or (< line-1 end-1)
	     (> line-2 end-2))
	 ;; We've walked off the end of the graph.
	 (values nil nil eof-1 eof-2))
      ;; If we've found a match, return it. Note that if we've hit
      ;; EOF on both files, it will be considered a match.
      (when (found-match file-1 line-1 file-2 line-2)
	(return (values line-1 line-2 eof-1 eof-2))))))

(defun find-next-rectilinear-match (file-1 start-1 file-2 start-2)
  "First difference detected, look ahead for a match [max(x,y) version]."
  (let ((line-1 start-1) 
	(line-2 start-2)
        eof-1 eof-2)
    (loop
     (when (and eof-1 eof-2)
       (return (values line-1 line-2)))
     (when (not eof-1)
       ;; Check next line from first file against lines from second file.
       ;; Finds horizontal match.
       (incf line-1)
       (let ((match (find-linear-match file-2 start-2 line-2 file-1 line-1)))
	 (cond ((eq match :eof)
		(setq eof-1 :eof))
	       (match
		(return (values line-1 match))))))
     (when (not eof-2)
       ;; Check next line from second file against lines from first file.
       ;; Finds vertical match.
       (incf line-2)
       (let ((match (find-linear-match file-1 start-1 line-1 file-2 line-2)))
	 (cond ((eq match :eof)
		(setq eof-2 :eof))
	       (match
		(return (values match line-2)))))))))

(defun find-linear-match (file line-start line-end comp-file comp-line-no)
  "Proceeds linearly in file from line-start to line-end until it 
   finds a match against comp-line-no of comp-file."
  (do ((line-no line-start (1+ line-no)))
      ((> line-no line-end))
    (cond ((found-match file line-no comp-file comp-line-no)
	   ;; returns the match
	   (return line-no))
	  ((file-cache-eof comp-file)
	   (return :eof)))))

(defun found-match (file-1 line-1 file-2 line-2)
  "Check if we've found a match by verifying that the next few lines
   are identical. If *minimum-match-length* is more than 1, has the
   effect of grouping together differences separated only by one 
   matching line."
  ;; Note that this declares a match as early as possible, keeping
  ;; comments out of the match region. so-co then has to
  ;; skip over the same blank lines as we did. Any way to optimize
  ;; this?
  (do ((line-1 line-1 (1+ line-1))
       (line-2 line-2 (1+ line-2))
       (first-match t)
       (count 0 (1+ count)))
      ((= count *minimum-match-length*)
       t)
    ;; Should we wrap a (let ((*ignore-comments* nil))) around this
    ;; so that comments *do* count for matching up? Probably not.
    (multiple-value-bind (lines-same line-1-blank line-2-blank)
	(compare-lines file-1 line-1 file-2 line-2)
      ;; Note that only if *ignore-blank-lines* is T could
      ;; line-1-blank and line-2-blank be non-nil. 
      (cond ((and lines-same (not (or line-1-blank line-2-blank)))
	     ;; A real line matching a real line. Do nothing since
	     ;; the count is automatically incremented.
	     nil)
	    (lines-same
	     ;; A fake line matching by at least one blank. Skip it
	     ;; and keep it out of the count.
	     (decf count))
	    ((or line-1-blank line-2-blank)
	     ;; We have a match fail, but because of at least one
	     ;; blank line. Skip over the blank line,
	     (cond ((and line-1-blank line-2-blank)
		    ;; Two blank lines. Do nothing -- they'll be
		    ;; skipped automatically.
		    nil)
		   (first-match 
		    ;; We have a mismatch of real against blank, and it's on
		    ;; the first real pairing. Skipping the blank line would
		    ;; lead to so-co getting out of sync, so we
		    ;; must fail here and exit.
		    (return nil))
		   (line-1-blank
		    ;; Skip over this blank line (the line number is
		    ;; automatically incremented), but not over the other.
		    (decf line-2))
		   (line-2-blank
		    (decf line-1)))
	     ;; and keep this match fail out of the count.
	     (decf count))
	    (t
	     ;; A true non-match. Exit.
	     (return nil)))
      (when first-match (setf first-match nil)))))



#|
;;; older version
(defun found-match (file-1 line-1 file-2 line-2)
  "Check if we've found a match by verifying that the next few lines
   are identical. If *minimum-match-length* is more than 1, has the
   effect of grouping together differences separated only by one 
   matching line."
  (do ((line-1 line-1 (1+ line-1))
       (line-2 line-2 (1+ line-2))
       (count 0 (1+ count)))
      ((= count *minimum-match-length*)
       t)
    ;; Should we wrap a (let ((*ignore-comments* nil))) around this
    ;; so that comments *do* count for matching up? Probably not.
    (multiple-value-bind (lines-same line-1-blank line-2-blank)
	(compare-lines file-1 line-1 file-2 line-2)
      ;; Note that only if *ignore-blank-lines* is T could
      ;; line-1-blank and line-2-blank be non-nil. 
      (cond ((and lines-same (not (or line-1-blank line-2-blank)))
	     ;; A real line matching a real line. Do nothing.
	     nil)
	    (lines-same
	     ;; A fake line matching by at least one blank. Skip it
	     ;; and keep it out of the count.
	     (decf count))
	    (t
	     ;; A non-match. Exit.
	     (return nil))))))
|#

;;; ********************************
;;; Line Contexts ******************
;;; ********************************
(defun start-context (file line-no)
  "Walks backwards from LINE-NO until it finds the beginning of a 
   definition (a line with a left-parenthesis on char 0)."
  (when (plusp line-no)
    (do* ((i (1- line-no) (1- i))
	  (line (get-line file i) (get-line file i)))
	((zerop i))
      (when (and (plusp (length line))
		 (char-equal #\( (char line 0)))
	(return (values line i))))))

;;; ********************************
;;; Report Generator ***************
;;; ********************************
(defun draw-header (filename-1 filename-2 
			       &key (stream *standard-output*)
			       print-fancy-header)
  "Draw the header for the source compare report."
  (draw-bar stream)
  (cond (print-fancy-header
	 ;; Print the file write dates of the files.
	 (format stream "~&Source compare of")
	 (format stream "~&     ~A~&     (written by ~A, ~A)"
		 filename-1
		 (file-author filename-1) 
		 (time-string (file-write-date filename-1)))
	 (format stream "~&  with")
	 (format stream "~&     ~A~&     (written by ~A, ~A)"
		 filename-2 
		 (file-author filename-2)
		 (time-string (file-write-date filename-2))))
	(t
	 (format stream "~&Source compare of ~A with ~A"
		 filename-1 filename-2)))
  (draw-bar stream)	 
  (finish-output stream))

;;; changed universal-time to u-time - gef 
(defun time-string (u-time)
  (when u-time
    (multiple-value-bind (secs min hour date month year dow)
	(decode-universal-time u-time)
      (format nil "~@:(~A ~A-~A-~A ~2,'0d:~2,'0d:~2,'0d~)"
	      (svref '#("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") dow)
	      date 
	      (svref '#(0 "Jan" "Feb" "Mar" "Apr" "May"
			  "Jun" "Jul" "Aug" "Sep" "Oct"
			  "Nov" "Dec")
		     month)
	      (mod year 100)
	      hour min secs))))

(defun draw-bar (&optional (stream *standard-output*))
  "Draws a dash across the line."
  (format stream "~&~V,,,'=A~%" 75 "="))

(defun print-range (start end &optional (stream *standard-output*))
  "Prints a representation of the range from START to END."
  (cond ((= start end)
	 (format stream "~D" start))
	((= start (1- end))
	 (format stream "~D" (1+ start)))
	(t
	 (format stream "~D,~D" (1+ start) end))))

(defun print-differences (file-1 start-1 end-1 file-2 start-2 end-2
				 &optional (stream *standard-output*))
  "Print the differences in the two files in a format similar to diff." 
  (print-range start-1 end-1 stream)
  (cond ((= end-1 start-1)
	 ;; We added the text in file2
	 (format stream "a"))
	((= end-2 start-2)
	 ;; We deleted the text from file1
	 (format stream "d"))
	(t
	 ;; We replaced the text from file1 with the text from file2
	 (format stream "c")))
  (print-range start-2 end-2 stream)
  (print-file-segment file-1 start-1 end-1 stream "< ")
  (format stream "~&---")
  (print-file-segment file-2 start-2 end-2 stream "> ")
  (draw-bar stream)
  ;; Make sure that the output is displayed piecemeal.
  (finish-output stream))

(defun print-file-segment (file start end 
				&optional (stream *standard-output*)
				(left-margin ""))
  "Prints the region of FILE from START to END."
  (when *print-context*
    ;; If we want to provide a little context for the changes,
    ;; first change the start and end to add in the specified number
    ;; of extra lines to print.
    (setf start (max 0 (- start *context-lines-before-difference*))
	  end (+ end *context-lines-after-difference*))
    ;; Then print the name of the file and the beginning of the
    ;; current definition.
    (let ((context (start-context file start)))
      (format stream "~&**** File ~A~@[, After \"~A\"~]"
	      (file-cache-file-name file) context)))
  ;; Then print the lines from start to end, with a left margin as specified.
  (do ((line-no start (1+ line-no))
       (line))
      ((= line-no end))
    (unless (setq line (get-line file line-no))
      (return nil))
    (format stream "~%~A~A" left-margin line)))

;;; *EOF*
