;;; -*- Package: USER -*-
;;;
;;; **********************************************************************
;;;
(ext:file-comment
  "$Header$")
;;;
;;; **********************************************************************
;;;
;;; This file compiles all of Hemlock.
;;;

#+bootstrap
(progn
  (when (ext:get-command-line-switch "slave")
    (error "Cannot compile Hemlock in a slave due to its clobbering needed
    typescript routines by renaming the package."))
  
  ;;; Blast the old packages in case they are around.  We do this solely to
  ;;; prove Hemlock can compile cleanly without its having to exist already.
  ;;;
  (copy-packages '("ED" "HI")))


;;; Stuff to set up the packages Hemlock uses.
;;;
(unless (find-package "HEMLOCK-INTERNALS")
  (make-package "HEMLOCK-INTERNALS"
		:nicknames '("HI")
		:use '("LISP" "EXTENSIONS" "SYSTEM")))

(unless (find-package "HEMLOCK")
  (make-package "HEMLOCK"
		:nicknames '("ED")
		:use '("LISP" "HEMLOCK-INTERNALS" "EXTENSIONS" "SYSTEM")))
;;;
(export 'c::compile-from-stream (find-package "C"))


(in-package "USER")

(defvar *byte-compile* #+small t #-small :maybe)

(pushnew :command-bits *features*)
(pushnew :buffered-lines *features*)

#-clx
;;; If CLX has not been loaded, but has been compiled, then load it.
;;;
(when (probe-file (make-pathname :defaults "target:clx/clx-library"
				 :type (c:backend-fasl-file-type c:*backend*)))
  #+(and (not pcl) (not no-pcl-clx))
  (load "target:pcl/pclload")
  (load "target:clx/clx-library")
  #+gencgc (gc :full t)
  #-gencgc (ext:purify))
  
(with-compiler-log-file
    ("target:compile-hemlock.log"
     :optimize
     '(optimize (debug #-small 2 #+small .5) 
		(speed 2) (inhibit-warnings 2)
		(safety #-small 1 #+small 0))
     :optimize-interface
     '(optimize-interface (debug .5))
     :context-declarations
     '(((:or :external (:match "$%SET-"))
	(declare (optimize (safety 2))
		 (optimize-interface (debug 1))))
       (:macro (declare (optimize (speed 0))))))

(comf "target:code/globals")
(comf "target:code/struct")
(comf "target:hemlock/charmacs")
(comf "target:hemlock/key-event" :load t)
(comf "target:hemlock/struct")
;(comf "target:hemlock/struct-ed")
(comf "target:hemlock/rompsite")
;;;
;;; This is necessary since all the #k uses in Hemlock will expand into
;;; EXT:MAKE-KEY-EVENT calls with keysyms and bits from the compiling Lisp, not
;;; for the Lisp new code will run in.  This destroys the compiling Lisp with
;;; respect to running code with #k's compiled for it, but it causes the
;;; compilation to see new keysyms, modifiers, and CLX modifier maps correctly
;;; for the new system.
;;;
(ext::re-initialize-key-events)
(comf "target:hemlock/keysym-defs")
(comf "target:hemlock/input")
(comf "target:hemlock/macros" :byte-compile t)
(comf "target:hemlock/line")
(comf "target:hemlock/ring")
(comf "target:hemlock/table")
(comf "target:hemlock/htext1")
(comf "target:hemlock/htext2")
(comf "target:hemlock/htext3")
(comf "target:hemlock/htext4")
(comf "target:hemlock/search1")
(comf "target:hemlock/search2")
(comf "target:hemlock/linimage")
(comf "target:hemlock/cursor")
(comf "target:hemlock/syntax")
(comf "target:hemlock/winimage")
#+clx (comf "target:hemlock/hunk-draw")
;(comf "target:hemlock/bit-stream")
(comf "target:hemlock/termcap")
(comf "target:hemlock/display")
#+clx (comf "target:hemlock/bit-display")
(comf "target:hemlock/tty-disp-rt")
(with-compilation-unit (:optimize '(optimize (safety 2) (debug 3)))
  (comf "target:hemlock/tty-display")) ; Buggy...
;(comf "target:hemlock/tty-stream")
(comf "target:hemlock/pop-up-stream")
(comf "target:hemlock/screen")
#+clx (comf "target:hemlock/bit-screen")
(comf "target:hemlock/tty-screen")
(comf "target:hemlock/window")
(comf "target:hemlock/font")
(comf "target:hemlock/interp")
(comf "target:hemlock/vars")
(comf "target:hemlock/buffer")
(comf "target:hemlock/files")
(comf "target:hemlock/streams")
(comf "target:hemlock/echo" :byte-compile t)
(comf "target:hemlock/main" :byte-compile t)
(comf "target:hemlock/echocoms" :byte-compile t)
(comf "target:hemlock/defsyn")

(comf "target:hemlock/ts-buf")
(comf "target:hemlock/ts-stream")

(with-compilation-unit
    (:optimize
     '(optimize (safety 2) (speed 0))
     :context-declarations
     '(((:match "-COMMAND$")
	(declare (optimize (safety #+small 0 #-small 1))
		 (optimize-interface (safety 2))))))

(comf "target:hemlock/command" :byte-compile t)
(comf "target:hemlock/morecoms" :byte-compile t)
(comf "target:hemlock/undo" :byte-compile t)
(comf "target:hemlock/killcoms" :byte-compile t)
(comf "target:hemlock/searchcoms" :byte-compile t)
(comf "target:hemlock/filecoms" :byte-compile t)
(comf "target:hemlock/indent" :byte-compile t)
(comf "target:hemlock/lispmode")
(comf "target:hemlock/comments" :byte-compile t)
(comf "target:hemlock/fill")
(comf "target:hemlock/text" :byte-compile t)
(comf "target:hemlock/doccoms" :byte-compile t)
(comf "target:hemlock/srccom" :byte-compile t)
(comf "target:hemlock/abbrev" :byte-compile t)
(comf "target:hemlock/group" :byte-compile t)
(comf "target:hemlock/overwrite" :byte-compile t)
(comf "target:hemlock/gosmacs" :byte-compile t)
(comf "target:hemlock/eval-server" :byte-compile t)
(comf "target:hemlock/dylan" :byte-compile t)
(comf "target:hemlock/lispbuf" :byte-compile t)
(comf "target:hemlock/lispeval" :byte-compile t)
(comf "target:hemlock/icom" :byte-compile t)
(comf "target:hemlock/hi-integrity" :byte-compile t)
(comf "target:hemlock/ed-integrity" :byte-compile t)
(comf "target:hemlock/scribe" :byte-compile t)
(comf "target:hemlock/pascal" :byte-compile t)
(comf "target:hemlock/edit-defs" :byte-compile t)
(comf "target:hemlock/auto-save" :byte-compile t)
(comf "target:hemlock/register" :byte-compile t)
(comf "target:hemlock/xcoms" :byte-compile t)
(comf "target:hemlock/unixcoms" :byte-compile t)
(comf "target:hemlock/mh")
(comf "target:hemlock/highlight" :byte-compile t)
(comf "target:hemlock/dired" :byte-compile t)
(comf "target:hemlock/diredcoms" :byte-compile t)
(comf "target:hemlock/bufed" :byte-compile t)
(comf "target:hemlock/lisp-lib" :byte-compile t)
(comf "target:hemlock/completion" :byte-compile t)
(comf "target:hemlock/shell" :byte-compile t)
(comf "target:hemlock/debug" :byte-compile t)
(comf "target:hemlock/netnews" :byte-compile t)
(comf "target:hemlock/rcs" :byte-compile t)

) ;WITH-COMPILATION-UNIT for commands

;; Stuff we want compiled native:

(comf "target:hemlock/spell-rt")
(comf "target:hemlock/spell-corr")
(comf "target:hemlock/spell-aug")
(comf "target:hemlock/spell-build")
(comf "target:hemlock/spellcoms")
(comf "target:hemlock/kbdmac")

(comf "target:hemlock/bindings")
(comf "target:hemlock/hacks")

) ;WITH-COMPILER-LOG-FILE

(unless (probe-file "target:hemlock/spell-dictionary.bin")
  (load "target:hemlock/spell-rt")
  (load "target:hemlock/spell-corr")
  (load "target:hemlock/spell-aug")
  (load "target:hemlock/spell-build")
  (funcall (fdefinition (intern "BUILD-DICTIONARY" "SPELL"))
	   "target:hemlock/spell-dictionary.text"
	   "target:hemlock/spell-dictionary.bin"))

(cat-if-anything-changed
 "target:hemlock/hemlock-library"
 "target:hemlock/rompsite"
 "target:hemlock/struct"
 ; "target:hemlock/struct-ed"
 "target:hemlock/charmacs"
 "target:hemlock/input"
 "target:hemlock/line"
 "target:hemlock/ring"
 "target:hemlock/vars"
 "target:hemlock/buffer"
 "target:hemlock/macros"
 "target:hemlock/interp"
 "target:hemlock/syntax"
 "target:hemlock/htext1"
 "target:hemlock/htext2"
 "target:hemlock/htext3"
 "target:hemlock/htext4"
 "target:hemlock/files"
 "target:hemlock/search1"
 "target:hemlock/search2"
 "target:hemlock/table"
 #+clx "target:hemlock/hunk-draw"
 "target:hemlock/window"
 "target:hemlock/screen"
 "target:hemlock/winimage"
 "target:hemlock/linimage"
 "target:hemlock/display"
 "target:hemlock/termcap"
 #+clx "target:hemlock/bit-display"
 "target:hemlock/tty-disp-rt"
 "target:hemlock/tty-display"
 "target:hemlock/pop-up-stream"
 #+clx "target:hemlock/bit-screen"
 "target:hemlock/tty-screen"
 "target:hemlock/cursor"
 "target:hemlock/font"
 "target:hemlock/streams"
 "target:hemlock/hacks"
 "target:hemlock/main"
 "target:hemlock/echo"
 "target:hemlock/echocoms"
 "target:hemlock/command"
 "target:hemlock/indent"
 "target:hemlock/comments"
 "target:hemlock/morecoms"
 "target:hemlock/undo"
 "target:hemlock/killcoms"
 "target:hemlock/searchcoms"
 "target:hemlock/filecoms"
 "target:hemlock/doccoms"
 "target:hemlock/srccom"
 "target:hemlock/group"
 "target:hemlock/fill"
 "target:hemlock/text"
 "target:hemlock/lispmode"
 "target:hemlock/ts-buf"
 "target:hemlock/ts-stream"
 "target:hemlock/eval-server"
 "target:hemlock/lispbuf"
 "target:hemlock/lispeval"
 "target:hemlock/spell-rt"
 "target:hemlock/spell-corr"
 "target:hemlock/spell-aug"
 "target:hemlock/spellcoms"
 "target:hemlock/overwrite"
 "target:hemlock/abbrev"
 "target:hemlock/icom"
 "target:hemlock/kbdmac"
 "target:hemlock/defsyn"
 "target:hemlock/scribe"
 "target:hemlock/pascal"
 "target:hemlock/dylan"
 "target:hemlock/edit-defs"
 "target:hemlock/auto-save"
 "target:hemlock/register"
 "target:hemlock/xcoms"
 "target:hemlock/unixcoms"
 "target:hemlock/mh"
 "target:hemlock/highlight"
 "target:hemlock/dired"
 "target:hemlock/diredcoms"
 "target:hemlock/bufed"
 "target:hemlock/lisp-lib"
 "target:hemlock/completion"
 "target:hemlock/shell"
 "target:hemlock/debug"
 "target:hemlock/netnews"
 "target:hemlock/rcs"
 "target:hemlock/bindings")
