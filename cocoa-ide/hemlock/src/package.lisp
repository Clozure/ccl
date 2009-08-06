(in-package :cl-user)

(defpackage :hemlock-interface
  (:use)
  (:export
   ;; Symbols from the CIM, by chapter:

   ;; Representation of Text
   #:linep
   #:line-string
   #:line-previous
   #:line-next
   #:line-buffer
   #:line-length
   #:line-character
   #:line-plist
   #:line-signature
   #:markp
   #:mark-line
   #:mark-charpos
   #:mark-kind
   #:mark-buffer
   #:mark-absolute-position
   #:previous-character
   #:next-character
   #:mark
   #:copy-mark
   #:delete-mark
   #:with-mark
   #:move-to-position
   #:move-to-absolute-position
   #:move-mark
   #:line-start
   #:line-end
   #:buffer-start
   #:buffer-end
   #:mark-before
   #:mark-after
   #:character-offset
   #:line-offset
   #:region
   #:regionp
   #:make-empty-region
   #:copy-region
   #:region-to-string
   #:string-to-region
   #:line-to-region
   #:region-start
   #:region-end
   #:region-bounds
   #:set-region-bounds
   #:count-lines
   #:count-characters

   ;; charprops
   #:next-charprop-value
   #:previous-charprop-value
   #:set-charprop-value
   #:find-charprop-value
   #:next-charprops
   #:previous-charprops
   #:set-charprops
   #:charprops-in-region
   #:apply-charprops
   #:find-charprops
   #:find-charprops-change
   #:charprop-equal
   #:charprops-get
   #:charprops-set
   #:charprops-equal
   #:charprops-as-plist
   #:charprops-as-hash
   #:charprops-names
   #:set-buffer-charprops
   #:buffer-charprops

   ;; Buffers
   #:current-buffer
   #:current-point-for-insertion
   #:current-point-for-deletion
   #:current-point-unless-selection
   #:current-point-collapsing-selection
   #:current-point-extending-selection
   #:current-point-for-selection-start
   #:current-point-for-selection-end
   #:current-point
   #:current-mark
   #:pop-buffer-mark
   #:push-buffer-mark
   #:push-new-buffer-mark
   #:all-buffers
   #:make-buffer
   #:bufferp
   #:buffer-name
   #:buffer-region
   #:buffer-pathname
   #:buffer-write-date
   #:buffer-point
   #:buffer-mark
   #:buffer-start-mark
   #:buffer-end-mark
   #:buffer-writable
   #:buffer-modified
   #:buffer-signature
   #:buffer-variables
   #:buffer-modes
   #:buffer-delete-hook
   #:buffer-package
   #:delete-buffer
   #:with-writable-buffer
   #:make-modeline-field
   #:modeline-field-p
   #:modeline-field-name
   #:modeline-field
   #:modeline-field-function
   #:modeline-field-width
   #:buffer-modeline-fields
   #:buffer-modeline-field-p
   #:update-modeline-fields

   ;; Altering and Searching Text
   #:insert-character
   #:insert-string
   #:insert-region
   #:ninsert-region
   #:delete-characters
   #:delete-region
   #:delete-and-save-region
   #:filter-region
   #:start-line-p
   #:end-line-p
   #:empty-line-p
   #:blank-line-p
   #:blank-before-p
   #:blank-after-p
   #:same-line-p
   #:mark<
   #:mark<=
   #:mark=
   #:mark/=
   #:mark>=
   #:mark>
   #:line<
   #:line<=
   #:line>=
   #:line>
   #:lines-related
   #:first-line-p
   #:last-line-p
   #:kill-region
   #:kill-characters
   #:*ephemerally-active-command-types*
   #:activate-region
   #:deactivate-region
   #:region-active-p
   #:check-region-active
   #:current-region
   #:new-search-pattern
   #:search-pattern-p
   #:get-search-pattern
   #:find-pattern
   #:replace-pattern
   #:*last-search-string*
   #:collapse-if-selection

   ;; Hemlock Variables
   #:*global-variable-names*
   #:current-variable-tables
   #:defhvar
   #:variable-value
   #:variable-documentation
   #:variable-hooks
   #:variable-name
   #:string-to-variable
   #:value
   #:setv
   #:hlet
   #:hemlock-bound-p
   #:delete-variable
   #:add-hook
   #:remove-hook
   #:invoke-hook

   ;; Commands
   #:*command-names*
   #:defcommand
   #:make-command
   #:commandp
   #:command-documentation
   #:command-function
   #:command-name
   #:bind-key
   #:command-bindings
   #:delete-key-binding
   #:get-command
   #:map-bindings
   #:key-translation
   #:last-command-type
   #:prefix-argument

   ;; Modes
   #:*mode-names*
   #:defmode
   #:mode-documentation
   #:buffer-major-mode
   #:buffer-minor-mode
   #:mode-variables
   #:mode-major-p

   ;; Character attributes
   #:*character-attribute-names*
   #:defattribute
   #:character-attribute-name
   #:character-attribute-documentation
   #:character-attribute
   #:character-attribute-p
   #:shadow-attribute
   #:unshadow-attribute
   #:find-attribute
   #:find-not-attribute
   #:reverse-find-attribute
   #:reverse-find-not-attribute
   #:character-attribute-hooks

   ;; Controlling the Display
   #:current-view
   #:hemlock-view-p
   #:hemlock-view-buffer
   #:mark-column
   #:move-to-column
   #:set-scroll-position

   ;; Logical Key Events
   #:*logical-key-event-names*
   #:define-logical-key-event
   #:logical-key-event-key-events
   #:logical-key-event-name
   #:logical-key-event-documentation
   #:logical-key-event-p

   ;; The Echo Area
   #:clear-echo-area
   #:message
   #:loud-message
   #:beep
   #:command-case
   #:prompt-for-buffer
   #:prompt-for-key-event
   #:prompt-for-key
   #:prompt-for-file
   #:prompt-for-integer
   #:prompt-for-keyword
   #:prompt-for-expression
   #:prompt-for-string
   #:prompt-for-variable
   #:prompt-for-y-or-n
   #:prompt-for-yes-or-no
   #:parse-for-something

   ;; Files
   #:define-file-option
   #:define-file-type-hook
   #:process-file-options
   #:pathname-to-buffer-name
   #:buffer-default-pathname
   #:read-file
   #:write-file
   #:write-buffer-file
   #:read-buffer-file
  ;; #:find-file-buffer

   ;;# Hemlock's Lisp Environment
   ;;   #:ed
   #:*key-event-history*
   #:last-key-event-typed
   #:last-char-typed
   #:make-hemlock-output-stream
   #:hemlock-output-stream-p
   #:make-hemlock-region-stream
   #:hemlock-region-stream-p
   #:with-input-from-region
   #:with-output-to-mark
   #:with-pop-up-display
   #:editor-error
   #:handle-lisp-errors
   #:in-lisp
   #:do-alpha-chars

   ;; Higher-Level Text Primitives
   #:indent-region
   #:indent-region-for-commands
   #:delete-horizontal-space
   #:pre-command-parse-check
   #:form-offset
   #:top-level-offset
   #:mark-top-level-form
   #:defun-region
   #:inside-defun-p
   #:start-defun-p
   #:forward-up-list
   #:backward-up-list
   #:valid-spot
   #:defindent
   #:word-offset
   #:sentence-offset
   #:paragraph-offset
   #:mark-paragraph
   #:fill-region
   #:fill-region-by-paragraphs

   ;; Utilities
   #:make-string-table
   #:string-table-p
   #:string-table-separator
   #:delete-string
   #:clrstring
   #:getstring
   #:complete-string
   #:find-ambiguous
   #:find-containing
   #:do-strings
   #:make-ring
   #:ringp
   #:ring-length
   #:ring-ref
   #:ring-push
   #:ring-pop
   #:rotate-ring
   #:save-for-undo
   #:make-region-undo

   ;; Miscellaneous

   #:define-keysym
   #:define-keysym-code
   #:define-mouse-keysym
   #:name-keysym
   #:keysym-names
   #:keysym-preferred-name
   #:define-key-event-modifier
   #:*all-modifier-names*
   #:make-key-event-bits
   #:key-event-modifier-mask
   #:key-event-bits-modifiers
   #:make-key-event
   #:key-event-p
   #:key-event-bits
   #:key-event-keysym
   #:char-key-event
   #:key-event-char
   #:key-event-bit-p
   #:do-alpha-key-events
   #:pretty-key-string
   ))

;; Functions defined externally (i.e. used by but not defined in hemlock).  In theory,
;; these (and codes for the symbolic keysyms in keysym-defs.lisp, q.v.) is all you need
;; to implement to port the IDE to a different window system.
(defpackage :hemlock-ext
  (:use)
  ;;
  (:export
   #:invoke-modifying-buffer-storage
   #:note-selection-set-by-search
   #:scroll-view
   #:ensure-selection-visible
   #:report-hemlock-error
   #:top-listener-output-stream
   #:top-listener-input-stream
   #:invalidate-modeline
   #:note-buffer-saved
   #:note-buffer-unsaved
   #:read-only-listener-p
   #:all-hemlock-views
   #:open-sequence-dialog
   #:edit-single-definition
   #:change-active-pane
   #:send-string-to-listener
   #:buffer-process-description
   #:raise-buffer-view
   ))

(defpackage :hi
  (:use :common-lisp :hemlock-interface)
  (:nicknames :hemlock-internals)
  (:import-from
   ;; gray streams
   #+EXCL  :excl
   #+CLISP :gray
   #+CMU   :ext
   #+sbcl  :sb-gray
   #+scl   :ext
   #+clozure :gray
   ;;
   ;; Note the patch i received from DTC mentions character-output and
   ;; character-input-stream here, so we actually see us faced to
   ;; provide for compatibility classes. --GB
   #-scl   #:fundamental-character-output-stream
   #-scl   #:fundamental-character-input-stream
   ;; There is conditionalization in streams.lisp, see above --GB
   #+scl   #:character-output-stream
   #+scl   #:character-input-stream
   
   #:stream-write-char
   #-scl   #:stream-write-string     ; wonder what that is called --GB
   #:stream-read-char
   #:stream-listen
   #:stream-unread-char
   #:stream-clear-input
   #:stream-finish-output
   #:stream-force-output
   #:stream-line-column)
  (:import-from :ccl
                #:delq #:memq #:assq
                #:getenv
                #:fixnump)
  (:import-from :gui
		#:log-debug)
  ;; ** TODO: get rid of this.  The code that uses it assumes it guarantees atomicity,
  ;; and it doesn't.
  (:import-from :ccl #:without-interrupts)
  ;;
  (:export
   #:*FAST*                             ;hmm not sure about this one
   
   ;; Imported
   #:delq #:memq #:assq #:getenv #:fixnump #:log-debug

   ;; hemlock-ext.lisp
   #:hemlock-char-code-limit
   #:file-writable #:default-directory #:complete-file #:ambiguous-files

   ;; rompsite.lisp
   #:editor-describe-function
   #:merge-relative-pathnames
   ;;
   ;; Export default-font to prevent a name conflict that occurs due to
   ;; the Hemlock variable "Default Font" defined in SITE-INIT below.
   ;;
   #:default-font
   #:*beep-function* #:beep

   ;; 
   #:mark #:mark-line #:mark-charpos #:mark-column #:move-to-column
   #:markp #:region #:region-start #:region-end
   #:regionp #:buffer #:bufferp #:buffer-modes #:buffer-point #:buffer-writable
   #:buffer-delete-hook #:buffer-variables #:buffer-write-date
   #:region #:regionp #:region-start #:region-end
   #:commandp #:command #:command-function
   #:command-documentation #:modeline-field #:modeline-field-p

   ;; from macros.lisp
   #:invoke-hook #:value #:setv #:hlet #:string-to-variable #:add-hook #:remove-hook
   #:defcommand #:with-mark #:use-buffer #:editor-error
   #:editor-error-format-string #:editor-error-format-arguments #:do-strings
   #:command-case #:reprompt #:with-output-to-mark #:with-input-from-region
   #:handle-lisp-errors #:with-pop-up-display

   ;; from views.lisp
   #:hemlock-view #:current-view #:hemlock-view-buffer
   #:current-prefix-argument-state #:last-key-event-typed #:last-char-typed
   #:invoke-command
   #:abort-to-toplevel #:abort-current-command
   #:set-scroll-position
   #:native-key-event-p

   ;; from line.lisp
   #:line #:linep #:line-previous #:line-next #:line-plist #:line-signature

   ;; from ring.lisp
   #:ring #:ringp #:make-ring #:ring-push #:ring-pop #:ring-length #:ring-ref
   #:rotate-ring

   ;; from table.lisp
   #:string-table #:string-table-p #:make-string-table
   #:string-table-separator #:getstring
   #:find-ambiguous #:complete-string #:find-containing
   #:delete-string #:clrstring #:do-strings

   ;; buffer.lisp
   #:buffer-modified #:buffer-region #:buffer-name #:buffer-pathname
   #:buffer-major-mode #:buffer-minor-mode #:buffer-modeline-fields
   #:buffer-modeline-field-p #:current-buffer #:current-point
   #:defmode #:mode-major-p #:mode-variables #:mode-documentation
   #:make-buffer #:delete-buffer #:with-writable-buffer #:buffer-start-mark
   #:buffer-end-mark #:*buffer-list*

   ;; charmacs.lisp
   #:syntax-char-code-limit #:search-char-code-limit #:do-alpha-chars

   ;; charprops.lisp
   #:next-charprop-value #:previous-charprop-value
   #:set-charprop-value #:find-charprop-value #:next-charprops
   #:previous-charprops #:set-charprops #:charprops-in-region
   #:apply-charprops #:find-charprops #:find-charprops-change
   #:charprop-equal #:charprops-get #:charprops-set #:charprops-equal
   #:charprops-as-plist #:charprops-as-hash #:charprops-names
   #:set-buffer-charprops #:buffer-charprops

   ;; key-event.lisp
   #:define-keysym-code #:define-mouse-keysym #:define-modifier-bit
   #:*all-modifier-names* #:*modifier-translations*
   #:make-key-event #:char-key-event #:do-alpha-key-events
   #:key-event-modifier-mask #:key-event-char #:key-event-bit-p
   #:pretty-key-string

   ;; echo.lisp
   #:*echo-area-stream*
   #:clear-echo-area #:message #:loud-message
   #:current-echo-parse-state #:exit-echo-parse
   #:eps-parse-type #:eps-parse-starting-mark #:eps-parse-input-region
   #:eps-parse-verification-function #:eps-parse-string-tables
   #:eps-parse-default #:eps-parse-help #:eps-parse-key-handler
   #:prompt-for-buffer #:prompt-for-file #:prompt-for-integer
   #:prompt-for-keyword #:prompt-for-expression #:prompt-for-string
   #:prompt-for-variable #:prompt-for-yes-or-no #:prompt-for-y-or-n
   #:prompt-for-key-event #:prompt-for-key
   #:*logical-key-event-names*
   #:logical-key-event-p #:logical-key-event-documentation
   #:logical-key-event-name #:logical-key-event-key-events
   #:define-logical-key-event #:current-variable-tables


   ;; commands
   #:make-prefix-argument-state #:prefix-argument-resetting-state

  
   ;; files.lisp
   #:read-file #:write-file


   ;; font.lisp
   #:font-mark #:delete-font-mark #:delete-line-font-marks #:move-font-mark
   #:window-font

   ;; htext1.lisp
   #:line-length #:line-buffer #:line-string #:line-character #:mark #:mark-kind
   #:copy-mark #:delete-mark #:move-to-position #:mark-absolute-position
   #:move-to-absolute-position #:buffer-selection-range #:region #:make-empty-region
   #:start-line-p #:end-line-p #:empty-line-p #:blank-line-p #:blank-before-p
   #:blank-after-p #:same-line-p #:mark< #:mark<= #:mark> #:mark>= #:mark= #:mark/=
   #:line< #:line<= #:line> #:line>= #:first-line-p #:last-line-p #:buffer-signature
   #:lines-related


   ;; htext2.lisp
   #:region-to-string #:string-to-region #:line-to-region
   #:previous-character #:next-character #:count-lines
   #:count-characters #:line-start #:line-end #:buffer-start
   #:buffer-end #:move-mark #:mark-before #:mark-after
   #:character-offset #:line-offset #:region-bounds
   #:set-region-bounds #:*print-region*


   ;; htext3.lisp
   #:insert-character #:insert-string #:insert-region #:ninsert-region
   #:paste-characters

   ;; htext4.lisp
   #:delete-characters #:delete-region #:delete-and-save-region #:copy-region
   #:filter-region


   ;; interp.lisp
   #:bind-key #:delete-key-binding #:get-command #:map-bindings
   #:make-command #:command-name #:command-bindings #:last-command-type
   #:prefix-argument #:key-translation


   ;; main.lisp
   #:*global-variable-names* #:*mode-names* #:*buffer-names*
   #:*character-attribute-names* #:*command-names* #:*buffer-list*
   #:after-editor-initializations

   ;; search1.lisp
   #:search-pattern #:search-pattern-p #:find-pattern #:replace-pattern
   #:new-search-pattern

   ;; modeline.lisp
   #:modeline-field-width
   #:modeline-field-function #:make-modeline-field
   #:update-modeline-field #:modeline-field-name #:modeline-field

   ;; streams.lisp
   #:make-hemlock-output-stream
   #:hemlock-region-stream #:hemlock-region-stream-p
   #:hemlock-output-stream #:make-hemlock-region-stream
   #:hemlock-output-stream-p #:make-kbdmac-stream
   #:modify-kbdmac-stream

   ;; syntax.lisp
   #:character-attribute-name
   #:defattribute #:character-attribute-documentation #:character-attribute
   #:character-attribute-hooks #:character-attribute-p #:shadow-attribute
   #:unshadow-attribute #:find-attribute #:reverse-find-attribute

   ;; vars.lisp
   #:variable-value #:variable-hooks #:variable-documentation #:variable-name
   #:hemlock-bound-p #:defhvar #:delete-variable

   #:input-stream-reading-line

   ))


(defpackage :hemlock
  (:use :common-lisp :hemlock-interface :hemlock-internals :hemlock-ext)
  )


;; $Log$
;; Revision 1.2  2005/08/01 10:54:17  gb
;; Don't export CHECK-REGION-QUERY-SIZE.
;;
;; Revision 1.1.1.1  2003/10/19 08:57:16  gb
;; recovered 0.14 sources
;;
;; Revision 1.1.2.1  2003/08/10 19:11:33  gb
;; New files, imported from upstream CVS as of 03/08/09.
;;
;; Revision 1.9  2003/08/05 19:58:21  gilbert
;; - we now have a HEMLOCK-INTERFACE package which exports symbols mentioned
;;   in the Command Implementors Manual.
;;
;; Revision 1.8  2003/07/28 20:35:32  jdz
;; BEEP function now works.
;;
;; Revision 1.7  2003/07/27 10:11:06  jdz
;; HEMLOCK-EXT package is now used by HEMLOCK.  Conflicting symbols from
;; EXTENSIONS package in CMUCL are shadowed.
;;
;; Revision 1.6  2003/05/12 11:01:48  gbyers
;; Conditionalize (Gray streams package) for OpenMCL.
;;
;; Revision 1.5  2003/03/26 07:50:10  gilbert
;; Port to SCL by Douglas Crosher
;;
;; Revision 1.4  2003/03/06 21:38:58  gilbert
;; The symbol *FAST* is now exported from HI (no idea if that is the
;; right thing to do) and imported into HEMLOCK. Fixes bug:
;; auto-save.lisp was not compiling.
;;
