(in-package :cl-user)

;; Note: I want real relative package names like the Symbolics has
;; them. In the mean time:

#+CMU
(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn
    ;; Just in case the original Hemlock is loaded.
    (dolist (p '("HEMLOCK" "HEMLOCK-INTERNALS"))
      (when (find-package p)
        (delete-package p)))))
    

(defpackage :hemlock-interface
  (:use)
  (:export
   ;; Functions from the CIM:
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
   #:previous-character
   #:next-character
   #:mark
   #:copy-mark
   #:delete-mark
   #:move-to-position
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
   #:current-buffer
   #:current-point-for-insertion
   #:current-point-for-deletion
   #:current-point-unless-selection
   #:current-point-collapsing-selection
   #:current-point-extending-selection
   #:current-point
   #:current-mark
   #:pop-buffer-mark
   #:push-buffer-mark
   #:change-to-buffer
   #:previous-buffer
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
   #:buffer-windows
   #:buffer-delete-hook
   #:buffer-package
   #:delete-buffer
   #:delete-buffer-if-possible
   #:make-modeline-field
   #:modeline-field-p
   #:modeline-field-name
   #:modeline-field
   #:modeline-field-function
   #:modeline-field-width
   #:buffer-modeline-fields
   #:buffer-modeline-field-p
   #:update-modeline-fields
   #:update-modeline-field
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
   #:current-variable-tables
   #:defhvar
   #:variable-value
   #:variable-documentation
   #:variable-hooks
   #:variable-name
   #:string-to-variable
   #:hemlock-bound-p
   #:delete-variable
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
   #:interactive
   #:last-command-type
   #:prefix-argument
   #:recursive-edit
   #:in-recursive-edit
   #:exit-recursive-edit
   #:abort-recursive-edit
   #:defmode
   #:mode-documentation
   #:buffer-major-mode
   #:buffer-minor-mode
   #:mode-variables
   #:mode-major-p
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
   #:current-window
   #:make-window
   #:windowp
   #:delete-window
   #:window-buffer
   #:window-display-start
   #:window-display-end
   #:window-display-recentering
   #:window-point
   #:center-window
   #:scroll-window
   #:displayed-p
   #:window-height
   #:window-width
   #:next-window
   #:previous-window
   #:mark-to-cursorpos
   #:cursorpos-to-mark
   #:last-key-event-cursorpos
   #:mark-column
   #:move-to-column
   #:show-mark
   #:redisplay
   #:redisplay-all
   #:editor-finish-output
   #:define-logical-key-event
   #:logical-key-event-key-events
   #:logical-key-event-name
   #:logical-key-event-documentation
   #:logical-key-event-p
   #:clear-echo-area
   #:message
   #:loud-message
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
   #:process-file-options
   #:pathname-to-buffer-name
   #:buffer-default-pathname
   #:read-file
   #:write-file
   #:write-buffer-file
   #:read-buffer-file
   #:find-file-buffer
   ;;   #:ed
   #:exit-hemlock
   #:pause-hemlock
   #:get-key-event
   #:unget-key-event
   #:recursive-get-key-event
   #:clear-editor-input
   #:listen-editor-input
   #:editor-sleep
   #:make-hemlock-output-stream
   #:hemlock-output-stream-p
   #:make-hemlock-region-stream
   #:hemlock-region-stream-p
   #:editor-error-format-string
   #:editor-error-format-arguments
   #:editor-error
   #:add-definition-dir-translation
   #:delete-definition-dir-translation
   #:schedule-event
   #:remove-scheduled-event
   #:in-lisp
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
   #:goto-page
   #:page-offset
   #:page-directory
   #:display-page-directory
   #:fill-region
   #:fill-region-by-paragraphs
   #:make-string-table
   #:string-table-p
   #:string-table-separator
   #:delete-string
   #:clrstring
   #:getstring
   #:complete-string
   #:find-ambiguous
   #:find-containing
   #:make-ring
   #:ringp
   #:ring-length
   #:ring-ref
   #:ring-push
   #:ring-pop
   #:rotate-ring
   #:save-for-undo
   #:make-region-undo
   #:supply-generic-pointer-up-function

   ;; Macros from the CIM:
   #:with-writable-buffer
   #:value
   #:setv
   #:add-hook
   #:remove-hook
   #:invoke-hook
   #:defcommand
   #:use-buffer
   #:command-case
   #:define-file-option
   #:define-file-type-hook
   #:do-active-group
   #:with-input-from-region
   #:with-output-to-mark
   #:with-pop-up-display
   #:handle-lisp-errors
   #:do-alpha-chars
   #:do-strings
   
   ))

(defpackage :hemlock-ext
  (:use :common-lisp
        :hemlock-interface)
  #+cmu
  (:import-from :ext #:complete-file)
  (:shadow #:char-code-limit)
  ;;
  (:export
   #:file-comment
   #:without-interrupts
   #:without-gcing
   #:define-setf-method
   #:getenv

   #:delq #:memq #:assq
   #:fixnump
   #:file-writable
     
   #:define-keysym #:define-mouse-keysym #:name-keysym #:keysym-names
   #:keysym-preferred-name #:define-key-event-modifier #:define-clx-modifier
   #:make-key-event-bits #:key-event-modifier-mask #:key-event-bits-modifiers
   #:*all-modifier-names* #:translate-key-event #:translate-mouse-key-event
   #:make-key-event #:key-event #:key-event-p #:key-event-bits #:key-event-keysym
   #:char-key-event #:key-event-char #:key-event-bit-p #:do-alpha-key-events
   #:print-pretty-key #:print-pretty-key-event

   ;; hemlock-ext.lisp
   #:disable-clx-event-handling
   #:quit
   #:serve-event
   #:sap-ref-8
   #:make-object-set
   #:default-clx-event-handler
   #:serve-exposure
   #:serve-graphics-exposure
   #:serve-no-exposure
   #:serve-configure-notify
   #:serve-destroy-notify
   #:serve-unmap-notify
   #:serve-map-notify
   #:serve-reparent-notify
   #:serve-gravity-notify
   #:serve-circulate-notify
   #:serve-client-message
   #:serve-key-press
   #:serve-button-press
   #:serve-button-release
   #:serve-enter-notify
   #:serve-leave-notify
   #:flush-display-events
   #:object-set-event-handler
   #:with-clx-event-handling
   #:complete-file
   #:default-directory))

(defpackage :hemlock-internals
  (:use :common-lisp :hemlock-interface)
  (:nicknames :hi)
  (:shadow #:char-code-limit)
  (:import-from
   ;; gray streams
   #+EXCL  :excl
   #+CLISP :gray
   #+CMU   :ext
   #+sbcl  :sb-gray
   #+scl   :ext
   #+openmcl :gray
   ;;
   ;; Note the pacth i received from DTC mentions character-output and
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
  (:import-from :hemlock-ext
                #:delq #:memq #:assq)
  ;;
  (:export
   #:*FAST*                             ;hmm not sure about this one
   
   ;; rompsite.lisp
   #:show-mark #:editor-sleep #:*input-transcript* #:fun-defined-from-pathname
   #:editor-describe-function #:pause-hemlock #:store-cut-string
   #:fetch-cut-string #:schedule-event #:remove-scheduled-event
   #:enter-window-autoraise #:directoryp #:merge-relative-pathnames
   ;;
   ;; Export default-font to prevent a name conflict that occurs due to
   ;; the Hemlock variable "Default Font" defined in SITE-INIT below.
   ;;
   #:default-font
   #:*beep-function* #:beep

   ;; 
   #:mark #:mark-line #:mark-charpos #:markp #:region #:region-start #:region-end
   #:regionp #:buffer #:bufferp #:buffer-modes #:buffer-point #:buffer-writable
   #:buffer-delete-hook #:buffer-windows #:buffer-variables #:buffer-write-date
   #:region #:regionp #:region-start #:region-end #:window #:windowp #:window-height
   #:window-width #:window-display-start #:window-display-end #:window-point
   #:window-display-recentering #:commandp #:command #:command-function
   #:command-documentation #:modeline-field #:modeline-field-p

   ;; from input.lisp
   #:get-key-event #:unget-key-event #:clear-editor-input #:listen-editor-input
   #:*last-key-event-typed* #:*key-event-history*
   #:input-waiting #:last-key-event-cursorpos

   ;; from macros.lisp
   #:invoke-hook #:value #:setv #:hlet #:string-to-variable #:add-hook #:remove-hook
   #:defcommand #:with-mark #:use-buffer #:editor-error
   #:editor-error-format-string #:editor-error-format-arguments #:do-strings
   #:command-case #:reprompt #:with-output-to-mark #:with-input-from-region
   #:handle-lisp-errors #:with-pop-up-display #:*random-typeout-buffers*

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

   ;; bit-display.lisp
   #:redisplay #:redisplay-all

   ;; bit-screen.lisp
   #:make-xwindow-like-hwindow #:*create-window-hook* #:*delete-window-hook*
   #:*random-typeout-hook* #:*create-initial-windows-hook*

   ;; buffer.lisp
   #:buffer-modified #:buffer-region #:buffer-name #:buffer-pathname
   #:buffer-major-mode #:buffer-minor-mode #:buffer-modeline-fields
   #:buffer-modeline-field-p #:current-buffer #:current-point
   #:in-recursive-edit #:exit-recursive-edit #:abort-recursive-edit
   #:recursive-edit #:defmode #:mode-major-p #:mode-variables #:mode-documentation
   #:make-buffer #:delete-buffer #:with-writable-buffer #:buffer-start-mark
   #:buffer-end-mark #:*buffer-list*

   ;; charmacs.lisp
   #:syntax-char-code-limit #:search-char-code-limit #:do-alpha-chars

   ;; cursor.lisp
   #:mark-to-cursorpos #:center-window #:displayed-p #:scroll-window
   #:mark-column #:cursorpos-to-mark #:move-to-column

   ;; display.lisp
   #:redisplay #:redisplay-all

   ;; echo.lisp
   #:*echo-area-buffer* #:*echo-area-stream* #:*echo-area-window*
   #:*parse-starting-mark* #:*parse-input-region*
   #:*parse-verification-function* #:*parse-string-tables*
   #:*parse-value-must-exist* #:*parse-default* #:*parse-default-string*
   #:*parse-prompt* #:*parse-help* #:clear-echo-area #:message #:loud-message
   #:prompt-for-buffer #:prompt-for-file #:prompt-for-integer
   #:prompt-for-keyword #:prompt-for-expression #:prompt-for-string
   #:prompt-for-variable #:prompt-for-yes-or-no #:prompt-for-y-or-n
   #:prompt-for-key-event #:prompt-for-key #:*logical-key-event-names*
   #:logical-key-event-p #:logical-key-event-documentation
   #:logical-key-event-name #:logical-key-event-key-events
   #:define-logical-key-event #:*parse-type* #:current-variable-tables

   ;; files.lisp
   #:read-file #:write-file


   ;; font.lisp
   #:font-mark #:delete-font-mark #:delete-line-font-marks #:move-font-mark
   #:window-font

   ;; htext1.lisp
   #:line-length #:line-buffer #:line-string #:line-character #:mark #:mark-kind
   #:copy-mark #:delete-mark #:move-to-position #:region #:make-empty-region
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


   ;; htext4.lisp
   #:delete-characters #:delete-region #:delete-and-save-region #:copy-region
   #:filter-region


   ;; interp.lisp
   #:bind-key #:delete-key-binding #:get-command #:map-bindings
   #:make-command #:command-name #:command-bindings #:last-command-type
   #:prefix-argument #:exit-hemlock #:*invoke-hook* #:key-translation


   ;; main.lisp
   #:*global-variable-names* #:*mode-names* #:*buffer-names*
   #:*character-attribute-names* #:*command-names* #:*buffer-list*
   #:*window-list* #:*last-key-event-typed* #:after-editor-initializations

   ;; screen.lisp
   #:make-window #:delete-window #:next-window #:previous-window


   ;; search1.lisp
   #:search-pattern #:search-pattern-p #:find-pattern #:replace-pattern
   #:new-search-pattern


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

   ;; window.lisp
   #:current-window #:window-buffer #:modeline-field-width
   #:modeline-field-function #:make-modeline-field #:update-modeline-fields
   #:update-modeline-field #:modeline-field-name #:modeline-field
   #:editor-finish-output #:*window-list*

   ))


(defpackage :hemlock
  (:use :common-lisp :hemlock-interface :hi :hemlock-ext)
;;;  (:import-from :hemlock-ext #:delq #:memq #:assq)
;;;  (:import-from :hemlock-internals #:*fast*)
  (:shadowing-import-from #:hemlock-ext
			  #:char-code-limit)
  ;;  #+cmu
  ;; These are defined in EXTENSONS package in CMUCL
  (:shadowing-import-from :hemlock-ext
   #:*ALL-MODIFIER-NAMES*
   #:ASSQ
   #:CHAR-KEY-EVENT
   #:DEFAULT-CLX-EVENT-HANDLER
   #:DEFAULT-DIRECTORY
   #:DEFINE-CLX-MODIFIER
   #:DEFINE-KEY-EVENT-MODIFIER
   #:DEFINE-KEYSYM
   #:DEFINE-MOUSE-KEYSYM
   #:DELQ
   #:DISABLE-CLX-EVENT-HANDLING
   #:DO-ALPHA-KEY-EVENTS
   #:FILE-WRITABLE
   #:FIXNUMP
   #:FLUSH-DISPLAY-EVENTS
   #:KEY-EVENT
   #:KEY-EVENT-BIT-P
   #:KEY-EVENT-BITS
   #:KEY-EVENT-BITS-MODIFIERS
   #:KEY-EVENT-CHAR
   #:KEY-EVENT-KEYSYM
   #:KEY-EVENT-MODIFIER-MASK
   #:KEY-EVENT-P
   #:KEYSYM-NAMES
   #:KEYSYM-PREFERRED-NAME
   #:MAKE-KEY-EVENT
   #:MAKE-KEY-EVENT-BITS
   #:MEMQ
   #:NAME-KEYSYM
   #:OBJECT-SET-EVENT-HANDLER
   #:PRINT-PRETTY-KEY
   #:PRINT-PRETTY-KEY-EVENT
   #:QUIT
   #:SERVE-BUTTON-PRESS
   #:SERVE-BUTTON-RELEASE
   #:SERVE-CIRCULATE-NOTIFY
   #:SERVE-CLIENT-MESSAGE
   #:SERVE-CONFIGURE-NOTIFY
   #:SERVE-DESTROY-NOTIFY
   #:SERVE-ENTER-NOTIFY
   #:SERVE-EXPOSURE
   #:SERVE-GRAPHICS-EXPOSURE
   #:SERVE-GRAVITY-NOTIFY
   #:SERVE-KEY-PRESS
   #:SERVE-LEAVE-NOTIFY
   #:SERVE-MAP-NOTIFY
   #:SERVE-NO-EXPOSURE
   #:SERVE-REPARENT-NOTIFY
   #:SERVE-UNMAP-NOTIFY

   ;; These four are from SYSTEM package
   #:MAKE-OBJECT-SET
   #:SAP-REF-8
   #:SERVE-EVENT
   #:WITHOUT-INTERRUPTS

   #:TRANSLATE-KEY-EVENT
   #:TRANSLATE-MOUSE-KEY-EVENT
   #:WITH-CLX-EVENT-HANDLING)
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
