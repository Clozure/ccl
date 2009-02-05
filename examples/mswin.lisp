;;;-*-Mode: LISP; Package: ccl -*-
;;;
;;;   Copyright (C) 2008, Clozure Associates and contributors
;;;   This file is part of OpenMCL.  
;;;
;;;   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with OpenMCL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   OpenMCL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

;;; This is very preliminary and very much a work-in-progress.

(in-package "CL-USER")

;;; This is a simple demo that creates an almost totally uninteresting
;;; window and does limited event handling.  It's intended to excercise
;;; Clozure CL's FFI a little and to serve as a proof of the concept
;;; that Windows GUI programming is possible.  It's not necessarily
;;; a good way to create or manage a window or a good way to structure
;;; a program or much of anything else ...

(eval-when (:compile-toplevel :load-toplevel :execute)
  (open-shared-library "user32.dll"))

;;; This function is called by Windows to process events ("messages")
;;; for a given class of window.  (It's specified when the window
;;; class is registered; see below.)  Note that - on Win32 -most
;;; Windows API functions and the functions that they call follow an
;;; unusual calling sequence in which the callee pops its arguments
;;; off of the stack before returning; this is indicated by the
;;; keyword :DISCARD-STACK-ARGS in the DEFCALLBACK argument list.

(defcallback simple-wndproc (;; See comment above.
                             :discard-stack-args
                             ;: the window to which this message is directed,
                             ;; of a null pointer if the message isn't
                             ;; directed to a window:
                             #>HWND hwnd
                             ;; An integer which idenfifies the type
                             ;; of message, usually matching a
                             ;; predefined constant whose name starts
                             ;; with WM_ :
                             #>UINT msg
                             ;; A parameter to the message.  In 16-bit
                             ;; Windows, this was a 16-bit integer and
                             ;; therefore couldn't convey much information.
                             ;; On 32 and 64-bit Windows, it's a 32-bit
                             ;; integer and therefore -could- contain
                             ;; more information, but usually doesn't ...
                             ;; Its value (and how it should be interpreted)
                             ;; depend on the value of "msg":
                             #>WPARAM wparam
                             ;; Another parameter, as wide as a pointer
                             ;; is (and sometimes used as a pointer.):
                             #>LPARAM lparam
                             ;; The callback function should return a
                             ;; pointer-sized (32/64-bit) integer: 0
                             ;; to indicate that the message was
                             ;; handled, and non-zero otherwise.
                             #>LRESULT)
  ;; At a bare minimum, a windows message procedure (wndproc) like this
  ;; one should handle the the WM_DESTROY message; if we actually did
  ;; any drawing of the window's content, we should handle WM_PAINT
  ;; messages, ...
  ;; There are a fairly large number of other message constants that
  ;; we could receive; if we don't want to handle them ourselves - and
  ;; in this simple example, we don't - we can just pass all of our
  ;; arguments to the default window procedure and return whatever it
  ;; returns.
  #+debug (format t "~& hwnd = ~s, msg = ~s, wparam = ~s, lparam = ~x"
                  hwnd msg wparam lparam)
  (cond ((eql msg #$WM_DESTROY)
         ;; If there were resources attached to the window - bitmaps,
         ;; etc. - we'd want to free them here.  Posting a quit message
         ;; (WM_QUIT) will arrange that we receive a WM_QUIT messsage
         ;; in very short order.
         (#_PostQuitMessage 0)          ; exit status 0: all is well.
         0)                             ; and we'll return 0
        (t
         ;; In a more realistic example, we'd handle more cases here.
         ;; Like many functions that deal with characters and strings,
         ;; DefWindowProc is actually implemented as two functions:
         ;; DefWindowProcA is the "ANSI" (8-bit character) version,
         ;; and DefWindowProcW is the wide-character (UTF-16LE)
         ;; version.
         (#_DefWindowProcA hwnd msg wparam lparam))))
                             

;;; Register a named window class. ("class" in this sense has nothing to
;;; do with CLOS or any other object system: windows of the same class
;;; share a common window procedure callback and other attributes, which
;;; we define here.)
;;; If the registration attempt is succesful, it'll return an "atom"
;;; (a small integer that identifies the registered class); otherwise,
;;;  it returns 0.
(defvar *simple-window-class-atom* nil)

(defun register-simple-window-class (class-name)
  ;; We'll use an ANSI version that accepts a simple C string as the
  ;; class name.
  (or *simple-window-class-atom*
      (setq *simple-window-class-atom*
            (with-cstrs ((cname class-name))
              (rlet ((wc #>WNDCLASSEX)) ; an "extended" WNDCLASS structure
                (setf (pref wc #>WNDCLASSEX.cbSize) (ccl::record-length #>WNDCLASSEX)
                      (pref wc #>WNDCLASSEX.style) (logior #$CS_HREDRAW #$CS_VREDRAW)
                      (pref wc #>WNDCLASSEX.lpfnWndProc) simple-wndproc
                      (pref wc #>WNDCLASSEX.cbClsExtra) 0
                      (pref wc #>WNDCLASSEX.cbWndExtra) 0
                      (pref wc #>WNDCLASSEX.hInstance) (#_GetModuleHandleA (%null-ptr))
                      (pref wc #>WNDCLASSEX.hIcon) (%null-ptr)
                      (pref wc #>WNDCLASSEX.hCursor) (#_LoadCursorA (%null-ptr) #$IDC_ARROW)
                      (pref wc #>WNDCLASSEX.hbrBackground) (#_GetStockObject #$WHITE_BRUSH)
                      (pref wc #>WNDCLASSEX.lpszMenuName) (%null-ptr)
                      (pref wc #>WNDCLASSEX.lpszClassName) cname
                      (pref wc #>WNDCLASSEX.hIconSm) (%null-ptr))
                (let* ((atom (#_RegisterClassExA wc)))
                  (if (eql 0 atom)
                    (let* ((err (#_GetLastError)))
                      (error "Error registering windows class ~s: ~d (~a)" class-name
                             err
                             (ccl::%windows-error-string err))))
                  atom))))))

;;; Main function: register a window class, make an instance of that
;;; class, handle events for that window until it's closed.
(defun make-simple-ms-window ()
  (let* ((class-atom (register-simple-window-class "very-simple")))
    (with-cstrs ((title "Look! A window!"))
      (let* ((hwnd (#_CreateWindowExA 0 ;extended style
                                      (%int-to-ptr class-atom) ; class name/atom
                                      title 
                                      (logior #$WS_EX_COMPOSITED #$WS_OVERLAPPEDWINDOW) ; style
                                      #$CW_USEDEFAULT ; x pos
                                      #$CW_USEDEFAULT ; y pos
                                      #$CW_USEDEFAULT ; width
                                      #$CW_USEDEFAULT ; height
                                      (%null-ptr) ;parent window
                                      (%null-ptr) ; menu handle
                                      (#_GetModuleHandleA (%null-ptr)) ; us
                                      (%null-ptr)))) ;info for MDI parents/children
        (when (%null-ptr-p hwnd)
          (error "CreateWindow failed: ~a" (ccl::%windows-error-string (#_GetLastError))))
	;; Depending on how the lisp process was created, the first call
	;; to #_ShowWindow in that process might ignore its argument
	;; (and instead use an argument specified in the STARTUPINFO
	;; structure passed to #_CreateProcess.)  SLIME under FSF Emacs
	;; runs the lisp with this flag set, and it's possible to waste
	;; a week or two trying to track this down.  (Trust me.)
        (#_ShowWindow hwnd #$SW_SHOW)
	;; In case the parent process said to ignore #_ShowWindow's argument
	;; the first time it's called, call #_ShowWindow again.  This seems
	;; to be harmless, if a little strange ...
        (#_ShowWindow hwnd #$SW_SHOW)
        (#_UpdateWindow hwnd)
        ;; Loop, fetching messages, translating virtual key events
        ;; into character-oriented events and dispatching each
        ;; message until #_GetMessageA returns 0.
        (rlet ((msg #>MSG))
          (do* ((result (#_GetMessageA msg
                                       (%null-ptr) ; for any window created by this thread)
                                       0
                                       0)
                        (#_GetMessageA msg (%null-ptr) 0 0)))
               ((eql 0 result)          ; message was WM_QUIT
                (pref msg #>MSG.wParam))
	    (cond ((< result 0)
		   (let* ((error (#_GetLastError)))
		     (format t "~& GetMessage: error = ~d (~a)" error
			     (ccl::%windows-error-string error)))
		   (return))
		  (t
		   (#_TranslateMessage msg)
		   (#_DispatchMessageA msg)))))))))
                                      
        
#||

;;; At the moment, attempts to create a window when running under SLIME
;;; fail for unknown reasons.  If those reasons have anything to do with
;;; the lisp process's "WindowStation" or the current thread's "Desktop"
;;; objects, these functions (which return information about those objects)
;;; may be useful.

(defun get-ws-info (ws)
  (rlet ((flags #>USEROBJECTFLAGS))
    (unless (eql 0 (#_GetUserObjectInformationA ws #$UOI_FLAGS flags (ccl::record-length #>USEROBJECTFLAGS) (%null-ptr)))
      (pref flags #>USEROBJECTFLAGS.dwFlags))))

;;; This only works on Vista or later.
(defun get-desktop-info (desktop)
  (rlet ((pbool #>BOOLEAN #$false))
    (if (eql 0 (#_GetUserObjectInformationA desktop 6 pbool (ccl::record-length #>BOOLEAN) (%null-ptr)))
      (ccl::%windows-error-string (#_GetLastError))
      (pref pbool #>BOOLEAN))))

(defun get-ui-object-name (handle)
  (%stack-block ((name 1000))
    (unless (eql 0 (#_GetUserObjectInformationA handle #$UOI_NAME name 1000 (%null-ptr)))
      (%get-cstring name))))
||#