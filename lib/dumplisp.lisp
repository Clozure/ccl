;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

; Dumplisp.lisp

(in-package "CCL")

(defvar *save-exit-functions* nil 
  "List of (0-arg)functions to call before saving memory image")

(defvar *restore-lisp-functions* nil
  "List of (0-arg)functions to call after restoring saved image")


(declaim (special *lisp-system-pointer-functions*)) ; defined in l1-init.

(defun kill-lisp-pointers ()
  (setq * nil ** nil *** nil + nil ++ nil +++ nil - nil
        / nil // nil /// nil
         @ nil)
  (clear-open-file-streams)
  (setf (*%saved-method-var%*) nil)
  (setq *%periodic-tasks%* nil)
  (setq *event-dispatch-task* nil)
  (setq *interactive-abort-process* nil)
  )

(defun clear-ioblock-streams ()
  (%map-areas (lambda (o)
                  (if (typep o 'basic-stream)
                    (let ((s (basic-stream.state o)))
                      (when (and (typep s 'ioblock)
                                 (ioblock-device s)
                                 (>= (ioblock-device s) 0))
                        (setf (basic-stream.state o) nil)))
                    ;; Have to be careful with use of TYPEP here (and
                    ;; in the little bit of Lisp code that runs before
                    ;; the image is saved.)  We may have just done
                    ;; things to forget about (per-session) foreign
                    ;; class addresses, and calling TYPEP on a pointer
                    ;; to such a class might cause us to remember
                    ;; those per-session addresses and confuse the
                    ;; startup code.
                    (if (and (eql (typecode o) target::subtag-instance)
                             (typep o 'buffered-stream-mixin)
                             (slot-boundp o 'ioblock))
                      (let ((s (slot-value o 'ioblock)))
                        (when (and (typep s 'ioblock)
                                   (ioblock-device s)
                                   (>= (ioblock-device s) 0))
                          (setf (slot-value o 'ioblock) nil))))))))

(defun save-application (filename
                         &rest rest
                         &key toplevel-function
			 init-file
                         error-handler application-class
			 clear-clos-caches
                         (purify t)
                         impurify
			 (mode #o644)
			 prepend-kernel
			 #+windows-target (application-type :console)
                         native)
  (declare (ignore toplevel-function error-handler application-class
                   clear-clos-caches init-file impurify))
  #+windows-target (check-type application-type (member :console :gui))
  (let ((path (translate-logical-pathname filename)))
    (unless (probe-file (make-pathname
                         :defaults nil
                         ;; Needed in Windows
                         :device (pathname-device path)
                         :directory (pathname-directory path)))
      (error "Directory containing ~s does not exist." filename)))
  (let* ((kind (%unix-file-kind (defaulted-native-namestring filename))))
    (when (and kind (not (eq kind :file )))
      (error "~S is not a regular file." filename)))
  (let* ((watched (watch)))
    (when watched
      (cerror "Un-watch them." "There are watched objects.")
      (mapc #'unwatch watched)))
  (when (and native prepend-kernel)
    (error "~S and ~S can't both be specified (yet)." :native :prepend-kernel))
  (let* ((ip *initial-process*)
	 (cp *current-process*))
    (when (process-verify-quit ip)
      (let* ((fd (open-dumplisp-file filename
                                     :mode mode
                                     :prepend-kernel prepend-kernel
                                     #+windows-target  #+windows-target 
                                     :application-type application-type)))
        (when native
          #+(or darwinx8632-target darwinx8664-target) (setq fd (- fd))
          #-(or darwinx8632-target darwinx8664-target)
          (progn
            (warn "native image support not available, ignoring ~s option." :native)))
            
        (process-interrupt ip
                           #'(lambda ()
                               (process-exit-application
                                *current-process*
                                #'(lambda ()
                                    (apply #'%save-application-internal
                                           fd
                                           :purify purify
                                           rest))))))
      (unless (eq cp ip)
	(process-kill cp)))))

(defun %save-application-internal (fd &key
                                      toplevel-function ;???? 
                                      error-handler ; meaningless unless application-class or *application* not lisp-development..
                                      application-class
                                      mode
                                      (purify t)
                                      (impurify nil)
                                      (init-file nil init-file-p)
                                      (clear-clos-caches t)
                                      prepend-kernel
                                      #+windows-target application-type
                                      native)
  (declare (ignore mode prepend-kernel #+windows-target application-type native))
  (when (and application-class (neq  (class-of *application*)
                                     (if (symbolp application-class)
                                       (find-class application-class)
                                       application-class)))
    (setq *application* (make-instance application-class)))
  (if (not toplevel-function)
    (setq toplevel-function 
          #'(lambda ()
              (toplevel-function *application*
				 (if init-file-p
				   init-file
				   (application-init-file *application*)))))
    (let* ((user-toplevel-function (coerce-to-function toplevel-function)))
      (setq toplevel-function
            (lambda ()
              (make-mcl-listener-process
               "toplevel"
                *stdin*
                *stdout*
                 #'false
               :initial-function (lambda ()
                                   (catch :toplevel
                                     (funcall user-toplevel-function)
                                     (quit)))
               :close-streams nil
               )
              (%set-toplevel #'housekeeping-loop)
              (toplevel)))))
  (when error-handler
    (make-application-error-handler *application* error-handler))
  
  (if clear-clos-caches (clear-clos-caches))
  (save-image #'(lambda () (%save-application fd
                                              (logior (if impurify 2 0)
                                                      (if purify 1 0))))
              toplevel-function))

(defun save-image (save-function toplevel-function)
  (let ((toplevel #'(lambda () (#_exit -1))))
      (%set-toplevel #'(lambda ()
                         (setf (interrupt-level) -1)
                         (%set-toplevel toplevel)       ; in case *save-exit-functions* error
                         (dolist (f *save-exit-functions*)
                           (funcall f))
                         (kill-lisp-pointers)
                         (clear-ioblock-streams)
                         (with-deferred-gc
                             (let* ((pop *termination-population*))
                               (with-lock-grabbed (*termination-population-lock*)
                                 (setf (population.data pop) nil
                                       (population.termination-list pop) nil))))
                         (%set-toplevel
                          #'(lambda ()
                              (%set-toplevel #'(lambda ()
                                                 (setf (interrupt-level) 0)
                                                 (funcall toplevel-function)))
                              (restore-lisp-pointers)))   ; do startup stuff
                         (funcall save-function)))
      (toplevel)))

;;; If file in-fd contains an embedded lisp image, return the file position
;;; of the start of that image; otherwise, return the file's length.
(defun skip-embedded-image (in-fd)
  (let* ((len (fd-lseek in-fd 0 #$SEEK_END)))
    (if (< len 0)
      (%errno-disp len)
      (%stack-block ((trailer 16))
	(let* ((trailer-pos (fd-lseek in-fd -16 #$SEEK_CUR)))
	  (if (< trailer-pos 0)
	    len
	    (if (not (= 16 (the fixnum (fd-read in-fd trailer 16))))
	      len
	      (if (not (dotimes (i 12 t)
			 (unless (eql (char-code (schar #+big-endian-target "OpenMCLImage"
                                                        #+little-endian-target "nepOILCMegam" i))
				      (%get-unsigned-byte trailer i))
			   (return nil))))
		len
		(let* ((header-pos (fd-lseek in-fd
					     (%get-signed-long
					      trailer
					      12)
					     #$SEEK_CUR)))
		  (if (< header-pos 0)
		    len
                    #+32-bit-target
		    header-pos
                    #-32-bit-target
                    (%stack-block ((header 64))
                      (if (or (/= (fd-read in-fd header 64) 64)
                              (not (dotimes (i 12 t)
                                     (unless (eql (%get-unsigned-byte trailer i)
                                                  (%get-unsigned-byte header i))
                                       (return)))))
                        len
                        (+ header-pos
                           (logior (ash (%get-signed-long header 36) 32)
                                   (%get-unsigned-long header 40)))))))))))))))
                        

		  
;;; Note that Windows executable files are in what they call "PE"
;;; (= "Portable Executable") format, not to be confused with the "PEF"
;;; (= "PowerPC Executable Format" or "Preferred Executable Format")
;;; executable format that Apple used on Classic MacOS.
(defun %prepend-file (out-fd in-fd len #+windows-target application-type)
  (declare (fixnum out-fd in-fd len))
  (fd-lseek in-fd 0 #$SEEK_SET)
  (let* ((bufsize (ash 1 15))
         #+windows-target (first-buf t))
    (%stack-block ((buf bufsize))
      (loop
	  (when (zerop len) (return))
	  (let* ((nread (fd-read in-fd buf (min len bufsize))))
	    (declare (fixnum nread))
	    (if (< nread 0)
	      (%errno-disp nread))
            #+windows-target
            (when (shiftf first-buf nil)
              (let* ((application-byte (ecase application-type
                                         (:console #$IMAGE_SUBSYSTEM_WINDOWS_CUI)
                                         (:gui #$IMAGE_SUBSYSTEM_WINDOWS_GUI)))
                     (offset (%get-long buf (get-field-offset #>IMAGE_DOS_HEADER.e_lfanew))))
                (assert (< offset bufsize) () "PE header not within first ~D bytes" bufsize)
                (assert (= (%get-byte buf (+ offset 0)) (char-code #\P)) ()
                        "File does not appear to be a PE file")
                (assert (= (%get-byte buf (+ offset 1)) (char-code #\E)) ()
                        "File does not appear to be a PE file")
                (assert (= (%get-byte buf (+ offset 2)) 0) ()
                        "File does not appear to be a PE file")
                (assert (= (%get-byte buf (+ offset 3)) 0) ()
                        "File does not appear to be a PE file")
                ;; File is a PE file -- Windows subsystem byte goes at offset 68 in the
                ;;  "optional header" which appears right after the standard header (20 bytes)
                ;;  and the PE cookie (4 bytes)
                (setf (%get-byte buf (+ offset 4 (record-length #>IMAGE_FILE_HEADER) (get-field-offset #>IMAGE_OPTIONAL_HEADER.Subsystem) )) application-byte)))
            (let* ((nwritten (fd-write out-fd buf nread)))
	      (declare (fixnum nwritten))
	      (unless (= nwritten nread)
		(error "I/O error writing to fd ~d" out-fd)))
	    (decf len nread))))))



(defun kernel-path ()
  (let* ((p (%null-ptr)))
    (declare (dynamic-extent p))
    (%get-kernel-global-ptr 'kernel-path p)
    (if (%null-ptr-p p)
      (%realpath (car *command-line-argument-list*))
      (let* ((string (%get-utf-8-cstring p)))
        #+windows-target (nbackslash-to-forward-slash string)
        #+darwin-target (precompose-simple-string string)
        #-(or windows-target darwin-target) string))))


(defun open-dumplisp-file (path &key (mode #o666) prepend-kernel
                           #+windows-target application-type)
  (let* ((prepend-path (if prepend-kernel
                         (if (eq prepend-kernel t)
                           (kernel-path)
                           (defaulted-native-namestring
                            (pathname prepend-kernel)))))
         (prepend-fd (if prepend-path (fd-open prepend-path #$O_RDONLY)))
	 (prepend-len (if prepend-kernel
                        (if (and prepend-fd (>= prepend-fd 0))
                          (skip-embedded-image prepend-fd)
                          (signal-file-error prepend-fd prepend-path))))
	 (filename (defaulted-native-namestring path)))
    (when (probe-file filename)
      (%delete-file filename))
    (when prepend-fd
      ;; Copy the execute mode bits from the prepended "kernel".
      (let ((prepend-fd-mode (nth-value 1 (%fstat prepend-fd))))
	(setq mode (logior (logand prepend-fd-mode #o111) mode))))
    (let* ((image-fd (fd-open filename (logior #$O_WRONLY #$O_CREAT) mode)))
      (unless (>= image-fd 0) (signal-file-error image-fd filename))
      (when prepend-fd
	(%prepend-file image-fd prepend-fd prepend-len #+windows-target application-type))
      (fd-chmod image-fd mode)
      image-fd)))


(defun %save-application (fd &optional (flags 1))
  (let* ((err (%%save-application flags fd)))
    (unless (eql err 0)
      (%err-disp err))))
  

(defun restore-lisp-pointers ()
  (setq *interactive-streams-initialized* nil)
  (setq *heap-ivectors* nil)
  (setq *batch-flag* (not (eql (%get-kernel-global 'batch-flag) 0)))
  (%revive-system-locks)
  (refresh-external-entrypoints)
  (restore-pascal-functions)
  (initialize-interactive-streams)
  (let ((system-ptr-fns (reverse *lisp-system-pointer-functions*))
        (restore-lisp-fns *restore-lisp-functions*)
        (user-pointer-fns *lisp-user-pointer-functions*)
        (lisp-startup-fns *lisp-startup-functions*))
    (unwind-protect
      (with-simple-restart (abort "Abort (possibly crucial) startup functions.")
        (let ((call-with-restart
               #'(lambda (f)
                   (with-simple-restart 
                     (continue "Skip (possibly crucial) startup function ~s."
                               (if (symbolp f) f (function-name f)))
                     (funcall f)))))
          (dolist (f system-ptr-fns) (funcall call-with-restart f))
          (dolist (f restore-lisp-fns) (funcall call-with-restart f))
          (dolist (f (reverse user-pointer-fns)) (funcall call-with-restart f))
          (dolist (f (reverse lisp-startup-fns)) (funcall call-with-restart f))))
      (setf (interrupt-level) 0)))
  nil)


(defun restore-pascal-functions ()
  (reset-callback-storage)
  (when (simple-vector-p %pascal-functions%)
    (dotimes (i (length %pascal-functions%))
      (let ((pfe (%svref %pascal-functions% i)))
        (when (vectorp pfe)
          (let* ((name (pfe.sym pfe))
		 (descriptor (pfe.routine-descriptor pfe)))
	    (%revive-macptr descriptor)
	    (%setf-macptr descriptor (make-callback-trampoline i (pfe.proc-info pfe)))
            (when name
              (set name descriptor))))))))

