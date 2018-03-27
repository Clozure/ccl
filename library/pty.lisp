;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;; Copyright 2002-2009 Clozure Associates
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

;;; (very) preliminary support for dealing with TTYs (and PTYs).

(in-package "CCL")

;;; Open a (connected) pair of pty file descriptors, such that anything
;;; written to one can be read from the other.
#+linuxppc-target
(eval-when (:load-toplevel :execute)
  (open-shared-library "libutil.so"))

(defun open-pty-pair ()
  (rlet ((alphap :unsigned 0)
	 (betap :unsigned 0))
    (let* ((status (#_openpty alphap betap (%null-ptr) (%null-ptr) (%null-ptr))))
      (if (eql status 0)
	(values (pref alphap :unsigned) (pref betap :unsigned))
	(%errno-disp (%get-errno))))))


(defun %get-tty-attributes (tty-fd &optional control-chars)
  (if (and control-chars
	   (not (and (typep control-chars 'simple-string)
		     (= (length control-chars) #$NCCS))))
    (report-bad-arg control-chars '(or null (simple-string #.#$NCCS))))
  (rlet ((attr :termios))
    (let* ((result (#_tcgetattr tty-fd attr)))
      (if (< result 0)
	(values nil nil nil nil nil nil nil)
	(progn
	  (if control-chars
            (%str-from-ptr (pref attr :termios.c_cc) #$NCCS control-chars))
	  (values
	   (pref attr :termios.c_iflag)
	   (pref attr :termios.c_oflag)
	   (pref attr :termios.c_cflag)
	   (pref attr :termios.c_lflag)
	   #+darwin-target 0
	   #-darwin-target
	   (pref attr :termios.c_line)
	   control-chars
	   (pref attr :termios.c_ispeed)
	   (pref attr :termios.c_ospeed)))))))

(defun %set-tty-attributes (tty &key
				input-modes
				output-modes
				control-modes
				local-modes
				control-chars
				input-speed
				output-speed)
  (if (and control-chars
	   (not (and (typep control-chars 'simple-string)
		     (= (length control-chars) #$NCCS))))
    (report-bad-arg control-chars '(or null (simple-string #.#$NCCS))))
  (rlet ((attr :termios))
	(let* ((get-ok (#_tcgetattr tty attr))
	       (write-back nil))
	  (when (eql 0 get-ok)
	    (when input-modes
	      (setf (pref attr :termios.c_iflag) input-modes)
	      (setq write-back t))
	    (when output-modes
	      (setf (pref attr :termios.c_oflag) output-modes)
	      (setq write-back t))
	    (when control-modes
	      (setf (pref attr :termios.c_cflag) control-modes)
	      (setq write-back t))
	    (when local-modes
	      (setf (pref attr :termios.c_lflag) local-modes)
	      (setq write-back t))
	    (when control-chars
              (%cstr-pointer control-chars (pref attr :termios.c_cc) nil)
	      (setq write-back t))
	    (when input-speed
	      (setf (pref attr :termios.c_ispeed) input-speed)
	      (setq write-back t))
	    (when output-speed
	      (setf (pref attr :termios.c_ospeed) output-speed)
	      (setq write-back t))
	    (and write-back
		 (eql 0 (#_tcsetattr tty #$TCSAFLUSH attr)))))))

(defun enable-tty-input-modes (tty mask)
  (let* ((old (nth-value 0 (%get-tty-attributes tty))))
    (when old
      (%set-tty-attributes tty :input-modes (logior old mask)))))

(defun disable-tty-input-modes (tty mask)
  (let* ((old (nth-value 0 (%get-tty-attributes tty))))
    (when old
      (%set-tty-attributes tty :input-modes (logand old (lognot mask))))))

(defun enable-tty-output-modes (tty mask)
  (let* ((old (nth-value 1 (%get-tty-attributes tty))))
    (when old
      (%set-tty-attributes tty :output-modes (logior old mask)))))

(defun disable-tty-output-modes (tty mask)
  (let* ((old (nth-value 1 (%get-tty-attributes tty))))
    (when old
      (%set-tty-attributes tty :output-modes (logand old (lognot mask))))))

(defun enable-tty-control-modes (tty mask)
  (let* ((old (nth-value 2 (%get-tty-attributes tty))))
    (when old
      (%set-tty-attributes tty :control-modes (logior old mask)))))

(defun disable-tty-control-modes (tty mask)
  (let* ((old (nth-value 2 (%get-tty-attributes tty))))
    (when old
      (%set-tty-attributes tty :control-modes (logand old (lognot mask))))))

(defun enable-tty-local-modes (tty mask)
  (let* ((old (nth-value 3 (%get-tty-attributes tty))))
    (when old
      (%set-tty-attributes tty :local-modes (logior old mask)))))

(defun disable-tty-local-modes (tty mask)
  (let* ((old (nth-value 3 (%get-tty-attributes tty))))
    (when old
      (%set-tty-attributes tty :local-modes (logand old (lognot mask))))))

(defun set-tty-raw (tty)
  (rlet ((attr :termios))
    (#_tcgetattr tty attr)
    (#_cfmakeraw attr)
    (eql 0 (#_tcsetattr tty #$TCSAFLUSH attr))))
