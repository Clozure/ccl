;;; Copyright 2009 Clozure Associates
;;; This file is part of Clozure CL.  
;;;
;;; Clozure CL is licensed under the terms of the Lisp Lesser GNU
;;; Public License , known as the LLGPL and distributed with Clozure
;;; CL as the file "LICENSE".  The LLGPL consists of a preamble and
;;; the LGPL, which is distributed with Clozure CL as the file "LGPL".
;;; Where these conflict, the preamble takes precedence.
;;;
;;; Clozure CL is referenced in the preamble as the "LIBRARY."
;;;
;;; The LLGPL is also available online at
;;; http://opensource.franz.com/preamble.html

(defstruct mach-o-file
  header
  load-commands
  segments
  symbols
  strings)

(defmethod print-object ((m mach-o-file) stream)
  (print-unreadable-object (m stream :type t :identity t)))


(defstruct mach-o-string-table
  (hash (make-hash-table :test #'equal))
  (string (make-array 100 :element-type '(unsigned-byte 8) :fill-pointer 1 :adjustable t)))

(defstruct mach-o-symbol
  string-index
  type
  sect
  desc
  value)

(defun init-mach-o-string-table (fd symtab-command)
  (fd-lseek fd (pref symtab-command #>symtab_command.stroff) #$SEEK_SET)
  (let* ((strsize (pref symtab-command #>symtab_command.strsize))
         (nbytes (+ strsize strsize))
         (bytes (make-array nbytes :element-type '(unsigned-byte 8)))
         (out 0))
    (declare (fixnum nbytes strsize out))
    (%stack-block ((buf 32768))
      (do* ((n strsize))
           ((= n 0))
        (let* ((bufsize (fd-read fd buf (min n 32768))))
          (%copy-ptr-to-ivector buf 0 bytes out bufsize)
          (incf out bufsize)
          (decf n bufsize))))
    (make-mach-o-string-table
     :string (make-array nbytes
                         :element-type '(unsigned-byte 8)
                         :displaced-to bytes
                         :fill-pointer strsize
                         :adjustable t))))

(defun init-mach-o-symbols64 (fd symtab-command)
  (fd-lseek fd (pref symtab-command #>symtab_command.symoff) #$SEEK_SET)
  (rlet ((nlist #>nlist_64))
    (let* ((nsyms (pref symtab-command #>symtab_command.nsyms))
           (nentries (* nsyms 2))
           (vec (make-array nentries)))
      (declare (fixnum nsyms nentries))
      (flet ((read-nlist ()
               (fd-read fd nlist (record-length #>nlist_64))
               (make-mach-o-symbol :string-index (pref nlist #>nlist_64.n_un.n_strx)
                                   :type (pref nlist #>nlist_64.n_type)
                                   :sect (pref nlist #>nlist_64.n_sect)
                                   :desc (pref nlist #>nlist_64.n_desc)
                                   :value (pref nlist #>nlist_64.n_value))))
        (dotimes (i nsyms (make-array nentries
                                      :displaced-to vec
                                      :fill-pointer nsyms
                                      :adjustable t))
          (setf (svref vec i) (read-nlist)))))))
    

(defun read-header-and-load-commands64 (fd)
  (fd-lseek fd 0 #$SEEK_SET)
  (let* ((mh (make-record :mach_header_64))
         (mach-o (make-mach-o-file :header mh)))
    (when (= (fd-read fd mh (record-length :mach_header_64))
             (record-length :mach_header_64))
      (collect ((commands))
        (flet ((read-command ()
                 (rlet ((cmd :load_command))
                   (fd-read fd cmd (record-length :load_command))
                   (let* ((n (pref cmd :load_command.cmdsize))
                          (p (#_malloc n))
                          (q (%inc-ptr p (record-length :load_command))))
                     (#_memcpy p cmd (record-length :load_command))
                     (fd-read fd q (- n (record-length :load_command)))
                     (let* ((lcmd (pref cmd :load_command.cmd))
                            (ftype 
                             (cond ((= lcmd #$LC_SEGMENT_64)
                                    (load-record #>segment_command_64))
                                   ((= lcmd #$LC_SYMTAB)
                                    (load-record #>symtab_command))
                                   ((= lcmd #$LC_DYSYMTAB)
                                    (load-record #>dysymtab_command))
                                   ((= lcmd #$LC_LOAD_DYLINKER)
                                    (load-record #>dylinker_command))
                                   ((= lcmd #$LC_UUID)
                                    (load-record #>uuid_command))
                                   ((= lcmd #$LC_LOAD_DYLIB)
                                    (load-record #>dylib_command))
                                   ((= lcmd #$LC_UNIXTHREAD)
                                    (load-record #>thread_command)))))

                       (if ftype
                         (%set-macptr-type p (foreign-record-type-ordinal ftype))
                         (format t "~&~x" lcmd)))
                     p))))
          (dotimes (i (pref mh :mach_header_64.ncmds))
            (commands (read-command)))
          (setf (mach-o-file-load-commands mach-o) (commands))
          (dolist (cmd (mach-o-file-load-commands mach-o))
            (when (= #$LC_SYMTAB (pref cmd #>load_command.cmd))
              (setf (mach-o-file-strings mach-o)
                    (init-mach-o-string-table fd cmd)
                    (mach-o-file-symbols mach-o)
                    (init-mach-o-symbols64 fd cmd))))
          mach-o)))))

(defun mach-o-string-index (mo string)
  (let* ((bytes (make-array (the fixnum (+ (length string) 2)) :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent bytes))
    (dotimes (i (length string))
      (setf (aref bytes (1+ i)) (char-code (char string i))))
    (let* ((pos (search bytes (mach-o-string-table-string (mach-o-file-strings mo)))))
      (when pos (1+ pos)))))
              
