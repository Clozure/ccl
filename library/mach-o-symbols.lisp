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

(in-package "CCL")

;;; String tables: used both for symbol names and for section names.
(defstruct mach-o-string-table
  (hash (make-hash-table :test #'equal))
  (string (make-array 100 :element-type '(unsigned-byte 8) :fill-pointer 1 :adjustable t)))

;;; Collect info about Mach-O symbols.
(defstruct mach-o-symbol-table
  (strings (make-mach-o-string-table))
  data                                  ; foreign pointer
  nsyms
  )

(defun mach-o-lisp-function-name (f)
  (let* ((name (format nil "~s" f)))
    (subseq (nsubstitute #\0 #\# (nsubstitute #\. #\Space name)) 1)))

(defun mach-o-register-string (string table)
  (let* ((hash (mach-o-string-table-hash table))
         (s (mach-o-string-table-string table)))
    (when (gethash string hash)
      (format t "~& duplicate: ~s" string))
    (or (gethash string hash)
        (setf (gethash string hash)
              (let* ((n (length s)))
                (dotimes (i (length string) (progn (vector-push-extend 0 s) n))
                  (let* ((code (char-code (char string i))))
                    (declare (type (mod #x110000 code)))
                    (if (> code 255)
                      (vector-push-extend (char-code #\sub) s)
                      (vector-push-extend code s)))))))))

(defun readonly-area-bounds ()
  (ccl::do-gc-areas (a)
    (when (eql (ccl::%fixnum-ref a target::area.code)
	       ccl::area-readonly)
      (return
	(values (ash (ccl::%fixnum-ref a target::area.low) target::fixnumshift)
		(ash (ccl::%fixnum-ref a target::area.active) target::fixnumshift))))))

#+ppc-target
(defun collect-mach-o-static-functions ()
  (purify)
  (multiple-value-bind (readonly-low readonly-high)
      (readonly-area-bounds)
    (let* ((hash (make-hash-table :test #'eq)))
      (ccl::%map-lfuns #'(lambda (f)
			   (let* ((code-vector (ccl:uvref f 0))
				  (startaddr (+ (ccl::%address-of code-vector)
						target::misc-data-offset)))
			     (when (and (>= startaddr readonly-low)
					(< startaddr readonly-high))
			       (push f (gethash code-vector hash))))))
      (collect ((functions))
	(maphash #'(lambda (k v)
		     (declare (ignore k))
		     (if (null (cdr v))
		       (functions (car v))))
		 hash)
        (values (sort (functions)
		      #'(lambda (x y)
			  (< (ccl::%address-of  (uvref x 0))
			     (ccl::%address-of  (uvref y 0)))))
		readonly-low
		(- readonly-high readonly-low))))))

(defun register-mach-o-functions (functions section-number)
  (let* ((n (length functions))
	 (nlist-len #+64-bit-target (record-length :nlist_64)
		    #+32-bit-target (record-length :nlist))
	 (data (#_calloc n nlist-len))
	 (string-table (make-mach-o-string-table)))
    (declare (fixnum n))
    (do* ((i 0 (1+ i))
	  (p (%inc-ptr data 0) (progn (%incf-ptr p nlist-len) p))
	  (f (pop functions) (pop functions)))
	 ((= i n)
	  (make-mach-o-symbol-table :strings string-table :data data :nsyms n))
      (declare (fixnum i))
      (let* ((namidx (mach-o-register-string (mach-o-lisp-function-name f) string-table))
	     (value (%address-of #+ppc-target (uvref f 0) #-ppc-target g))
	     (type #$N_SECT))
      #+32-bit-target
      (setf (pref p :nlist.n_un.n_strx) namidx
	    (pref p :nlist.n_value) value
	    (pref p :nlist.n_type) type
	    (pref p :nlist.n_other) section-number)
      #+64-bit-target
      (setf (pref p :nlist_64.n_un.n_strx) namidx
	    (pref p :nlist_64.n_value) value
	    (pref p :nlist_64.n_type) type
	    (pref p :nlist_64.n_sect) section-number)))))

(defun write-mach-o-symbol-info (fd symtab)
  (let* ((symoff *host-page-size*)
	 (nsyms (mach-o-symbol-table-nsyms symtab))
	 (symsize (* nsyms (record-length #+64-bit-target :nlist_64
						   #+32-bit-target :nlist)))
	 (stroff (+ symoff symsize))
	 (string (mach-o-string-table-string (mach-o-symbol-table-strings symtab)))
	 (strsize (length string))
	 (bytes (array-data-and-offset string))
	 (strbuf (#_malloc strsize)))
    (%copy-ivector-to-ptr bytes 0 strbuf 0 strsize)
    (fd-lseek fd symoff #$SEEK_SET)
    (fd-write fd (mach-o-symbol-table-data symtab) symsize)
    (fd-write fd strbuf strsize)
    (values symoff nsyms stroff strsize)))

(defun write-mach-o-load-commands (fd pos)
  (multiple-value-bind (functions start length)
      (collect-mach-o-static-functions)
    (let* ((symbols (register-mach-o-functions functions 1)))
      (multiple-value-bind (symoff nsyms stroff strsize)
	  (write-mach-o-symbol-info fd symbols)
	(rlet ((symtab :symtab_command
		 :cmd #$LC_SYMTAB
		 :cmdsize (record-length :symtab_command)
		 :symoff symoff
		 :nsyms nsyms
		 :stroff stroff
		 :strsize strsize))
	  (let* ((segsize (record-length #+64-bit-target :segment_command_64
					 #+32-bit-target :segment_command))
		 (sectsize (record-length #+64-bit-target :section_64
					 #+32-bit-target :section))
		 (totalsize (+ segsize sectsize)))
	    (%stack-block ((segment totalsize :clear t))
	      (let* ((section (%inc-ptr segment segsize)))
		#+64-bit-target
		(progn
		  (setf (pref segment :segment_command_64.cmd) #$LC_SEGMENT_64
			(pref segment :segment_command_64.cmdsize) totalsize)
		  (%cstr-pointer #$SEG_DATA
				 (pref segment :segment_command_64.segname)
				 nil)
		  (setf (pref segment :segment_command_64.vmaddr) start
			(pref segment :segment_command_64.vmsize) length
			(pref segment :segment_command_64.fileoff) 0
			(pref segment :segment_command_64.filesize) 0
			(pref segment :segment_command_64.maxprot) 0
			(pref segment :segment_command_64.initprot) 0
			(pref segment :segment_command_64.nsects) 1)
		  (%cstr-pointer "__lisp" (pref section :section_64.sectname) nil)
		  (%cstr-pointer #$SEG_DATA (pref section :section_64.segname) nil)
		  (setf (pref section :section_64.addr) start
			(pref section :section_64.size) length
			(pref section :section_64.align) 12))
		#+32-bit-target
		(progn
		  (setf (pref segment :segment_command.cmd) #$LC_SEGMENT
			(pref segment :segment_command.cmdsize) totalsize)
		  (%cstr-pointer #$SEG_DATA
				 (pref segment :segment_command.segname)
				 nil)
		  (setf (pref segment :segment_command.vmaddr) start
			(pref segment :segment_command.vmsize) length
			(pref segment :segment_command.fileoff) 0
			(pref segment :segment_command.filesize) 0
			(pref segment :segment_command.maxprot) 0
			(pref segment :segment_command.initprot) 0
			(pref segment :segment_command.nsects) 1)
		  (%cstr-pointer "__lisp" (pref section :section.sectname) nil)
		  (%cstr-pointer #$SEG_DATA (pref section :section.segname) nil)
		  (setf (pref section :section.addr) start
			(pref section :section.size) length
			(pref section :section.align) 12))
		(fd-lseek fd pos #$SEEK_SET)
		(fd-write fd segment totalsize)
		(fd-write fd symtab (record-length :symtab_command))
		(values 2
			(+ totalsize (record-length :symtab_command)))))))))))

    
(defun write-mach-header (fd)
  (let* ((n (record-length #+64-bit-target :mach_header_64
			   #+32-bit-target :mach_header)))
    (multiple-value-bind (ncmds cmd-size)
	(write-mach-o-load-commands fd n)
      (rlet ((header #+64-bit-target :mach_header_64 #+32-bit-target :mach_header
		     :magic #+64-bit-target #$#$MH_MAGIC_64 #+32-bit-target #$MH_MAGIC
		     :cputype (logior #+64-bit-target #$CPU_ARCH_ABI64
				      #+32-bit-target 0
				      #+ppc-target #$CPU_TYPE_POWERPC
				      #+x86-target #$CPU_TYPE_X86)
		     :cpusubtype #+x86-target #$CPU_SUBTYPE_X86_ALL #+ppc-target #$CPU_SUBTYPE_POWERPC_ALL
		     :filetype #$MH_BUNDLE
		     :ncmds ncmds
		     :sizeofcmds cmd-size
		     :flags (logior #$MH_NOUNDEFS)))
	(fd-lseek fd 0 #$SEEK_SET)
	(let* ((res (fd-write fd header n)))
	  (unless (eql res n)
	    (%errno-disp res)))
	(fd-close fd)))))
	   

    
		 
  
		  
