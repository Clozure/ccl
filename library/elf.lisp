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

(eval-when (:compile-toplevel :execute)
  (use-interface-dir :elf))


(defloadvar *readonly-area*
    (do-consing-areas (a)
      (when (eql (%fixnum-ref a target::area.code)
                 ccl::area-readonly)
        (return a))))

;;; String tables: used both for symbol names and for section names.
(defstruct elf-string-table
  (hash (make-hash-table :test #'equal))
  (string (make-array 100 :element-type '(unsigned-byte 8) :fill-pointer 1 :adjustable t)))

;;; Collect info about Elf symbols.
(defstruct elf-symbol-table
  (strings (make-elf-string-table))
  data                                  ; foreign pointer
  nsyms
  )

;;; Wrapper around libelf's "elf" pointer
(defstruct elf-object
  libelf-pointer
  fd
  pathname
  )


;;; Is libelf thread-safe ?  Who knows, there's no
;;; documentation ...
(defun libelf-error-string (&optional (errnum -1))
  (let* ((p (#_elf_errmsg errnum)))
    (if (%null-ptr-p p)
      (format nil "ELF error ~d" errnum)
      (%get-cstring p))))

(defloadvar *checked-libelf-version* nil)

(defun check-libelf-version ()
  (or *checked-libelf-version*
      (progn
        (open-shared-library "libelf.so")
        (let* ((version (#_elf_version #$EV_CURRENT)))
          (if (eql #$EV_NONE version)
            (error "ELF library initialization failed: ~a" (libelf-error-string)))
          (setq *checked-libelf-version* version)))))


;;; Prepate to create an ELF object file at PATHNAME, overwriting
;;; whatever might have been there.
(defun create-elf-object (pathname)
  (let* ((namestring (native-translated-namestring pathname))
         (fd (ccl::fd-open namestring
                           (logior #$O_RDWR #$O_CREAT #$O_TRUNC)
                           #o755)))
    (if (< fd 0)
      (signal-file-error fd pathname)
      (progn
        (check-libelf-version)
        (let* ((ptr (#_elf_begin fd #$ELF_C_WRITE +null-ptr+)))
          (if (%null-ptr-p ptr)
            (error "Can't initialize libelf object for ~s: ~a"
                   pathname (libelf-error-string))
            (make-elf-object :libelf-pointer (assert-pointer-type ptr :<E>lf)
                             :fd fd
                             :pathname pathname)))))))

(defun elf-end (object)
  (#_elf_end (elf-object-libelf-pointer object))
  (setf (elf-object-libelf-pointer object) nil
        (elf-object-fd object) nil))

(defun new-elf-file-header (object format type machine)
  (let* ((ehdr (#+64-bit-target #_elf64_newehdr #+32-bit-target #_elf32_newehdr (elf-object-libelf-pointer object))))
    (if (%null-ptr-p ehdr)
      (error "Can't create ELF file header for ~s: ~a"
             (elf-object-pathname object)
             (libelf-error-string))
      (progn
        (setf (paref (pref ehdr
                           #+64-bit-target :<E>lf64_<E>hdr.e_ident
                           #+32-bit-target :<E>lf32_<E>hdr.e_ident) (:* :unsigned-char) #$EI_DATA) format
              (pref ehdr
                    #+64-bit-target :<E>lf64_<E>hdr.e_machine
                    #+32-bit-target :<E>lf32_<E>hdr.e_machine) machine
              (pref ehdr
                    #+64-bit-target :<E>lf64_<E>hdr.e_type
                    #+32-bit-target :<E>lf32_<E>hdr.e_type) type
              (pref ehdr
                    #+64-bit-target :<E>lf64_<E>hdr.e_version
                    #+32-bit-target :<E>lf32_<E>hdr.e_version) *checked-libelf-version*)
        (assert-pointer-type ehdr
                             #+64-bit-target :<E>lf64_<E>hdr
                             #+32-bit-target :<E>lf32_<E>hdr)))))

(defun new-elf-program-header (object &optional (count 1))
  (let* ((phdr (#+64-bit-target #_elf64_newphdr #+32-bit-target #_elf32_newphdr (elf-object-libelf-pointer object) count)))
    (if (%null-ptr-p phdr)
      (error "Can't create ELF program header for ~s: ~a"
             (elf-object-pathname object)
             (libelf-error-string))
      (assert-pointer-type phdr
                           #+64-bit-target :<E>lf64_<P>hdr
                           #+32-bit-target :<E>lf32_<P>hdr))))

(defun new-elf-section (object)
  (let* ((scn (#_elf_newscn (elf-object-libelf-pointer object))))
    (if (%null-ptr-p scn)
      (error "Can' create ELF section for ~s: ~a"
             (elf-object-pathname object)
             (libelf-error-string))
      (assert-pointer-type scn :<E>lf_<S>cn))))

(defun elf-section-header-for-section (object section)
  (let* ((shdr (#+64-bit-target #_elf64_getshdr #+32-bit-target #_elf32_getshdr  section)))
    (if (%null-ptr-p shdr)
      (error "Can' obtain ELF section header for ~s: ~a"
             (elf-object-pathname object)
             (libelf-error-string))
      (assert-pointer-type shdr
                           #+64-bit-target :<E>lf64_<S>hdr
                           #+32-bit-target :<E>lf32_<S>hdr))))

(defun elf-data-pointer-for-section (object section)
  (let* ((data (#_elf_newdata section)))
    (if (%null-ptr-p data)
      (error "Can' obtain ELF data pointer for ~s: ~a"
             (elf-object-pathname object)
             (libelf-error-string))
      (assert-pointer-type data :<E>lf_<D>ata))))
                   

(defun elf-register-string (string table)
  (let* ((hash (elf-string-table-hash table))
         (s (elf-string-table-string table)))
    (when (gethash string hash)
      (format t "~& duplicate: ~s" string))
    (or (gethash string hash)
        (setf (gethash string hash)
              (let* ((n (length s)))
                (dotimes (i (length string) (progn (vector-push-extend 0 s) n))
                  (let* ((code (char-code (char string i))))
                    (declare (type (mod #x110000) code))
                    (if (> code 255)
                      (vector-push-extend (char-code #\sub) s)
                      (vector-push-extend code s)))))))))


(defun elf-lisp-function-name (f)
  (let* ((name (format nil "~s" f)))
    (subseq (nsubstitute #\0 #\# (nsubstitute #\. #\Space name)) 1)))



(defun collect-elf-static-functions ()
  (collect ((functions))
    (purify)
    (block walk
      (%map-areas (lambda (o)
                    (when (typep o
                                 #+x8664-target 'function-vector
                                 #-x8664-target 'function)
                      (functions (function-vector-to-function o))))
                  ccl::area-readonly
                  ccl::area-readonly
                  ))
    (functions)))

(defun register-elf-functions (section-number)
  (let* ((functions (collect-elf-static-functions))
         (n (length functions))
         (data (#_calloc (1+ n) (record-length #+64-bit-target :<E>lf64_<S>ym
                                               #+32-bit-target :<E>lf32_<S>ym)))
         (string-table (make-elf-string-table)))
    (declare (fixnum n))
    (do* ((i 0 (1+ i))
          (p (%inc-ptr data
                       (record-length #+64-bit-target :<E>lf64_<S>ym
                                      #+32-bit-target :<E>lf32_<S>ym))
             (progn (%incf-ptr p
                               (record-length #+64-bit-target :<E>lf64_<S>ym
                                              #+32-bit-target :<E>lf32_<S>ym))
                    p))
          (f (pop functions) (pop functions)))
         ((= i n)
          (make-elf-symbol-table :strings string-table :data data :nsyms n))
      (declare (fixnum n))
      (setf (pref p
                  #+64-bit-target :<E>lf64_<S>ym.st_name
                  #+32-bit-target :<E>lf32_<S>ym.st_name)
            (elf-register-string (elf-lisp-function-name f) string-table)
            (pref p
                  #+64-bit-target :<E>lf64_<S>ym.st_info
                  #+32-bit-target :<E>lf32_<S>ym.st_info)
            (logior (ash #$STB_GLOBAL 4) #$STT_FUNC)
            (pref p
                  #+64-bit-target :<E>lf64_<S>ym.st_shndx
                  #+32-bit-target :<E>lf32_<S>ym.st_shndx) section-number
            (pref p
                  #+64-bit-target :<E>lf64_<S>ym.st_value
                  #+32-bit-target :<E>lf32_<S>ym.st_value) (%address-of f)
            (pref p
                  #+64-bit-target :<E>lf64_<S>ym.st_size
                  #+32-bit-target :<E>lf32_<S>ym.st_size) (1+ (ash (1- (%function-code-words f)) target::word-shift))))))

(defun elf-section-index (section)
  (#_elf_ndxscn section))

(defun elf-set-shstrab-section (object scn)
  #+freebsd-target
  (#_elf_setshstrndx (elf-object-libelf-pointer object) (elf-section-index scn))
  #-freebsd-target
  (declare (ignore object scn)))


(defun elf-init-section-data-from-string-table (object section string-table)
  (let* ((strings-data (elf-data-pointer-for-section object section))
         (s (elf-string-table-string string-table))
         (bytes (array-data-and-offset s))
         (n (length s))
         (buf (#_malloc n)))
    (%copy-ivector-to-ptr bytes 0 buf 0 n)
    (setf (pref strings-data :<E>lf_<D>ata.d_align) 1
          (pref strings-data :<E>lf_<D>ata.d_off) 0
          (pref strings-data :<E>lf_<D>ata.d_type) #$ELF_T_BYTE
          (pref strings-data :<E>lf_<D>ata.d_version) #$EV_CURRENT
          (pref strings-data :<E>lf_<D>ata.d_size) n
          (pref strings-data :<E>lf_<D>ata.d_buf) buf)
    n))

(defun elf-init-symbol-section-from-symbol-table (object section symbols)
  (let* ((symbols-data (elf-data-pointer-for-section object section))
         (buf (elf-symbol-table-data symbols))
         (nsyms (elf-symbol-table-nsyms symbols) )
         (n (* (1+ nsyms) (record-length #+64-bit-target :<E>lf64_<S>ym
                                         #+32-bit-target :<E>lf32_<S>ym))))
    (setf (pref symbols-data :<E>lf_<D>ata.d_align) 8
          (pref symbols-data :<E>lf_<D>ata.d_off) 0
          (pref symbols-data :<E>lf_<D>ata.d_type) #$ELF_T_SYM
          (pref symbols-data :<E>lf_<D>ata.d_version) #$EV_CURRENT
          (pref symbols-data :<E>lf_<D>ata.d_size) n
          (pref symbols-data :<E>lf_<D>ata.d_buf) buf)
    nsyms))

(defun elf-make-empty-data-for-section (object section &optional (size 0))
  (let* ((data (elf-data-pointer-for-section object section))
         (buf +null-ptr+))
    (setf (pref data :<E>lf_<D>ata.d_align) 0
          (pref data :<E>lf_<D>ata.d_off) 0
          (pref data :<E>lf_<D>ata.d_type) #$ELF_T_BYTE
          (pref data :<E>lf_<D>ata.d_version) #$EV_CURRENT
          (pref data :<E>lf_<D>ata.d_size) size
          (pref data :<E>lf_<D>ata.d_buf) buf)
    0))
  

(defun elf-flag-phdr (object cmd flags)
  (#_elf_flagphdr (elf-object-libelf-pointer object) cmd flags))

(defun elf-update (object cmd)
  (let* ((size (#_elf_update (elf-object-libelf-pointer object) cmd)))
    (if (< size 0)
      (error "elf_update failed for for ~s: ~a"
             (elf-object-pathname object)
             (libelf-error-string))
      size)))

(defun fixup-lisp-section-offset (fd eof sectnum)
  (fd-lseek fd 0 #$SEEK_SET)
  (rlet ((fhdr #+64-bit-target :<E>lf64_<E>hdr
               #+32-bit-target :<E>lf32_<E>hdr)
         (shdr #+64-bit-target :<E>lf64_<S>hdr
               #+32-bit-target :<E>lf32_<S>hdr))
    (fd-read fd fhdr (record-length #+64-bit-target :<E>lf64_<E>hdr
                                    #+32-bit-target :<E>lf32_<E>hdr))
    (let* ((pos (+ (pref fhdr #+64-bit-target :<E>lf64_<E>hdr.e_shoff
                         #+32-bit-target :<E>lf32_<E>hdr.e_shoff)
                   (* sectnum (pref fhdr #+64-bit-target :<E>lf64_<E>hdr.e_shentsize
                                    #+32-bit-target :<E>lf32_<E>hdr.e_shentsize)))))
      (fd-lseek fd pos #$SEEK_SET)
      (fd-read fd shdr (record-length #+64-bit-target :<E>lf64_<S>hdr
                                      #+32-bit-target :<E>lf32_<S>hdr))
      ;; On 64-bit platforms, the section data precedes the image
      ;; header; on 32-bit platforms, the image header and image
      ;; section table precede the image data for the first (static)
      ;; section.  With alignment, the header/section headers are
      ;; one 4K page, and the static section size is 8K ...
      (setf (pref shdr #+64-bit-target :<E>lf64_<S>hdr.sh_offset
                  #+32-bit-target :<E>lf32_<S>hdr.sh_offset)
            (+ #+32-bit-target #x1000 #+64-bit-target 0  #x2000 (logandc2 (+ eof 4095) 4095))) 
      (setf (pref shdr #+64-bit-target :<E>lf64_<S>hdr.sh_type
                  #+32-bit-target :<E>lf32_<S>hdr.sh_type)
            #$SHT_PROGBITS)
      (fd-lseek fd pos #$SEEK_SET)
      (fd-write fd shdr (record-length #+64-bit-target :<E>lf64_<S>hdr
                                       #+32-bit-target :<E>lf32_<S>hdr))
      t)))
  
(defun write-elf-symbols-to-file (pathname)
  (let* ((object (create-elf-object pathname))
         (file-header (new-elf-file-header object
                                           #+little-endian-target #$ELFDATA2LSB
                                           #+big-endian-target #$ELFDATA2MSB
                                           #$ET_DYN
                                           #+x8664-target #$EM_X86_64
                                           #+x8632-target #$EM_386
                                           #+ppc32-target #$EM_PPC
                                           #+ppc64-target #$EM_PPC64
                                           ))
         (program-header (new-elf-program-header object))
         (lisp-section (new-elf-section object))
         (symbols-section (new-elf-section object))
         (strings-section (new-elf-section object))
         (shstrtab-section (new-elf-section object))
         (section-names (make-elf-string-table))
         (lisp-section-index (elf-section-index lisp-section))
         (symbols (register-elf-functions lisp-section-index))
         (lisp-section-header (elf-section-header-for-section object lisp-section))
         (symbols-section-header (elf-section-header-for-section object symbols-section))
         (strings-section-header (elf-section-header-for-section object strings-section))
         (shstrtab-section-header (elf-section-header-for-section object shstrtab-section)))
    
    (setf (pref file-header #+64-bit-target :<E>lf64_<E>hdr.e_shstrndx
                #+32-bit-target :<E>lf32_<E>hdr.e_shstrndx) (elf-section-index shstrtab-section))
    (setf (pref lisp-section-header #+64-bit-target :<E>lf64_<S>hdr.sh_name
                #+32-bit-target :<E>lf32_<S>hdr.sh_name) (elf-register-string ".lisp" section-names)
          (pref lisp-section-header #+64-bit-target :<E>lf64_<S>hdr.sh_type
                #+32-bit-target :<E>lf32_<S>hdr.sh_type) #$SHT_NOBITS
          (pref lisp-section-header #+64-bit-target :<E>lf64_<S>hdr.sh_flags
                #+32-bit-target :<E>lf32_<S>hdr.sh_flags) (logior #$SHF_WRITE #$SHF_ALLOC #$SHF_EXECINSTR)
          (pref lisp-section-header #+64-bit-target :<E>lf64_<S>hdr.sh_addr
                #+32-bit-target :<E>lf32_<S>hdr.sh_addr) (ash (%fixnum-ref *readonly-area* target::area.low) target::fixnumshift)
          (pref lisp-section-header #+64-bit-target :<E>lf64_<S>hdr.sh_size
                #+32-bit-target :<E>lf32_<S>hdr.sh_size) (ash (- (%fixnum-ref *readonly-area* target::area.active) (%fixnum-ref *readonly-area* target::area.low) )target::fixnumshift)
          (pref lisp-section-header #+64-bit-target :<E>lf64_<S>hdr.sh_offset
                #+32-bit-target :<E>lf32_<S>hdr.sh_offset) 0
          (pref lisp-section-header #+64-bit-target :<E>lf64_<S>hdr.sh_addralign
                #+32-bit-target :<E>lf32_<S>hdr.sh_addralign) 1)
    (setf (pref symbols-section-header #+64-bit-target :<E>lf64_<S>hdr.sh_name
                #+32-bit-target :<E>lf32_<S>hdr.sh_name) (elf-register-string ".symtab" section-names)
          (pref symbols-section-header #+64-bit-target :<E>lf64_<S>hdr.sh_type
                #+32-bit-target :<E>lf32_<S>hdr.sh_type) #$SHT_SYMTAB
          (pref symbols-section-header #+64-bit-target :<E>lf64_<S>hdr.sh_entsize
                #+32-bit-target :<E>lf32_<S>hdr.sh_entsize) (record-length #+64-bit-target :<E>lf64_<S>ym
                                                                           #+32-bit-target :<E>lf32_<S>ym)
          (pref symbols-section-header #+64-bit-target :<E>lf64_<S>hdr.sh_link
                #+32-bit-target :<E>lf32_<S>hdr.sh_link) (elf-section-index strings-section))
    (setf (pref strings-section-header #+64-bit-target :<E>lf64_<S>hdr.sh_name
                #+32-bit-target :<E>lf32_<S>hdr.sh_name) (elf-register-string ".strtab" section-names)
          (pref strings-section-header #+64-bit-target :<E>lf64_<S>hdr.sh_type
                #+32-bit-target :<E>lf32_<S>hdr.sh_type) #$SHT_STRTAB
          (pref strings-section-header #+64-bit-target :<E>lf64_<S>hdr.sh_flags
                #+32-bit-target :<E>lf32_<S>hdr.sh_flags) (logior #$SHF_STRINGS #$SHF_ALLOC))
    (setf (pref shstrtab-section-header #+64-bit-target :<E>lf64_<S>hdr.sh_name
                #+32-bit-target :<E>lf32_<S>hdr.sh_name) (elf-register-string ".shstrtab" section-names)
          (pref shstrtab-section-header #+64-bit-target :<E>lf64_<S>hdr.sh_type
                #+32-bit-target :<E>lf32_<S>hdr.sh_type) #$SHT_STRTAB
          (pref shstrtab-section-header #+64-bit-target :<E>lf64_<S>hdr.sh_flags
                #+32-bit-target :<E>lf32_<S>hdr.sh_flags) (logior #$SHF_STRINGS #$SHF_ALLOC))
    (elf-make-empty-data-for-section object lisp-section (ash (- (%fixnum-ref *readonly-area* target::area.active) (%fixnum-ref *readonly-area* target::area.low) )target::fixnumshift))
    (elf-init-section-data-from-string-table object strings-section (elf-symbol-table-strings symbols))
    (elf-init-section-data-from-string-table object shstrtab-section section-names)
    (elf-init-symbol-section-from-symbol-table object symbols-section symbols)
    ;; Prepare in-memory data structures.
    (elf-update object #$ELF_C_NULL)
    ;; Fix up the program header.
    (setf (pref program-header
                #+64-bit-target :<E>lf64_<P>hdr.p_type
                #+32-bit-target :<E>lf32_<P>hdr.p_type) #$PT_PHDR
          (pref program-header #+64-bit-target :<E>lf64_<P>hdr.p_offset
                #+32-bit-target :<E>lf32_<P>hdr.p_offset)
          (pref file-header
                #+64-bit-target :<E>lf64_<E>hdr.e_phoff
                #+32-bit-target :<E>lf32_<E>hdr.e_phoff)
          (pref program-header
                #+64-bit-target :<E>lf64_<P>hdr.p_filesz
                #+32-bit-target :<E>lf32_<P>hdr.p_filesz)
          (#+64-bit-target #_elf64_fsize #+32-bit-target #_elf32_fsize #$ELF_T_PHDR 1 #$EV_CURRENT))
    ;; Mark the program header as being dirty.
    (elf-flag-phdr object #$ELF_C_SET #$ELF_F_DIRTY)
    (let* ((eof (elf-update object #$ELF_C_WRITE))
           (fd (elf-object-fd object)))
      (elf-end object)
      (fixup-lisp-section-offset fd eof lisp-section-index)
      (fd-close fd))
    pathname))

      
    
    
