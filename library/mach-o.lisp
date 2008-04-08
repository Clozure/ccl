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

;;; Wrapper around libmach-o's "mach-o" pointer
(defstruct elf-object
  libelf-pointer
  fd
  pathname
  )




;;; Prepate to create an MACH-O object file at PATHNAME, overwriting
;;; whatever might have been there.
(defun create-mach-o-object (pathname)
  (let* ((namestring (native-translated-namestring pathname))
         (fd (ccl::fd-open namestring
                           (logior #$O_RDWR #$O_CREAT #$O_TRUNC)
                           #o755)))
    (if (< fd 0)
      (signal-file-error fd pathname)
      (progn
        (check-libmach-o-version)
        (let* ((ptr (#_mach-o_begin fd #$MACH-O_C_WRITE +null-ptr+)))
          (if (%null-ptr-p ptr)
            (error "Can't initialize libmach-o object for ~s: ~a"
                   pathname (libmach-o-error-string))
            (make-mach-o-object :libmach-o-pointer (assert-pointer-type ptr :<E>lf)
                             :fd fd
                             :pathname pathname)))))))

(defun mach-o-end (object)
  (#_mach-o_end (mach-o-object-libelf-pointer object))
  (setf (elf-object-libelf-pointer object) nil
        (elf-object-fd object) nil))

(defun new-elf-file-header (object format type machine)
  (let* ((ehdr (#_elf64_newehdr (elf-object-libelf-pointer object))))
    (if (%null-ptr-p ehdr)
      (error "Can't create ELF file header for ~s: ~a"
             (elf-object-pathname object)
             (libelf-error-string))
      (progn
        (setf (paref (pref ehdr :<E>lf64_<E>hdr.e_ident) (:* :unsigned-char) #$EI_DATA) format
              (pref ehdr :<E>lf64_<E>hdr.e_machine) machine
              (pref ehdr :<E>lf64_<E>hdr.e_type) type
              (pref ehdr :<E>lf64_<E>hdr.e_version) *checked-libelf-version*)
        (assert-pointer-type ehdr :<E>lf64_<E>hdr)))))

(defun new-elf-program-header (object &optional (count 1))
  (let* ((phdr (#_elf64_newphdr (elf-object-libelf-pointer object) count)))
    (if (%null-ptr-p phdr)
      (error "Can't create ELF program header for ~s: ~a"
             (elf-object-pathname object)
             (libelf-error-string))
      (assert-pointer-type phdr :<E>lf64_<P>hdr))))

(defun new-elf-section (object)
  (let* ((scn (#_elf_newscn (elf-object-libelf-pointer object))))
    (if (%null-ptr-p scn)
      (error "Can' create ELF section for ~s: ~a"
             (elf-object-pathname object)
             (libelf-error-string))
      (assert-pointer-type scn :<E>lf_<S>cn))))

(defun elf-section-header-for-section (object section)
  (let* ((shdr (#_elf64_getshdr section)))
    (if (%null-ptr-p shdr)
      (error "Can' obtain ELF section header for ~s: ~a"
             (elf-object-pathname object)
             (libelf-error-string))
      (assert-pointer-type shdr :<E>lf64_<S>hdr))))

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
                    (declare (type (mod #x110000 code)))
                    (if (> code 255)
                      (vector-push-extend (char-code #\sub) s)
                      (vector-push-extend code s)))))))))


(defun elf-lisp-function-name (f)
  (let* ((name (format nil "~s" f)))
    (subseq (nsubstitute #\0 #\# (nsubstitute #\. #\Space name)) 1)))

(defx86lapfunction dynamic-dnode ((x arg_z))
  (movq (% x) (% imm0))
  (ref-global x86::heap-start arg_y)
  (subq (% arg_y) (% imm0))
  (shrq ($ x8664::dnode-shift) (% imm0))
  (box-fixnum imm0 arg_z)
  (single-value-return))

(defun collect-elf-static-functions ()
  (collect ((functions))
    (freeze)
    (block walk
      (let* ((frozen-dnodes (frozen-space-dnodes)))
        (%map-areas (lambda (o)
                      (when (>= (dynamic-dnode o) frozen-dnodes)
                        (return-from walk nil))
                      (when (typep o 'function-vector)
                        (functions (function-vector-to-function o))))
                    ccl::area-dynamic
                    ccl::area-dynamic
                    )))
    (functions)))

(defun register-elf-functions (section-number)
  (let* ((functions (collect-elf-static-functions))
         (n (length functions))
         (data (#_calloc (1+ n) (record-length :<E>lf64_<S>ym)))
         (string-table (make-elf-string-table)))
    (declare (fixnum n))
    (do* ((i 0 (1+ i))
          (p (%inc-ptr data (record-length :<E>lf64_<S>ym)) (progn (%incf-ptr p (record-length :<E>lf64_<S>ym)) p))
          (f (pop functions) (pop functions)))
         ((= i n)
          (make-elf-symbol-table :strings string-table :data data :nsyms n))
      (declare (fixnum i))
      (setf (pref p :<E>lf64_<S>ym.st_name) (elf-register-string (elf-lisp-function-name f) string-table)
            (pref p :<E>lf64_<S>ym.st_info) (logior (ash #$STB_GLOBAL 4) #$STT_FUNC)
            (pref p :<E>lf64_<S>ym.st_shndx) section-number
            (pref p :<E>lf64_<S>ym.st_value) (%address-of f)
            (pref p :<E>lf64_<S>ym.st_size) (1+ (ash (1- (%function-code-words f)) target::word-shift))))))

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
         (n (* (1+ nsyms) (record-length :<E>lf64_<S>ym))))
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
  (rlet ((fhdr :<E>lf64_<E>hdr)
         (shdr :<E>lf64_<S>hdr))
    (fd-read fd fhdr (record-length :<E>lf64_<E>hdr))
    (let* ((pos (+ (pref fhdr :<E>lf64_<E>hdr.e_shoff)
                   (* sectnum (pref fhdr :<E>lf64_<E>hdr.e_shentsize)))))
      (fd-lseek fd pos #$SEEK_SET)
      (fd-read fd shdr (record-length :<E>lf64_<S>hdr))
      (setf (pref shdr :<E>lf64_<S>hdr.sh_offset)
            (+ #x2000 (logandc2 (+ eof 4095) 4095))) ; #x2000 for nilreg-area
      (fd-lseek fd pos #$SEEK_SET)
      (fd-write fd shdr (record-length :<E>lf64_<S>hdr))
      t)))
  
(defun write-elf-symbols-to-file (pathname)
  (let* ((object (create-elf-object pathname))
         (file-header (new-elf-file-header object #$ELFDATA2LSB #$ET_DYN #$EM_X86_64))
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
    
    (setf (pref file-header :<E>lf64_<E>hdr.e_shstrndx) (elf-section-index shstrtab-section))
    (setf (pref lisp-section-header :<E>lf64_<S>hdr.sh_name) (elf-register-string ".lisp" section-names)
          (pref lisp-section-header :<E>lf64_<S>hdr.sh_type) #$SHT_NOBITS
          (pref lisp-section-header :<E>lf64_<S>hdr.sh_flags) (logior #$SHF_WRITE #$SHF_ALLOC #$SHF_EXECINSTR)
          (pref lisp-section-header :<E>lf64_<S>hdr.sh_addr) (ash (%get-kernel-global heap-start) target::fixnumshift)
          (pref lisp-section-header :<E>lf64_<S>hdr.sh_size) (ash (frozen-space-dnodes) target::dnode-shift)
          (pref lisp-section-header :<E>lf64_<S>hdr.sh_offset) 0
          (pref lisp-section-header :<E>lf64_<S>hdr.sh_addralign) 1)
    (setf (pref symbols-section-header :<E>lf64_<S>hdr.sh_name) (elf-register-string ".symtab" section-names)
          (pref symbols-section-header :<E>lf64_<S>hdr.sh_type) #$SHT_SYMTAB
          (pref symbols-section-header :<E>lf64_<S>hdr.sh_entsize) (record-length :<E>lf64_<S>ym)
          (pref symbols-section-header :<E>lf64_<S>hdr.sh_link) (elf-section-index strings-section))
    (setf (pref strings-section-header :<E>lf64_<S>hdr.sh_name) (elf-register-string ".strtab" section-names)
          (pref strings-section-header :<E>lf64_<S>hdr.sh_type) #$SHT_STRTAB
          (pref strings-section-header :<E>lf64_<S>hdr.sh_flags) (logior #$SHF_STRINGS #$SHF_ALLOC))
    (setf (pref shstrtab-section-header :<E>lf64_<S>hdr.sh_name) (elf-register-string ".shstrtab" section-names)
          (pref shstrtab-section-header :<E>lf64_<S>hdr.sh_type) #$SHT_STRTAB
          (pref shstrtab-section-header :<E>lf64_<S>hdr.sh_flags) (logior #$SHF_STRINGS #$SHF_ALLOC))
    (elf-make-empty-data-for-section object lisp-section (ash (frozen-space-dnodes) target::dnode-shift))
    (elf-init-section-data-from-string-table object strings-section (elf-symbol-table-strings symbols))
    (elf-init-section-data-from-string-table object shstrtab-section section-names)
    (elf-init-symbol-section-from-symbol-table object symbols-section symbols)
    ;; Prepare in-memory data structures.
    (elf-update object #$ELF_C_NULL)
    ;; Fix up the program header.
    (setf (pref program-header :<E>lf64_<P>hdr.p_type) #$PT_PHDR
          (pref program-header :<E>lf64_<P>hdr.p_offset) (pref file-header :<E>lf64_<E>hdr.e_phoff)
          (pref program-header :<E>lf64_<P>hdr.p_filesz) (#_elf64_fsize #$ELF_T_PHDR 1 #$EV_CURRENT))
    ;; Mark the program header as being dirty.
    (elf-flag-phdr object #$ELF_C_SET #$ELF_F_DIRTY)
    (let* ((eof (elf-update object #$ELF_C_WRITE))
           (fd (elf-object-fd object)))
      (elf-end object)
      (fixup-lisp-section-offset fd eof lisp-section-index)
      (fd-close fd))
    pathname))

      
    
    
