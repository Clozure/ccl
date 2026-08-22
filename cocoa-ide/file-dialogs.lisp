;;;
;;; Copyright 2016 Clozure Associates
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
(in-package "GUI")

;;;; MCL-ish file dialogs
;;;;
;;;; Darwin/arm64 (and modern AppKit generally): the old
;;;; runModalForDirectory:file:types: / runModalForDirectory:file:
;;;; entry points talk to the out-of-process openAndSavePanelservice in a
;;;; way that can wedge the IDE (Search Files → Browse never returns;
;;;; same class for Open/Save).  Use #/setDirectoryURL: + #/runModal and
;;;; read the selection from #/URL / #/URLs instead.

(defun %ns-file-url (path)
  "PATH is a native namestring, or NIL."
  (if path
    (#/fileURLWithPath: ns:ns-url (#/autorelease (%make-nsstring path)))
    +null-ptr+))

(defun %panel-path-string (url)
  (unless (%null-ptr-p url)
    (lisp-string-from-nsstring (#/path url))))

(defun %cocoa-choose-file-dialog (directory file-types file button-string)
  ;; `file' was only used by the old runModalForDirectory:file:… name field.
  (declare (ignore file))
  (assume-cocoa-thread)
  (let* ((open-panel (#/openPanel ns:ns-open-panel))
         (types-array +null-ptr+))
    ;; Maybe support multiple file selection later.
    (#/setAllowsMultipleSelection: open-panel #$NO)
    (when directory
      (#/setDirectoryURL: open-panel (%ns-file-url directory)))
    (when file-types
      (setq types-array (make-instance 'ns:ns-mutable-array))
      (dolist (type file-types)
        (let ((s (%make-nsstring type)))
          (#/addObject: types-array s)
          (#/release s)))
      (#/autorelease types-array)
      (#/setAllowedFileTypes: open-panel types-array))
    (when button-string
      (#/setPrompt: open-panel (#/autorelease (%make-nsstring button-string))))
    (let ((result (#/runModal open-panel)))
      (cond ((= result $modal-response-ok)
             (%panel-path-string (#/URL open-panel)))
            ((= result $modal-response-cancel)
             nil)
            (t
             (error "couldn't run the open panel: error code ~d" result))))))

(defun cocoa-choose-file-dialog (&key directory file-types file button-string)
  (when directory
    (setq directory (directory-namestring directory)))
  (when file-types
    (unless (and (listp file-types)
                 (every #'stringp file-types))
      (error "~s is not a list of strings." file-types)))
  (when file
    (setq file (file-namestring file)))
  (check-type button-string (or null string))
  (execute-in-gui #'(lambda () (%cocoa-choose-file-dialog directory file-types file button-string))))

(defun %cocoa-choose-new-file-dialog (directory file-types file)
  (assume-cocoa-thread)
  (let* ((save-panel (#/savePanel ns:ns-save-panel))
         (types-array +null-ptr+))
    #-cocotron (#/setCanSelectHiddenExtension: save-panel t)
    (when directory
      (#/setDirectoryURL: save-panel (%ns-file-url directory)))
    (when file
      (#/setNameFieldStringValue: save-panel (#/autorelease (%make-nsstring file))))
    (when file-types
      (setq types-array (make-instance 'ns:ns-mutable-array))
      (dolist (type file-types)
        (let ((s (%make-nsstring type)))
          (#/addObject: types-array s)
          (#/release s)))
      (#/autorelease types-array))
    #-cocotron (#/setAllowedFileTypes: save-panel types-array)
    (let ((result (#/runModal save-panel)))
      (cond ((= result $modal-response-ok)
             (%panel-path-string (#/URL save-panel)))
            ((= result $modal-response-cancel)
             nil)
            (t
             (error "couldn't run the save panel: error code ~d" result))))))

(defun cocoa-choose-new-file-dialog (&key directory file-types file)
  (when directory
    (setq directory (directory-namestring directory)))
  (when file
    (setq file (file-namestring file)))
  (when file-types
    (unless (and (listp file-types)
                 (every #'stringp file-types))
      (error "~s is not a list of strings." file-types)))
  (execute-in-gui #'(lambda () (%cocoa-choose-new-file-dialog directory file-types file))))

(defun cocoa-choose-file-dialog-hook-function (must-exist prompt file-types)
  (declare (ignore prompt))
  (if must-exist
    (cocoa-choose-file-dialog :file-types file-types)
    (cocoa-choose-new-file-dialog :file-types file-types)))

(setq ccl::*choose-file-dialog-hook* 'cocoa-choose-file-dialog-hook-function)
(setq ccl::*choose-directory-dialog-hook* 'cocoa-choose-directory-dialog)

(defun %cocoa-choose-directory-dialog (directory)
  (assume-cocoa-thread)
  (let ((open-panel (#/openPanel ns:ns-open-panel)))
    (#/setCanChooseFiles: open-panel #$NO)
    (#/setCanChooseDirectories: open-panel #$YES)
    (#/setAllowsMultipleSelection: open-panel #$NO)
    (#/setTitle: open-panel #@"Choose Directory")
    (#/setPrompt: open-panel #@"Choose")
    (when directory
      (#/setDirectoryURL: open-panel (%ns-file-url directory)))
    (let ((result (#/runModal open-panel)))
      (cond ((= result $modal-response-ok)
             ;; Selected directory URL — not the deprecated #/directory
             ;; (panel's shown folder), which was easy to confuse with
             ;; the selection when browsing for a folder.
             (let ((path (%panel-path-string (#/URL open-panel))))
               (when path
                 (pathname (if (and (plusp (length path))
                                    (char/= (char path (1- (length path))) #\/))
                             (concatenate 'string path "/")
                             path)))))
            ((= result $modal-response-cancel)
             nil)
            (t
             (error "couldn't run the open panel: error code ~d" result))))))

(defun cocoa-choose-directory-dialog (&key directory)
  (when directory
    (setq directory (directory-namestring directory)))
  (execute-in-gui #'(lambda () (%cocoa-choose-directory-dialog directory))))
