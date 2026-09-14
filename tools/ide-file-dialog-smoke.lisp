;;;; Smoke: tip file-dialogs use modern NSOpenPanel APIs (no modal UI).
;;;; Hang class: deprecated #/runModalForDirectory:file:types: wedges
;;;; openAndSavePanelservice (Search Files → Browse on Darwin/arm64).
;;;;   ./darm64cl --no-init --batch --eval '(load "tools/ide-file-dialog-smoke.lisp")'
(in-package :ccl)

(format t "~&;; ide-file-dialog-smoke~%")
(finish-output)

(let* ((src (probe-file (merge-pathnames "cocoa-ide/file-dialogs.lisp"
                                         (ccl-directory))))
       (text (with-open-file (s src)
               (let* ((n (file-length s))
                      (buf (make-string n)))
                 (read-sequence buf s)
                 buf))))
  (assert src () "missing cocoa-ide/file-dialogs.lisp")
  (assert (not (search "#/runModalForDirectory" text)) ()
          "file-dialogs.lisp still calls #/runModalForDirectory…")
  (assert (search "#/runModal" text) () "missing #/runModal")
  (assert (search "setDirectoryURL" text) () "missing setDirectoryURL")
  (assert (search "%panel-path-string" text) () "missing URL path helper")
  (format t "~&;; tip file-dialogs.lisp uses runModal + setDirectoryURL (~d bytes)~%"
          (length text))
  (finish-output))

(format t "~&;; PASS ide-file-dialog-smoke~%")
(finish-output)
(ff-call (foreign-symbol-address "exit") :signed-fullword 0 :void)
)
