;;;; Smoke: Darwin/arm64 cocoa CDB has ObjC classes (post objective-c regen).
;;;;
;;;; Does not load objc-bridge — only checks interface DB keys.
;;;;
;;;;   ./darm64cl --no-init --batch < tools/darwin-cocoa-smoke.lisp
(in-package :ccl)
(use-interface-dir :cocoa)
(let* ((d (use-interface-dir :cocoa))
       (classes (cdb-enumerate-keys (db-objc-classes d)))
       (methods (cdb-enumerate-keys (db-objc-methods d)))
       (nclass (length classes))
       (nmeth (length methods)))
  (format t "~&cocoa objc-classes=~d objc-methods=~d~%" nclass nmeth)
  (unless (> nclass 100)
    (error "cocoa CDB looks like C-only regen (objc-classes=~d)" nclass))
  (unless (member "NSObject" classes :test #'string=)
    (error "NSObject missing from cocoa objc-classes CDB"))
  (format t "NSObject present in objc-classes CDB~%"))
(format t "~&DARWIN-COCOA-SMOKE-OK~%")
(quit)
