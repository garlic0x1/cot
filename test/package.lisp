(defpackage #:cot/test
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-testing
   #:cot/geometry)
  (:local-nicknames
   (#:iter #:coalton-library/iterator))
  (:export
   #:run-tests))

(in-package #:cot/test)

(fiasco:define-test-package #:cot/fiasco-test-package)
(coalton-fiasco-init #:cot/fiasco-test-package)

(cl:defun run-tests (cl:&optional interactive)
  (fiasco:run-package-tests
   :packages '(#:cot/fiasco-test-package)
   :interactive interactive))
