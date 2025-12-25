(asdf:defsystem "cot"
  :description "The Coalton Omnipotent TUI"
  :author "garlic0x1"
  :license "MIT"
  :depends-on ("coalton" "termbox2" "cl-ppcre")
  :components ((:module "src"
                :components ((:file "utilities")
                             (:file "termbox")
                             (:file "geometry")
                             (:file "base")
                             (:file "classes")
                             ;; (:file "symbols")
                             (:file "cot")
                             (:file "extras"))))
  :in-order-to ((test-op (test-op "cot/test"))))

(asdf:defsystem "cot/example"
  :depends-on ("cot")
  :components ((:file "example"))
  :build-operation "program-op"
  :build-pathname "example.exe"
  :entry-point "cot/example::entrypoint")

(asdf:defsystem "cot/test"
  :depends-on ("cot" "coalton/testing")
  :components ((:module "test"
                :components ((:file "package")
                             (:file "geometry-tests"))))
  :perform (asdf:test-op
            (o s)
            (uiop:symbol-call '#:cot/test '#:run-tests)))
