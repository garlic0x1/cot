(asdf:defsystem "cot"
  :description "The Coalton Omnipotent TUI"
  :author "garlic0x1"
  :license "MIT"
  :depends-on ("coalton" "termbox2" "cl-ppcre")
  :components ((:module "src"
                :components ((:file "utilities")
                             (:file "termbox")
                             (:file "symbols")
                             (:file "cot")
                             (:file "extras")))))

(asdf:defsystem "cot/example"
  :depends-on ("cot")
  :components ((:file "example"))
  :build-operation "program-op"
  :build-pathname "example.exe"
  :entry-point "cot/example::entrypoint")
