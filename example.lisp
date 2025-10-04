(defpackage #:cot/example
  (:use
   #:coalton
   #:coalton-prelude
   #:cot
   #:cot/extras)
  (:local-nicknames
   (#:tb #:cot/termbox)))

(in-package #:cot/example)

(coalton-toplevel
  (define (render-test)
    (render
     (Overlapping
      (Frame (MarginBorder 10 10 40 40)
             (Frame (LineBorder True True True True)
                    (Selection
                     (make-list "Onion" "Jalapeno" "Lime")
                     (fn (x) x)
                     1)))
      (Frame (LineBorder True True True True)
             (VSplit 1/3
                     (HSplit 1/2
                             "hello"
                             (Frame (LineBorder True True True True)
                                    "LineBorder in Frame"))
                     (Frame (MarginBorder 2 2 20 2)
                            (Frame (LineBorder True True True True)
                                   "world")))))))

  (define (main)
    ;; Render the stuff
    (render-test)
    ;; Quit when Q key is pressed
    (match (tb:poll-event)
      ((Ok (tb:Event _ _ _ #\q _ _ _ _)) Unit)
      (_ (main)))))

(cl:defun entrypoint ()
  (coalton (tb:with-tb main)))
