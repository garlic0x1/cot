(defpackage #:cot/example
  (:use
   #:coalton
   #:coalton-prelude
   #:cot
   #:cot/classes
   #:cot/extras)
  (:local-nicknames
   (#:tb #:cot/termbox)))

(in-package #:cot/example)

(coalton-toplevel
  (define (render-test-2 tb)
    (render
     (Frame
      (MarginBorder 1 1 1 1)
      (VSplitLine
       (the Integer -10)
       (Centered "whoami")
       ;; (Centered "lsblk")
       (the TextBox tb)
       ))))

  (define (render-test tb)
    (render
     (Overlapping
      (Frame (MarginBorder 10 10 40 40)
             (OpaqueFrame (LineBorder True True True True)
                          (VSplit 2/4
                                  (Selection
                                   (make-list "Onion" "Jalapeno" "Lime")
                                   (fn (x) x)
                                   1)
                                  tb)))
      (Frame (LineBorder True True True True)
             (VSplit 1/3
                     (HSplit 1/2
                             (lisp String () (cl:format cl:nil "hello~%Hello"))
                             (Frame (LineBorder True True True True)
                                    ;;(CenteredString)
                                    "Centered in Frame with really really long text"))
                     (Frame (MarginBorder 5 10 20 2)
                            (Frame (LineBorder True True True True)
                                   (lisp String () (cl:format cl:nil "hello~%~%Hello~%~%hi")))))))))

  (define (game-loop tb)
    ;; Render the stuff
    (render-test-2 tb)
    ;; Process input
    (let ev = (tb:poll-event))
    (traceobject "ev" ev)
    (match ev
      ((Ok (tb:Event (tb::EventKey) _ _ #\q _ _ _ _))
       Unit)
      ((Ok (tb:Event _ _ (tb::Backspace2Key) _ _ _ _ _))
       (game-loop (textbox-backspace tb)))
      ((Ok (tb:Event (tb::EventKey) (tb::ModNone) _ x _ _ _ _))
       (game-loop (textbox-insert tb x)))
      (_
       (game-loop tb))))

  (define (main)
    (lisp Unit ()
      (termbox2:tb-set-input-mode (cl:logior 4 2))
      Unit)
    (game-loop (Textbox (LineBorder False False False False) "initial"))
    ))

(cl:defun entrypoint ()
  (coalton (tb:with-tb main)))
