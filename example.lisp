(defpackage #:cot/example
  (:use
   #:coalton
   #:coalton-prelude
   #:cot
   #:cot/classes)
  (:local-nicknames
   (#:list #:coalton-library/list)
   (#:tb #:cot/termbox)))

(in-package #:cot/example)

(coalton-toplevel
  (define (example-1)
    (render
     (Frame
      (Border (Modeline "Example 1 - Basic Layout")
              (Modeline "<Space> - Cycle : <q> - Quit")
              (Margin 0)
              (Margin 0))
      (HSplit
       (the Integer 16)
       (Centered "16 chars wide")
       (VSplitLine
        1/3
        (Centered "1/3rd of vertical space")
        (Centered "rest of space"))))))

  (define (example-2)
    (render
     (Frame
      (Border (Modeline "Example 2 - Negative Layout")
              (Modeline "<Space> - Cycle : <q> - Quit")
              (Margin 0)
              (Margin 0))
      (HSplit
       (the Integer -20)
       (VSplitLine
        -1/5
        (Centered "rest of space")
        (Centered "1/5th from bottom"))
       (Centered "20 from right")))))

  (define (example-3)
    (render
     (Frame
      (Border (Modeline "Example 3 - Overlapping Layout")
              (Modeline "<Space> - Cycle : <q> - Quit")
              (Margin 0)
              (Margin 0))
      (Overlapping
       (Frame
        (Border (Margin 8) (Margin 8) (Margin 8) (Margin 8))
        (OpaqueFrame (Border (Modeline "Modal") Line Line Line)
                     (Centered "OpaqueFrame covers the rest!")))
       (VSplit 1/2
               (Centered "TOP TEXT")
               (Frame (Border Line Line Line Line)
                      "BOTTOM TEXT"))))))

  (define (game-loop examples)
    ;; Render the example
    ((list:car examples))

    ;; Await input
    (match (tb:poll-event)
      ;; Quit the game loop
      ((Ok (tb:Event (tb::EventKey) _ _ #\q _ _ _ _))
       Unit)
      ;; Display a new example
      ((Ok (tb:Event (tb::EventKey) _ _ #\Space _ _ _ _))
       (game-loop (list:append (list:cdr examples) (make-list (list:car examples)))))
      (_
       (game-loop examples))))

  (define (main)
    (game-loop (make-list example-1 example-2 example-3))))

(cl:defun entrypoint ()
  (coalton (tb:with-tb main)))
