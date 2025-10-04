(defpackage #:cot/extras
  (:use
   #:coalton
   #:coalton-prelude
   #:cot)
  (:local-nicknames
   (#:tb #:cot/termbox)
   (#:list #:coalton-library/list))
  (:export
   #:Selection
   #:selection-down
   #:selection-up
   #:selection-choice))

(in-package #:cot/extras)

(coalton-toplevel
  (define-struct (Selection :t)
    (choices (List :t))
    (printer (:t -> String))
    (cursor UFix))

  (declare selection-down (Selection :t -> Selection :t))
  (define (selection-down sel)
    (let (Selection choices printer cursor) = sel)
    (Selection
     choices
     printer
     (min (1+ cursor) (if (list:null? choices) 0 (1- (length choices))))))

  (declare selection-up (Selection :t -> Selection :t))
  (define (selection-up sel)
    (let (Selection choices printer cursor) = sel)
    (Selection
     choices
     printer
     (if (zero? cursor) 0 (1- cursor))))

  (declare selection-choice (Selection :t -> :t))
  (define (selection-choice sel)
    (list:nth (.cursor sel) (.choices sel)))

  (define-instance (Drawable (Selection :t))
    (define (draw bounds sel)
      (let (Bounds (Point x y) (Dimensions w h)) = bounds)
      (rec f ((choices (.choices sel)) (i 0))
        (match (head choices)
          ((Some choice)
           (tb:print x
                     (+ i y)
                     (if (== i (.cursor sel)) 5 0)
                     0
                     (lisp String (i choice) (cl:format cl:nil "~A> ~A" i choice)))
           (f (unwrap (tail choices)) (1+ i)))
          ((None)
           Unit))))))
