(defpackage #:cot/extras
  (:use
   #:coalton
   #:coalton-prelude
   #:cot
   #:cot/classes
   #:cot/base)
  (:local-nicknames
   (#:tb #:cot/termbox)
   (#:str #:coalton-library/string)
   (#:list #:coalton-library/list))
  (:export
   #:TextBox
   #:textbox-backspace
   #:textbox-insert
   #:Selection
   #:selection-down
   #:selection-up
   #:selection-choice))

(in-package #:cot/extras)

(coalton-toplevel
  (derive Default)
  (define-struct TextBox
    (border Border)
    (content String))

  (declare textbox-backspace (TextBox -> TextBox))
  (define (textbox-backspace tb)
    (TextBox (.border tb)
             (str:substring (.content tb)
                            0
                            (unwrap
                             (tryinto
                              (max 0
                                   (1- (the Integer (into (str:length (.content tb)))))))))))

  (declare textbox-insert (TextBox -> Char -> TextBox))
  (define (textbox-insert tb ch)
    (TextBox (.border tb)
             (str:concat (.content tb) (into ch))))

  (define-instance (Drawable TextBox)
    (define (draw bounds tb)
      (draw bounds (Frame (.border tb) (.content tb))))))

(coalton-toplevel
  (define *selected-style* (Style 5 0 (Some #x02000000)))

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
      (rec f ((choices (.choices sel)) (i (Point 0 0)))
        (match (head choices)
          ((Some choice)
           (let iy = (.y i))
           (let choice-str = ((.printer sel) choice))
           (printb bounds
                   i
                   (if (== (.y i) (.cursor sel)) *selected-style* (default))
                   (lisp String (iy choice-str)
                     (cl:format cl:nil "~A> ~A" iy choice-str)))
           (f (unwrap (tail choices)) (+ i (Point 0 1))))
          ((None)
           Unit)))))

  (define-instance (Centerable (Selection :t))
    (define (compute-dimensions sel)
      (Dimensions
       (into 
        (+ 4 (reduce max 0
                     (map coalton-library/string:length
                          (map (fn (x) ((.printer sel) x))
                               (.choices sel))))))
       (into (length (.choices sel)))
       ))))
