(defpackage #:cot
  (:use
   #:coalton
   #:coalton-prelude
   #:cot/utilities)
  (:import-from #:coalton-library/experimental #:dotimes)
  (:local-nicknames
   (#:math #:coalton-library/math)
   (#:str #:coalton-library/string)
   (#:tb #:cot/termbox)
   (#:sym #:cot/symbols))
  (:export
   #:Color
   #:Point
   #:Dimensions
   #:Bounds
   #:Drawable
   #:draw
   #:render
   #:Frame
   #:LineBorder
   #:MarginBorder
   #:VSplit
   #:HSplit
   #:Overlapping
   #:CenteredString
   ))

(in-package #:cot)

(coalton-toplevel
  (define-struct Color
    (fg UFix)
    (bg UFix))

  (define-struct Point
    (x UFix)
    (y UFix))

  (define-struct Dimensions
    (width  UFix)
    (height UFix))

  (define-struct Bounds
    (point Point)
    (dimensions Dimensions))

  (define-class (Drawable :t)
    (draw (Bounds -> :t -> Unit)))

  (declare render (Drawable :t => :t -> Unit))
  (define (render obj)
    (let point = (Point 0 0))
    (let dimensions = (Dimensions (tb:current-width) (tb:current-height)))
    (tb:clear 0 0)
    (draw (Bounds point dimensions) obj)
    (tb:present)
    Unit))

(coalton-toplevel
  (define-instance (Drawable String)
    (define (draw bounds str)
      (let (Bounds (Point x y) (Dimensions w h)) = bounds)
      (let lines = (lisp (List String) (str) (ppcre:split "\\n" str)))
      (rec f ((lines lines) (i 0))
        (match (head lines)
          ((Some line)
           (tb:print x (+ i y) 0 0 (str:substring line 0 w))
           (if (< i h)
               (f (unwrap (tail lines)) (1+ i))
               Unit))
          ((None) Unit)))
      Unit)))

(coalton-toplevel
  (define-type Border
    (LineBorder Boolean Boolean Boolean Boolean)
    (MarginBorder UFix UFix UFix UFix))

  (define-instance (Drawable Border)
    (define (draw bounds border)
      (match border
        ((MarginBorder _ _ _ _)
         Unit)
        ((LineBorder n s e w)
         (let (Bounds (Point x y) (Dimensions width height)) = bounds)
         (when n (dotimes (i width) (tb:set-cell (+ i x) y sym:+bar-h+ 0 0)))
         (when s (dotimes (i width) (tb:set-cell (+ i x) (ufix-dec (+ y height)) sym:+bar-h+ 0 0)))
         (when e (dotimes (i height) (tb:set-cell (ufix-dec (+ x width)) (+ i y) sym:+bar-v+ 0 0)))
         (when w (dotimes (i height) (tb:set-cell x (+ i y) sym:+bar-v+ 0 0)))
         (when (and n w) (tb:set-cell x y sym:+elbow-nw+ 0 0) Unit)
         (when (and s w) (tb:set-cell x (ufix-dec (+ height y)) sym:+elbow-sw+ 0 0) Unit)
         (when (and n e) (tb:set-cell (ufix-dec (+ width x)) y sym:+elbow-ne+ 0 0) Unit)
         (when (and s e) (tb:set-cell (ufix-dec (+ width x)) (ufix-dec (+ height y)) sym:+elbow-se+ 0 0) Unit)))))

  (declare border-inner-bounds (Border -> Bounds -> Bounds))
  (define (border-inner-bounds border b)
    (match border
      ((LineBorder n s e w)
       (border-inner-bounds
        (MarginBorder (if n 1 0) (if s 1 0) (if e 1 0) (if w 1 0))
        b))
      ((MarginBorder n s e w)
       (let (Bounds (Point x y) (Dimensions width height)) = b)
       (Bounds
        (Point (min (+ x width) (+ x w)) (min (+ y height) (+ y n)))
        (Dimensions (ufix- width (+ e w) 2)
                    (ufix- height (+ n s) 2))))))

  (define-struct (Frame :t)
    (border Border)
    (content :t))

  (define-instance (Drawable :t => Drawable (Frame :t))
    (define (draw bounds frame)
      (let (Frame border content) = frame)
      (draw bounds border)
      (draw (border-inner-bounds border bounds) content))))

(coalton-toplevel
  (define-struct (VSplit :top :bottom)
    (ratio Fraction)
    (top :top)
    (bottom :bottom))

  (define-instance ((Drawable :top) (Drawable :bottom) => Drawable (VSplit :top :bottom))
    (define (draw b vsplit)
      (let (Bounds (= p (Point x y)) (= d (Dimensions w h))) = b)
      (let (VSplit r top bottom) = vsplit)
      (draw
       (Bounds
        (Point x (+ x (unwrap (tryinto (ceiling (* r (into h)))))))
        (Dimensions w (ufix- h (unwrap (tryinto (ceiling (* r (into h))))) 0)))
       bottom)
      (draw
       (Bounds
        p
        (Dimensions w (unwrap (tryinto (ceiling (* r (into h)))))))
       top))))

(coalton-toplevel
  (define-struct (HSplit :left :right)
    (ratio Fraction)
    (left :left)
    (right :right))

  (define-instance ((Drawable :left) (Drawable :right)
                    => Drawable (HSplit :left :right))
    (define (draw b hsplit)
      (let (Bounds (= p (Point x y)) (= d (Dimensions w h))) = b)
      (let (HSplit r top bottom) = hsplit)
      (draw
       (Bounds
        (Point (+ x (unwrap (tryinto (ceiling (* r (into w)))))) y)
        (Dimensions (ufix- w (unwrap (tryinto (ceiling (* r (into w))))) 0) h))
       bottom)
      (draw
       (Bounds
        p
        (Dimensions (unwrap (tryinto (ceiling (* r (into w))))) h))
       top))))

(coalton-toplevel
  (define-struct (Overlapping :top :bottom)
    (top :top)
    (bottom :bottom))

  (define-instance ((Drawable :top) (Drawable :bottom)
                    => Drawable (Overlapping :top :bottom))
    (define (draw bounds (Overlapping top bottom))
      (draw bounds bottom)
      (draw bounds top))))

(coalton-toplevel
  (define-type CenteredString (CenteredString String))

  (define-instance (Drawable CenteredString)
    (define (draw bounds (CenteredString str))
      (let (Bounds _ (Dimensions w h)) = bounds)
      (let lines = (lisp (List String) (str) (ppcre:split "\\n" str)))
      (let sw = (reduce max 0 (map str:length lines)))
      (let sh = (length lines))
      (let mw = (math:div (ufix- w sw 0) 2))
      (let mh = (math:div (ufix- h sh 0) 2))
      (draw bounds
            (Frame (MarginBorder mh mh mw mw)
                   str)))))
