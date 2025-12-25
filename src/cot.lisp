(defpackage #:cot
  (:use
   #:coalton
   #:coalton-prelude
   #:cot/utilities
   #:cot/geometry
   #:cot/base
   #:cot/classes)
  (:import-from #:coalton-library/experimental #:dotimes)
  (:local-nicknames
   (#:math #:coalton-library/math)
   (#:str #:coalton-library/string)
   (#:tb #:cot/termbox))
  (:export
   #:Frame
   #:OpaqueFrame
   #:Border
   #:LineBorder
   #:MarginBorder
   #:VSplit
   #:VSplitLine
   #:HSplit
   #:Overlapping
   #:Centered
   ))

(in-package #:cot)

(coalton-toplevel
  (define-type Border
    (LineBorder Boolean Boolean Boolean Boolean)
    (MarginBorder Integer Integer Integer Integer))

  (define-instance (Default Border)
    (define (default)
      (LineBorder True True True True)))

  (define-instance (Drawable Border)
    (define (draw bounds border)
      (match border
        ((MarginBorder _ _ _ _)
         Unit)
        ((LineBorder n s e w)
         (let (Corners nw ne sw se) = (bounds-corners bounds))
         (when n (set-cells nw ne (default) +bar-h+))
         (when s (set-cells sw se (default) +bar-h+))
         (when e (set-cells ne se (default) +bar-v+))
         (when w (set-cells nw sw (default) +bar-v+))
         (when (and n w) (set-cell nw (default) +elbow-nw+))
         (when (and s w) (set-cell sw (default) +elbow-sw+))
         (when (and n e) (set-cell ne (default) +elbow-ne+))
         (when (and s e) (set-cell se (default) +elbow-se+))))))

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
        (Dimensions (max 2 (- width (+ e w)))
                    (max 2 (- height (+ n s))))))))

  (define-struct (Frame :t)
    (border Border)
    (content :t))

  (define-instance (Drawable :t => Drawable (Frame :t))
    (define (draw bounds frame)
      (let (Frame border content) = frame)
      (draw bounds border)
      (draw (border-inner-bounds border bounds) content)))

  (define-struct (OpaqueFrame :t)
    (border Border)
    (content :t))

  (define-instance (Drawable :t => Drawable (OpaqueFrame :t))
    (define (draw bounds frame)
      (let (OpaqueFrame border content) = frame)
      (let (Corners nw _ _ se) = (bounds-corners bounds))
      (set-cells nw se (default) #\Space)
      (draw bounds border)
      (draw (border-inner-bounds border bounds) content))))

(coalton-toplevel
  (define-type (Centered :t) (Centered :t))

  (define-instance ((Drawable :t) (Centerable :t) => Drawable (Centered :t))
    (define (draw bounds (Centered obj))
      (let (Bounds _ (Dimensions w h)) = bounds)
      (let (Dimensions ow oh) = (compute-dimensions obj))
      (let mw = (math:div (max 0 (- w ow)) 2))
      (let mh = (math:div (max 0 (- h oh)) 2))
      (draw bounds (Frame (MarginBorder mh mh mw mw) obj)))))

(coalton-toplevel
  (define-struct (VSplit :split :top :bottom)
    (split :split)
    (top :top)
    (bottom :bottom))

  (define-instance ((Drawable :top) (Drawable :bottom) => Drawable (VSplit Fraction :top :bottom))
    (define (draw b vsplit)
      (let (VSplit r top bottom) = vsplit)
      (let (Tuple tbound bbound) = (vsplit-ratio b r))
      (draw bbound bottom)
      (draw tbound top)))

  (define-instance ((Drawable :top) (Drawable :bottom) => Drawable (VSplit Integer :top :bottom))
    (define (draw b vsplit)
      (let (VSplit r top bottom) = vsplit)
      (let (Tuple tbound bbound) = (vsplit-int b r))
      (draw bbound bottom)
      (draw tbound top))))

(coalton-toplevel 
  (define-struct (VSplitLine :split :top :bottom)
    (ratio :split)
    (top :top)
    (bottom :bottom))

  (define-instance ((Drawable :top) (Drawable :bottom) => Drawable (VSplitLine Fraction :top :bottom))
    (define (draw b vsplit)
      (let (VSplitLine r top bottom) = vsplit)
      (let (Tuple (Bounds p (Dimensions w h)) bbound) = (vsplit-ratio b r))
      (let tbound = (Bounds p (Dimensions w (1+ h))))
      (let (Tuple tbound bbound) = (vsplit-ratio b r))
      (draw bbound (Frame (default) bottom))
      (draw tbound (Frame (default) top))
      (set-cell (.point bbound) (default) +tee-w+)
      (set-cell (+ (Point w 0) (.point bbound)) (default) +tee-e+)))

  (define-instance ((Drawable :top) (Drawable :bottom) => Drawable (VSplitLine Integer :top :bottom))
    (define (draw b vsplit)
      (let (VSplitLine r top bottom) = vsplit)
      (let (Tuple (Bounds p (Dimensions w h)) bbound) = (vsplit-int b r))
      (let tbound = (Bounds p (Dimensions w (1+ h))))
      (let (Tuple tbound bbound) = (vsplit-int b r))
      (draw bbound (Frame (default) bottom))
      (draw tbound (Frame (default) top))
      (set-cell (.point bbound) (default) +tee-w+)
      (set-cell (+ (Point w 0) (.point bbound)) (default) +tee-e+))))

(coalton-toplevel
  (define-struct (HSplit :split :top :bottom)
    (split :split)
    (top :top)
    (bottom :bottom))

  (define-instance ((Drawable :top) (Drawable :bottom) => Drawable (HSplit Fraction :top :bottom))
    (define (draw b hsplit)
      (let (HSplit r left right) = hsplit)
      (let (Tuple left-b right-b) = (hsplit-ratio b r))
      (draw right-b right)
      (draw left-b left)))

  (define-instance ((Drawable :top) (Drawable :bottom) => Drawable (HSplit Integer :top :bottom))
    (define (draw b hsplit)
      (let (HSplit r left right) = hsplit)
      (let (Tuple left-b right-b) = (hsplit-int b r))
      (draw right-b right)
      (draw left-b left))))

(coalton-toplevel
  (define-struct (Overlapping :top :bottom)
    (top :top)
    (bottom :bottom))

  (define-instance ((Drawable :top) (Drawable :bottom)
                    => Drawable (Overlapping :top :bottom))
    (define (draw bounds (Overlapping top bottom))
      (draw bounds bottom)
      (draw bounds top))))


