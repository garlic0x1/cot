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
   #:Line
   #:Modeline
   #:Margin
   #:Border
   #:VSplit
   #:VSplitLine
   #:HSplit
   #:Overlapping
   #:Centered
   ))

(in-package #:cot)

;;;
;;;- Frame
;;;

(coalton-toplevel
  (define-type BorderEdge
    Line
    (Modeline String)
    (Margin Integer))

  (declare line-edge-p (BorderEdge -> Boolean))
  (define (line-edge-p edge)
    (match edge
      ((Margin _) False)
      (_ True)))

  (declare edge-width (BorderEdge -> Integer))
  (define (edge-width edge)
    (match edge
      ((Margin x) x)
      (_ 1)))

  (define-struct Border
    (north BorderEdge)
    (south BorderEdge)
    (east  BorderEdge)
    (west  BorderEdge))

  (define-instance (Default Border)
    (define (default)
      (Border Line Line Line Line)))

  (define-instance (Drawable Border)
    (define (draw bounds (Border n s e w))
      (let (Corners nw ne sw se) = (bounds-corners bounds))
      (match n
        ((Line)
         (set-cells nw ne (default) +bar-h+))
        ((Modeline str)
         (set-cells nw ne (default) +bar-h+)
         (printb (points-bounds nw (+ (Point 0 1) ne)) (Point 2 0) (default) str))
        ((Margin _)
         Unit))
      (match s
        ((Line)
         (set-cells sw se (default) +bar-h+))
        ((Modeline str)
         (set-cells sw se (default) +bar-h+)
         (printb (points-bounds sw (+ (Point 0 1) se)) (Point 2 0) (default) str))
        ((Margin _)
         Unit))
      (match e
        ((Line)
         (set-cells ne se (default) +bar-v+))
        ((Modeline str)
         (set-cells ne se (default) +bar-v+)
         ;; TODO
         ;;(printvb (points-bounds ne (+ (Point 1 0) se)) (Point 0 1) (default) str)
         )
        ((Margin _)
         Unit))
      (match w
        ((Line)
         (set-cells nw sw (default) +bar-v+))
        ((Modeline str)
         (set-cells nw sw (default) +bar-v+)
         ;; TODO
         ;;(printvb (points-bounds nw (+ (Point 1 0) sw)) (Point 0 1) (default) str)
         )
        ((Margin _)
         Unit))
      (when (and (line-edge-p n) (line-edge-p w)) (set-cell nw (default) +elbow-nw+))
      (when (and (line-edge-p s) (line-edge-p w)) (set-cell sw (default) +elbow-sw+))
      (when (and (line-edge-p n) (line-edge-p e)) (set-cell ne (default) +elbow-ne+))
      (when (and (line-edge-p s) (line-edge-p e)) (set-cell se (default) +elbow-se+))
      ))

  (declare border-inner-bounds (Border -> Bounds -> Bounds))
  (define (border-inner-bounds (Border n s e w) b)
    (let (Bounds (Point x y) (Dimensions width height)) = b)
    (Bounds
     (Point (min (+ x width) (+ x (edge-width w)))
            (min (+ y height) (+ y (edge-width n))))
     (Dimensions (max 2 (- width (+ (edge-width e) (edge-width w))))
                 (max 2 (- height (+ (edge-width n) (edge-width s)))))))

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
      (let border = (Border (Margin mh) (Margin mh) (Margin mw) (Margin mw)))
      (draw bounds (Frame border obj)))))

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


