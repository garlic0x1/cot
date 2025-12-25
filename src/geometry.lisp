(defpackage #:cot/geometry
  (:use
   #:coalton
   #:coalton-prelude
   #:cot/utilities)
  (:local-nicknames
   (#:math #:coalton-library/math))
  (:export
   #:Point                              ; STRUCT
   #:Dimensions                         ; STRUCT
   #:Bounds                             ; STRUCT
   #:Corners                            ; STRUCT
   #:bounds-corners                     ; FUNCTION
   #:positivize-bounds                  ; FUNCTION
   #:contains?                          ; FUNCTION
   #:VSplittable                        ; CLASS
   #:HSplittable                        ; CLASS
   #:vsplit-split                       ; METHOD
   #:hsplit-split                       ; METHOD
   #:vsplit-ratio                       ; FUNCTION
   #:hsplit-ratio                       ; FUNCTION
   #:vsplit-int                       ; FUNCTION
   #:hsplit-int                       ; FUNCTION
   ))

(in-package #:cot/geometry)

;;;
;;;- Basic 2D Geometry Types
;;;

(coalton-toplevel
  (derive Eq Default)
  (define-struct Point
    (x Integer)
    (y Integer))

  (derive Eq Default)
  (define-struct Dimensions
    (width  Integer)
    (height Integer))

  (derive Eq Default)
  (define-struct Bounds
    (point Point)
    (dimensions Dimensions))

  (define-instance (Num Point)
    (define (+ (Point ax ay) (Point bx by))
      (Point (+ ax bx) (+ ay by)))
    (define (- (Point ax ay) (Point bx by))
      (Point (- ax bx) (- ay by)))
    (define (* (Point ax ay) (Point bx by))
      (Point (* ax bx) (* ay by)))
    (define (fromInt x)
      (Point 0 x)))

  (define-instance (Into Dimensions Point)
    (define (into (Dimensions w h))
      (Point w h)))

  (define-instance (Into Point Dimensions)
    (define (into (Point x y))
      (Dimensions x y)))

  (define-struct Corners
    (nw Point)
    (ne Point)
    (sw Point)
    (se Point)))

;;;
;;;- Bounds Functions
;;;

(coalton-toplevel
  (declare bounds-corners (Bounds -> Corners))
  (define (bounds-corners b)
    (let (Bounds (Point x y) (Dimensions w h)) = (positivize-bounds b))
    (let nw = (Point x y))
    (let ne = (Point (+ x w) y))
    (let sw = (Point x (+ y h)))
    (let se = (Point (+ x w) (+ y h)))
    (Corners nw ne sw se))

  ;; https://english.stackexchange.com/a/253797
  (declare positivize-bounds (Bounds -> Bounds))
  (define (positivize-bounds (= b (Bounds p (Dimensions w h))))
    (cond
      ((negative? w)
       (positivize-bounds
        (Bounds (+ p (Point (1+ w) 0)) (Dimensions (abs w) h))))

      ((negative? h)
       (positivize-bounds
        (Bounds (+ p (Point 0 (1+ h))) (Dimensions w (abs h)))))

      (True
       b)))

  (declare contains? (Bounds -> Point -> Boolean))
  (define (contains? bounds point)
    "`bounds' inclusively contain `point'."
    (let (Point min-x min-y) = (.point bounds))
    (let (Point max-x max-y) = (+ (.point bounds) (into (.dimensions bounds))))
    (let (Point x y) = point)
    (let (Dimensions w h) = (.dimensions bounds))

    (and
     (if (positive? w)
         (and (<= min-x x) (< x max-x))
         (and (< max-x x)  (<= x min-x)))
     (if (positive? h)
         (and (<= min-y y) (< y max-y))
         (and (< max-y y)  (<= y min-y)))))

  (declare vsplit-ratio ((Into Integer :t) (Rational :t) => Bounds -> :t -> (Tuple Bounds Bounds)))
  (define (vsplit-ratio (= b (Bounds p (Dimensions w h))) ratio)
    (cond
      ;; Case #1:
      ;; Dimensions are too small to meaningfully split,
      ;; return the original Bounds twice.
      ((< (abs h) 2)
       (Tuple b b))

      ;; Case #2:
      ;; Ratio is greater than the whole,
      ;; just use the extreme value.
      ((< 1 (abs ratio))
       (if (positive? ratio)
           (vsplit-ratio b (the Fraction 1/1))
           (vsplit-ratio b (the Fraction 0/1))))

      ;; Case #3:
      ;; Ratio is a negative value,
      ;; measure from the bottom.
      ((negative? ratio)
       (vsplit-ratio b (- 1 (abs ratio))))

      ;; Case #4:
      ;; Sensible split.
      (True
       (let split =
         (if (positive? h)
             (clamp 1 (math:round (* ratio (into h))) (1- h))
             (clamp (1- h) (math:round (* ratio (into h))) -1)))
       (let top = (Bounds p (Dimensions w split)))
       (let bot = (Bounds (+ p (Point 0 split)) (Dimensions w (- h split))))
       (Tuple top bot))))

  (declare hsplit-ratio ((Into Integer :t) (Rational :t) => Bounds -> :t -> (Tuple Bounds Bounds)))
  (define (hsplit-ratio (= b (Bounds p (Dimensions w h))) ratio)
    (cond
      ;; Case #1:
      ;; Dimensions are too small to meaningfully split,
      ;; return the original Bounds twice.
      ((< (abs w) 2)
       (Tuple b b))

      ;; Case #2:
      ;; Ratio is greater than the whole,
      ;; just use the extreme value.
      ((< 1 (abs ratio))
       (if (positive? ratio)
           (hsplit-ratio b (the Fraction 1/1))
           (hsplit-ratio b (the Fraction 0/1))))

      ;; Case #3:
      ;; Ratio is a negative value,
      ;; measure from the right.
      ((negative? ratio)
       (hsplit-ratio b (- 1 (abs ratio))))

      ;; Case #4:
      ;; Sensible split.
      (True
       (let split =
         (if (positive? w)
             (clamp 1 (round (* ratio (into w))) (1- w))
             (clamp (1+ w) (round (* ratio (into w))) -1)))
       (let left  = (Bounds p (Dimensions split h)))
       (let right = (Bounds (+ p (Point split 0)) (Dimensions (- w split) h)))
       (Tuple left right))))

  ;; TODO these are kinda broken in neg bounds

  (declare vsplit-int (Bounds -> Integer -> (Tuple Bounds Bounds)))
  (define (vsplit-int b x)
    (let (= b (Bounds p (Dimensions w h))) = (positivize-bounds b))
    (cond
      ;; Case #1:
      ;; Dimensions are too small to meaningfully split,
      ;; return the original Bounds twice.
      ((< (abs h) 2)
       (Tuple b b))

      ;; Case #2:
      ;; Split index is greater than the whole,
      ;; just use the extreme value.
      ((< (abs h) (abs x))
       (if (positive? x)
           (vsplit-int b h)
           (vsplit-int b 0)))

      ;; Case #3:
      ;; Ratio is a negative value,
      ;; measure from the bottom.
      ((negative? x)
       (vsplit-int b (+ h x)))

      ;; Case #4:
      ;; Sensible split.
      (True
       (let split = (clamp 1 x (1- (abs h))))
       (let split = (if (positive? h) split (negate split)))
       (let top = (Bounds p (Dimensions w split)))
       (let bot = (Bounds (+ p (Point 0 split)) (Dimensions w (- h split))))
       (Tuple top bot))))

  (declare hsplit-int (Bounds -> Integer -> (Tuple Bounds Bounds)))
  (define (hsplit-int b x)
    (let (= b (Bounds p (Dimensions w h))) = (positivize-bounds b))
    (cond
      ;; Case #1:
      ;; Dimensions are too small to meaningfully split,
      ;; return the original Bounds twice.
      ((< (abs w) 2)
       (Tuple b b))

      ;; Case #2:
      ;; Split index is greater than the whole,
      ;; just use the extreme value.
      ((< (abs w) (abs x))
       (if (positive? x)
           (hsplit-int b w)
           (hsplit-int b 0)))

      ;; Case #3:
      ;; Ratio is a negative value,
      ;; measure from the right.
      ((negative? x)
       (hsplit-int b (+ w x)))

      ;; Case #4:
      ;; Sensible split.
      (True
       (let split = (clamp 1 x (1- (abs w))))
       (let split = (if (positive? w) split (negate split)))
       (let left = (Bounds p (Dimensions split h)))
       (let right = (Bounds (+ p (Point split 0)) (Dimensions (- w split) h)))
       (Tuple left right)))))

(coalton-toplevel
  (define-class (VSplittable :split)
    (vsplit-split (Bounds -> :split -> (Tuple Bounds Bounds))))

  (define-instance (VSplittable Fraction)
    (define (vsplit-split b s)
      (vsplit-ratio b s)))

  (define-instance (VSplittable Integer)
    (define (vsplit-split b s)
      (vsplit-int b s))))

(coalton-toplevel
  (define-class (HSplittable :split)
    (hsplit-split (Bounds -> :split -> (Tuple Bounds Bounds))))

  (define-instance (HSplittable Fraction)
    (define (hsplit-split b s)
      (hsplit-ratio b s)))

  (define-instance (HSplittable Integer)
    (define (hsplit-split b s)
      (hsplit-int b s))))
