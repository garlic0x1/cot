(defpackage #:cot/classes
  (:use
   #:coalton
   #:coalton-prelude
   #:cot/geometry
   #:cot/base)
  (:local-nicknames
   (#:tb #:cot/termbox)
   (#:str #:coalton-library/string))
  (:export
   #:Drawable                           ; CLASS
   #:draw                               ; METHOD
   #:Centerable                         ; CLASS
   #:compute-dimensions                 ; METHOD
   #:render                             ; FUNCTION
   ))

(in-package #:cot/classes)

;;;
;;;- Base Classes
;;;

(coalton-toplevel
  (define-class (Drawable :t)
    (draw (Bounds -> :t -> Unit)))

  (define-class (Centerable :t)
    (compute-dimensions (:t -> Dimensions)))

  (declare render (Drawable :t => :t -> Unit))
  (define (render obj)
    (let point = (Point 0 0))
    (let dimensions = (Dimensions (tb:current-width) (tb:current-height)))
    (tb:clear 0 0)
    (draw (Bounds point dimensions) obj)
    (tb:present)
    Unit)
  )

;;;
;;;- Primitive Class Instances
;;;

(coalton-toplevel
  (define-instance (Drawable String)
    (define (draw bounds str)
      (printb bounds (default) (default) str)
      Unit))

  (define-instance (Centerable String)
    (define (compute-dimensions str)
      (let lines = (lisp (List String) (str) (ppcre:split "\\n" str)))
      (let w = (reduce max 0 (map str:length lines)))
      (let h = (length lines))
      (Dimensions (into w) (into h)))))
