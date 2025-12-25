(defpackage #:cot/base
  (:use
   #:coalton
   #:coalton-prelude
   #:cot/utilities
   #:cot/geometry
   )
  (:shadow
   #:print)
  (:local-nicknames
   (#:tb #:cot/termbox)
   (#:str #:coalton-library/string))
  (:export
   ;; #:render
   #:Style
   #:computed-fg
   #:Point
   #:Dimensions
   #:Bounds
   #:printb
   #:printvb
   #:set-cell
   #:set-cells
   ))

(in-package #:cot/base)

(coalton-toplevel
  (derive Eq Default)
  (define-struct Style
    (fg UFix)
    (bg UFix)
    (attr (Optional UFix)))

  (define (computed-fg style)
    (match style
      ((Style fg _ (Some attr))
       (lisp UFix (fg attr) (cl:logior fg attr)))
      ((Style fg _ _)
       fg))))

(coalton-toplevel
  (declare set-cell (Point -> Style -> Char -> Unit))
  (define (set-cell (Point x y) style char)
    (tb:set-cell x y char (computed-fg style) (.bg style))
    Unit)

  (declare set-cells (Point -> Point -> Style -> Char -> Unit))
  (define (set-cells (Point ax ay) (Point bx by) style char)
    (rec %x ((x (min ax bx)))
      (rec %y ((y (min ay by)))
        (set-cell (Point x y) style char)
        (if (== y (max ay by))
            Unit
            (%y (1+ y))))
      (if (== x (max ax bx))
          Unit
          (%x (1+ x)))))

  (declare print (Point -> Style -> String -> (Result tb:TBError Unit)))
  (define (print (Point x y) (Style fg bg attr) str)
    (tb:print x y
              (match attr
                ((Some attr)
                 (lisp UFix (fg attr) (cl:logior fg attr)))
                ((None)
                 fg))
              bg
              str))

  (declare printb (Bounds -> Point -> Style -> String -> Unit))
  (define (printb bounds pnt style str)
    (let (Bounds bp (Dimensions w _)) = bounds)
    (let lines = (lisp (List String) (str) (ppcre:split "\\n" str)))
    (rec f ((lines lines) (i (Point 0 0)))
      (let pp = (+ i (+ bp pnt)))
      (match (head lines)
        ((Some line)
         (when (contains? bounds pp)
           (print pp style (str:substring line 0 (the UFix (unwrap (tryinto w)))))
           (f (unwrap (tail lines)) (+ i (Point 0 1)))))
        ((None) Unit))))

  (declare printvb (Bounds -> Point -> Style -> String -> Unit))
  (define (printvb bounds pnt style str)
    (let (Bounds bp (Dimensions _ h)) = bounds)
    (let lines = (lisp (List String) (str) (ppcre:split "\\n" str)))
    (rec f ((lines lines) (i (Point 0 0)))
      (let pp = (+ i (+ bp pnt)))
      (match (head lines)
        ((Some line)
         (when (contains? bounds pp)
           (print pp style (str:substring line 0 (the UFix (unwrap (tryinto h)))))
           (f (unwrap (tail lines)) (+ i (Point 1 0)))))
        ((None) Unit))))
  )


