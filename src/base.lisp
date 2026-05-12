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

  (declare computed-fg (Style -> UFix))
  (define (computed-fg style)
    (match style
      ((Style fg _ (Some attr))
       (lisp (-> UFix) (fg attr) (cl:logior fg attr)))
      ((Style fg _ _)
       fg))))

(coalton-toplevel
  (declare set-cell (Point * Style * Char -> Void))
  (define (set-cell (Point x y) style char)
    (tb:set-cell x y char (computed-fg style) (.bg style))
    (values))

  (declare set-cells (Point * Point * Style * Char -> Void))
  (define (set-cells (Point ax ay) (Point bx by) style char)
    (rec %x ((x (min ax bx)))
      (rec %y ((y (min ay by)))
        (set-cell (Point x y) style char)
        (if (== y (max ay by))
            (values)
            (%y (1+ y))))
      (if (== x (max ax bx))
          (values)
          (%x (1+ x)))))

  (declare print (Point * Style * String -> (Result tb:TBError Unit)))
  (define (print (Point x y) (Style fg bg attr) str)
    (tb:print x y
              (match attr
                ((Some attr)
                 (lisp (-> UFix) (fg attr) (cl:logior fg attr)))
                ((None)
                 fg))
              bg
              str))

  (declare printb (Bounds * Point * Style * String -> Void))
  (define (printb bounds pnt style str)
    (let (Bounds bp (Dimensions w _)) = bounds)
    (let lines = (lisp (-> (List String)) (str) (ppcre:split "\\n" str)))
    (rec f ((lines lines) (i (Point 0 0)))
      (let pp = (+ i (+ bp pnt)))
      (match (head lines)
        ((Some line)
         (when (contains? bounds pp)
           (print pp style (str:substring line 0 (the UFix (unwrap (tryinto w)))))
           (f (unwrap (tail lines)) (+ i (Point 0 1)))))
        ((None) (values)))))

  (declare printvb (Bounds * Point * Style * String -> Void))
  (define (printvb bounds pnt style str)
    (let (Bounds bp (Dimensions _ h)) = bounds)
    (let lines = (lisp (-> (List String)) (str) (ppcre:split "\\n" str)))
    (rec f ((lines lines) (i (Point 0 0)))
      (let pp = (+ i (+ bp pnt)))
      (match (head lines)
        ((Some line)
         (when (contains? bounds pp)
           (print pp style (str:substring line 0 (the UFix (unwrap (tryinto h)))))
           (f (unwrap (tail lines)) (+ i (Point 1 0)))))
        ((None) (values))))))


