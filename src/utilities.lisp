(defpackage #:cot/utilities
  (:use
   #:coalton
   #:coalton-prelude)
  (:export
   #:ufix-
   #:ufix-dec))

(in-package #:cot/utilities)

(coalton-toplevel
 (declare ufix- (UFix -> UFix -> UFix -> UFix))
 (define (ufix- a b min)
     (if (< a b)
         min
         (max min (- a b))))

 (declare ufix-dec (Ufix -> UFix))
 (define (ufix-dec x)
     (if (zero? x)
         0
         (1- x))))
