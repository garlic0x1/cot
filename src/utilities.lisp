(defpackage #:cot/utilities
  (:use
   #:coalton
   #:coalton-prelude)
  (:export
   ;; #:ufix-                              ; FUNCTION
   ;; #:ufix-dec                           ; FUNCTION
   #:clamp                              ; FUNCTION
   #:+bar-h+                            ; CONSTANT
   #:+bar-v+                            ; CONSTANT
   #:+elbow-nw+                         ; CONSTANT
   #:+elbow-sw+                         ; CONSTANT
   #:+elbow-ne+                         ; CONSTANT
   #:+elbow-se+                         ; CONSTANT
   #:+tee-w+                            ; CONSTANT
   #:+tee-e+                            ; CONSTANT
   #:+tee-n+                            ; CONSTANT
   #:+tee-s+                            ; CONSTANT
   #:+cross+                            ; CONSTANT
   ))

(in-package #:cot/utilities)

(coalton-toplevel
  ;; (declare ufix- (UFix -> UFix -> UFix -> UFix))
  ;; (define (ufix- a b min)
  ;;     (if (< a b)
  ;;         min
  ;;         (max min (- a b))))

  ;; (declare ufix-dec (Ufix -> UFix))
  ;; (define (ufix-dec x)
  ;;     (if (zero? x)
  ;;         0
  ;;         (1- x)))

  (declare clamp (Ord :t => :t -> :t -> :t -> :t))
  (define (clamp min-value x max-value)
    (max min-value (min max-value x))))

(cl:eval-when (:compile-toplevel :load-toplevel :execute) 
  (cl:defmacro define-char (name str)
    `(define ,name
       (coalton-library/vector:index-unsafe
        0
        (the (coalton-library/vector:Vector Char) (into ,str))))))

(coalton-toplevel
  (define-char +bar-h+ "─")
  (define-char +bar-v+ "│")
  (define-char +elbow-nw+ "╭")
  (define-char +elbow-sw+ "╰")
  (define-char +elbow-ne+ "╮")
  (define-char +elbow-se+ "╯")
  (define-char +tee-w+ "├")
  (define-char +tee-e+ "┤")
  (define-char +tee-n+ "┬")
  (define-char +tee-s+ "┴")
  (define-char +cross+ "┼"))
