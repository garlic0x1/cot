(defpackage #:cot/symbols
  (:use
   #:coalton
   #:coalton-prelude)
  (:export
   #:+bar-h+
   #:+bar-v+
   #:+elbow-nw+
   #:+elbow-sw+
   #:+elbow-ne+
   #:+elbow-se+
   #:+tee-w+
   #:+tee-e+
   #:+tee-n+
   #:+tee-s+
   #:+cross+))

(in-package #:cot/symbols)

(defmacro define-char (name str)
  `(define ,name
     (coalton-library/vector:index-unsafe
      0
      (the (coalton-library/vector:Vector Char) (into ,str)))))

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
