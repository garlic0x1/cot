(in-package #:cot/test)

(define-test test-point-num ()
  (is (== (Point 1 1) (+ (Point 0 1) (Point 1 0))))
  (is (== (Point 4 4) (* (Point 4 2) (Point 1 2))))
  (is (== (Point -1 0) (- (Point 1 1) (Point 2 1)))))

(define-test test-dimensions-into ()
  (is (== (Point 1 1) (+ (Point 0 0) (into (Dimensions 1 1))))))

(define-test test-bounds-positivize-bounds ()
  (let b1 = (Bounds (default) (default)))
  (is (== b1 (positivize-bounds b1)))

  (let b2 = (Bounds (default) (Dimensions 10 100)))
  (is (== b2 (positivize-bounds b2)))
  (is (== b2 (positivize-bounds (Bounds (Point 0 99) (Dimensions 10 -100)))))
  (is (== b2 (positivize-bounds (Bounds (Point 9 0) (Dimensions -10 100)))))
  (is (== b2 (positivize-bounds (Bounds (Point 9 99) (Dimensions -10 -100)))))
  (is (contains? (positivize-bounds
                  (Bounds (Point 9 99) (Dimensions -10 -100)))
                 (Point 9 99)))
  (is (not (contains? (positivize-bounds
                       (Bounds (Point 9 99) (Dimensions -10 -100)))
                      (Point 10 99)))))

(define-test test-bounds-contains ()
  (let b1 = (Bounds (default) (Dimensions 10 10)))
  (is (contains? b1 (Point 0 0)))
  (is (contains? b1 (Point 9 9)))
  (is (not (contains? b1 (Point 9 10))))
  (is (not (contains? b1 (Point 1 -1))))
  (is (not (contains? b1 (Point -1 -1))))

  (let b2 = (Bounds (default) (Dimensions -10 -10)))
  (is (contains? b2 (Point -1 -1)))
  (is (not (contains? b2 (Point 9 9))))
  (is (contains? b2 (Point 0 0)))

  (let b3 = (Bounds (default) (Dimensions 10 -10)))
  (is (contains? b3 (Point 3 -3)))
  (is (not (contains? b3 (Point -3 3))))

  (let b4 = (Bounds (default) (default)))
  (is (not (contains? b4 (default))))

  (let b5 = (Bounds (Point -1 -1) (Dimensions 3 3)))
  (is (contains? b5 (default)))
  (is (contains? b5 (Point -1 -1)))
  (is (contains? b5 (Point 1 -1)))
  (is (not (contains? b5 (Point -2 -1)))))

(define-test test-bounds-vsplit-ratio ()
  (let b1 = (Bounds (default) (default)))

  (let (Tuple b1a b1b) = (vsplit-ratio b1 1/2))
  (is (== b1a b1b))

  (let (Tuple b1a b1b) = (vsplit-ratio b1 1/10))
  (is (== b1a b1b))

  (let b2 = (Bounds (default) (Dimensions 10 10)))

  (is (== (vsplit-ratio b2 1/3) (vsplit-ratio b2 -2/3)))
  (is (== (vsplit-ratio b2 4/3) (vsplit-ratio b2 (the Fraction 3/3))))
  (is (== (vsplit-ratio b2 -4/3) (vsplit-ratio b2 (the Fraction 0/3))))

  (let (Tuple b2a b2b) = (vsplit-ratio b2 1/2))
  (is (== (Dimensions 10 5) (.dimensions b2a)))
  (is (== (Dimensions 10 5) (.dimensions b2b)))
  (is (not (contains? b2a (.point b2b))))
  (is (== (Point 0 0) (.point b2a)))
  (is (== (Point 0 5) (.point b2b)))

  (let (Tuple b2a b2b) = (vsplit-ratio b2 1/3))
  (is (== (Dimensions 10 3) (.dimensions b2a)))
  (is (== (Dimensions 10 7) (.dimensions b2b)))
  (is (not (contains? b2a (.point b2b))))
  (is (== (Point 0 0) (.point b2a)))
  (is (== (Point 0 3) (.point b2b)))

  (let (Tuple b2a b2b) = (vsplit-ratio b2 (the Fraction 3/3)))
  (is (/= b2 b2a))
  (is (contains? b2 (.point b2b)))
  (is (== (Bounds (Point 0 9) (Dimensions 10 1)) b2b))

  (let b3 = (Bounds (Point 2 2) (Dimensions -5 -5)))

  (is (== (vsplit-ratio b3 1/3) (vsplit-ratio b3 -2/3)))
  (is (== (vsplit-ratio b3 4/3) (vsplit-ratio b3 (the Fraction 3/3))))
  (is (== (vsplit-ratio b3 -4/3) (vsplit-ratio b3 (the Fraction 0/3))))

  (let (Tuple b3a b3b) = (vsplit-ratio b3 1/3))
  (is (== (Dimensions -5 -2) (.dimensions b3a)))
  (is (== (Point 2 2) (.point b3a)))
  (is (not (contains? b3a (.point b3b))))
  (is (== (Dimensions -5 -3) (.dimensions b3b)))
  (is (== (Point 2 0) (.point b3b))))

(define-test test-bounds-hsplit-ratio ()
  (let b1 = (Bounds (default) (default)))

  (let (Tuple b1a b1b) = (hsplit-ratio b1 1/2))
  (is (== b1a b1b))

  (let (Tuple b1a b1b) = (hsplit-ratio b1 1/10))
  (is (== b1a b1b))

  (let b2 = (Bounds (default) (Dimensions 10 10)))

  (is (== (hsplit-ratio b2 1/3) (hsplit-ratio b2 -2/3)))
  (is (== (hsplit-ratio b2 4/3) (hsplit-ratio b2 (the Fraction 3/3))))
  (is (== (hsplit-ratio b2 -4/3) (hsplit-ratio b2 (the Fraction 0/3))))

  (let (Tuple b2a b2b) = (hsplit-ratio b2 1/2))
  (is (== (Dimensions 5 10) (.dimensions b2a)))
  (is (== (Dimensions 5 10) (.dimensions b2b)))
  (is (not (contains? b2a (.point b2b))))
  (is (== (Point 0 0) (.point b2a)))
  (is (== (Point 5 0) (.point b2b)))

  (let (Tuple b2a b2b) = (hsplit-ratio b2 1/3))
  (is (== (Dimensions 3 10) (.dimensions b2a)))
  (is (== (Dimensions 7 10) (.dimensions b2b)))
  (is (not (contains? b2a (.point b2b))))
  (is (== (Point 0 0) (.point b2a)))
  (is (== (Point 3 0) (.point b2b)))

  (let (Tuple b2a b2b) = (hsplit-ratio b2 (the Fraction 3/3)))
  (is (/= b2 b2a))
  (is (contains? b2 (.point b2b)))
  (is (== (Bounds (Point 9 0) (Dimensions 1 10)) b2b))

  (let b3 = (Bounds (Point 2 2) (Dimensions -5 -5)))

  (is (== (hsplit-ratio b3 1/3) (hsplit-ratio b3 -2/3)))
  (is (== (hsplit-ratio b3 4/3) (hsplit-ratio b3 (the Fraction 3/3))))
  (is (== (hsplit-ratio b3 -4/3) (hsplit-ratio b3 (the Fraction 0/3))))

  (let (Tuple b3a b3b) = (hsplit-ratio b3 1/3))
  (is (== (Dimensions -2 -5) (.dimensions b3a)))
  (is (== (Point 2 2) (.point b3a)))
  (is (not (contains? b3a (.point b3b))))
  (is (== (Dimensions -3 -5) (.dimensions b3b)))
  (is (== (Point 0 2) (.point b3b))))

(define-test test-bounds-vsplit-int ()
  (let b1 = (Bounds (default) (default)))

  (let (Tuple b1a b1b) = (vsplit-int b1 0))
  (is (== b1a b1b))

  (let (Tuple b1a b1b) = (vsplit-int b1 10))
  (is (== b1a b1b))

  (let b2 = (Bounds (default) (Dimensions 10 10)))

  (is (== (vsplit-int b2 5) (vsplit-int b2 -5)))
  (is (== (vsplit-int b2 9) (vsplit-int b2 -1)))
  (is (== (vsplit-int b2 -100) (vsplit-int b2 0)))
  (is (== (vsplit-int b2 100) (vsplit-int b2 60)))

  (let (Tuple b2a b2b) = (vsplit-int b2 5))
  (is (== (Dimensions 10 5) (.dimensions b2a)))
  (is (== (Dimensions 10 5) (.dimensions b2b)))
  (is (not (contains? b2a (.point b2b))))
  (is (== (Point 0 0) (.point b2a)))
  (is (== (Point 0 5) (.point b2b)))

  (let (Tuple b2a b2b) = (vsplit-int b2 3))
  (is (== (Dimensions 10 3) (.dimensions b2a)))
  (is (== (Dimensions 10 7) (.dimensions b2b)))
  (is (not (contains? b2a (.point b2b))))
  (is (== (Point 0 0) (.point b2a)))
  (is (== (Point 0 3) (.point b2b)))

  (let (Tuple b2a b2b) = (vsplit-int b2 10))
  (is (/= b2 b2a))
  (is (contains? b2 (.point b2b)))
  (is (== (Bounds (Point 0 9) (Dimensions 10 1)) b2b))

  (let b3 = (Bounds (Point 2 2) (Dimensions -5 -5)))

  (is (== (vsplit-int b3 -3) (vsplit-int b3 2)))
  (is (== (vsplit-int b3 100) (vsplit-int b3 10)))
  (is (== (vsplit-int b3 -100) (vsplit-int b3 0)))

  (let (Tuple b3a b3b) = (vsplit-int b3 2))
  (is (not (contains? b3a (.point b3b))))
  (is (not (contains? b3b (.point b3a)))))

(define-test test-bounds-hsplit-int ()
  (let b1 = (Bounds (default) (default)))

  (let (Tuple b1a b1b) = (hsplit-int b1 5))
  (is (== b1a b1b))

  (let (Tuple b1a b1b) = (hsplit-int b1 1))
  (is (== b1a b1b))

  (let b2 = (Bounds (default) (Dimensions 10 10)))

  (is (== (hsplit-int b2 3) (hsplit-int b2 -7)))
  (is (== (hsplit-int b2 100) (hsplit-int b2 10)))
  (is (== (hsplit-int b2 -100) (hsplit-int b2 0)))

  (let (Tuple b2a b2b) = (hsplit-int b2 5))
  (is (== (Dimensions 5 10) (.dimensions b2a)))
  (is (== (Dimensions 5 10) (.dimensions b2b)))
  (is (not (contains? b2a (.point b2b))))
  (is (== (Point 0 0) (.point b2a)))
  (is (== (Point 5 0) (.point b2b)))

  (let (Tuple b2a b2b) = (hsplit-int b2 3))
  (is (== (Dimensions 3 10) (.dimensions b2a)))
  (is (== (Dimensions 7 10) (.dimensions b2b)))
  (is (not (contains? b2a (.point b2b))))
  (is (== (Point 0 0) (.point b2a)))
  (is (== (Point 3 0) (.point b2b)))

  (let (Tuple b2a b2b) = (hsplit-int b2 10))
  (is (/= b2 b2a))
  (is (contains? b2 (.point b2b)))
  (is (== (Bounds (Point 9 0) (Dimensions 1 10)) b2b))

  (let b3 = (Bounds (Point 2 2) (Dimensions -5 -5)))

  (is (== (hsplit-int b3 -3) (hsplit-int b3 2)))
  (is (== (hsplit-int b3 1000) (hsplit-int b3 10)))
  (is (== (hsplit-int b3 0) (hsplit-int b3 -100)))

  (let (Tuple b3a b3b) = (hsplit-int b3 2))
  (is (not (contains? b3a (.point b3b))))
  (is (not (contains? b3b (.point b3a)))))
