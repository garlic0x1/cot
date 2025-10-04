(defpackage #:cot/termbox
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:tb #:termbox2))
  (:shadow #:print)
  (:export
   #:TBError
   #:EventType
   #:ModKey
   #:Event
   #:init
   #:shutdown
   #:with-tb
   #:current-width
   #:current-height
   #:clear
   #:present
   #:set-cursor
   #:hide-cursor
   #:set-cell
   #:print
   #:peek-event
   #:poll-event))

(in-package #:cot/termbox)

(coalton-toplevel
  (define-type EnumError InvalidValue)
  (define-class (Enum :a :b)
    (dump (:a -> :b))
    (load (:b -> (Result EnumError :a)))))

(cl:defmacro define-enum (name type cl:&body ctors)
  `(progn
     (derive Eq)
     (define-type ,name
       ,@(cl:mapcar #'cl:first ctors))
     (define-instance (Enum ,name ,type)
       (define (load value)
         (match value
           ,@(cl:mapcar
              (cl:lambda (ctor)
                `(,(cl:second ctor)
                  (Ok ,(cl:first ctor))))
              ctors)
           (_ (Err InvalidValue))))
       (define (dump enum)
         (match enum
           ,@(cl:mapcar
              (cl:lambda (ctor)
                `((,(cl:first ctor))
                  ,(cl:second ctor)))
              ctors))))))

(cl:defmacro lisp-tb-result (capture cl:&body body)
  `(match (lisp Integer ,capture ,@body)
     (0 (Ok Unit))
     (x (match (the (Result EnumError TBError) (load x))
          ((Err _) (Err UnspecifiedErr))
          ((Ok error) (Err error))))))

(coalton-toplevel
  (define-enum TBError Integer
    (UnspecifiedErr     -1)
    (NeedMoreErr        -2)
    (InitAlreadyErr     -3)
    (InitOpenErr        -4)
    (MemoryErr          -5)
    (NoEventErr         -6)
    (NoTermErr          -7)
    (NotInitErr         -8)
    (OutOfBoundsErr     -9)
    (ReadErr            -10)
    (ResizeIoCTLErr     -11)
    (ResizePipeErr      -12)
    (ResizeSigActionErr -13)
    (PollErr            -14)
    (TCGetAttrErr       -15)
    (TCSetAttrErr       -16)
    (UnsupportedTermErr -17)
    (ResizeWriteErr     -18)
    (ResizePollErr      -19)
    (ResizeReadErr      -20)
    (ResizeSScanfErr    -21)
    (CapCollisionErr    -22))

  (define-enum EventType U8
    (EventKey    1)
    (EventResize 2)
    (EventMouse  3))

  (define-enum ModKey U8
    (ModNone   0)
    (ModAlt    1)
    (ModCtrl   2)
    (ModShift  4)
    (ModMotion 8)))

(coalton-toplevel 
  (derive Eq)
  (define-struct Event
    (type   EventType)
    (mod    ModKey)
    (key    UFix)
    (char   Char)
    (width  UFix)
    (height UFix)
    (x      UFix)
    (y      UFix)))

(coalton-toplevel
  (declare init (Unit -> (Result TBError Unit)))
  (define (init)
    (lisp-tb-result ()
      (tb:tb-init)))

  (declare shutdown (Unit -> (Result TBError Unit)))
  (define (shutdown)
    (lisp-tb-result ()
      (tb:tb-shutdown)))

  (declare with-tb ((Unit -> Unit) -> Unit))
  (define (with-tb thunk)
    (lisp Unit (thunk)
      (cl:unwind-protect
           (cl:progn
             (tb:tb-init)
             (call-coalton-function thunk Unit))
        (tb:tb-shutdown))))

  (declare current-width (Unit -> UFix))
  (define (current-width)
    (lisp UFix ()
      (tb:tb-width)))

  (declare current-height (Unit -> UFix))
  (define (current-height)
    (lisp UFix ()
      (tb:tb-height)))

  (declare clear (UFix -> UFix -> (Result TBError Unit)))
  (define (clear fg bg)
    (lisp-tb-result (fg bg)
      (tb:tb-set-clear-attrs fg bg)
      (tb:tb-clear)))

  (declare present (Unit -> (Result TBError Unit)))
  (define (present)
    (lisp-tb-result ()
      (tb:tb-present)))

  (declare set-cursor (UFix -> UFix -> (Result TBError Unit)))
  (define (set-cursor x y)
    (lisp-tb-result (x y)
      (tb:tb-set-cursor x y)))

  (declare hide-cursor (Unit -> (Result TBError Unit)))
  (define (hide-cursor)
    (lisp-tb-result ()
      (tb:tb-hide-cursor)))

  (declare set-cell (UFix -> UFix -> Char -> UFix -> UFix -> (Result TBError Unit)))
  (define (set-cell x y ch fg bg)
    (lisp-tb-result (x y ch fg bg)
      (tb:tb-set-cell x y (cl:char-code ch) fg bg)))

  (declare print (UFix -> UFix -> UFix -> UFix -> String -> (Result TBError Unit)))
  (define (print x y fg bg str)
    (lisp-tb-result (x y fg bg str)
      (tb:tb-print x y fg bg str)))

  (declare unsafe-load-type (U8 -> EventType))
  (define (unsafe-load-type obj)
    (unwrap (load obj)))
  (declare unsafe-load-mod (U8 -> ModKey))
  (define (unsafe-load-mod obj)
    (unwrap (load obj)))
  (declare unsafe-load-err (Integer -> TBError))
  (define (unsafe-load-err obj)
    (unwrap (load obj)))

  (declare peek-event (UFix -> (Result TBError Event)))
  (define (peek-event timeout)
    (lisp (Result TBError Event) (timeout)
      (cffi:with-foreign-object (ev '(:struct tb:tb-event*))
        (cl:let ((status (tb:tb-peek-event* ev timeout)))
          (cl:if (cl:zerop status)
                 (cffi:with-foreign-slots ((tb::type tb::mod tb::key tb::ch tb::w tb::h tb::x tb::y) ev (:struct tb:tb-event*))
                   (Ok (Event (call-coalton-function unsafe-load-type tb::type)
                              (call-coalton-function unsafe-load-mod tb::mod)
                              tb::key
                              (cl:code-char tb::ch)
                              tb::w tb::h
                              tb::x tb::y)))
                 (Err unspecifiederr))))))

  (declare poll-event (Unit -> (Result TBError Event)))
  (define (poll-event)
    (lisp (Result TBError Event) ()
      (cffi:with-foreign-object (ev '(:struct tb:tb-event*))
        (cl:let ((status (tb:tb-poll-event* ev)))
          (cl:if (cl:zerop status)
                 (cffi:with-foreign-slots ((tb::type tb::mod tb::key tb::ch tb::w tb::h tb::x tb::y) ev (:struct tb:tb-event*))
                   (Ok (Event (call-coalton-function unsafe-load-type tb::type)
                              (call-coalton-function unsafe-load-mod tb::mod)
                              tb::key
                              (cl:code-char tb::ch)
                              tb::w tb::h
                              tb::x tb::y)))
                 (Err (call-coalton-function unsafe-load-err status))))))))
