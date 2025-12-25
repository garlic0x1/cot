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
   #:init-file
   #:shutdown
   #:with-tb
   ;; TODO broken?
   #:with-tb-tty
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
                `(,(cl:eval (cl:second ctor))
                  (Ok ,(cl:first ctor))))
              ctors)
           (_ (Err InvalidValue))))
       (define (dump enum)
         (match enum
           ,@(cl:mapcar
              (cl:lambda (ctor)
                `((,(cl:first ctor))
                  ,(cl:eval (cl:second ctor))))
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
    (ModMotion 8))

  (define-enum Key U16
    (CtrlTildeKey       #x00)
    ;; (Ctrl2Key           #x00) ; "clash with 'CTRL_TILDE'"
    (CtrlAKey           #x01)
    (CtrlBKey           #x02)
    (CtrlCKey           #x03)
    (CtrlDKey           #x04)
    (CtrlEKey           #x05)
    (CtrlFKey           #x06)
    (CtrlGKey           #x07)
    (BackspaceKey       #x08)
    ;; (CtrlHKey           #x08) ; "clash with 'CTRL_BACKSPACE'"
    (TabKey             #x09)
    ;; (CtrlIKey           #x09) ; "clash with 'TAB'"
    (CtrlJKey           #x0a)
    (CtrlKKey           #x0b)
    (CtrlLKey           #x0c)
    (EnterKey           #x0d)
    ;; (CtrlMKey           #x0d) ; "clash with 'ENTER'"
    (CtrlNKey           #x0e)
    (CtrlOKey           #x0f)
    (CtrlPKey           #x10)
    (CtrlQKey           #x11)
    (CtrlRKey           #x12)
    (CtrlSKey           #x13)
    (CtrlTKey           #x14)
    (CtrlUKey           #x15)
    (CtrlVKey           #x16)
    (CtrlWKey           #x17)
    (CtrlXKey           #x18)
    (CtrlYKey           #x19)
    (CtrlZKey           #x1a)
    (EscKey             #x1b)
    ;; (CtrlLsqBracketKey  #x1b) ; "clash with 'ESC'"
    ;; (Ctrl3Key           #x1b) ; "clash with 'ESC'"
    (Ctrl4Key           #x1c)
    ;; (CtrlBackslashKey   #x1c) ; "clash with 'CTRL_4'"
    (Ctrl5Key           #x1d)
    ;; (CtrlRsqBracketKey  #x1d) ; "clash with 'CTRL_5'"
    (Ctrl6Key           #x1e)
    (Ctrl7Key           #x1f)
    ;; (CtrlSlashKey       #x1f) ; "clash with 'CTRL_7'"
    ;; (CtrlUnderscoreKey  #x1f) ; "clash with 'CTRL_7'"
    (SpaceKey           #x20)
    (Backspace2Key      #x7f)
    ;; (Ctrl8Key           #x7f) ; "clash with 'BACKSPACE2'"
    (F1Key              (cl:- #xffff 0))
    (F2Key              (cl:- #xffff 1))
    (F3Key              (cl:- #xffff 2))
    (F4Key              (cl:- #xffff 3))
    (F5Key              (cl:- #xffff 4))
    (F6Key              (cl:- #xffff 5))
    (F7Key              (cl:- #xffff 6))
    (F8Key              (cl:- #xffff 7))
    (F9Key              (cl:- #xffff 8))
    (F10Key             (cl:- #xffff 9))
    (F11Key             (cl:- #xffff 10))
    (F12Key             (cl:- #xffff 11))
    (InsertKey          (cl:- #xffff 12))
    (DeleteKey          (cl:- #xffff 13))
    (HomeKey            (cl:- #xffff 14))
    (EndKey             (cl:- #xffff 15))
    (PGUpKey            (cl:- #xffff 16))
    (PGDnKey            (cl:- #xffff 17))
    (ArrowUpKey         (cl:- #xffff 18))
    (ArrowDownKey       (cl:- #xffff 19))
    (ArrowLeftKey       (cl:- #xffff 20))
    (ArrowRightKey      (cl:- #xffff 21))
    (BackTabKey         (cl:- #xffff 22))
    (MouseLeftKey       (cl:- #xffff 23))
    (MouseRightKey      (cl:- #xffff 24))
    (MouseMiddleKey     (cl:- #xffff 25))
    (MouseReleaseKey    (cl:- #xffff 26))
    (MouseWheelUpKey    (cl:- #xffff 27))
    (MouseWheelDownKey  (cl:- #xffff 28))))

(coalton-toplevel
  (derive Eq)
  (define-struct Event
    (type   EventType)
    (mod    ModKey)
    (key    Key)
    (char   Char)
    (width  Integer)
    (height Integer)
    (x      Integer)
    (y      Integer)))

(coalton-toplevel
  (declare init (Unit -> (Result TBError Unit)))
  (define (init)
    (lisp-tb-result ()
      (tb:tb-init)))

  (declare init-file (String -> (Result TBError Unit)))
  (define (init-file file)
    (lisp-tb-result (file)
      (tb:tb-init-file file)))

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

  (declare with-tb-tty (String -> (Unit -> Unit) -> Unit))
  (define (with-tb-tty tty thunk)
    (lisp Unit (tty thunk)
      (cl:unwind-protect
        (cl:progn
          (tb:tb-init-file tty)
          (call-coalton-function thunk Unit))
        (tb:tb-shutdown))))

  (declare current-width (Unit -> Integer))
  (define (current-width)
    (lisp Integer ()
      (tb:tb-width)))

  (declare current-height (Unit -> Integer))
  (define (current-height)
    (lisp Integer ()
      (tb:tb-height)))

  (declare clear (Integer -> Integer -> (Result TBError Unit)))
  (define (clear fg bg)
    (lisp-tb-result (fg bg)
      (tb:tb-set-clear-attrs fg bg)
      (tb:tb-clear)))

  (declare present (Unit -> (Result TBError Unit)))
  (define (present)
    (lisp-tb-result ()
      (tb:tb-present)))

  (declare set-cursor (Integer -> Integer -> (Result TBError Unit)))
  (define (set-cursor x y)
    (lisp-tb-result (x y)
      (tb:tb-set-cursor x y)))

  (declare hide-cursor (Unit -> (Result TBError Unit)))
  (define (hide-cursor)
    (lisp-tb-result ()
      (tb:tb-hide-cursor)))

  (declare set-cell (Integer -> Integer -> Char -> UFix -> UFix -> (Result TBError Unit)))
  (define (set-cell x y ch fg bg)
    (lisp-tb-result (x y ch fg bg)
      (tb:tb-set-cell x y (cl:char-code ch) fg bg)))

  (declare print (Integer -> Integer -> UFix -> UFix -> String -> (Result TBError Unit)))
  (define (print x y fg bg str)
    (lisp-tb-result (x y fg bg str)
      (tb:tb-print x y fg bg str)))

  (declare unsafe-load-type (U8 -> EventType))
  (define (unsafe-load-type obj)
    (unwrap (load obj)))
  (declare unsafe-load-mod (U8 -> ModKey))
  (define (unsafe-load-mod obj)
    (unwrap (load obj)))
  (declare unsafe-load-key (U16 -> Key))
  (define (unsafe-load-key obj)
    (unwrap (load obj)))
  (declare unsafe-load-err (Integer -> TBError))
  (define (unsafe-load-err obj)
    (unwrap (load obj)))

  (declare peek-event (Integer -> (Result TBError Event)))
  (define (peek-event timeout)
    (lisp (Result TBError Event) (timeout)
      (cffi:with-foreign-object (ev '(:struct tb:tb-event*))
        (cl:let ((status (tb:tb-peek-event* ev timeout)))
          (cl:if (cl:zerop status)
                 (cffi:with-foreign-slots ((tb::type tb::mod tb::key tb::ch tb::w tb::h tb::x tb::y) ev (:struct tb:tb-event*))
                   (Ok (Event (call-coalton-function unsafe-load-type tb::type)
                              (call-coalton-function unsafe-load-mod tb::mod)
                              (call-coalton-function unsafe-load-type tb::key)
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
                              (call-coalton-function unsafe-load-key tb::key)
                              (cl:code-char tb::ch)
                              tb::w tb::h
                              tb::x tb::y)))
                 (Err (call-coalton-function unsafe-load-err status))))))))
