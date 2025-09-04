(in-package :moonli)

;; FIXME: Do something about the excess whitespace
(5am:in-suite :moonli)

(define-moonli-short-macro defvar
  ((name good-symbol)
   (_ +whitespace/internal)
   (value moonli-expression))
  `(defvar ,name ,value))

(define-moonli-short-macro defparameter
  ((name good-symbol)
   (_ +whitespace/internal)
   (value moonli-expression))
  `(defparameter ,name ,value))

(define-moonli-short-macro in-package
  ((name symbol))
  `(in-package ,name))

(define-moonli-short-macro if
  ((test moonli-expression)
   (_ +whitespace/internal)
   (then moonli-expression)
   (_ (esrap:? +whitespace/internal))
   (else (esrap:? moonli-expression)))
  `(if ,test ,then ,else))

(5am:def-test short-macro-call ()
  (5am:is (equal `(defvar a 5)
                 (esrap:parse 'short-macro-call "defvar a 5")))
  (5am:is (equal `(defparameter a 5)
                 (esrap:parse 'short-macro-call "defparameter a 5")))
  (5am:is (equal `(if a 5 nil)
                 (esrap:parse 'short-macro-call "if a 5")))
  (5am:is (equal `(if a :hello :bye)
                 (esrap:parse 'short-macro-call "if a :hello :bye"))))
