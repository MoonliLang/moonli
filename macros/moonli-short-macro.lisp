(in-package :moonli)

;; FIXME: Do something about the excess whitespace
(5am:in-suite :moonli)

(define-moonli-short-macro defvar
  ((name good-symbol)
   (_ whitespace/internal)
   (value moonli-expression))
  `(defvar ,name ,value))

(define-moonli-short-macro defparameter
  ((name good-symbol)
   (_ whitespace/internal)
   (value moonli-expression))
  `(defparameter ,name ,value))

(define-moonli-short-macro in-package
  ((name symbol))
  `(in-package ,name))

(5am:def-test short-macro-call ()
  (5am:is (equal `(defvar a 5)
                 (esrap:parse 'short-macro-call "defvar a 5")))
  (5am:is (equal `(defparameter a 5)
                 (esrap:parse 'short-macro-call "defparameter a 5"))))
