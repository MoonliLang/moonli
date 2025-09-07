(defpackage :moonli
  (:use :cl)
  (:export #:moonli
           #:moonli-expression
           #:read-moonli-from-string
           #:compile-moonli-file
           #:load-moonli-file
           #:define-moonli-macro
           #:define-moonli-short-macro))


(5am:def-suite :moonli)
(5am:in-suite :moonli)
