(defpackage :moonli/expressions
  (:use)
  (:export #:symbol
           #:character
           #:list
           #:function-call
           #:function-arglist
           #:hash-table
           #:hash-set

           #:let
           #:defun
           #:if
           #:loop

           #:defpackage
           #:defvar
           #:defparameter
           #:in-package
           #:declare
           #:declaim
           #:lambda))


(defpackage :moonli
  (:use :cl)
  (:export #:moonli
           #:moonli-expression
           #:read-moonli-from-stream
           #:read-moonli-from-string
           #:moonli-string-to-lisp-string
           #:compile-moonli-file
           #:load-moonli-file
           #:transpile-moonli-file
           #:define-moonli-macro
           #:define-moonli-short-macro

           #:main)
  (:local-nicknames (:expr :moonli/expressions)))


(5am:def-suite :moonli)
(5am:in-suite :moonli)
