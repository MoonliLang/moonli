;;; This file was automatically generated.
;;; Do NOT edit by hand. It will be overwritten.
;;; Edit or Replace the corrsponding .moonli file instead!

(defpackage :moonli-sample
  (:use :cl)
  (:export :foo))

(in-package :moonli-sample)

(defvar foo 'abc)

(print *package*)

(print foo)

(format t "hello! ~s: ~a~%" 'foo foo)

(defparameter ht (moonli::fill-hash-table (:a 2) ("b" 3)))

(let ((a 1))
  a)

(let ((a 1))
  (format t "a + gethash(:a, ht) + gethash(\"b\", ht) = ~a~%"
          (+ (+ a (gethash :a ht)) (gethash "b" ht))))

