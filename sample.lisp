;;; This file was automatically generated.
;;; Do NOT edit by hand. It will be overwritten.
;;; Edit or Replace the corrsponding .moonli file instead!

(defpackage :moonli-sample
  (:use :cl)
  (:export foo))

(in-package :moonli-sample)

(defvar foo 'abc)

(print *package*)

(print foo)

(format t "hello! ~s:~a~%" 'foo foo)

(let ((a 1))
  a)

