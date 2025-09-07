(defpackage :moonli-repl
  (:use :cl))

(in-package :moonli-repl)

(setf cl-repl:*read-function* 'moonli:read-moonli-from-string)
