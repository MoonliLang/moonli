(defpackage :moonli-repl
  (:use :cl))

(in-package :moonli-repl)

(setf cl-repl:*read-function* 'moonli:read-moonli-from-string)
(setf cl-repl:*line-continue-function*
      (lambda (string)
        (nth-value 1 (ignore-errors (moonli:read-moonli-from-string string)))))
