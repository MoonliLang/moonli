(in-package :moonli)

(defun load-moonli-file (source-file fasl-file)
  (declare (ignore fasl-file))
  (let ((source (alexandria:read-file-into-string source-file)))
    (print source)
    (eval (esrap:parse 'moonli source)))
  ;; (let ((token-list (with-open-file (in source-file)
  ;;                     (loop :while (listen in)
  ;;                           :collect (read-mjlisp in)))))
  ;;   (print (parse-mjlisp-token-list-to-lisp-forms token-list)))
  )

(defun compile-moonli-file (source-file fasl-file)
  (load-moonli-file source-file fasl-file)
  ;; (let ((token-list (with-open-file (in source-file)
  ;;                     (loop :while (listen in)
  ;;                           :collect (read-mjlisp in)))))
  ;;   (print (parse-mjlisp-token-list-to-lisp-forms token-list)))
  )

#|
1. We want an extensible system to recognize moonli macros such as "LET". ; ;
2. This neats to interface with the file reading above. ; ;
|#
