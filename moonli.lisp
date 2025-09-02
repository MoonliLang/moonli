(in-package :moonli)

(defun load-moonli-file (source-file fasl-file)
  (print :loading)
  (if (string= "fasl" (pathname-type source-file))
      (load source-file)
      (error "Don't know how to load when source-file is ~S and fasl-file is ~S" source-file fasl-file)))

(defun compile-moonli-file (source-file fasl-file)
  (print :compiling)
  (print (list "compilation-source: " source-file))
  (format *standard-output* "; transpiling ~A~%" (namestring source-file))
  (let* ((source (alexandria:read-file-into-string source-file))
         (target-file (make-pathname :defaults source-file :type "lisp"))
         (target (esrap:parse 'moonli source)))
    (format *standard-output* ";  to ~A~%" (namestring target-file))
    ;; (setq *file-string* source)
    (with-open-file (out target-file
                         :if-does-not-exist :create
                         :if-exists :supersede
                         :direction :output)
      (format out ";;; This file was automatically generated.~%")
      (format out ";;; Do NOT edit by hand. It will be overwritten.~%")
      (format out ";;; Edit or Replace the corrsponding .moonli file instead!~%~%")
      (dolist (form (cdr target))
        (write form :stream out)
        (terpri out)
        (terpri out)))
    (format *standard-output* "; wrote ~A~%" (namestring target-file))
    (format *standard-output* "; compiling file ~A~%" (namestring target-file))
    (asdf:compile-file* target-file :output-file fasl-file)))

#|
1. We want an extensible system to recognize moonli macros such as "LET". ; ;
2. This neats to interface with the file reading above. ; ;
|#
