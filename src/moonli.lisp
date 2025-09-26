(in-package :moonli)

(defun read-moonli-from-stream (stream)
  (loop :with text := ""
        :with readp := t
        :while readp
        :do (multiple-value-bind (expr errorp)
                (ignore-errors
                 (read-moonli-from-string
                  (setf text (uiop:strcat text (read-line stream) #\newline))))
              (unless errorp
                (setf readp nil)
                (return expr)))))

(defun read-moonli-from-string (string)
  "NOTE: Some moonli forms like defpackage and in-package can have side-effects."
  (let ((end (length string))
        (pos 0)
        (exprs ())
        (*moonli-parse-string* string))
    (loop :initially (setf pos (or (nth-value 1 (esrap:parse '*whitespace/all string :start pos :junk-allowed t))
                                   end))
          :while (< pos end)
          :do (multiple-value-bind (result next-pos success)
                  (esrap:parse 'moonli-expression string :start pos :junk-allowed t)
                (unless success
                  (error 'moonli-parse-error :position pos))
                (push result exprs)
                (setf pos (or next-pos end))
                (setf pos (or (nth-value 1 (esrap:parse '*whitespace/all string :start pos :junk-allowed t))
                              end))))
    `(progn ,@(nreverse exprs))))

(defun load-moonli-file (moonli-file &key (transpile t))
  (assert (string= "moonli" (pathname-type moonli-file)))
  (if transpile
      (load (transpile-moonli-file moonli-file))
      (eval (read-moonli-from-string
             (alexandria:read-file-into-string moonli-file)))))

(defun transpile-moonli-file (moonli-file)
  (format *standard-output* "; transpiling ~A~%" (namestring moonli-file))
  (let* ((source (alexandria:read-file-into-string moonli-file))
         (target-file (make-pathname :defaults moonli-file :type "lisp"))
         (target (read-moonli-from-string source)))
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
        (write form :stream out :case :downcase)
        (terpri out)
        (terpri out)))
    (format *standard-output* "; wrote ~A~%" (namestring target-file))
    target-file))

(defun compile-moonli-file (source-file fasl-file)
  (let ((lisp-source-file (transpile-moonli-file source-file)))
    (asdf:compile-file* lisp-source-file :output-file fasl-file)))

#|
1. We want an extensible system to recognize moonli macros such as "LET". ; ;
2. This neats to interface with the file reading above. ; ;
|#
