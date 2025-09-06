(in-package :moonli)

(defun format-moonli-parse-error (string position)
  (let* ((pn (position #\newline string :end position :from-end t))
         (nn (position #\newline string :start position))
         (start-dist (- position pn))
         (end-dist (- nn position)))
    (with-output-to-string (*standard-output*)
      (write-string (subseq string 0 nn))
      (write-char #\newline)
      (loop :repeat start-dist :do (write-char #\-))
      (write-char #\^)
      (loop :repeat (- end-dist 2) :do (write-char #\-))
      (write-string (subseq string nn)))))

(defun read-moonli-from-string (string)
  (let ((end (length string))
        (pos 0)
        (exprs ()))
    (loop :initially (setf pos (or (nth-value 1 (esrap:parse '*whitespace/all string :start pos :junk-allowed t))
                                   end))
          :while (< pos end)
          :do (multiple-value-bind (result next-pos success)
                  (esrap:parse 'moonli-expression string :start pos :junk-allowed t)
                (unless success
                  (error "Error while parsing from position ~A: ~%~%~A"
                         pos
                         (format-moonli-parse-error string pos)))
                (push result exprs)
                (setf pos (or next-pos end)))
              (setf pos (or (nth-value 1 (esrap:parse '*whitespace/all string :start pos :junk-allowed t))
                            end)))
    `(progn ,@(nreverse exprs))))

(defun load-moonli-file (source-file fasl-file)
  (if (string= "fasl" (pathname-type source-file))
      (load source-file)
      (error "Don't know how to load when source-file is ~S and fasl-file is ~S" source-file fasl-file)))

(defun compile-moonli-file (source-file fasl-file)
  (format *standard-output* "; transpiling ~A~%" (namestring source-file))
  (let* ((source (alexandria:read-file-into-string source-file))
         (target-file (make-pathname :defaults source-file :type "lisp"))
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
    (format *standard-output* "; compiling file ~A~%" (namestring target-file))
    (asdf:compile-file* target-file :output-file fasl-file)))

#|
1. We want an extensible system to recognize moonli macros such as "LET". ; ;
2. This neats to interface with the file reading above. ; ;
|#
