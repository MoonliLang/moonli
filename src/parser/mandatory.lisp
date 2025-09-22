(in-package #:moonli)

(defmacro define-mandatory (name expr)
  `(esrap:defrule ,(alexandria:symbolicate "MANDATORY-" name)
       (esrap:? ,expr)
     (:lambda (char esrap:&bounds start)
       (if (and (stringp char)
                (string= char ,expr))
           ,expr
           (error 'moonli-parse-error :expectation ,expr :position start)))))

(define-mandatory colon ":")
(define-mandatory comma ",")
(define-mandatory open-paren "(")
(define-mandatory close-paren ")")
(define-mandatory open-brace "{")
(define-mandatory close-brace "}")
