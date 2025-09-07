(in-package :moonli)

(5am:in-suite :moonli)

(esrap:defrule infix-expression
    (and moonli-expression
         *whitespace
         infix-operator
         *whitespace
         moonli-expression)
  (:function (lambda (expr)
               (destructuring-bind (arg1 w1 op w2 arg2) expr
                 (declare (ignore w1 w2))
                 (list op arg1 arg2)))))

(esrap:defrule bracketed-expression
    (and #\( *whitespace moonli-expression *whitespace #\))
  (:function third))

(esrap:defrule quoted-expression
    (and #\$ moonli-expression)
  (:function (lambda (expr)
               `(cl:quote ,(second expr)))))

(esrap:defrule character-literal
    (and #\' character #\')
  (:function second))

;; TODO: Generalization for escape chars

(esrap:defrule string-literal
    (and #\"
         (* (or (and #\\ #\")
                (not #\")))
         #\")
  (:function (lambda (expr)
               (with-output-to-string (s)
                 (dolist (elt (second expr))
                   (etypecase elt
                     (character (write-char elt s))
                     (cons
                      (assert (and (null (cddr elt))
                                   (string= "\\" (first elt))))
                      (write-string (second elt) s))))))))

(esrap:defrule string-designator
    (or string-literal symbol))

(esrap:defrule list
    (or (and #\( *whitespace #\))
        (and #\(
             moonli-expression
             *whitespace
             (+ (and #\, *whitespace moonli-expression *whitespace))
             #\))
        (and #\(
             (+ (and *whitespace
                     moonli-expression
                     *whitespace #\, *whitespace))
             #\)))
  (:function (lambda (expr)
               (cons 'list
                     (if (null (cdddr expr)) ; length = 3
                         (mapcar #'second (second expr))
                         (cons (second expr)
                               (mapcar #'third (fourth expr))))))))

(5am:def-test list ()
  (5am:is (equal '(list)
                 (esrap:parse 'list "()")))
  (5am:is (equal '(list)
                 (esrap:parse 'list "( )")))
  (5am:is (equal '(list)
                 (esrap:parse 'list (format nil "(~%)"))))
  (5am:is (equal '(list 3)
                 (esrap:parse 'list "(3 ,)")))
  (5am:is (equal '(list 3)
                 (esrap:parse 'list "(3,)")))
  (5am:is (equal '(list 3 :hello)
                 (esrap:parse 'list "(3,:hello)")))
  (5am:is (equal '(list 3 :hello)
                 (esrap:parse 'list "(3, :hello)")))
  (5am:is (equal '(list 3 :hello)
                 (esrap:parse 'list "(3, :hello )")))
  (5am:is (equal '(list 3 :hello)
                 (esrap:parse 'list "(3, :hello, )")))
  (5am:is (equal '(list 3 (null a))
                 (esrap:parse 'list "(3,null(a))"))))

(esrap:defrule function-call
    ;; Don't put a whitespace
    ;; FIXME: Handle macros
    (and good-symbol
         (or (and #\( *whitespace moonli-expression *whitespace #\))
             list))
  (:function (lambda (expr)
               (cons (first expr)
                     (if (eq 'list (first (second expr)))
                         (rest (second expr))
                         (cons (third (second expr)) nil))))))

"
begin let a=3, b=5:
  a+b
end
"
