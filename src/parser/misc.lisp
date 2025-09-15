(in-package :moonli)

(5am:in-suite :moonli)

(esrap:defrule bracketed-expression
    (and #\( *whitespace moonli-expression *whitespace #\))
  (:function third))

(esrap:defrule quoted-expression
    (and #\$ atomic-expression)
  (:function (lambda (expr)
               `(cl:quote ,(second expr)))))

(esrap:defrule expr:character
    (and #\' character #\')
  (:function second))

;; TODO: Generalization for escape chars

(esrap:defrule string
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
    (or string expr:symbol))

(esrap:defrule expr:list
    (or (and #\( *whitespace #\))
        (and #\(
             *whitespace
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
                     (if (null (cdddr expr)) ; length = 3, first or last
                         (mapcar #'second (second expr))
                         (cons (third expr) ; middle
                               (mapcar #'third (fifth expr))))))))

(5am:def-test expr:list ()
  (5am:is (equal '(list)
                 (esrap:parse 'expr:list "()")))
  (5am:is (equal '(list)
                 (esrap:parse 'expr:list "( )")))
  (5am:is (equal '(list)
                 (esrap:parse 'expr:list (format nil "(~%)"))))
  (5am:is (equal '(list 3)
                 (esrap:parse 'expr:list "(3 ,)")))
  (5am:is (equal '(list 3)
                 (esrap:parse 'expr:list (format nil "(~%  3~%,)"))))
  (5am:is (equal '(list 3)
                 (esrap:parse 'expr:list "(3,)")))
  (5am:is (equal '(list 3 :hello)
                 (esrap:parse 'expr:list "(3,:hello)")))
  (5am:is (equal '(list 3 :hello)
                 (esrap:parse 'expr:list "(3, :hello)")))
  (5am:is (equal '(list 3 :hello)
                 (esrap:parse 'expr:list "(3, :hello )")))
  (5am:is (equal '(list 3 :hello)
                 (esrap:parse 'expr:list "(3, :hello, )")))
  (5am:is (equal '(list 3 (null a))
                 (esrap:parse 'expr:list "(3,null(a))"))))

(esrap:defrule expr:function-call
    ;; Don't put a whitespace
    ;; FIXME: Handle macros
    (and atomic-expression
         (or (and #\( *whitespace moonli-expression *whitespace #\))
             expr:list))
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
