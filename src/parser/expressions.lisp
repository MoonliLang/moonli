(in-package :moonli)

(5am:in-suite :moonli)

(esrap:defrule atomic-expression
    (or character-literal
        string-literal
        number
        good-symbol))

(esrap:defrule moonli-expression
    (or macro-call
        short-macro-call
        infix-expression
        typed-expression
        bracketed-expression
        function-call
        list
        quoted-expression
        atomic-expression))

(esrap:defrule typed-expression
    (and moonli-expression
         (or (and *whitespace
                  "::"
                  *whitespace
                  moonli-expression)
             (and)))
  (:function (lambda (expr)
               (if (second expr)
                   `(the ,(fourth (second expr))
                         ,(first expr))
                   (first expr)))))

(esrap:defrule moonli-expression/whitespace
    (and *whitespace
         moonli-expression
         *whitespace)
  (:function second))

(esrap:defrule moonli
    (and (* (or whitespace/internal whitespace/end))
         (and moonli-expression
              (* (or whitespace/internal)))
         (* (and (* whitespace/end)
                 (* (or whitespace/internal whitespace/end))
                 moonli-expression
                 (* whitespace/internal)))
         (* (or whitespace/internal whitespace/end)))
  (:function (lambda (exprs)
               `(progn
                  ,(first (second exprs))
                  ,@(mapcar #'third (third exprs))))))

(5am:def-test typed-expression ()
  (5am:is (equal `(the number 2)
                 (esrap:parse 'moonli-expression "2::number")))
  (5am:is (equal `(the number 2)
                 (esrap:parse 'moonli-expression "(2::number)")))
  (5am:is (equal `(the number a)
                 (esrap:parse 'moonli-expression "a::number")))
  (5am:is (equal `(the number a)
                 (esrap:parse 'moonli-expression "(a::number)")))
  (5am:is (equal `(+ 2 (the number a))
                 (esrap:parse 'moonli-expression "2 + (a::number)")))
  (5am:is (equal `(the number (+ 2 a))
                 (esrap:parse 'moonli-expression "(2 + a)::number"))))
