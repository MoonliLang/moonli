(in-package :moonli)

(5am:in-suite :moonli)

(esrap:defrule atomic-expression
    (or bracketed-expression
        expr:function-call
        quoted-expression
        expr:character
        string
        good-symbol
        number))

(esrap:defrule moonli-expression
    (or macro-call
        short-macro-call
        infix-expression
        expr:list
        expr:hash-table
        expr:hash-set))

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
