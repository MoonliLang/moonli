(in-package :moonli)

(esrap:defrule for-binding
    (and (or expr:symbol expr:list)
         *whitespace/internal
         expr:symbol
         *whitespace/internal
         moonli-expression)
  (:function (lambda (expr)
               (optima:match (first expr)
                 ((list* 'list args)
                  `(,args ,(third expr) ,(fifth expr)))
                 (_
                  `(,(first expr) ,(third expr) ,(fifth expr)))))))

(esrap:defrule for-bindings
    (and for-binding
         *whitespace
         (* (and mandatory-comma *whitespace for-binding *whitespace)))
  (:function (lambda (expr)
               `(,(first expr)
                 ,@(mapcar #'third (third expr))))))

(define-moonli-macro for:for
  ((for-bindings for-bindings)
   (_ *whitespace)
   (_ mandatory-colon)
   (_ *whitespace)
   (body (esrap:? moonli)))
  `(for:for ,for-bindings
     ,@(rest body)))

(def-test for:for (macro-call)
  (:lisp (for-minimal:for (((i j) in (list (list 1 2) (list 3 4))))
           (print (+ i j)))
   :moonli "for:for (i,j) in ((1,2),(3,4)):
  print(i + j)
end")
  (:lisp (for-minimal:for ((i in (list 1 2 3))
                           (j in (list 2 3 4)))
           (print (+ i j)))
   :moonli "for:for i in (1,2,3), j in (2,3,4):
  print(i + j)
end"))
