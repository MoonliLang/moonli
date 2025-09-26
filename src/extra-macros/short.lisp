(in-package :moonli)

(define-moonli-short-macro ifelse
  ((test moonli-expression)
   (_ +whitespace/internal)
   (then moonli-expression)
   (_ (esrap:? +whitespace/internal))
   (else (esrap:? moonli-expression)))
  `(if ,test ,then ,else))

(def-test ifelse (short-macro-call)
  (:lisp (if a 5 nil)
   :moonli "ifelse a 5")
  (:lisp (if a :hello :bye)
   :moonli "ifelse a :hello :bye"))

(define-moonli-short-macro lm
  ((lambda-list lambda-parameter-list)
   (_ *whitespace/internal)
   (_ mandatory-colon)
   (_ *whitespace/internal)
   (form moonli-expression))
  `(lambda ,lambda-list ,form))

(def-test lm (short-macro-call)
  (:lisp (lambda () nil)
   :moonli "lm (): nil")
  (:lisp (lambda (x) x)
   :moonli "lm (x): x")
  (:lisp (lambda (x y) (+ x y))
   :moonli "lm (x, y): x + y"))
