(in-package :moonli)

(5am:in-suite :moonli)
;; FIXME: Do something about the excess whitespace

(esrap:defrule let-bindings
    (or (and (and good-symbol
                  +whitespace
                  #\=
                  +whitespace
                  moonli-expression
                  *whitespace)
             (* (and #\,
                     +whitespace
                     good-symbol
                     +whitespace
                     #\=
                     +whitespace
                     moonli-expression
                     *whitespace)))
        (* whitespace))
  (:function (lambda (expr)
               (if (null expr)
                   nil
                   (cons (list (nth 0 (first expr))
                               (nth 4 (first expr)))
                         (mapcar (lambda (expr)
                                   (list (nth 2 expr)
                                         (nth 6 expr)))
                                 (second expr)))))))

(define-moonli-macro let

  ((let-bindings let-bindings)
   (_ #\:)
   (let-body (esrap:? moonli)))

  `(let ,let-bindings
     ,@(rest let-body)))

(5am:def-test let ()
  (5am:is (equal `(let ((a 2) (b 3))
                    (+ a b))
                 (esrap:parse 'macro-call
                              "begin let a = 2, b = 3:
   a + b
end let"))))

(esrap:defrule elif-clause
    (and *whitespace
         "elif"
         *whitespace
         moonli-expression
         *whitespace
         "then"
         moonli
         *whitespace)
  (:function (lambda (expr)
               (optima:ematch expr
                 ((list _ _ _ condition _ _ statements _)
                  `(,condition ,@(rest statements)))))))

(define-moonli-macro if
  ((condition moonli-expression)
   (_ (and *whitespace "then" *whitespace))
   (then-part moonli)
   (_ *whitespace)
   (elif-clauses (* elif-clause))
   (_ (esrap:? (and *whitespace "else" *whitespace)))
   (else-part (esrap:? moonli)))
  `(cond (,condition
          ,@(rest then-part))
         ,@elif-clauses
         (t
          ,@(rest else-part))))


(5am:def-test if ()
  (5am:is (equal `(cond (a b) (t))
                 (esrap:parse 'macro-call "begin if a
then b
end if")))
  (5am:is (equal `(cond (a b c) (t))
                 (esrap:parse 'macro-call "begin if a
then b; c
end if")))
  (5am:is (equal `(cond (a b) (t c))
                 (esrap:parse 'macro-call "begin if a
then b
else c
end if")))
  (5am:is (equal `(cond (a b d) (t c e))
                 (esrap:parse 'macro-call "begin if a
then b; d
else c; e
end if")))
  (5am:is (equal `(cond (a b) (c d e) (t f))
                 (esrap:parse 'macro-call "begin if a
then b
elif c then d; e
else f
end if")))
  (5am:is (equal `(the boolean (cond (a b) (t c)))
                 (esrap:parse 'moonli-expression
                              "(begin if a
then b
else c
end if)::boolean")))
  (5am:is (equal `(cond ((null args)
                         0)
                        (t
                         1))
                 (esrap:parse 'macro-call "begin if null(args) then 0 else 1 end if")))
  (5am:is (equal `(cond ((null args)
                         0)
                        (t
                         (first args)))
                 (esrap:parse 'macro-call "begin if null(args) then
    0
    else first(args)
end if")))

  (5am:is (equal `(cond ((null args)
                         0)
                        (t
                         (+ 2 3)))
                 (esrap:parse 'macro-call "begin if null(args)
then 0
else 2 + 3
end if")))
  (5am:is (equal `(cond ((null args)
                         0)
                        (t
                         (+ (first args)
                            (add (rest args)))))
                 (esrap:parse 'macro-call "begin if null(args)
then 0
else first(args) + add(rest(args))
end if"))))

(define-moonli-macro defun
  ((name good-symbol)
   (_ *whitespace)
   (lambda-list list)
   (_ #\:)
   (body (esrap:? moonli)))
  `(defun ,name ,(rest lambda-list) ,@(rest body)))


(5am:def-test defun ()
  (5am:is (equal `(defun add (&rest args) args)
                 (esrap:parse 'macro-call "begin defun add (&rest, args):
 args
end defun")))
  (5am:is (equal `(progn
                    (defun add (&rest args)
                      (cond ((null args)
                             0)
                            (t
                             (+ (first args)
                                (add (rest args)))))))
                 (esrap:parse 'moonli "
begin defun add(&rest, args):
  begin if null(args)
  then 0
  else first(args) + add(rest(args))
  end if
end defun
"))))

(esrap:defrule defpackage-option
    (and string-designator
         *whitespace
         (esrap:? (or (and string-designator
                           *whitespace
                           (* (and #\,
                                   *whitespace
                                   string-designator
                                   *whitespace)))
                      (+ (and *whitespace
                              string-designator
                              *whitespace
                              #\,
                              *whitespace))))
         #\;
         *whitespace)
  (:function (lambda (expr)
               (optima:ematch expr
                 ((list option-name _ args _ _)
                  `(,(intern (string-upcase option-name) :keyword)
                    ,@(if (null (nthcdr 3 args)) ; length=3, first option
                          (cons (first args)
                                (mapcar #'third (third args)))
                          (mapcar #'second args))))))))

(define-moonli-macro defpackage
  ((name string-designator)
   (_ *whitespace)
   (options (* defpackage-option)))
  `(defpackage ,name ,@options))
