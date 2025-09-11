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

(5am:def-test expr:let ()
  (5am:is (equal `(let ((a 2) (b 3))
                    (+ a b))
                 (esrap:parse 'macro-call
                              "let a = 2, b = 3:
   a + b
end")))
  (5am:is (equal `(let ((a 2) (b 3))
                    (+ a b))
                 (esrap:parse 'macro-call
                              "let a = 2, b = 3:
   a + b
end let")
                 )))

(esrap:defrule elif-clause
    (and *whitespace
         "elif"
         *whitespace
         moonli-expression
         *whitespace/internal
         ":"
         moonli
         *whitespace)
  (:function (lambda (expr)
               (optima:ematch expr
                 ((list _ _ _ condition _ _ statements _)
                  `(,condition ,@(rest statements)))))))

(define-moonli-macro if
  ((condition moonli-expression)
   (_ (and *whitespace/internal ":" *whitespace))
   (then-part moonli)
   (_ *whitespace)
   (elif-clauses (* elif-clause))
   (_ (esrap:? (and *whitespace "else" *whitespace/internal ":" *whitespace)))
   (else-part (esrap:? moonli)))
  `(cond (,condition
          ,@(rest then-part))
         ,@elif-clauses
         (t
          ,@(rest else-part))))


(5am:def-test expr:if ()
  (5am:is (equal `(cond (a b) (t))
                 (esrap:parse 'macro-call "if a: b
end if")))
  (5am:is (equal `(cond (a b c) (t))
                 (esrap:parse 'macro-call "if a:
  b; c
end")))
  (5am:is (equal `(cond (a b) (t c))
                 (esrap:parse 'macro-call "if a: b
else: c
end if")))
  (5am:is (equal `(cond (a b d) (t c e))
                 (esrap:parse 'macro-call "if a:
   b; d
else:
   c; e
end if")))
  (5am:is (equal `(cond (a b) (c d e) (t f))
                 (esrap:parse 'macro-call "if a: b
elif c: d; e
else: f
end if")))
  (5am:is (equal `(the boolean (cond (a b) (t c)))
                 (esrap:parse 'moonli-expression
                              "(if a: b else: c; end)::boolean")))
  (5am:is (equal `(cond ((null args)
                         0)
                        (t
                         1))
                 (esrap:parse 'macro-call "if null(args): 0; else: 1 end")))
  (5am:is (equal `(cond ((null args)
                         0)
                        (t
                         (first args)))
                 (esrap:parse 'macro-call "if null(args):
    0
else:
    first(args)
end if")))

  (5am:is (equal `(cond ((null args)
                         0)
                        (t
                         (+ 2 3)))
                 (esrap:parse 'macro-call "if null(args):
  0
else:
  2 + 3
end if")))
  (5am:is (equal `(cond ((null args)
                         0)
                        (t
                         (+ (first args)
                            (add (rest args)))))
                 (esrap:parse 'macro-call "if null(args):
  0
else:
  first(args) + add(rest(args))
end if"))))



(5am:def-test macros-are-package-local ()
  (unwind-protect
       (handler-bind ((warning #'muffle-warning))
         (make-package "DUMMY")
         (intern "IF" "DUMMY")
         (export (find-symbol "IF" "DUMMY") "DUMMY")
         (eval `(define-moonli-macro ,(find-symbol "IF" "DUMMY")
                  ((test moonli-expression)
                   (_ +whitespace/internal)
                   (then moonli-expression)
                   (_ +whitespace/internal)
                   (else moonli-expression))
                  (list 'if test then else)))
         (let ((*package* (find-package "DUMMY")))
           (5am:is (equal `(if "hello" "world" "bye")
                          (esrap:parse 'macro-call "if \"hello\" \"world\" \"bye\" end"))))
         (let ((*package* (find-package :moonli)))
           (5am:is (equal `(cond ("hello" "world") (t "bye"))
                          (esrap:parse 'macro-call "if \"hello\": \"world\"; else: \"bye\" end")))
           (5am:is (equal `(if "hello" "world" "bye")
                          (esrap:parse 'macro-call "dummy:if \"hello\" \"world\" \"bye\" end")))))
    (if (find-package "DUMMY") (delete-package "DUMMY"))))


(define-moonli-macro defun
  ((name good-symbol)
   (_ *whitespace)
   (lambda-list (or (and #\( *whitespace good-symbol *whitespace #\))
                    expr:list))
   (_ #\:)
   (body (esrap:? moonli)))
  `(defun ,name ,(if (eq 'list (first lambda-list))
                     (rest lambda-list)
                     `(,(third lambda-list)))
     ,@(rest body)))


(5am:def-test expr:defun ()
  (5am:is (equal `(defun our-identity (x) x)
                 (esrap:parse 'macro-call "defun our-identity(x): x end")))
  (5am:is (equal `(defun add (&rest args) args)
                 (esrap:parse 'macro-call "defun add (&rest, args):
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
defun add(&rest, args):
  if null(args):
    0
  else:
    first(args) + add(rest(args))
  end if
end
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
  (let ((form `(defpackage ,name ,@options)))
    (eval form)
    form))
