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

(define-moonli-macro |let|

  ((let-bindings let-bindings)
   (_ #\:)
   (let-body (esrap:? moonli)))

  `(let ,let-bindings
     ,@(rest let-body)))

(5am:def-test |let| ()
  (5am:is (equal `(let ((a 2) (b 3))
                    (+ a b))
                 (esrap:parse 'macro-call
                              "begin let A = 2, B = 3:
   A + B
end let"))))


(define-moonli-macro |if|
  ((condition moonli-expression)
   (_ *whitespace)
   (_ "then")
   (_ *whitespace)
   (then-part moonli-expression)
   (_ *whitespace)
   (_ "else")
   (_ *whitespace)
   (else-part moonli-expression))
  `(if ,condition ,then-part ,else-part))


(5am:def-test |if| ()
  (5am:is (equal `(if a b c)
                 (esrap:parse 'macro-call "begin if A
then B
else C
end if")))
  (5am:is (equal `(the boolean (if a b c))
                 (esrap:parse 'moonli-expression
                              "(begin if A
then B
else C
end if)::BOOLEAN")))
  (5am:is (equal `(if (null args)
                      0
                      1)
                 (esrap:parse 'macro-call "begin if NULL(ARGS) then 0 else 1 end if")))
  (5am:is (equal `(if (null args)
                      0
                      (first args))
                 (esrap:parse 'macro-call "begin if NULL(ARGS) then
    0
    else FIRST(ARGS)
end if")))

  (5am:is (equal `(if (null args)
                      0
                      (+ 2 3))
                 (esrap:parse 'macro-call "begin if NULL(ARGS)
then 0
else 2 + 3
end if")))
  (5am:is (equal `(if (null args)
                      0
                      (+ (first args)
                         (add (rest args))))
                 (esrap:parse 'macro-call "begin if NULL(ARGS)
then 0
else FIRST(ARGS) + ADD(REST(ARGS))
end if"))))

(define-moonli-macro |defun|
  ((name good-symbol)
   (_ *whitespace)
   (lambda-list list)
   (_ #\:)
   (body (esrap:? moonli)))
  `(defun ,name ,(rest lambda-list) ,@(rest body)))


(5am:def-test |defun| ()
  (5am:is (equal `(defun add (&rest args) args)
                 (esrap:parse 'macro-call "begin defun ADD (&REST, ARGS):
 ARGS
end defun")))
  (5am:is (equal `(progn
                    (defun add (&rest args)
                      (if (null args)
                          0
                          (+ (first args)
                             (add (rest args))))))
                 (esrap:parse 'moonli "
begin defun ADD(&REST, ARGS):
  begin if NULL(ARGS)
  then 0
  else FIRST(ARGS) + ADD(REST(ARGS))
  end if
end defun
"))))


;; (define-moonli-macro defpackage
;;   ((name (or good-symbol string-literal))
;;    (options defpackage-options))
;;   `(defpackage ,name ,@options))
