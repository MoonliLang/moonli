(in-package :moonli)

;; FIXME: Do something about the excess whitespace
(5am:in-suite :moonli)

(define-moonli-short-macro defvar
  ((name good-symbol)
   (_ +whitespace/internal)
   (_ "=")
   (_ +whitespace/internal)
   (value moonli-expression))
  `(defvar ,name ,value))

(define-moonli-short-macro defparameter
  ((name good-symbol)
   (_ +whitespace/internal)
   (_ "=")
   (_ +whitespace/internal)
   (value moonli-expression))
  `(defparameter ,name ,value))

(define-moonli-short-macro in-package
  ((name expr:symbol))
  (setf *package* (find-package name))
  `(in-package ,name))

(5am:def-test expr:in-package ()
  (unwind-protect
       (progn
         (make-package "MOONLI/TEST/IN-PACKAGE")
         (5am:is (eq (find-package "MOONLI/TEST/IN-PACKAGE")
                     (symbol-package
                      (alexandria:lastcar
                       (read-moonli-from-string
                        "in-package moonli/test/in-package; foo"))))))
    (delete-package "MOONLI/TEST/IN-PACKAGE")))

(define-moonli-short-macro ifelse
  ((test moonli-expression)
   (_ +whitespace/internal)
   (then moonli-expression)
   (_ (esrap:? +whitespace/internal))
   (else (esrap:? moonli-expression)))
  `(if ,test ,then ,else))

(5am:def-test short-macro-call ()
  (5am:is (equal `(defvar a 5)
                 (esrap:parse 'short-macro-call "defvar a = 5")))
  (5am:is (equal `(defparameter a 5)
                 (esrap:parse 'short-macro-call "defparameter a = 5")))
  (5am:is (equal `(if a 5 nil)
                 (esrap:parse 'short-macro-call "ifelse a 5")))
  (5am:is (equal `(if a :hello :bye)
                 (esrap:parse 'short-macro-call "ifelse a :hello :bye"))))

(define-moonli-short-macro declare
  ((decl-specs (and expr:function-call
                    (* (and *whitespace/internal
                            #\,
                            *whitespace/internal
                            expr:function-call)))))
  `(declare ,(first decl-specs)
            ,@(mapcar #'fourth (second decl-specs))))

(5am:def-test expr:declare ()
  (5am:is (equal `(declare (type single-float x y))
                 (esrap:parse 'short-macro-call "declare type(single-float, x, y)")))
  (5am:is (equal `(declare (type single-float x y)
                           (optimize (debug 3)))
                 (esrap:parse 'short-macro-call "declare type(single-float, x, y), optimize(debug(3))"))))

(define-moonli-short-macro declaim
  ((decl-specs (and expr:function-call
                    (* (and *whitespace/internal
                            #\,
                            *whitespace/internal
                            expr:function-call)))))
  `(declaim ,(first decl-specs)
            ,@(mapcar #'fourth (second decl-specs))))

(5am:def-test expr:declaim ()
  (5am:is (equal `(declaim (inline foo))
                 (esrap:parse 'short-macro-call "declaim inline(foo)")))
  (5am:is (equal `(declaim (type hash-table *map*))
                 (esrap:parse 'short-macro-call "declaim type(hash-table, *map*)"))))
