(in-package :moonli)

(esrap:defrule hash-table-entry
    (and moonli-expression
         *whitespace #\:
         *whitespace moonli-expression)
  (:function (lambda (expr)
               `(,(first expr) ,(fifth expr)))))

(esrap:defrule hash-table
    (or (and #\{ *whitespace #\})
        (and #\{
             *whitespace
             hash-table-entry
             *whitespace
             (* (and #\, *whitespace hash-table-entry *whitespace))
             #\})
        (and #\{
             (+ (and *whitespace hash-table-entry
                     *whitespace #\,))
             #\}))
  (:function (lambda (expr)
               (let ((ht (gensym "HASH-TABLE"))
                     (key-value-pairs
                       (if (null (cdddr expr)) ; length = 3, first or last
                           (mapcar #'second (second expr))
                           (cons (third expr) ; middle
                                 (mapcar #'third (fifth expr))))))
                 (if (null key-value-pairs)
                     `(make-hash-table :test #'equal)
                     `(let ((,ht (make-hash-table
                                  :test #'equal
                                  :size ,(length key-value-pairs))))
                        (setf ,@(alexandria:mappend
                                 (lambda (key-value)
                                   (destructuring-bind (key value) key-value
                                     `((gethash ,key ,ht) ,value)))
                                 key-value-pairs))
                        ,ht))))))

(5am:def-test hash-table ()
  (5am:is (equal `(make-hash-table :test #'equal)
                 (esrap:parse 'hash-table "{}")))

  (optima:ematch (esrap:parse 'hash-table "{:a : 2}")
    ((list 'let (list (list ht-symbol expr))
           (list 'setf (list 'gethash key ht-symbol-2) value)
           ht-symbol-3)
     (5am:is (equal `(make-hash-table :test #'equal
                                      :size 1)
                    expr))
     (5am:is (equal ht-symbol ht-symbol-2))
     (5am:is (equal :a key))
     (5am:is (equal 2 value))
     (5am:is (equal ht-symbol ht-symbol-3))))

  (optima:ematch (esrap:parse 'hash-table "{:a : 2,}")
    ((list 'let (list (list ht-symbol expr))
           (list 'setf (list 'gethash key ht-symbol-2) value)
           ht-symbol-3)
     (5am:is (equal `(make-hash-table :test #'equal
                                      :size 1)
                    expr))
     (5am:is (equal ht-symbol ht-symbol-2))
     (5am:is (equal :a key))
     (5am:is (equal 2 value))
     (5am:is (equal ht-symbol ht-symbol-3))))

  (optima:ematch (esrap:parse 'hash-table "{:a : 2, \"b\": $cl:progn }")

    ((list 'let (list (list ht-symbol expr))
           (list 'setf
                 (list 'gethash key-1 ht-symbol-2) value-1
                 (list 'gethash key-2 ht-symbol-3) value-2)
           ht-symbol-4)

     (5am:is (equal `(make-hash-table :test #'equal
                                      :size 2)
                    expr))

     (5am:is (equal ht-symbol ht-symbol-2))
     (5am:is (equal :a key-1))
     (5am:is (equal 2 value-1))

     (5am:is (equal ht-symbol ht-symbol-3))
     (5am:is (equal "b" key-2))
     (5am:is (equal ''cl:progn value-2))

     (5am:is (equal ht-symbol ht-symbol-4)))))

(esrap:defrule hash-set
    (or (and #\{
             moonli-expression
             *whitespace
             (* (and #\, *whitespace moonli-expression *whitespace))
             #\})
        (and #\{
             (+ (and *whitespace moonli-expression
                     *whitespace #\,))
             #\}))
  (:function (lambda (expr)
               (let ((ht (gensym "HASH-TABLE"))
                     (members
                       (if (null (cdddr expr)) ; length = 3, second
                           (mapcar #'second (second expr))
                           (cons (second expr) ; first
                                 (mapcar #'third (fourth expr))))))
                 (if (null members)
                     `(make-hash-table :test #'equal)
                     `(let ((,ht (make-hash-table
                                  :test #'equal
                                  :size ,(length members))))
                        (setf ,@(alexandria:mappend
                                 (lambda (member)
                                   `((gethash ,member ,ht) cl:t))
                                 members))
                        ,ht))))))

(5am:def-test hash-set ()

  (optima:ematch (esrap:parse 'hash-set "{:a}")
    ((list 'let (list (list ht-symbol expr))
           (list 'setf (list 'gethash key ht-symbol-2) value)
           ht-symbol-3)
     (5am:is (equal `(make-hash-table :test #'equal
                                      :size 1)
                    expr))
     (5am:is (equal ht-symbol ht-symbol-2))
     (5am:is (equal :a key))
     (5am:is (equal t value))
     (5am:is (equal ht-symbol ht-symbol-3))))

  (optima:ematch (esrap:parse 'hash-set "{:a,}")
    ((list 'let (list (list ht-symbol expr))
           (list 'setf (list 'gethash key ht-symbol-2) value)
           ht-symbol-3)
     (5am:is (equal `(make-hash-table :test #'equal
                                      :size 1)
                    expr))
     (5am:is (equal ht-symbol ht-symbol-2))
     (5am:is (equal :a key))
     (5am:is (equal t value))
     (5am:is (equal ht-symbol ht-symbol-3))))

  (optima:ematch (esrap:parse 'hash-set "{:a, \"b\" , $cl:progn }")

    ((list 'let (list (list ht-symbol expr))
           (list 'setf
                 (list 'gethash key-1 ht-symbol-2) value-1
                 (list 'gethash key-2 ht-symbol-3) value-2
                 (list 'gethash key-3 ht-symbol-4) value-3)
           ht-symbol-last)

     (5am:is (equal `(make-hash-table :test #'equal
                                      :size 3)
                    expr))

     (5am:is (equal ht-symbol ht-symbol-2))
     (5am:is (equal :a key-1))
     (5am:is (equal t value-1))

     (5am:is (equal ht-symbol ht-symbol-3))
     (5am:is (equal "b" key-2))
     (5am:is (equal t value-2))

     (5am:is (equal ht-symbol ht-symbol-4))
     (5am:is (equal ''progn key-3))
     (5am:is (equal t value-3))

     (5am:is (equal ht-symbol ht-symbol-last)))))
