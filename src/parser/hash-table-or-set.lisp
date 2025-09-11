(in-package :moonli)

(5am:in-suite :moonli)

(defmacro fill-hash-table (&rest key-value-pairs)
  (let ((ht (gensym "HASH-TABLE")))
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
           ,ht))))

(esrap:defrule hash-table-entry
    (and moonli-expression
         *whitespace #\:
         *whitespace moonli-expression)
  (:function (lambda (expr)
               `(,(first expr) ,(fifth expr)))))

(esrap:defrule expr:hash-table
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
               (let ((key-value-pairs
                       (if (null (cdddr expr)) ; length = 3, first or last
                           (mapcar #'second (second expr))
                           (cons (third expr) ; middle
                                 (mapcar #'third (fifth expr))))))
                 `(fill-hash-table ,@key-value-pairs)))))

(5am:def-test expr:hash-table ()
  (5am:is (equal `(fill-hash-table)
                 (esrap:parse 'expr:hash-table "{}")))
  (5am:is (equal `(fill-hash-table (:a 2))
                 (esrap:parse 'expr:hash-table "{:a : 2}")))
  (5am:is (equal `(fill-hash-table (:a 2))
                 (esrap:parse 'expr:hash-table "{:a : 2,}")))
  (5am:is (equal `(fill-hash-table (:a 2) ("b" 'progn))
                 (esrap:parse 'expr:hash-table "{:a : 2, \"b\": $cl:progn }"))))

(defmacro fill-hash-set (&rest members)
  (let ((hs (gensym "HASH-SET")))
    (if (null members)
        `(make-hash-table :test #'equal)
        `(let ((,hs (make-hash-table
                     :test #'equal
                     :size ,(length members))))
           (setf ,@(alexandria:mappend
                    (lambda (member)
                      `((gethash ,member ,hs) cl:t))
                    members))
           ,hs))))

(esrap:defrule expr:hash-set
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
               (let ((members
                       (if (null (cdddr expr)) ; length = 3, second
                           (mapcar #'second (second expr))
                           (cons (second expr) ; first
                                 (mapcar #'third (fourth expr))))))
                 `(fill-hash-set ,@members)))))

(5am:def-test expr:hash-set ()
  (5am:is (equal `(fill-hash-set :a)
                 (esrap:parse 'expr:hash-set "{:a}")))
  (5am:is (equal `(fill-hash-set :a)
                 (esrap:parse 'expr:hash-set "{:a,}")))
  (5am:is (equal `(fill-hash-set :a "b" 'progn)
                 (esrap:parse 'expr:hash-set "{:a, \"b\" , $cl:progn }"))))
