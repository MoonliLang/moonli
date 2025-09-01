(in-package :moonli)

(esrap:defrule number
    (or float decimal)
  (:text t)
  (:function parse-number:parse-number))

(esrap:defrule float
    (and decimal
         (or #\d #\e #\f #\l #\s
             #\D #\E #\F #\L #\S)
         decimal))

(esrap:defrule decimal
    (or non-negative-decimal
        (and #\- non-negative-decimal)))

(esrap:defrule non-negative-decimal
    (or (and (+ numeric-character)
             #\.
             (+ numeric-character))
        (and #\. (+ numeric-character))
        (and (+ numeric-character) #\.)
        (+ numeric-character)))
