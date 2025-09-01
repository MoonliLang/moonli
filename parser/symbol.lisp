(in-package :moonli)

(esrap:defrule simple-symbol
    (or (and #\|
             (+ (or (and #\\ #\|)
                    (not #\|)))
             #\|)
        (and (or (alpha-char-p character)
                 #\&
                 #\*
                 #\-)
             (* (not non-symbol-chars))))
  (:text t))

(esrap:defrule symbol
    (or (and #\: simple-symbol)
        (and simple-symbol #\: simple-symbol)
        (and simple-symbol))
  (:function (lambda (expr)
               (optima:match expr
                 ((list package-name ":" symbol-name)
                  (intern symbol-name (find-package package-name)))
                 ((list ":" symbol-name)
                  (intern symbol-name :keyword))
                 ((list symbol-name)
                  (intern symbol-name))))))

(defun good-symbol-p (symbol)
  (not (member symbol '(begin end true false) :test #'string-equal)))

(esrap:defrule good-symbol (good-symbol-p symbol))
