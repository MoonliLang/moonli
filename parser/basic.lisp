(in-package :moonli)

(esrap:defrule whitespace/internal
    (or #\Space #\Tab)
  (:constant nil))

(esrap:defrule +whitespace/internal
    (+ whitespace/internal)
  (:constant nil))

(esrap:defrule whitespace/end
    (or #\newline #\return #\;)
  (:constant nil))

(esrap:defrule whitespace
    (or #\space #\tab #\newline #\return)
  (:constant nil))

(esrap:defrule +whitespace
    (+ whitespace)
  (:constant nil))

(esrap:defrule *whitespace
    (* whitespace)
  (:constant nil))

(esrap:defrule *whitespace/end
    (* whitespace/end)
  (:constant nil))

(esrap:defrule open-bracket
    (and (or #\( #\[ #\{) *whitespace)
  (:function first))
(esrap:defrule close-bracket
    (and (or #\) #\] #\}) *whitespace)
  (:function first))

(esrap:defrule numeric-character (esrap:character-ranges (#\0 #\9)))

(esrap:defrule non-symbol-chars
    (or #\| #\: #\, #\$ #\' #\" open-bracket close-bracket
        whitespace/internal whitespace/end))

(esrap:defrule infix-operator
    (and (or "==" "=" "+" "-" "*" "/" "^") +whitespace/internal)
  ;; FIXME: Precedence and brackets?
  (:function (lambda (arg)
               (alexandria:switch ((first arg) :test #'string=)
                 ("=" 'setf)
                 ("==" '=)
                 ("^" 'expt)
                 (t (find-symbol (first arg) :cl))))))

