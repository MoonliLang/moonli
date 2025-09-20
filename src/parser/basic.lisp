(in-package :moonli)

(esrap:defrule whitespace/internal
    (or #\Space #\Tab)
  (:constant nil)
  (:error-report nil))

(esrap:defrule *whitespace/internal
    (* whitespace/internal)
  (:constant nil)
  (:error-report nil))

(esrap:defrule +whitespace/internal
    (+ whitespace/internal)
  (:constant nil)
  (:error-report nil))

(esrap:defrule whitespace/end
    (or #\newline #\return #\;)
  (:constant nil)
  (:error-report nil))

(esrap:defrule whitespace
    (or #\space #\tab #\newline #\return)
  (:constant nil)
  (:error-report nil))

(esrap:defrule +whitespace
    (+ whitespace)
  (:constant nil)
  (:error-report nil))

(esrap:defrule *whitespace
    (* whitespace)
  (:constant nil)
  (:error-report nil))

(esrap:defrule *whitespace/end
    (* whitespace/end)
  (:constant nil)
  (:error-report nil))

(esrap:defrule *whitespace/all
    (* (or whitespace/end whitespace/internal))
  (:constant nil)
  (:error-report nil))

(esrap:defrule numeric-character (esrap:character-ranges (#\0 #\9))
  (:error-report :context))

(esrap:defrule non-symbol-chars
    (or #\| #\: #\, #\$ #\' #\" #\( #\[ #\{ #\) #\] #\}
        whitespace/internal whitespace/end)
  (:error-report nil))
