(uiop:define-package :moonli-user
  (:mix-reexport #:cl #:let-plus #:for #:parse-float)
  (:local-nicknames (#:json #:com.inuoe.jzon))
  (:export #:lm
           #:ifelse))
