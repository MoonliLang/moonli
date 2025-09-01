(defpackage :moonli
  (:use :cl)
  ;; (:import-from #:string-atom-tries
  ;;               #:make-string-atom-trie)
  ;; (:local-nicknames (#:tries #:string-atom-tries))
  (:export #:compile-moonli-file
           #:load-moonli-file
           #:define-moonli-macro
           #:define-moonli-short-macro))


(5am:def-suite :moonli)
(5am:in-suite :moonli)
