(defsystem "moonli-repl"
  :depends-on ("moonli"
               "cl-repl")
  :build-operation "program-op"
  :build-pathname "moonli"
  :entry-point "cl-repl:main"
  :components ((:file "repl/main")))
