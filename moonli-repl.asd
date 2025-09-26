(defsystem "moonli-repl"
  :depends-on ("moonli"
               "cl-repl"
               "let-plus"
               "for"
               "com.inuoe.jzon"
               "parse-float")
  :build-operation "program-op"
  :build-pathname "moonli.repl"
  :entry-point "cl-repl:main"
  :depends-on ()
  :pathname #p"src/"
  :components ((:file "repl/package")
               (:module "extra-macros"
                :components ((:file "short")
                             (:file "let-plus")
                             (:file "for")))
               (:file "repl/main")))
