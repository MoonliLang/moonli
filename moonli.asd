(defsystem "moonli"
  :depends-on ("alexandria"
               "esrap"
               "parse-number"
               "optima"
               "fiveam")
  :pathname #p"src/"
  :serial t
  :components ((:file "package")
               (:module "parser"
                :components ((:file "basic")
                             (:file "number")
                             (:file "symbol")
                             (:file "hash-table-or-set")
                             (:file "macros")
                             (:file "misc")
                             (:file "expressions")))
               (:module "macros"
                :components ((:file "moonli-macro")
                             (:file "moonli-short-macro")))
               (:file "moonli"))
  :perform (test-op (c s)
             (eval (read-from-string "(5AM:RUN! :MOONLI)"))))
