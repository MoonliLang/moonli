(defsystem "moonli"
  :depends-on ("alexandria"
               "esrap"
               "parse-number"
               "optima"
               "fiveam")
  :components ((:file "package")
               (:module "parser"
                :components ((:file "basic")
                             (:file "number")
                             (:file "symbol")
                             (:file "macros")
                             (:file "misc")
                             (:file "expressions")))
               (:module "macros"
                :components ((:file "moonli-macro")
                             (:file "moonli-short-macro")))
               (:file "moonli")))
