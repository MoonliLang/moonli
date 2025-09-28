(defsystem "moonli"
  :depends-on ("alexandria"
               "esrap"
               "fiveam"
               "let-plus"
               "optima"
               "parse-number"
               "unix-opts")
  :licence "MIT"
  :author "Shubhamkar Ayare (digikar@proton.me)"
  :version "0.0.2"
  :pathname #p"src/"
  :serial t
  :components ((:file "package")
               (:file "testdoc")
               (:module "parser"
                :components ((:file "basic")
                             (:file "mandatory")
                             (:file "number")
                             (:file "symbol")
                             (:file "hash-table-or-set")
                             (:file "macros")
                             (:file "misc")
                             (:file "infix")
                             (:file "vector")
                             (:file "chain")
                             (:file "expressions")))
               (:module "macros"
                :components ((:file "moonli-macro")
                             (:file "moonli-short-macro")))
               (:file "moonli")
               (:file "binary"))
  :perform (test-op (c s)
             (eval (read-from-string "(5AM:RUN! :MOONLI)")))
  :perform (program-op (o c)
             (uiop:dump-image "moonli" :executable t
                                       :compression #+sb-core-compression 22 #-sb-core-compression nil))
  :build-operation "program-op"
  :entry-point "moonli:main")
