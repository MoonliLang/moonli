(defsystem "moonli"
  :depends-on ("alexandria"
               "esrap"
               "fiveam"
               "optima"
               "parse-number"
               "unix-opts")
  :licence "MIT"
  :author "Shubhamkar Ayare (digikar@proton.me)"
  :version "0.0.1"
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
                             (:file "infix")
                             (:file "expressions")))
               (:module "macros"
                :components ((:file "moonli-macro")
                             (:file "moonli-short-macro")))
               (:file "moonli")
               (:file "binary"))
  :perform (test-op (c s)
             (eval (read-from-string "(5AM:RUN! :MOONLI)")))
  :perform (program-op (o c)
             (dolist (thread (uiop:symbol-call :bt :all-threads))
               #+sbcl (progn
                        (require :sb-sprof)
                        (require :sb-cltl2)
                        (require :sb-introspect)
                        (require :sb-bsd-sockets)
                        (require :sb-posix))
               (unless (eq thread (uiop:symbol-call :bt :current-thread))
                 (uiop:symbol-call :bt :destroy-thread thread))
               #+sbcl (sb-ext:disable-debugger)
               ;; Leave the compression to tar.gz and the likes, since
               ;; storage and file size should not be a problem in 2023.
               ;; Only network can be a problem. Or perhaps, do it anyways.
               (uiop:dump-image "moonli" :executable t :compression 22)))
  :build-operation "program-op"
  :entry-point "moonli:main")
