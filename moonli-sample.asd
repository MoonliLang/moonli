(defsystem "moonli-sample"
  :defsystem-depends-on ("moonli-asdf")
  :pathname #p"sample/"
  :components ((:moonli-file "sample")))
