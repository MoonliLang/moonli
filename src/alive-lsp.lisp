(in-package :moonli)

(pushnew (cons :moonli 'moonli-string-to-lisp-string)
         alive/packages:*parse-function-alist*
         :key #'car)

(pushnew (cons :moonli 'read-moonli-from-stream)
         alive/sys/eval:*read-function-alist*
         :key #'car)
