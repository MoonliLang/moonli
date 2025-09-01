(in-package :moonli)

(defvar *moonli-macro-functions* (make-hash-table))

(defun expand-moonli-macro (expression)
  (funcall (car (gethash (first expression) *moonli-macro-functions*))
           (rest expression)))

(defmacro define-moonli-macro (name &body (moonli-macro-bindings . body))
  (alexandria:with-gensyms (expr subexpr args fn idx)
    `(progn
       (destructuring-bind (&optional ,fn &rest ,idx)
           (gethash ',name *moonli-macro-functions*)
         (let* ((,expr (esrap:rule-expression
                        (esrap:find-rule 'macro-call)))
                (,subexpr (fifth (third ,expr)))
                (,idx (or ,idx (1- (length ,subexpr)))))
           (if ,fn
               (setf (nth ,idx (cdr ,subexpr))
                     `(and ,@',(mapcar #'second moonli-macro-bindings)
                           *whitespace "end"
                           +whitespace/internal good-symbol))
               (setf (cdr ,subexpr)
                     (nconc (cdr ,subexpr)
                            (list `(and ,@',(mapcar #'second moonli-macro-bindings)
                                        *whitespace "end"
                                        +whitespace/internal good-symbol)))))
           (esrap:change-rule 'macro-call ,expr)
           (setf (gethash ',name *moonli-macro-functions*)
                 (flet ((,name (,args)
                          (optima:ematch ,args
                            ((list ,@(mapcar #'first moonli-macro-bindings)
                                   _ "end" _ ',name)
                             ,@body))))
                   (cons (function ,name)
                         ,idx))))))))

(esrap:add-rule 'macro-call
                (make-instance
                 'esrap:rule
                 :expression
                 (copy-tree `(and "begin"
                                  (and +whitespace/internal
                                       good-symbol
                                       +whitespace/internal
                                       (or) ;; This will be filled by the macro
                                       )))
                 :transform
                 (lambda (production start end)
                   (declare (ignorable start end))
                   ((lambda (expr)
                      ;; (print expr)
                      (expand-moonli-macro
                       `(,(second (second expr))
                         ,@(fourth (second expr)))))
                    production))))
;; (setf *moonli-macro-functions* (make-hash-table))

(defmacro define-moonli-short-macro
    (name &body (moonli-macro-bindings . body))
  (alexandria:with-gensyms (expr subexpr args fn idx)
    `(progn
       (destructuring-bind (&optional ,fn &rest ,idx)
           (gethash ',name *moonli-macro-functions*)
         (let* ((,expr (esrap:rule-expression
                        (esrap:find-rule 'short-macro-call)))
                (,subexpr (fourth ,expr))
                (,idx (or ,idx (1- (length ,subexpr)))))
           (if ,fn
               (setf (nth ,idx (cdr ,subexpr))
                     `(and ,@',(mapcar #'second moonli-macro-bindings)))
               (setf (cdr ,subexpr)
                     (nconc (cdr ,subexpr)
                            (list `(and ,@',(mapcar #'second moonli-macro-bindings))))))
           (esrap:change-rule 'short-macro-call ,expr)
           (setf (gethash ',name *moonli-macro-functions*)
                 (flet ((,name (,args)
                          (optima:ematch ,args
                            ((list ,@(mapcar #'first moonli-macro-bindings))
                             ,@body))))
                   (cons (function ,name)
                         ,idx))))))))

(esrap:add-rule 'short-macro-call
                (make-instance
                 'esrap:rule
                 :expression (copy-tree
                              `(and good-symbol whitespace/internal (or)))
                 :transform
                 (lambda (production start end)
                   (declare (ignorable start end))
                   ((lambda (expr)
                      (expand-moonli-macro
                       `(,(first expr) ,@(third expr))))
                    production))))

