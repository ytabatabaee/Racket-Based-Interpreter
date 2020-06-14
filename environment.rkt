#lang racket

; we may need some functions like create-env extend-env (for adding new vars) or these things too
; extend-env can be implemented as a part of update-env. handling interpreter will be easier this way
; add any other local function we might need

(provide update-env)
(provide apply-env)

(define update-env
  (lambda (var val env)
    (if (null? env)
        (list (list var val))
        (if (eqv? (caar env) var)
            (cons (list var val) (cdr env))
            (cons (car env) (update-env var val (cdr env)))))))


(define apply-env
  (lambda (var env)
    (if (null? env)
        (error "Error: Variable is not defined!")
        (if (eqv? (caar env) var)
            (cadr (car env))
            (apply-env var (cdr env))))))

