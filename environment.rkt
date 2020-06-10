#lang racket

; we may need some functions like create-env extend-env (for adding new vars) or these things too
; extend-env can be implemented as a part of update-env. handling interpreter will be easier this way
; add any other local function we might need

(provide update-env)
(provide apply-env)

(define update-env
  (lambda (var val env)
    ; todo complete this
    env))


(define apply-env
  (lambda (var env)
    ; todo complete this
    env))