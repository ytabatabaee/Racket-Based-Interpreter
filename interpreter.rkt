#lang racket

(require "environment.rkt")

  
; command → unitcom | command; unitcom
; parser output: ((uc) (uc) ... (uc))
(define interpret-cmd
  (lambda (cmd env)
    (if (null? cmd)
        env
        (interpret-cmd (cdr cmd) (interpret-unitcom (car cmd) env)))))


; unitcom → whilecom | ifcom | assign | return
; whilecom | ifcom | assign | return
(define interpret-unitcom
  (lambda (unitcom env)
    (cond
      ((eq? (car unitcom) 'if) (interpret-ifcom unitcom env))
      ((eq? (car unitcom) 'while) (interpret-whilecom unitcom env))
      ((eq? (car unitcom) 'assign) (interpret-assign unitcom env))
      ((eq? (car unitcom) 'return) (interpret-return unitcom env)))))


; if exp then command else command endif
; parser output: (if (exp) (command) (command))
(define interpret-ifcom
  (lambda (unitcom env)
    (if (interpret-exp (cadr unitcom) env)
        (interpret-cmd (caddr unitcom) env)
        (interpret-cmd (cadddr unitcom) env))))


; while exp do command end
; parser output: (while (exp) (command))
(define interpret-whilecom
  (lambda (unitcom env)
    (if (interpret-exp (cadr unitcom) env)
        (interpret-whilecom unitcom (interpret-cmd (caddr unitcom) env))
        env)))


; variable = exp
; parser output: (assign variable (exp))
(define interpret-assign
  (lambda (unitcom env)
    (let
        ((var (cadr unitcom))
         (val (interpret-exp-value (caddr unitcom) env)))
      (update-env var val env))))
      
 


; return exp
; parser output: (return (exp))
(define interpret-return
  (lambda (unitcom env)
    env
    ; todo complete this
    ))


(define interpret-exp-value
  (lambda (exp env)
    env
    ; todo complete this
    ))


(define interpret-aexp
  (lambda (exp env)
    env
    ; todo complete this
    ))


(define interpret-exp
  (lambda (exp env)
    (cond
      ((eq? (car exp) '>) (interpret-aexp (cadr exp) env))
      ((eq? (car exp) '<) (interpret-aexp (cadr exp) env))
      ((eq? (car exp) '==) (interpret-aexp (cadr exp) env))
      ((eq? (car exp) '!=) (interpret-aexp (cadr exp) env)))))










    
