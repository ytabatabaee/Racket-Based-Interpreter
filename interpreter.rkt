#lang racket

; todo: add the rest
(define-datatype cexp cexp?
  (var
   (id symbol?))
  (pos-number
   (num number?))
  (neg-exp
   (body-exp cexp?))
  (paren-exp
   (body-exp exp?)))


(define-datatype bexp bexp?
  (cexp-exp
    (exp1 cexp?))
  (mul-exp
    (exp1 cexp?)
    (exp2 bexp?))
  (div-exp
    (exp1 cexp?)
    (exp2 bexp?)))


(define-datatype aexp aexp?
  (bexp-exp
    (exp1 bexp?))
  (sum-exp
    (exp1 bexp?)
    (exp2 aexp?))
  (sub-exp
    (exp1 bexp?)
    (exp2 aexp?)))


(define-datatype exp exp?
  (aexp-exp
    (exp1 aexp?))
  (gt-exp
    (exp1 aexp?)
    (exp2 aexp?))
  (lt-exp
    (exp1 aexp?)
    (exp2 aexp?))
  (eq-exp
    (exp1 aexp?)
    (exp2 aexp?))
  (neq-exp
    (exp1 aexp?)
    (exp2 aexp?)))

(define-datatype exp exp?
  (while-exp
    (exp1 aexp?))
  (if-exp
    (exp1 aexp?)
    (exp2 aexp?))
  (assign-exp
    (exp1 aexp?)
    (exp2 aexp?))
  (ret-exp
    (exp1 aexp?)
    (exp2 aexp?))
  (neq-exp
    (exp1 aexp?)
    (exp2 aexp?)))


  

(define interpret-cmd
  (lambda (cmd env)
    env
    ))

; whilecom | ifcome | assign | return
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



; return exp
; parser output: (return (exp))



(define interpret-exp
  (lambda (exp env)
    (cond
      ((eq? (car exp) '>) (interpret-aexp (cadr exp) env))
      ((eq? (car exp) '<) (interpret-aexp (cadr exp) env))
      ((eq? (car exp) '==) (interpret-aexp unitcom env))
      ((eq? (car exp) '!=) (interpret-aexp unitcom env))










    
