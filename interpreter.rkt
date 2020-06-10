#lang racket

(require "environment.rkt")
  
; command → unitcom | command; unitcom
; parser output: ((uc) (uc) ... (uc))
; returns updated env
(define interpret-cmd
  (lambda (cmd env)
    (if (null? cmd)
        env
        (interpret-cmd (cdr cmd) (interpret-unitcom (car cmd) env)))))


; unitcom → whilecom | ifcom | assign | return
; whilecom | ifcom | assign | return
; returns updated env
(define interpret-unitcom
  (lambda (unitcom env)
    (cond
      ((eq? (car unitcom) 'if) (interpret-ifcom unitcom env))
      ((eq? (car unitcom) 'while) (interpret-whilecom unitcom env))
      ((eq? (car unitcom) 'assign) (interpret-assign unitcom env))
      ((eq? (car unitcom) 'return) (interpret-return unitcom env)))))


; if exp then command else command endif
; parser output: (if (exp) (command) (command))
; returns updated env
(define interpret-ifcom
  (lambda (unitcom env)
    (if (value-of-exp (cadr unitcom) env)
        (interpret-cmd (caddr unitcom) env)
        (interpret-cmd (cadddr unitcom) env))))


; while exp do command end
; parser output: (while (exp) (command))
; returns updated env
(define interpret-whilecom
  (lambda (unitcom env)
    (if (value-of-exp (cadr unitcom) env)
        (interpret-whilecom unitcom (interpret-cmd (caddr unitcom) env))
        env)))


; variable = exp
; parser output: (assign variable (exp))
; returns updated env
(define interpret-assign
  (lambda (unitcom env)
    (let
        ((var (cadr unitcom))
         (val (value-of-exp (caddr unitcom) env)))
      (update-env var val env))))
      
 
; return exp
; parser output: (return (exp))
; returns value of return exp
(define interpret-return
  (lambda (exp env)
    (value-of-exp exp env)))



; todo complete this
; handle > or < according to doc
; returns #t or #f
(define compare
  (lambda (op)
    (lambda (val1 val2)
      #t)))


; todo complete this
; handle == according to doc (as much as i've seen, != is just the opposite of ==)
; returns #t or #f
(define is-equal
    (lambda (val1 val2)
      #t))


; todo complete this
; returns #t or #f
; op is +, -, *, /
; we may need to break this function into pieces, idk
(define binary-operation
  (lambda (op)
    (lambda (val1 val2)
    #t)))


; todo complete this
(define negation
   (lambda (cexp)
    #t))

; todo complete this
; note: this function should return the VALUES for each expression, e.g. a real racket list or number or string
; parser output: (- cexp), (lparen exp rparen), (posnum), (null), (var), (true), (false), (string), (lbracket listValues rbracket), (listValues), (listmem)
(define value-of-cexp
  (lambda (exp env)
    (cond
      ((eq? (car exp) '-) (negation (value-of-cexp (cadr exp) env)))
      ((eq? (car exp) 'lparen) (if (eq? (caddr exp) 'rparen)
                                   (value-of-exp (cadr exp) env)
                                   (error "Error: Parenteses not closed!")))
      ((number? (car exp)) (car exp))
      ((eq? (car exp) 'null) '())
      ((eq? (car exp) 'true) #t)
      ((eq? (car exp) 'false) #f)
      ((string? (car exp)) (car exp))
      ; not sure how to handle this, should we have a function for value of listvals?
      ((eq? (car exp) 'lbacket) (if (eq? (caddr exp) 'rbracket)
                                   (value-of-cexp (cadr exp) env)
                                   (error "Error: Bracket not closed!")))
      ; check listValues
      ; check listmem 
      )))

  
; parser output: (op cexp bexp) or (cexp)
(define value-of-bexp
  (lambda (exp env)
    (cond
      ((eq? (car exp) '*) ((binary-operation *) (value-of-cexp (cadr exp) env) (value-of-bexp (caddr exp) env)))
      ((eq? (car exp) '/) ((binary-operation /) (value-of-cexp (cadr exp) env) (value-of-bexp (caddr exp) env)))
      (else (value-of-cexp (car exp) env)))))


; parser output: (op bexp aexp) or (bexp)
(define value-of-aexp
  (lambda (exp env)
    (cond
      ((eq? (car exp) '+) ((binary-operation +) (value-of-bexp (cadr exp) env) (value-of-aexp (caddr exp) env)))
      ((eq? (car exp) '-) ((binary-operation -) (value-of-bexp (cadr exp) env) (value-of-aexp (caddr exp) env)))
      (else (value-of-bexp (car exp) env)))))


; parser output: (op aexp aexp) or (aexp)
(define value-of-exp
  (lambda (exp env)
    (cond
      ((eq? (car exp) '>) ((compare >) (value-of-aexp (cadr exp) env) (value-of-aexp (caddr exp) env)))
      ((eq? (car exp) '<) ((compare <) (value-of-aexp (cadr exp) env) (value-of-aexp (caddr exp) env)))
      ((eq? (car exp) '==) (is-equal (value-of-aexp (cadr exp) env) (value-of-aexp (caddr exp) env)))
      ((eq? (car exp) '!=) (not (is-equal (value-of-aexp (cadr exp) env) (value-of-aexp (caddr exp) env))))
      (else (value-of-aexp (car exp) env)))))


; todo complete this
(define id?
  (lambda (sym)
    #t))








    
