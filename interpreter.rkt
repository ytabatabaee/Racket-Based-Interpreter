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
    (value-of-exp (cadr exp) env)))


; handle > or < according to doc
; returns #t or #f
(define compare
	(lambda (op)
		(lambda (val1 val2)
		(cond
                  [(and (string? val1) (string? val2))
                   (if (eqv? op <)
                       (string<? val1 val2)
                       (if (eqv? op >)
                           (string>? val1 val2)
                           (error "Error: Operation not allowed!")
                           ))]

                  [(and (number? val1) (number? val2))
                   (op val1 val2)]
                        
                  [(list? val1)
                   (if (compare op (car val1) val2)
                       (compare op (cdr val1) val2)
                       #f)]
                        
                  [(list? val2)
                   (if (compare op val1 (car val2))
                       (compare op val1 (cdr val2))
                       #f)]
		))))



; handle == according to doc (as much as i've seen, != is just the opposite of ==)
; returns #t or #f
(define is-equal
  (lambda (val1 val2)
    (cond
      [(and (list? val1) (list? val2))
       (if (is-equal (value-of-exp (car val1)) (value-of-exp (car val2)))
           (is-equal (cdr val1) (cdr val2))
           (#f))]
    
      [(list? val1)
       (if (eqv? (value-of-exp(car val1)) val2)
           (compare (cdr val1) val2)
           #f)]
    
      [(list? val2)
       (if (eqv? val1 (value-of-exp (car val2)))
           (compare val1 (cdr val2))
           #f)]

      [else (equal? val1 val2)]
      ))
  )
  


; returns #t or #f (I don't think so)
; op is +, -, *, /
; we may need to break this function into pieces, idk
(define binary-operation
  (lambda (op)
    (lambda (val1 val2)
      (cond
        [(and (number? val1) (number? val2))
         (op val1 val2)]

        [(and (boolean? val1) (boolean? val2))
         (if (eqv? op +) (or val1 val2)
             (if (eqv? op *) (and val1 val2) (error "Error: Operation not allowed!")))]

        [(and (string? val1) (string? val2))
         (if (eqv? op +) (string-append val1 val2) (error "Error: Operation not allowed!"))]

        [(and (list? val1) (list? val2) (op +))
         (append val1 val2)]

        [(and (list? val1) (string? val2) (op +))
         (if (null? val1)
             '()
             (cons (string-append val2 (car val1))
                   (binary-operation op (cdr val1) val2))
             )]

        [(and (string? val1) (list? val2) (op +))
         (if (null? val2)
             '()
             (cons (string-append val1 (car val2))
                   (binary-operation op val1 (cdr val2)))
             )]

        [(and (list? val1) (boolean? val2))
         (if (null? val1)
             '()
             (cons (binary-operation op (car val1) val2)
                   (binary-operation op (cdr val1) val2))
             )]

        [(and (list? val2) (boolean? val1))
         (if (null? val2)
             '()
             (cons (binary-operation op (car val2) val1)
                   (binary-operation op (cdr val2) val1))
             )]

        [(and (list? val1) (number? val2))
         (if (null? val1)
             '()
             (if (number? (car val1))
                 (cons (op (car val1) val2)
                       (binary-operation op (cdr val1) val2))
                 (error "ERROR: All list members must be of type number."))
             )]

        [(and (number? val1) (list? val2))
         (if (null? val2)
             '()
             (if (number? (car val2))
                 (cons (op val1 (car val2)
                       (binary-operation op val1 (cdr val2))))
                 (error "ERROR: All list members must be of type number."))
             )]
        ))))


(define negation
   (lambda (cexp)
     (cond
       [(number? cexp) (- cexp)]
       
       [(boolean? cexp) (not cexp)]
       
       [(list? cexp)
        (if (null? cexp)
            '()
            (if (number? (car cexp))
                (cons (- (car cexp))
                      (negation (cdr cexp)))
                (error "ERROR: All list members must be of type number."))
            )]
       )
    ))

; todo complete this
; note: this function should return the VALUES for each expression, e.g. a real racket list or number or string
; parser output: (- cexp), (lparen exp rparen), (posnum), (null), (var), (true), (false), (string), (lbracket listValues rbracket), (listValues), (listmem)
(define value-of-cexp
  (lambda (exp env)
    (cond
      ((eq? (car exp) 'neg) (negation (value-of-cexp (cadr exp) env)))
      ((eq? (car exp) 'par) (if (eq? (caddr exp) 'rparen)
                                   (value-of-exp (cadr exp) env)
                                   (error "Error: Parenteses not closed!")))
      ((eq? (car exp) 'num) (cadr exp))
      ((eq? (car exp) 'null) '())
      ((eq? (car exp) 'true) #t)
      ((eq? (car exp) 'false) #f)
      ((eq? (car exp) 'string) (car exp))
      ; not sure how to handle this, should we have a function for value of listvals?
      ((eq? (car exp) 'lbacket) (if (eq? (caddr exp) 'rbracket)
                                   (value-of-cexp (cadr exp) env)
                                   (error "Error: Bracket not closed!")))
  
      ((eq? (car exp) 'list) (if (eq? (caadr exp) 'empty)
                                 '()
                                 (display (value-of-exp (list 'list (cdadr exp))))
                                 ;(cons (value-of-exp (caadr exp))
                                  ;     (value-of-exp (list 'list (cdadr exp))))
                                 ))
      ; check listmem
      )))


; parser output: (op cexp bexp) or (cexp)
(define value-of-bexp
  (lambda (exp env)
    (cond
      ((eq? (car exp) '*) ((binary-operation *) (value-of-cexp (cadr exp) env) (value-of-bexp (caddr exp) env)))
      ((eq? (car exp) '/) ((binary-operation /) (value-of-cexp (cadr exp) env) (value-of-bexp (caddr exp) env)))
      (else (value-of-cexp exp env)))))


; parser output: (op bexp aexp) or (bexp)
(define value-of-aexp
  (lambda (exp env)
    (cond
      ((eq? (car exp) '+) ((binary-operation +) (value-of-bexp (cadr exp) env) (value-of-aexp (caddr exp) env)))
      ((eq? (car exp) '-) ((binary-operation -) (value-of-bexp (cadr exp) env) (value-of-aexp (caddr exp) env)))
      (else (value-of-bexp exp env)))))


; parser output: (op aexp aexp) or (aexp)
(define value-of-exp
  (lambda (exp env)
    (cond
      ((eq? (car exp) '>) ((compare >) (value-of-aexp (cadr exp) env) (value-of-aexp (caddr exp) env)))
      ((eq? (car exp) '<) ((compare <) (value-of-aexp (cadr exp) env) (value-of-aexp (caddr exp) env)))
      ((eq? (car exp) '==) (is-equal (value-of-aexp (cadr exp) env) (value-of-aexp (caddr exp) env)))
      ((eq? (car exp) '!=) (not (is-equal (value-of-aexp (cadr exp) env) (value-of-aexp (caddr exp) env))))
      (else (value-of-aexp exp env)))))


; todo complete this
(define id?
  (lambda (sym)
    #t))


;(interpret-cmd '((return (false)) (return (true))) '())
;(interpret-cmd '((while (mult (num 10) (list ((num 34) NULL empty))) ((return (true))))) '())
;(interpret-cmd '((if (num 10) ((return (true))) ((return (false))))) '())