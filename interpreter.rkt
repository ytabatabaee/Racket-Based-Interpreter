#lang racket

(require "environment.rkt")
(provide interpret-cmd)

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


; variable = exp | variable = function | variable = call
; parser output: (assign variable (exp))
; returns updated env
(define interpret-assign
  (lambda (unitcom env)
    (let ((var (cadr unitcom)))
      (cond
        [(eq? (caaddr unitcom) 'func)
         (let ((func-def (caddr unitcom)))
           (add-thunk-to-env var func-def env))]
        
        [(eq? (caaddr unitcom) 'func_call)
         (let ((func-call (caddr unitcom)))
           (add-thunk-to-env var func-call env))]
        
        [else (let ((exp (caddr unitcom)))
                (add-thunk-to-env var exp env))]
        ))))


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
                           (error 'compare "Operation not allowed!")
                           ))]

                  [(and (number? val1) (number? val2))
                   (op val1 val2)]
                        
                  [(and (list? val1) (or (number? val2) (string? val2)))
                   (if (null? val1)
                       #t
                       (if (or (and (number? val2) (number? (car val1))) (and (string? val2) (string? (car val1))))
                           (and ((compare op) (car val1) val2) ((compare op) (cdr val1) val2))
                           (error 'compare "Comparison not allowed due to type inconsistency!"))
                       )]

                  [else (error 'compare "Comparison not allowed!")]
                        
		))))



; handle == according to doc (as much as i've seen, != is just the opposite of ==)
; returns #t or #f
(define is-equal
  (lambda (val1 val2)
    (cond
      [(and (list? val1) (list? val2))
       (if (xor (null? val1) (null? val2))
           #f ;(error "ERROR: For comparison, lists must be of same size.")
           (if (null? val1)
               #t
               (if (is-equal (car val1) (car val2))
                   (is-equal (cdr val1) (cdr val2))
                   #f)
               ))]
    
      [(and (list? val1) (or (string? val2) (number? val2) (boolean? val2) (null-type? val2)))
       (if (null? val1)
           #t
           (cond
             [(and (string? val2) (not (string? (car val1)))) #f]
             [(and (number? val2) (not (number? (car val1)))) #f]
             [(and (boolean? val2) (not (boolean? (car val1)))) #f]
             [(and (null-type? val2) (not (null-type? (car val1)))) #f]
             [else (and (is-equal (car val1) val2) (is-equal (cdr val1) val2))])
           )]

      [(or (number? val1) (string? val1) (null-type? val1) (boolean? val1)) (equal? val1 val2)]
      
      [else #f])
      ))
  


; returns #t or #f (I don't think so)
; op is +, -, *, /
; we may need to break this function into pieces, idk
(define binary-operation
  (lambda (op)
    (lambda (val1 val2)
      (cond
        [(and (eqv? op *) (number? val1) (thunk? val2))
         (if (eqv? val1 0)
             0
             ((binary-operation op) val1 (value-of-thunk 'x val2)))]

        [(and (eqv? op *) (boolean? val1) (thunk? val2))
         (if (eqv? val1 #f)
             #f
             ((binary-operation op) val1 (value-of-thunk 'x val2)))]

        [(and (number? val1) (number? val2))
         (op val1 val2)]

        [(and (boolean? val1) (boolean? val2))
         (if (eqv? op +) (or val1 val2)
             (if (eqv? op *) (and val1 val2) (error 'binary-operation "Operation * not allowed!")))]

        [(and (string? val1) (string? val2))
         (if (eqv? op +) (string-append val1 val2) (error 'binary-operation "Operation + not allowed!"))]

        [(and (list? val1) (list? val2) (eq? op +))
         (append val1 val2)]

        [(and (list? val1) (string? val2) (eq? op +))
         (if (null? val1)
             '()
             (if (string? (car val1))
                 (cons (string-append (car val1) val2)
                       ((binary-operation op) (cdr val1) val2))
                 (error 'binary-operation "All list members must be of type string."))
             )]

        [(and (string? val1) (list? val2) (eq? op +))
         (if (null? val2)
             '()
             (if (string? (car val2))
                 (cons (string-append val1 (car val2))
                       ((binary-operation op) val1 (cdr val2)))
                 (error 'binary-operation "All list members must be of type string."))
             )]

        [(and (list? val1) (boolean? val2))
         (if (null? val1)
             '()
             (if (boolean? (car val1))
                 (cons ((binary-operation op) (car val1) val2)
                       ((binary-operation op) (cdr val1) val2))
                 (error 'binary-operation "All list members must be of type boolean."))
             )]

        [(and (list? val2) (boolean? val1))
         (if (null? val2)
             '()
             (if (boolean? (car val2))
                 (cons ((binary-operation op) (car val2) val1)
                       ((binary-operation op) (cdr val2) val1))
                 (error 'binary-operation "All list members must be of type boolean."))
             )]

        [(and (list? val1) (number? val2))
         (if (null? val1)
             '()
             (if (number? (car val1))
                 (cons (op (car val1) val2)
                       ((binary-operation op) (cdr val1) val2))
                 (error 'binary-operation "All list members must be of type number."))
             )]

        [(and (number? val1) (list? val2))
         (if (null? val2)
             '()
             (if (number? (car val2))
                 (cons (op val1 (car val2))
                       ((binary-operation op) val1 (cdr val2)))
                 (error 'binary-operation "All list members must be of type number."))
             )]

        [else (error 'binary-operation "Operation not allowed!")] 
        ))))


(define negation
   (lambda (cexp)
     (cond
       [(number? cexp) (- cexp)]
       
       [(boolean? cexp) (not cexp)]
       
       [(list? cexp)
        (if (null? cexp)
            '()
            (if (or (number? (car cexp)) (boolean? (car cexp)))
                (cons (negation (car cexp))
                      (negation (cdr cexp)))
                (error 'negation "All list members must be of type number or boolean."))
            )]
       [else (error 'negation "Operation - not allowed!")]
       )
    ))

; note: this function takes an array and alist of indexes and returns corresponding element.
(define array-value
  (lambda (var idx_list env)
    (cond
      [(not (list? var)) (error 'array-value "Variable must be of type list.")]
      [(> 0 (value-of-exp (car idx_list) env)) (error 'array-value "List index cannot be negative.")]
      [(<= (length var) (value-of-exp (car idx_list) env)) (error 'array-value "Index is greater than array length.")]
      [else (let ([var (list-ref var (value-of-exp (car idx_list) env))])
        (if (eqv? (length idx_list) 1)
            var
            (array-value var (cdr idx_list) env)))]
      )))
       
       

; note: this function should return the VALUES for each expression, e.g. a real racket list or number or string
; parser output: (- cexp), (lparen exp rparen), (posnum), (null), (var), (true), (false), (string), (lbracket listValues rbracket), (listValues), (listmem)
(define value-of-cexp
  (lambda (exp env)
    (cond
      ((eq? (car exp) 'var) (value-of-thunk (cadr exp) (apply-env (cadr exp) env)))
      ((eq? (car exp) 'array_var) (array-value (value-of-thunk (cadr exp) (apply-env (cadr exp) env)) (caddr exp) env))
      ((eq? (car exp) 'neg) (negation (value-of-cexp (cadr exp) env)))
      ((eq? (car exp) 'par) (value-of-exp (cadr exp) env))
      ((eq? (car exp) 'num) (cadr exp))
      ((eq? (car exp) 'null) 'null)
      ((eq? (car exp) 'true) #t)
      ((eq? (car exp) 'false) #f)
      ((eq? (car exp) 'string) (cadr exp))  
      ((eq? (car exp) 'list) (if (eq? (caadr exp) 'empty)
                                 '()
                                 ;(display (value-of-exp (list 'list (cdadr exp))))
                                 (cons (value-of-exp (caadr exp) env)
                                       (value-of-exp (list 'list (cdadr exp)) env))
                                 ))
      ; check listmem
      )))


; parser output: (op cexp bexp) or (cexp)
(define value-of-bexp
  (lambda (exp env)
    (cond
      ((eq? (car exp) '*) ((binary-operation *) (value-of-cexp (cadr exp) env) (make-thunk (caddr exp) env)))
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

(define value-of-thunk
  (lambda (var thunk)
    (if (thunk? thunk)
        (cond
          ;if thunk is function definition
          [(eqv? (caadr thunk) 'func)
           (let ((func-def (cadr thunk))
                 (saved-env (caddr thunk)))
             (let ((function (define_function var func-def saved-env)))
               function))]

          ;if thunk is function call
          [(eqv? (caadr thunk) 'func_call)
           (let ((args (cadr thunk))
                 (saved-env (caddr thunk)))
             (let ((function-val (call_function args saved-env)))
               function-val))]

          ;if thunk is exp
          [else
           (let ((exp (cadr thunk))
                 (saved-env (caddr thunk)))
             (let ((val (value-of-exp exp saved-env)))
               val))]
          )
        
        ;if there is no thunk, just return the value
        thunk)))

;thunk: ('thunk exp env)
(define make-thunk
  (lambda (exp env)
    (list 'thunk exp env)))

(define add-thunk-to-env
  (lambda (var exp env)
    (update-env var (list 'thunk exp env) env)))

  
;thunk is a list starting with 'thunk e.g. ('thunk ...)
(define thunk?
  (lambda (lst)
    (and
     (list? lst)
     (not (null? lst))
     (eqv? (car lst) 'thunk))))
    

(define null-type?
  (lambda (sym)
    (eqv? sym 'null)))

;returns function call's value
(define call_function
  (lambda (arguments env)
    (let* ([f_name (cadr arguments)]
           [f_args (make-thunk (cons 'list (cddr arguments)) env)])
          ((value-of-thunk f_name (apply-env f_name env)) f_args))
    ))


;returns a function, given the definition
(define define_function
  (lambda (f_name definition env)
    (let* ([f_args (cadr definition)]
           [f_body (caddr definition)])
      (lambda (arguments)
        (if (eq? (length (cdadr arguments)) (- (length f_args) 1))
            (begin
              (set! env (update-env f_name (define_function f_name definition env) env))
              (for/list ([i (build-list (length (cdadr arguments)) values)])
                (set! env (update-env (list-ref f_args i)
                                      (list-ref (cdadr arguments) i) env)))
              (interpret-cmd f_body env))
            (error "ERROR: Invalid number of arguments.")
            )
      ))))
;--------------------------------> TODO! REMOVE ALL STUFF BELOW!



;(interpret-cmd '((return (false)) (return (true))) '())
;(interpret-cmd '((while (mult (num 10) (list ((num 34) NULL empty))) ((return (true))))) '())
;(interpret-cmd '((if (num 10) ((return (true))) ((return (false))))) '())
;(interpret-cmd '((return (list ((num 19) null false (list ((num 10) empty)) empty)))) '())
;(interpret-cmd '((if (> (num 10) (num 4)) ((return (list ((* (num 2) (num 3)) (- (num 7) (num 9)) empty)))) ((return (/ (num 8) (num 10)))))) '())

(define (i a) (interpret-cmd a '()))