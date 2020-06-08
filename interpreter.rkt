#lang racket


(define interpret-cmd
  (lambda (cmd env)
    env
    ))


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


    
