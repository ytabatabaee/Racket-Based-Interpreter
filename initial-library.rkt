#lang racket

; assuming base is a number, exponent is an integer 
(define pow
  (lambda (a b)
    (if (number? a)
        (if (integer? b)
            (if (zero? b)
                1
                (if (> b 0)
                    (* a (pow a (- b 1)))
                    (* (/ 1  a) (pow a (+ b 1)))))
            (error 'pow "Exponent should be of type integer!"))
        (error 'pow "Base should be a number!"))))



; make a list of a number of bs
(define make-list
  (lambda (a b)
    (if (integer? a)
        (if (<= a 0)
            '()
            (cons b (make-list (- a 1) b)))
        (error 'make-list "Count should be an integer!"))))


(define reverse
  (lambda (a)
    0))

(define reverse-all
  (lambda (a)
    0))

(define set
  (lambda (a index value)
    0))

(define merge
  (lambda (a b)
    0))

(define merge-sort
  (lambda (a)
    0))

(define eval
  (lambda (s)
    0))

;;;;;;;;;;;;;;;;;;;
;tests

;(pow 1.3 1)
;(pow 1.3 0)
;(pow 1.3 3)
;(pow 1.3 -3)

;(make-list 10 'a)
;(make-list -1 'a)
;(make-list 'b 'a)
;(make-list 10 '(1 1))
