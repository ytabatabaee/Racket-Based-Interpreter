#lang racket

; assuming base is a number, exponent is an integer
; can also use 'expt' which is racket primitive function
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


; one-layer reverse
(define reverse
  (lambda (a)
    (cond
    [(null? a) '()]
    [(list? a) (append (reverse (cdr a)) (list (car a)))]
    [else a])))

; recursive reverse
(define reverse-all
  (lambda (a)
    (cond
    [(null? a) '()]
    [(list? a)
      (append (reverse-all (cdr a)) (list (reverse-all (car a))))]
    [else a])))


; set a[index] = value
(define set
  (lambda (a index value)
    (if (list? a)
        (if (<= (length a) index)
            (error 'set "Index is out of bound!")
            (if (zero? index)
                (cons value (cdr a))
                (cons (car a) (set (cdr a) (- index 1) value))))
        (error 'set "Inpus must be a list!"))))


; merge two sorted lists        
(define merge
  (lambda (a b)
    (if (null? a)
        b
        (if (null? b)
            a
            (if (< (car a) (car b))
                (cons (car a) (merge (cdr a) b))
                (cons (car b) (merge a (cdr b))))))))


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

;(reverse-all '(1 (2 3) (4 (5 6))))
;(reverse '(1 (2 3) (4 (5 6))))

;(set '(1 2 3 4) 2 5)
;(set '(1 2 3 4) 0 'b)
;(set '(1) 2 5)
;(set 3 2 5)

;(merge '(1 4) '(1 2 8))
;(merge '(35 62 81 90 91) '(3 83 85 90))