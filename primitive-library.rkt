#lang racket

(provide pow)
(provide make-list)
(provide reverse)
(provide set)
(provide merge)
(provide merge-sort)
;(provide eval)

(define pow
  (lambda (lst)
    (let ((a (car lst)))
      (let ((b (cadr lst)))
        (pow-func a b)))))

; assuming base is a number, exponent is an integer
; can also use 'expt' which is racket primitive function
(define pow-func
  (lambda (a b)
    (if (number? a)
        (if (integer? b)
            (if (zero? b)
                1
                (if (> b 0)
                    (* a (pow-func a (- b 1)))
                    (* (/ 1  a) (pow-func a (+ b 1)))))
            (error 'pow "Exponent should be of type integer!"))
        (error 'pow "Base should be a number!"))))


(define make-list
  (lambda (lst)
    (let ((a (car lst)))
      (let ((b (cadr lst)))
        (make-list-func a b)))))


; make a list of a number of bs
(define make-list-func
  (lambda (a b)
    (if (integer? a)
        (if (<= a 0)
            '()
            (cons b (make-list-func (- a 1) b)))
        (error make-list "Count should be an integer!"))))


(define reverse
  (lambda (lst)
    (let ((a (car lst)))
       (reverse-func a))))

; one-layer reverse
(define reverse-func
  (lambda (a)
    (cond
    [(null? a) '()]
    [(list? a) (append (reverse-func (cdr a)) (list (car a)))]
    [else a])))

(define reverse-all
  (lambda (lst)
    (let ((a (car lst)))
       (reverse-all-func a))))

; recursive reverse
(define reverse-all-func
  (lambda (a)
    (cond
    [(null? a) '()]
    [(list? a)
      (append (reverse-all-func (cdr a)) (list (reverse-all-func (car a))))]
    [else a])))


(define set
  (lambda (lst)
    (let ((a (car lst)))
      (let ((index (cadr lst)))
        (let ((value (caddr lst)))
          (set-func a index value))))))


; set a[index] = value
(define set-func
  (lambda (a index value)
    (if (list? a)
        (if (<= (length a) index)
            (error 'set "Index is out of bound!")
            (if (zero? index)
                (cons value (cdr a))
                (cons (car a) (set-func (cdr a) (- index 1) value))))
        (error 'set "Inpus must be a list!"))))


(define merge
  (lambda (lst)
    (let ((a (car lst)))
      (let ((b (cadr lst)))
        (merge-func a b)))))


; merge two sorted lists        
(define merge-func
  (lambda (a b)
    (if (null? a)
        b
        (if (null? b)
            a
            (if (< (car a) (car b))
                (cons (car a) (merge-func (cdr a) b))
                (cons (car b) (merge-func a (cdr b))))))))


(define merge-sort
  (lambda (lst)
    (let ((a (car lst)))
       (merge-sort-func a))))


(define merge-sort-func
  (lambda (a)
    (cond
    [(null? a) a]
    [(null? (cdr a)) a]
    [(null? (cddr a)) (merge-func (list (car a)) (cdr a))]
    [else
     (let ([mid (ceiling (/ (length a) 2))])
       (merge-func (merge-sort-func (take a mid)) (merge-sort-func (drop a mid))))])))


;(define eval
;  (lambda (s)
;    (let* ([prog (s)]
;          [lex-this (lambda (lexer input) (lambda () (lexer input)))]
;          [lex (lex-this my-lexer (open-input-string prog))])
;      (let ((parser-res (gram-parser lex))) (interpret-cmd parser-res '()))
;      )
;    ))

;;;;;;;;;;;;;;;;;;;
;tests

;(pow '(1.3 1))
;(pow '(1.3 0))
;(pow '(1.3 3))
;(pow '(1.3 -3))

;(make-list '(10 'a))
;(make-list '(-1 'a))
;(make-list '('b 'a))
;(make-list '(10 '(1 1)))

;(reverse-all '((1 (2 3) (4 (5 6)))))
;(reverse '((1 (2 3) (4 (5 6)))))

;(set '((1 2 3 4) 2 5))
;(set '((1 2 3 4) 0 'b))
;(set '(1) 2 5)
;(set '(3 2 5))

;(merge '((1 4) (1 2 8)))
;(merge '((35 62 81 90 91) (3 83 85 90)))

;(merge-sort '((2 1 -10 3 8)))
