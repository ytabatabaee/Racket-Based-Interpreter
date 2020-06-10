#lang racket

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