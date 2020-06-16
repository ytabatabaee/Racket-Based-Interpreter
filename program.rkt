#lang racket

(require "lexer.rkt")
(require "parser.rkt")
(require "interpreter.rkt")

; use this file as a glue for all parts


; todo complete this
(define evaluate
  (lambda (filename)
    #t))


(define test10 "return [\"b\", 1] + \"a\"")
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define lex (lex-this my-lexer (open-input-string test10)))
(let ((parser-res (gram-parser lex))) parser-res)
  