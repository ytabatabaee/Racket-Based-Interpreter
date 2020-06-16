#lang racket

(require "lexer.rkt")
(require "parser.rkt")
(require "interpreter.rkt")
(require 2htdp/batch-io) ;Is it ok to use this library?

; use this file as a glue for all parts


; todo complete this
(define evaluate
  (lambda (filename)
    (let* ([prog (read-file filename)]
          [lex-this (lambda (lexer input) (lambda () (lexer input)))]
          [lex (lex-this my-lexer (open-input-string prog))])
      (let ((parser-res (gram-parser lex))) (interpret-cmd parser-res '()))
      )
    ))
