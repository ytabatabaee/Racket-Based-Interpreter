#lang racket

(require "lexer.rkt")
(require "parser.rkt")
(require "interpreter.rkt")
(require "primitive-library.rkt")
(require "environment.rkt")
(require 2htdp/batch-io) 

; use this file as a glue for all parts

(define add-library
  (lambda (env)
    (begin
      (set! env (update-env 'pow pow env))
      (set! env (update-env 'make-list make-list env))
      (set! env (update-env 'reverse reverse env))
      (set! env (update-env 'set set env))
      (set! env (update-env 'merge merge env))
      (set! env (update-env 'merge-sort merge-sort env))
      (set! env (update-env 'eval eval env))
      env
      )))
            
       
; todo complete this
(define evaluate
  (lambda (filename)
    (let* ([prog (read-file filename)]
          [lex-this (lambda (lexer input) (lambda () (lexer input)))]
          [lex (lex-this my-lexer (open-input-string prog))])
      (let ((parser-res (gram-parser lex))) (interpret-cmd parser-res (add-library '())))
      )
    ))
