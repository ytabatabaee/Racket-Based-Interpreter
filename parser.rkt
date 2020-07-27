#lang racket

(require "lexer.rkt")
(provide gram-parser)


(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define-tokens a (VAR NUM STR))
(define-empty-tokens b (comma semicolon EOF plus do while end if then else endif equal notequal return greater less minus mult div TRUE FALSE NULL Num Lbr Lpar Rbr Rpar Lcbr Rcbr assign func))


(define gram-parser
  (parser
   (start command)
   (end EOF)
   (error (lambda (ok? name value)
            (printf "Couldn't parse: ~a\n" name))) ;May need changing
   (tokens a b)
   (grammar

    (command ((unitcom) (list $1))
             ((command semicolon unitcom) (append $1 (list $3)))) 
    
    (unitcom ((whilecom) (values $1))
             ((ifcom) (values $1))
             ((assigncom) (values $1))
             ((returncom) (values $1)))
    
    (whilecom ((while exp do command end) (list 'while $2 $4)))
    
    (ifcom ((if exp then command else command endif) (list 'if $2 $4 $6)))
    
    (assigncom ((VAR assign exp) (list 'assign $1 $3))
               ((VAR assign function) (list 'assign $1 $3))
               ((VAR assign call) (list 'assign $1 $3)
               ))
    
    (returncom ((return exp) (list 'return $2)))
    
    (exp ((aexp) (values $1))
         ((aexp greater aexp) (list '> $1 $3))
         ((aexp less aexp) (list '< $1 $3))
         ((aexp equal aexp) (list '== $1 $3))
         ((aexp notequal aexp) (list '!= $1 $3)))
    
    (aexp ((bexp) (values $1))
          ((bexp minus aexp) (list '- $1 $3))
          ((bexp plus aexp)(list '+ $1 $3)))
    
    (bexp ((cexp) (values $1))
          ((cexp mult bexp) (list '* $1 $3))
          ((cexp div bexp) (list '/ $1 $3)))
    
    (cexp ((minus cexp) (list 'neg $2))
          ((Lpar exp Rpar) (list 'par $2))
          ((NUM) (list 'num $1))
          ((NULL) (list 'null))
          ((VAR) (list 'var $1))
          ((TRUE) (list 'true))
          ((FALSE) (list 'false))
          ((STR) (list 'string $1))
          ((list) (list 'list $1))
          ((VAR listmem) (list 'array_var $1 $2)))
    
    (list ((Lbr listValues Rbr) (values $2))
          ((Lbr Rbr) (list 'empty)))
    
    (listValues ((exp) (list $1 'empty))
                ((exp comma listValues) (cons $1 $3))) 
     
    (listmem ((Lbr exp Rbr) (list $2))
             ((Lbr exp Rbr listmem) (append (list $2) $4))
             )

    (function ((func Lpar vars Rpar Lcbr command Rcbr) (list 'func $3 $6)))

    (vars ((VAR) (list $1 'empty))
          ((VAR comma vars) (cons $1 $3)))

    (call ((VAR Lpar args Rpar) (list 'func_call $1 $3)))

    (args ((exp) (list $1 'empty))
          ((exp comma args) (cons $1 $3)))
    ))
   )
