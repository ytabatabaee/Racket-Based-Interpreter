#lang racket


(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define-tokens a (VAR NUM STR))
(define-empty-tokens b (comma semicolon EOF plus do while end if then else endif equal notequal return greater less minus mult div TRUE FALSE NULL Num Lbr Lpar Rbr Rpar assign))

(define my-lexer
	(lexer
		[","
		;=>
		(token-comma)]

		[";"
		;=>
		(token-semicolon)]
                
		["+"
		;=>
		(token-plus)]
                
		["-"
		;=>
		(token-minus)]

                ["*"
                ;=>
                (token-mult)]

		["/"
		;=>
		(token-div)]

		[">"
		;=>
		(token-greater)]

		["<"
		;=>
		(token-less)]

		["=="
		;=>
		(token-equal)]

		["!="
		;=>
		(token-notequal)]

		["="
		;=>
		(token-assign)]

		["("
		;=>
		(token-Lpar)]

		[")"
		;=>
		(token-Rpar)]

		["["
		;=>
		(token-Lbr)]

		["]"
		;=>
		(token-Rbr)]

		["true"
		;=>
		(token-TRUE)]

		["false"
		;=>
		(token-FALSE)]

		["null"
		;=>
		(token-NULL)]

		["if"
		;=>
		(token-if)]

		["then"
		;=>
		(token-then)]

		["else"
		;=>
		(token-else)]

		["endif"
		;=>
		(token-endif)]

		["while"
		;=>
		(token-while)]

		["do"
		;=>
		(token-do)]

		["end"
		;=>
		(token-end)]

		["return"
		;=>
		(token-return)]

		[(:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9))))
		;=>
		(token-NUM (string->number lexeme))]

		[(:+ (:or (char-range #\a #\z) (char-range #\A #\Z)))
                 ; =>
                 (token-VAR (string->symbol lexeme))]

		[(:: #\" (:* any-char whitespace) #\")
		;=>
                 (token-STR lexeme)]

		[whitespace
		;=>
		(my-lexer input-port)]

		[(eof)
		;=>
		(token-EOF)]
		))

(define gram_parser
  (parser
   (start command)
   (end EOF)
   (error void) ;May need changing
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
    
    (assigncom ((VAR assign exp) (list 'assign $1 $3)))
    
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

    ))
   )

(define test1 "while 10 do return true end")
(define test2 "while 10 * [1, 3 , 2 , true, []] do return true end")
(define test3 "return false; return true")
(define test4 "if 10 then return true else return false endif")
(define test5 "return [19, 29]")
(define test6 "return null")
(define test7 "if 10 > 4 then return [2*3, 7-9] else return 8/10 endif")
(define test8 "x= [[1, 2], [2, 3]]; return x[1][1]")
(define test9 "x = 5; y= [1, 2, 3, [4, 5]]; return false + [x>10, x<20, x!=y[3][1]]")
(define test10 "return [\"b\", 1] + \"a\"")

(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define lex (lex-this my-lexer (open-input-string test10)))
(let ((parser-res (gram_parser lex))) parser-res)
