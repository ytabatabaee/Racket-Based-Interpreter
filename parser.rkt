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

    (command ((unitcom) (values $1))
             ((command semicolon unitcom) (values $1 $3))) 
    
    (unitcom ((whilecom) (values $1))
             ((ifcom) (values $1))
             ((assigncom) (values $1))
             ((returncom) (values $1)))
    
    (whilecom ((while exp do command end) (list 'while $2 $4)))
    
    (ifcom ((if exp then command else command endif) (list 'if $2 $4 $6)))
    
    (assigncom ((VAR assign exp) (list 'assign $1 $3)))
    
    (returncom ((return exp) (list 'return $2)))
    
    (exp ((aexp) (values $1))
         ((aexp greater aexp) (list 'gt $1 $3))
         ((aexp less aexp) (list 'lt $1 $3))
         ((aexp equal aexp) (list 'eq $1 $3))
         ((aexp notequal aexp) (list 'neq $1 $3)))
    
    (aexp ((bexp) (values $1))
          ((bexp minus aexp) (list 'sub $1 $3))
          ((bexp plus aexp)(list 'add $1 $3)))
    
    (bexp ((cexp) (values $1))
          ((cexp mult bexp) (list 'mult $1 $3))
          ((cexp div bexp) (list 'div $1 $3)))
    
    (cexp ((minus cexp) (list 'neg $2))
          ((Lpar exp Rpar) (list 'par $2))
          ((NUM) (list 'num $1))
          ((NULL) (values 'NULL))
          ((VAR) (list 'var $1))
          ((TRUE) (list 'bool 'true))
          ((FALSE) (list 'bool 'false))
          ((STR) (list 'string $1))
          ((list) (list 'list $1))
          ((VAR listmem) (list 'list_var $2)))
    
    (list ((Lbr listValues Rbr) (list 'br (list 'list_val $2)))
          ((Lbr Rbr) (list 'br)))
    
    (listValues ((exp) (values $1))
                ((exp comma listValues) (values $1))) ;Needs to be completed
     
    (listmem ((Lbr exp Rbr) (list 'list_idx $2))
             ((Lbr exp Rbr listmem) (values $2)) ;Needs to be completed
             )

    ))
   )

(define test1 "while 10 * [34, null] do return true end")

(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define lex (lex-this my-lexer (open-input-string test1)))
(let ((parser-res (gram_parser lex))) parser-res)
