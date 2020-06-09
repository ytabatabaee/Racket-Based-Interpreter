#lang racket


(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define my-lexer
	(lexer
		[","
		;=>
		(cons (token-comma)
          (my-lexer input-port))]

		[";"
		;=>
		(cons (token-semicolon)
          (my-lexer input-port))]
                
		["+"
		;=>
		(cons (token-plus)
          (my-lexer input-port))]
                
		["-"
		;=>
		(cons (token-minus)
          (my-lexer input-port))]

		["*"
		;=>
		(cons (token-mult)
          (my-lexer input-port))]
		["/"
		;=>
		(cons (token-div)
          (my-lexer input-port))]

		[">"
		;=>
		(cons (token-greater)
          (my-lexer input-port))]
		["<"
		;=>
		(cons (token-less)
          (my-lexer input-port))]

		["=="
		;=>
		(cons (token-equal)
          (my-lexer input-port))]
		["!="
		;=>
		(cons (token-notequal)
          (my-lexer input-port))]


		["="
		;=>
		(cons (token-assign)
          (my-lexer input-port))]
		["("
		;=>
		(cons (token-Lpar)
          (my-lexer input-port))]

		[")"
		;=>
		(cons (token-Rpar)
          (my-lexer input-port))]

		["["
		;=>
		(cons (token-Lbr)
          (my-lexer input-port))]
		["]"
		;=>
		(cons (token-Rbr)
          (my-lexer input-port))]

		["true"
		;=>
		(cons (token-TRUE)
          (my-lexer input-port))]
		["false"
		;=>
		(cons (token-FALSE)
          (my-lexer input-port))]

		["null"
		;=>
		(cons (token-NULL)
          (my-lexer input-port))]

		["if"
		;=>
		(cons (token-if)
          (my-lexer input-port))]
		["then"
		;=>
		(cons (token-then)
          (my-lexer input-port))]

		["else"
		;=>
		(cons (token-else)
          (my-lexer input-port))]
		["endif"
		;=>
		(cons (token-endif)
          (my-lexer input-port))]


		["while"
		;=>
		(cons (token-while)
          (my-lexer input-port))]
		["do"
		;=>
		(cons (token-do)
          (my-lexer input-port))]

		["end"
		;=>
		(cons (token-end)
          (my-lexer input-port))]

		["return"
		;=>
		(cons (token-return)
          (my-lexer input-port))]


		[(:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9))))
		;=>
		(cons (token-NUM (string->number lexeme))
          (my-lexer input-port))]

		[(:+ (:or (char-range #\a #\z) (char-range #\A #\Z)))
    ; =>
    (cons (token-VAR (string->symbol lexeme))
          (my-lexer input-port))]

		[(:: #\" (:* any-char whitespace) #\")
		;=>
                 (cons (token-STR lexeme)
          (my-lexer input-port))]

		[whitespace
			;=>
			(my-lexer input-port)]

		[(eof)
			;=>
			(token-EOF)]
		))


(define-tokens a (NUM VAR STR))
(define-empty-tokens b (EOF plus minus mult div greater less equal notequal assign Lpar Rpar Lbr Rbr TRUE FALSE NULL if then else endif while do end return comma semicolon)) ;?? NULL??

(define test1  "1+2+ 3 +   4
if a == 2.011 * 6 then
    b = \"Salam, chetori?! 1234 \"
endif")

(define test2 "while 10 * [34 null] then do return return true end")

(my-lexer (open-input-string test2))

