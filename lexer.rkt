#lang racket

(provide my-lexer)

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

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

		[(:: "\"" (:* (:- any-char "\"")) "\"" )
		;=>
                 (token-STR (substring lexeme 1 (- (string-length lexeme) 1)))]

		[whitespace
		;=>
		(my-lexer input-port)]

		[(eof)
		;=>
		(token-EOF)]

                [any-char (error "Error: no token matched for" lexeme)]
		))


(define-tokens a (NUM VAR STR))
(define-empty-tokens b (EOF plus minus mult div greater less equal notequal assign Lpar Rpar Lbr Rbr TRUE FALSE NULL if then else endif while do end return comma semicolon)) ;?? NULL??

(define test1  "1+2+ 3 +   4
if a == 2.011 * 6 then
    b = \"Salam, chetori?! 1234 \"
endif")

(define test2 "while 10 * [34 null] then do return return true end")
(define test3 "return [\"blue sky\", 1] + \"a\" #x ")

;(my-lexer (open-input-string test2))

(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define lex (lex-this my-lexer (open-input-string test3)))
(lex)
(lex)
(lex)
(lex)
(lex)
(lex)
(lex)
(lex)
(lex)
(lex)
