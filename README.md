# Racket-Based Interpreter

In this project we developed an interpreter in Racket as our Programming Languages course project.

## Grammer
```
COMMAND → UNIT COM | COMMAND; UNIT COM
UNIT COM → W HILECOM | IF COM | ASSIGN | RET URN
WHILECOM → while EXP do COMMAND end
IF COM → if EXP then COMMAND else COMMAND endif
ASSIGN → variable = EXP | variable = F UNCT ION | variable = CALL
RET URN → return EXP
EXP → AEXP | AEXP > AEXP | AEXP < AEXP | AEXP == AEXP
| AEXP! = AEXP
AEXP → BEXP | BEXP − AEXP | BEXP + AEXP
BEXP → CEXP | CEXP ∗ BEXP | CEXP/BEXP
CEXP → −CEXP | (EXP) | posNumber | null | variable | true | f alse
| string | LIST | variable LISTMEM
LIST → [LIST V ALUES] | [ ]
LIST V ALUES → EXP | EXP, LIST V ALUES
LISTMEM → [EXP] | [EXP] LISTMEM
F UNCT ION → func(V ARS) {COMMAND}
V ARS → variable | variable, V ARS
CALL → variable(ARGS)
ARGS → EXP | EXP, ARGS
```
## Run
Run `interpreter.rkt` with Racket and exectute the following command.
```
(evalute 'test_file.txt')
```
