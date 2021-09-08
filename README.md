#  ΛΣ - LambdaScript - ΛΣ

A tiny turing complete language with lisp syntax.


We only have the following data types:

- `Double`: `0.01`
- `String`: `"Hello World"`
- `Boolean`: `true` | `false`

Here is a hello world for the LambdaScript

```
; this is a hello world script for
; lambdascript
(seq "Hello World")

```

Here is a function computing a fibonacci sequence

```
; simple fibonacci code with a loop
(seq (fn fibon (n)
         (seq (def n1 0.0)
              (def n2 1.0)
              (def i 0.0)
              (if (do < (n 0.0) )
                  (then (seq n1))
                  (else (seq (loop (do < (i n))
                                   (then (seq (def sum (do + (n1 n2)))
                                              (def n1 n2)
                                              (def n2 sum)
                                              (def i (do + (i 1.0)))
                                         )
                                   )
                             )
                             n1
                        )
                  )
              )
        )
     )
    (def x 9.0)
    (def y (do fibon (x)))
    y
)

```
Should output `34.0`.

## Current Features

- No return
- Turing complete
- You can define new functions with local variables.
- Very regular and simple syntax:
    - `seq` introduces a sequence of expressions where the computation flows
      from top to bottom
    - `def` is for defining new variables
    - `fn` introduces procedure definition statements
    - `do` introduces function calls
    - `if` introduces conditional statements
    - `then` introduces consequent branch in `if` and `loop` statements
    - `else` introduces alternate branch in `if` statements
    - `loop` introduces while statements where the computation goes on until
      the condition evaluates to false.

Here is the grammar of lambdascript in a bnf-ish form

```
expression := <literal> | <identifier> | <procedure call>

statement := <conditional>
            | <assignment>
            | <procedure definition> 
            | <loop>
            | <sequence>

literal := <boolean> | <number> | <string>

number := <digit>+.<digit>+
boolean := true | false
string := "...any number of char"

identifier := <letter>+ <digit>*
letter := a | b | c | d | e | f | ... | A | B | ... | Z
digit := 0 | ... | 9

procedural call := (do <operator> <operand>)
operator := <identifier> | opchar
opchar := + | - | * | / | < | > | & | \| | !
operand := (<expression>*)

assignment := (def <identifier> <expression>)

conditional := (if <test> <consequent> <alternate>)
test := (<literal>) | <procedural call> | (<identifier>)
consequent := (then <sequence>)
alternate := (else <sequence>)

loop := (loop <test> <consequent>)

procedure definition := (fn <identifier> <arguments> <body>)
arguments := (<identifier>*)
body := <sequence>

sequence := ( seq <expression>+ )

```

## Planned Features

- Static type checking
- input-output functions
- Adapt more functions from Haskell's Prelude to LambdaScript standard
  library.

## Usage

Currently there are two ways of evaluating LambdaScript code.

- `./lambdascript.out myAwesomeInputScript.lambda myAwesomeResultFile.txt`: This
  would evaluate your code in `myAwesomeInputScript.lambda` and write the
  result into `myAwesomeResultFile.txt`

- `./lambdascript.out myAwesomeInputScript.lambda stdout`: This would print the
  result of your code into terminal
