# ΞΞ£ - LambdaScript - ΞΞ£

A tiny turing complete language with lisp syntax.


We only have the following data types:

- `Double`: `0.01`
- `String`: `"Hello World"`
- `Boolean`: `true` | `false`

Here is a hello world for the LambdaScript

```
; this is a hello world script for lambdascript
(seq "Hello World")

```

Here is a function computing a fibonacci sequence

```
; simple fibonacci code with a loop
(seq (fn fibon: float (n: float)
         (seq (def n1: float 0.0)
              (def n2: float 1.0)
              (def i: float 0.0)
              (if (do < (n 0.0) )
                  (then (seq n1))
                  (else (seq (loop (do < (i n))
                                   (then (seq (def sum: float (do + (n1 n2)))
                                              (def n1: float n2)
                                              (def n2: float sum)
                                              (def i: float (do + (i 1.0)))
                                         )
                                   )
                             )
                             n1
                        )
                  )
              )
        )
     )
    (def x: float 9.0)
    (def y: float (do fibon (x)))
    y
)

```
Should output `34.0`.

## Highlights

The main feature of this language is its exchangeable keywords:

Here is the same fibonacci sequence function with different keywords:

- Egyptian Hieroglyphs:

```
; fibonacci with egyptian hieroglyphs
(ππ (πΏπ fibon: float (n: float)
         (ππ (π π¨ n1: float 0.0)
              (π π¨ n2: float 1.0)
              (π π¨ i: float 0.0)
              (ππ (πΉ < (n 0.0) )
                  (ππ (ππ n1))
                  (πππ (ππ (ππ£ (πΉ < (i n))
                                   (ππ (ππ (π π¨ sum: float (πΉ + (n1 n2)))
                                              (π π¨ n1: float n2)
                                              (π π¨ n2: float sum)
                                              (π π¨ i: float (πΉ + (i 1.0)))
                                         )
                                   )
                             )
                             n1
                        )
                  )
              )
        )
     )
    (π π¨ x: float 9.0)
    (π π¨ y: float (πΉ fibon (x)))
    y
)
```

If you have code2003 font installed in your system, you should see the
following image:

![hiero-fibonacci](./images/fibonacci-egypt.png)

- Turkish

```
; fibonacci tr
(liste (edim fibon: float (n: float)
         (liste (tanΔ±m n1: float 0.0)
              (tanΔ±m n2: float 1.0)
              (tanΔ±m i: float 0.0)
              (eΔer (yap < (n 0.0) )
                  (ise (liste n1))
                  (yoksa (liste (dongu (yap < (i n))
                                   (ise (liste (tanΔ±m sum: float (yap + (n1 n2)))
                                              (tanΔ±m n1: float n2)
                                              (tanΔ±m n2: float sum)
                                              (tanΔ±m i: float (yap + (i 1.0)))
                                         )
                                   )
                             )
                             n1
                        )
                  )
              )
        )
     )
    (tanΔ±m x: float 9.0)
    (tanΔ±m y: float (yap fibon (x)))
    y
)

```

- Kurdish

```
; fibonacci kur
(listeya (kari fibon: float (n: float)
         (listeya (binavkirin n1: float 0.0)
              (binavkirin n2: float 1.0)
              (binavkirin i: float 0.0)
              (eger (webike < (n 0.0) )
                  (paΕan (listeya n1))
                  (wekidi (listeya (Γ§erxeke (webike < (i n))
                                   (paΕan (listeya (binavkirin sum: float
                                   (webike + (n1 n2)))
                                              (binavkirin n1: float n2)
                                              (binavkirin n2: float sum)
                                              (binavkirin i: float (webike + (i 1.0)))
                                         )
                                   )
                             )
                             n1
                        )
                  )
              )
        )
     )
    (binavkirin x: float 9.0)
    (binavkirin y: float (webike fibon (x)))
    y
)
```

- Arabic

```
; fibonacci ara
(ΩΨ§Ψ¦ΩΨ© (ΩΨΈΩΩΨ© fibon: float (n: float)
         (ΩΨ§Ψ¦ΩΨ© (Ψ­Ψ―Ψ― n1: float 0.0)
              (Ψ­Ψ―Ψ― n2: float 1.0)
              (Ψ­Ψ―Ψ― i: float 0.0)
              (ΩΩ (Ψ§ΩΨΉΩ < (n 0.0) )
                  (Ψ«Ω (ΩΨ§Ψ¦ΩΨ© n1))
                  (Ψ’Ψ?Ψ± (ΩΨ§Ψ¦ΩΨ© (Ψ­ΩΩΩΨ²Ψ―Ω (Ψ§ΩΨΉΩ < (i n))
                                   (Ψ«Ω (ΩΨ§Ψ¦ΩΨ© (Ψ­Ψ―Ψ― sum: float (Ψ§ΩΨΉΩ + (n1 n2)))
                                              (Ψ­Ψ―Ψ― n1: float n2)
                                              (Ψ­Ψ―Ψ― n2: float sum)
                                              (Ψ­Ψ―Ψ― i: float (Ψ§ΩΨΉΩ + (i 1.0)))
                                         )
                                   )
                             )
                             n1
                        )
                  )
              )
        )
     )
    (Ψ­Ψ―Ψ― x: float 9.0)
    (Ψ­Ψ―Ψ― y: float (Ψ§ΩΨΉΩ fibon (x)))
    y
)

```

- Persian

```
; fibonacci per
(ΩΫΨ³Ψͺ (ΨΉΩΩΪ©Ψ±Ψ― fibon: float (n: float)
         (ΩΫΨ³Ψͺ (ΩΨ΄Ψ?Ψ΅Ϊ©Ψ±Ψ―Ω n1: float 0.0)
              (ΩΨ΄Ψ?Ψ΅Ϊ©Ψ±Ψ―Ω n2: float 1.0)
              (ΩΨ΄Ψ?Ψ΅Ϊ©Ψ±Ψ―Ω i: float 0.0)
              (Ψ§Ϊ―Ψ± (Ψ§ΩΨ¬Ψ§ΩΨ―ΩΫΨ― < (n 0.0) )
                  (Ψ³ΩΎΨ³ (ΩΫΨ³Ψͺ n1))
                  (Ψ―ΫΪ―Ψ±Ϋ (ΩΫΨ³Ψͺ (Ψ­ΩΩΩΨ²Ψ―Ω (Ψ§ΩΨ¬Ψ§ΩΨ―ΩΫΨ― < (i n))
                                   (Ψ³ΩΎΨ³ (ΩΫΨ³Ψͺ (ΩΨ΄Ψ?Ψ΅Ϊ©Ψ±Ψ―Ω sum: float (Ψ§ΩΨ¬Ψ§ΩΨ―ΩΫΨ― + (n1 n2)))
                                              (ΩΨ΄Ψ?Ψ΅Ϊ©Ψ±Ψ―Ω n1: float n2)
                                              (ΩΨ΄Ψ?Ψ΅Ϊ©Ψ±Ψ―Ω n2: float sum)
                                              (ΩΨ΄Ψ?Ψ΅Ϊ©Ψ±Ψ―Ω i: float (Ψ§ΩΨ¬Ψ§ΩΨ―ΩΫΨ― + (i 1.0)))
                                         )
                                   )
                             )
                             n1
                        )
                  )
              )
        )
     )
    (ΩΨ΄Ψ?Ψ΅Ϊ©Ψ±Ψ―Ω x: float 9.0)
    (ΩΨ΄Ψ?Ψ΅Ϊ©Ψ±Ψ―Ω y: float (Ψ§ΩΨ¬Ψ§ΩΨ―ΩΫΨ― fibon (x)))
    y
)

```

- French

```
; fibonacci fr
(tableau (fonction fibon: float (n: float)
         (tableau (dΓ©f n1: float 0.0)
              (dΓ©f n2: float 1.0)
              (dΓ©f i: float 0.0)
              (si (faire < (n 0.0) )
                  (alors (tableau n1))
                  (sinon (tableau (boucle (faire < (i n))
                                   (alors (tableau (dΓ©f sum: float (faire + (n1 n2)))
                                              (dΓ©f n1: float n2)
                                              (dΓ©f n2: float sum)
                                              (dΓ©f i: float (faire + (i 1.0)))
                                         )
                                   )
                             )
                             n1
                        )
                  )
              )
        )
     )
    (dΓ©f x: float 9.0)
    (dΓ©f y: float (faire fibon (x)))
    y
)
```

- German

```
; fibonacci ger
(List (Prozedur fibon: float (n: float)
         (List (beschreib n1: float 0.0)
              (beschreib n2: float 1.0)
              (beschreib i: float 0.0)
              (wenn (tun < (n 0.0) )
                  (dann (List n1))
                  (sonst (List (Schleife (tun < (i n))
                                   (dann (List (beschreib sum: float (tun + (n1 n2)))
                                              (beschreib n1: float n2)
                                              (beschreib n2: float sum)
                                              (beschreib i: float (tun + (i 1.0)))
                                         )
                                   )
                             )
                             n1
                        )
                  )
              )
        )
     )
    (beschreib x: float 9.0)
    (beschreib y: float (tun fibon (x)))
    y
)
```

Basically if you want to use extended keywords feature you need to provide a
`keywords.lambda.txt` file to compiler. As in:

```
./lambdascript.out keywords.lambda.txt fibonacci-egy.lambda stdout
```
An example has been provided in `bin` folder. The only restriction is that
each keyword must be composed of alphanumeric characters as defined by utf-8.
Each keyword must be unique in `keywords.lambda.txt` file. You can mix up
languages if you wish, so `loop dΓ©f alors dann ΨΉΩΩΪ©Ψ±Ψ―` would be a valid choice
of keywords if you would like to write your code in that way.

## Current Features

- No return
- Turing complete
- Garbage collection
- You can define new functions with local variables.
- Exchangeable keywords .
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
expression := <get> | <statement>

statement := <conditional>
            | <assignment>
            | <procedure definition> 
            | <loop>
            | <procedure call>
            | <sequence>


get := <varname> | <literal>

literal := <boolean> | <number> | <string>

number := <digit>+ | <digit>+.<digit>+
boolean := true | false
string := "...any number of char"

identifier := <varname> <typename>
varname := <letter>+ <digit>*
letter := a | b | c | d | e | f | ... | A | B | ... | Z
digit := 0 | ... | 9
typename := : <varname>

procedural call := (do/yap <operator> <operand>)
operator := opchar | <varname>
opchar := + | - | * | / | % | < | > | & | \| | !
operand := (<expression>*)

assignment := ( def/tanim <identifier> <expression> )

conditional := (eger/if <test> <consequent> <alternate>)
test := (<literal>) | <procedural call> | (<varname>)
consequent := (then/ise <sequence>)
alternate := (else/yoksa <sequence>)

loop := (loop/dongu <test> <consequent>)

procedure definition := (fn/edim <identifier> <arguments> <body>)
arguments := (<identifier>*)
body := <sequence>

sequence := ( seq/liste <expression>+ )

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
