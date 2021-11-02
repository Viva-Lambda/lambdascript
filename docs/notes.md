# Design Notes

The new grammar in bnf like form:


```
program := <expression> <expression>*

expression := <constant> 
            | <abstraction-declaration>
            | <call-expression>


get := <constant> | <call-expression>
call-expression := <varname> | <application>


constant := <bool> | <number> | <string>
bool := true | false
number := <int> | <real>
int := 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
real := <int>+.<int>+
string := " any unicode character that does not break quotes "

varname := <alphabetic>+<numeric>* | alphabetic>+_*<numeric>*_*
alphabetic := [a-zA-Z]
numeric := <int>

abstraction-declaration := <procedure-declaration>
                         | <context-declaration>
                         | <bind-declaration>

procedure := (<in-marker><codomain-indicator> <abstraction-name>(<identifier>*)<substitute-marker>(<expression>+))
codomain-indicator := <typename>
abstraction-name := <varname>
identifier := <varname><in-marker> <typename>
in-marker := :
typename := <base-typename> | <compound-typename>
base-typename := <constant-typename> | <proc-type-name>
constant-typename := <numeric-typename> | <string-typename> | <bool-typename>
numeric-typename := int | float
string-typename := str
bool-typename := bool
context-type-name := context
proc-type-name := proc
substitute-marker := <s-m>
s-m := .
compound-typename := <context-name>

bind-declaration := <context-bind> | <flow-bind>

context-bind := <abstraction-bind> | <member-bind>
abstraction-bind := (:= <context-name>(<context-varname>+)<s-m><abstraction-name>)
context-name := <varname>
context-varname := <varname>
member-bind := <context-get-bind> | <context-context-bind>
context-get-bind := (:= <context-name>(<context-varname>)<s-m><get>)
context-context-bind := (:= <context-name>(<context-varname>)<s-m><context-name>)

flow-bind := (:= <context-name>(<abstraction-name>)<s-m>(<signaled-expression>+))
signaled-expression := <readwrite-context-declaration>
                     | <readonly-context-declaration>

readwrite-context-declaration := (<signal>)<s-m>(<abstraction-name> <fetch-operator>)
readonly-context-declaration := (<signal>)<s-m>(<abstraction-name>)
signal := <constant> | <default-branch-marker>
default-branch-marker := _


context-declaration := <full-declaration> | <partial-declaration>
full-declaration := (<basis-marker> <context-name><s-m>(<type-assumption>+))
basis-marker := |-
type-assumption := <identifier> | <multi-assumption>
multi-assumption := (<varname>+ <in-marker> <typename>)
partial-declaration := (<basis-marker> <context-name><s-m><type-assumption>)

application := <fetch-app> | <numeric-app> | <abstract-app>
fetch-app := (<fetch-operator>)
           | (<fetch-operator> <abstraction-name>)
           | (<fetch-operator> <abstraction-name><in-marker><context-name>)

fetch-operator := $

numeric-app := <arithmetic-app> | <compare-app>
arithmetic-app := (<arithmetic-operator> <get>+)
compare-app := (<compare-operator> <get>) | (<compare-operator> <get> <get>)
arithmetic-operator := <plus> | <multiply> | <minus> | <divide> | <power>

plus := +
minus := -
divide := /
multiply := *
power := ^

compare-operator := <or> | <and> | <equal> | <not> | <greater>

not := ~
equal := ==
or := ||
and := &&
greater := >

abstract-app := (<operator> <operand>*)
operator := <free-operator> | <bounded-operator>
free-operator := <abstraction-name>
bounded-operator := <context-name>(<abstraction-name>)
operand := <get>

```

`f x.x + 2` => `: int f(x: int) = x + 2`

Usage examples in new grammar:

```clojure

;; marks comment

;; we have two ways of defining abstractions

;; simple way where the declaration of the signature and body is combined
(:int f1(x: float, y: float).(floor (* x 2) (+ y 3)))
;; read this as the f2 is the abstraction that binds the variable x to the
;; expression (* x 2) so for (f 3) we will have (* 3 2) which would evaluate
;; to 6. For functions with literal values such as these the expression would
;; be directly evaluated during the compilation.

;; declarative way
(:int f2(:int(float, float), int, float)) 
;; read this as f1 is the abstraction that binds the abstraction binding two
;; floating points to an integer to an integer and a float
;; Notice that this is curried as 
;; (:int(float, float) -> (int -> (float -> int)))
;; if we have (f2 f1 6 1.3) this transforms to
;; (f2 f1) == 
;;   ((:int(float, float) -> (int -> (float -> int))) :int(float, float))
;; and we get (int -> (float -> int)) to which we apply 6
;; and we get (float -> int) to which we apply 1.3
;; and we get int
;; we can not organize the expressions inside the function in this manner
;; then there must be a problem with the function.

;; let's define several more abstractions
(:float g(y: float).(** y 3))
(:bool h(z: bool).(== z true))

;; we can combine these abstractions and bind them to different names
;; to obtain more complex abstractions such as
(:float f_g(x: float).(g (h x)))

;; context declaration 

(|- MyContext
    (:int x)
    (:float y)
    (:floats xs)
    (:ints ys).(6) ;; equals (0 0 0 0 0 0)
    (:str z)
    (:strs zs).(3) ;; equals ("" "" "")
    (:bool f(arg1: bool arg2: bool))
)

;; you can also declare context with default values
(|- MyContext
    (:int x.(6))
    (:float y.(6.7))
    (:floats xs.(6.7 7.3 75.0 0.5))
    (:ints ys.(6)) ;; equals (0 0 0 0 0 0)
    (:str z.("my string"))
    (:strs zs.("my string" "my another string" "some other string"))
    (:bool f(arg1: bool arg2: bool).(&& arg1 arg2))
)
;; this creates accessor functions automatically so we have
(x MyContext) ;; gives 6
(xs MyContext) ;; gives (6.7 7.3 75.0 0.5)

;; we can also declare contexts partially
(|- AnotherContext.(6) ;; we have six slots for binding abstractions
)

;; binding abstractions to partially declared contexts is very easy
;; suppose we want to have something like the following at the end
(|- AnotherContext
    (:int a).(4)
    (:float b).(3.7)
    (:floats as).(5.7 5.3 81.0 -2.5)
    (:ints bs).(5 9 70 2)
    (:str c).("string")
    (:strs cs).("string" "m string" "n string")
)
;; we need to declare them as the following:
(|- AnotherContext.(6)
)

(:int a.(4))
(:= AnotherContext.a)

(:= AnotherContext.(:float b(3.7)))
(:= AnotherContext.(:floats as(5.7 5.3 81.0 -2.5)))
(:= AnotherContext.(:ints bs(5 9 70 2)))
(:= AnotherContext.(:str c("string")))
(:= AnotherContext.(:strs cs("string" "m string" "n string")))

;; the difference between binding 'a' and other bindings is that 
;; binding a can be overloaded with other contexts so it is possible 
;; to do something like this
(|- MyA.(2))
(|- MyB.(1))

(:int p.(6))
(:= MyA.p)
(:= MyB.p)
(:= MyA.(:str c("mystring")))

;; these make the following usage possible
(p MyA) ;; results in 6
(p MyB) ;; results in 6
(c MyA) ;; resutls in "mystring"

;; the accessor functions must be unique to each context unless explicitly
;; declared as free abstractions.

;; Notice that one can define abstractions such as
(:int fn1(x: int).(* x 2))
(:int fn2(y: int).(* y 3))

;; then bind it to different contexts
(|- MContA.(2))
(|- MContB.(3))

(:= MContA.(fn1))
(:= MContA.(fn2))

(:= MContB.(fn1))
(:= MContB.(fn2))
(:= MContB.(:bool f(arg: bool arg2: bool).(|| arg arg2)))

;; notice that abstractions can produce contexts that are previously declared
(|- ContA
    (:int x)
    (:int y)
    (:floats z).(2)
    (:int f(arg1: int arg2: int))
)
(:ContA 
    contMaker(x_: int, y_: int, z_1: float, z_2: float).(
    )
)

```
Constructs of the language:
flow statements: goto statements on steroids
context: corresponds more or less structs in C-like languages
abstraction: corresponds to procedures.

procedure takes a context as input, and outputs a context and a signal.
The signal is used for branching in the resulting computational graph.

Flows are bindings. They are essentially stitches that join the computations.
Computations can be expressed within contexts or can inherit the context of
previous caller.
The implicit flow and the explicit flow.
The implicit flow is when a procedure calls another procedure in its body.
The explicit flow is when a procedure is tied to another procedure explicitly
using a flow statement.

The implicit declaration of explicit flow is when a procedure declares what is
to be follow or precede it in its body (this is not yet supported !).

Couple of restrictions apply. implicit flows can not have external
contexts. They are either empty context functions produced by some combinators
or functions that have the same context. The restriction does not apply to
implicit declaration of explicit flows.

Procedures that access multiple contexts imply that there are multiple flow
statements pointing towards the procedure each coming with different context.


Context is essentially a struct. Meaning that it has some heterogeneous
fields with names. They are to be declared with default values, no null
reference etc.

The old grammar in bnf like form:

```
Here is the grammar of the language

expression := <get> | <statement>

statement := <conditional>
            | <assignment>
            | <procedure definition> 
            | <loop>
            | <procedure call>
            | <sequence>


-- expressions
get := <identifier name> | <literal>

-- literals
literal := <boolean> | <number> | <string>

number := <digit>+ | <digit>+.<digit>+
boolean := true | false
string := "...any number of char"

identifier := <identifier name> <typename>
identifier name := <varname>
varname := <letter>+ <digit>*
letter := a | b | c | d | e | f | ... | A | B | ... | Z
digit := 0 | ... | 9
typename := : <varname>

procedure call := (do/yap <operator> <operand>)
operator := opchar | <varname>
opchar := + | - | * | / | % | < | > | & | \| | !
operand := (<typed expression>*)

assignment := ( def/tanim <identifier> <typed expression> )

typed expression :=  <literal> | <identifier name> | <procedure call>

conditional := (eger/if <test> <consequent> <alternate>)
test := (<literal>) | <procedure call> | (<varname>)
consequent := (then/ise <sequence>)
alternate := (else/yoksa <sequence>)

loop := (loop/dongu <test> <consequent>)
-- (loop/dongu (do/yap < (1.6 6.0)) (then/ise fdsak,m))

procedure definition := (fn/edim <identifier> <arguments> <body>)
arguments := (<identifier>*)
body := <sequence>

sequence := ( seq/liste <expression>+ )



```
