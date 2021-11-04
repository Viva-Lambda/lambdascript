# Design Notes

The new grammar in bnf like form: TODO needs to be updated to usage examples

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
                         | <record-declaration>
                         | <bind-declaration>

lambda-body := (<expression>*)
lambda-arguments := (<identifier>*)
lambda-signature := <in-marker><codomain-indicator>
lambda-declaration := (<lambda-signature> <lambda-arguments><s-m><lambda-body>)
procedure := (<lambda-signature> <abstraction-name><lambda-arguments><s-m><lambda-body>)
codomain-indicator := <typename>
abstraction-name := <varname>
identifier := <varname><in-marker> <typename>
in-marker := :
typename := <base-typename> | <compound-typename>
base-typename := <constant-typename>
constant-typename := <numeric-typename> | <string-typename> | <bool-typename>
numeric-typename := int | float
string-typename := str
bool-typename := bool
substitute-marker := <s-m>
s-m := .
compound-typename := <record-name>

bind-declaration := <record-bind> | <flow-bind> | <abstraction-bind>

record-bind := (:= <record-name><s-m>( <record-element>+ ))
record-element := <abstraction-member> | <literal-member> 
abstraction-member := <record-varname><s-m><lambda-declaration>
record-name := <varname>
record-varname := <varname>
literal-member := <record-varname><s-m>(<constant> | <record-name>)

flow-bind := (:= <context-name>(<abstraction-name>)<s-m>(<signaled-expression>+))
signaled-expression := <explicit-context-declaration>
                     | <implicit-context-declaration>

explicit-context-declaration := (<signal>)<s-m>(<abstraction-name> <context-name>)
implicit-context-declaration := (<signal>)<s-m>(<abstraction-name>)
signal := <constant> | <default-branch-marker>
default-branch-marker := _

abstraction-bind := (:= <abstraction-name><s-m><abstraction-name>)

context-declaration := <full-context-declaration>
full-context-declaration := (<basis-marker> <context-name><s-m>(<type-assumption>+))
basis-marker := |-
type-assumption := (<in-marker><predicate> <subject>)
predicate := <typename>
subject := <varname>

record-declaration := <full-record-declaration> | <partial-record-declaration>
full-record-declaration := (<record-marker> <record-name><s-m>(<record-member>+))
partial-record-declaration := (<record-marker> <record-name><s-m>(<int>+))


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

;; context declaration. It includes typing judgements that can be used
;; with abstractions

(|- MyContext.(
    (:int x)
    (:float y)
    (:floats xs(2)) ;; equals(float float)
    (:ints ys(6)) ;; equals (int int int int int int)
    (:str z)
    (:strs zs(3)) ;; equals (str str str)
    (:bool f(arg1: bool arg2: bool))
    )
)

;; record declaration. Records hold heterogeneous data bind to nmaes

(:# MyRecord.(
    (:int a).(4)
    (:float b).(3.7)
    (:floats as).(5.7 5.3 81.0 -2.5)
    (:ints bs).(5 9 70 2)
    (:str c).("string")
    (:strs cs).("string" "m string" "n string")
    ) 
)

;; this creates accessor functions automatically so we have
(a MyRecord) ;; gives 4
(as MyRecord) ;; gives (5.7 5.3 81.0 -2.5)

;; we can also declare records partially
(:# MyOtherRecord.(6) ;; we have six slots for binding abstractions
)

;; binding abstractions to partially declared records is very easy
;; suppose we want to have something like the following at the end
(:# MyOtherRecord.(
    (:int a).(4)
    (:float b).(3.7)
    (:floats as).(5.7 5.3 81.0 -2.5)
    (:ints bs).(5 9 70 2)
    (:str c).("string")
    (:strs cs).("string" "m string" "n string")
    )
)
;; we need to declare them as the following:
(:# MyOtherRecord.(6)
)

(:int a.(4))
(:# MyOtherRecord.a)

(:# MyOtherRecord.(:float b(3.7)))
(:# MyOtherRecord.(:floats as(5.7 5.3 81.0 -2.5)))
(:# MyOtherRecord.(:ints bs(5 9 70 2)))
(:# MyOtherRecord.(:str c("string")))
(:# MyOtherRecord.(:strs cs("string" "m string" "n string")))


;; if all slots are not bind with expressions the compiler will generate
;; error.

;; the difference between binding 'a' and other bindings is that 
;; binding a can be overloaded with other records that have the same type for
;; the "a" so it is possible to do something like the following

(|- MyContextA.(
    (:int a)
    (:int b)
    )
)
(|- MyContextB.(
        (:int a)
        (:float b)
    )
)

(:int a.(6))
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

;; then bind it to different records to obtain and interface like behaviour
(:# MRecA.(2))
(:# MRecB.(3))

(:= MRecA.(fn1))
(:= MRecA.(fn2))

(:= MRecB.(fn1))
(:= MRecB.(fn2))
(:= MRecB.(:bool f(arg: bool arg2: bool).(|| arg arg2)))

;; notice that abstractions can produce records that are previously declared
(:# RecordA
    (:int x)
    (:int y)
    (:floats z).(2)
    (:int f(arg1: int arg2: int))
)
(:RecordA
    contMaker(x_: int, y_: int, z_1: float, z_2: float).(
        (:= RecordA.(
                x.(x_)
                y.(y_)
                z.(z_1 z_2)
                f.(:int (arg1: int arg2: int).(+ (* arg1 x_) (* arg2 y_)))
            )
        )
    )
)
;; if the record is previously not declared, trying to produce it would result
;; in compile time error

;; now let's see flow binding
(:int f1.(4)) ;; abstraction
(:int f2.(-1)) ;; abstraction
(:int f3.(7)) ;; abstraction

(:= MyContext(f1).( ;; flow binding
        (4).(f2) ;; if f1 outputs 4 f2 is evaluated afterwards
        (_).(f3) ;; otherwise f3 evaluated
    )
)

;; what happens if we want to reuse abstractions further down the evaluation
;; path. Simply, what happens if we have something like 
;; f1 -> f2 -> f3 -> f1 -> f4 where pointing to f1 in f3 would cause
;; revaluation of f2

;; the solution is simple: just bind the relative abstraction to a new variable
(:= fa.f1)

;; then the flow statement would do f1 -> f2 -> f3 -> fa -> f4
```

Constructs of the language:
flow statements: goto statements on steroids
context: holds typing information with respect to some evaluation context.
abstraction: corresponds to procedures.
record: corresponds more or less structs in C-like languages.

procedure takes a context as input, and outputs an atomic value or a record
and a signal. The signal is used for branching in the resulting computational
graph. The idea is that for values such as true, false, and other constants
required for deciding flow direction, the developer knows which branch goes
along with which value. If the developer does not know which a constant value
one can always recourse to wild card notations for specifying the branch.

Flows are bindings. They are essentially stitches that join computations.
Computations can be expressed within contexts or can inherit the context of
previous caller. The transmission of values from one procedure to another is
done using captures. If the value is used in the next procedure, it is
captured in the next procedure, if it is not used, it is not captured thus
discarded.


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

Procedures that access multiple contexts result in compile error. There must
only be single context associated with any given procedure at each time. If
the typing basis of the abstraction requires more elaborate contexts, the
developer can merge contexts using context binds. Context binds can stitch two
contexts to a different name if all of their variables are distinct from each
other.

Notice that contexts are typing contexts. They simply tell
you how variables of the abstraction are typed. This mostly apply to the
keywords of functions. We deduce the rest from the body of the function.They
are to be interpreted as typing basis in which some variable is associated
to a type, or in simply typed lambda calculus terms, some subjects are
associated with predicates.

Record is essentially a struct. Meaning that it has some heterogeneous
fields with names. They are to be declared with default values, no null
reference etc.

Types 
------

There are only two types in LambdaScript: function space types, and atomic
types. 

Atomic types are: `int, str, bool, float`. Notice the absence of the null, or
none type. If one wants to return null from a function, one should simply not
capture the function's value while writing the relative flow statements of the
function.

Function space types have the following structure: `A -> B`. Notice that
record in this sense are simply function space types, since the so called
members are just accessory procedures that output a literal value or another
expression given the record.


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
