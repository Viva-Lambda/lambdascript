# Design Notes

The new grammar in bnf like form

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
partial-record-declaration := <partial-recod-header> <partial-record-member>+
partial-record-header := (<record-marker> <record-name><s-m>(<int>+))
partial-recod-member := (<record-marker> <record-name><s-m>(<record-member>))
record-marker := :#


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

(:int f1(float, float)) ;; declaration of the abstraction
(:= f1(x, y).(floor (* x 2) (+ y 3))) ;; bindin expression to an abstraction

;; declaration of an abstraction which takes another abstraction as argument
(:int f2( (:int float, float), float, float))

;; binding of an expression to the abstraction
(:= f2(fn, arg1, arg2).(fn arg1 arg2))


;; let's define several more abstractions
(:float g(float))
(:= g(y).(** y 3))
(:bool h(float))
(:= h(z).(== z 1.0))

;; we can combine these abstractions and bind them to different names
;; to obtain more complex abstractions such as
(:float f_g(x: float).(g (h x)))

;; context declaration. It includes typing judgements that can be used
;; with abstractions

(|- MyContext.(
    (:int x)
    (:float y)
    (:float(2) xs) ;; equals(float float)
    (:int(6) ys) ;; equals (int int int int int int)
    (:str z)
    (:str(3) zs) ;; equals (str str str)
    (:bool f(arg1: bool arg2: bool))
    )
)

;; once you declared a context, you can extend it by binding other contexts
;; which do not contain duplicates using bind statements

;; Let's define some contexts
(|- MyContextA.( (:int a) (:float b) ))
(|- MyContextB.( (:int c) (:float d) ))
(|- MyContextC.( (:int e) (:float f) ))
(|- MyContextD.( (:float a) (:float g) ))

;; the following is valid
(:= MyContextC.(MyContextA MyContextB))

;; the following is invalid
(:= MyContextD.(MyContextA MyContextB)) ;; since "a" is duplicate

;; record declaration. Records hold heterogeneous data bind to names

(:# MyRecord.(
    (:int a.(4))
    (:float b.(3.7))
    (:float(4) as.(5.7 5.3 81.0 -2.5))
    (:int(4) bs.(5 9 70 2))
    (:str c.("string"))
    (:str(3) cs.("string" "m string" "n string"))
    ) 
)

;; this creates accessor functions automatically so we have
(a MyRecord) ;; gives 4
(as MyRecord) ;; gives (5.7 5.3 81.0 -2.5)

;; we can also declare records partially
(:# MyOtherRecord(6) ;; we have six slots for binding abstractions
)

;; binding abstractions to partially declared records is very easy
;; suppose we want to have something like the following at the end
(:# MyOtherRecord.(
    (:int a.(4))
    (:float b.(3.7))
    (:float(4) as.(5.7, 5.3, 81.0, -2.5))
    (:int(4) bs.(5, 9, 70, 2))
    (:str c.("string"))
    (:str(3) cs.("string" "m string" "n string"))
    )
)
;; we need to declare them as the following:
(:# MyOtherRecord(6)
)

(:int a.(4))
(:# MyOtherRecord.a)

(:# MyOtherRecord.(:float b.(3.7)))
(:# MyOtherRecord.(:float(4) as.(5.7 5.3 81.0 -2.5)))
(:# MyOtherRecord.(:int(4) bs.(5 9 70 2)))
(:# MyOtherRecord.(:str c.("string")))
(:# MyOtherRecord.(:str(4) cs.("string" "m string" "n string")))


;; if all slots are not bind with expressions the compiler will generate
;; error.

;; the difference between binding 'a' and other bindings is that 
;; binding a can be overloaded with other records that have the same type for
;; the "a" so it is possible to do something like the following

(:# MyRecA(2))
(:# MyRecB(2))

(:int a.(6))
(:# MyRecA.a)
(:# MyRecB.a)
(:# MyRecA.(:str c("mystring")))

(:# MyRecB.(:float b.(4.8)))

;; these make the following usage possible
(p MyA) ;; results in 6
(p MyB) ;; results in 6
(c MyA) ;; resutls in "mystring"

;; the accessor functions must be unique to each context unless explicitly
;; declared as free abstractions.

;; Notice that one can define abstractions such as
(:int fn1(x: int).(* x 2))
(:int fn2(y: int).(* y 3))

;; then compose them to different records to obtain and interface like behaviour
(:# MRecA(2))
(:# MRecB(3))

(:# MRecA.(fn1))
(:# MRecA.(fn2))

(:# MRecB.(fn1))
(:# MRecB.(fn2))
(:# MRecB.(:bool f(arg: bool arg2: bool).(|| arg arg2)))

;; notice that abstractions can produce records that are previously declared
(:# RecordA
    (:int x.(0))
    (:int y.(0))
    (:float z.(2))
    (:int f.(arg1: int arg2: int))
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
;; this is abstraction binding

;; what if you want to bind another context to the next execution

;; some metaprogramming facilities are provided with macros

;; let's declare a macro
@MyMacro(|- MyContC.( (:int a) (:int b) (:int c) ))

;; you can think of this as attaching a list of elements to MyMacro
;; something like 
;; MyMacro = ["|-", ["MyContC", ["( (:int a) (:int b) (:int c) )"]] ]

;; you can access these elements with
;; @^ for the head of the list and @$ for the tail of the list
;; if you want to access all of the content you can use the @ operator
;; these operators are used in the following way
(@ MyMacro) ;; results in (|- MyContC.( (:int a) (:int b) (:int c) ))
(@^ MyMacro) ;; results in ( |- )
(@$ MyMacro) ;; results in ( MyContC.( (:int a) (:int b) (:int c) ) ) 

;; how do we get rid of paranthesis ?

;; by specifying the macro expansion environments
'(@^ MyMacro) ;; results in |-
'(@$ MyMacro) ;; results in MyContC.( (:int a) (:int b) (:int c) )

;; notice the quote mark '

;; inside macro expansion environments the expansions would consume only the
;; outer parenthesis. So we can nest expansions

'(@^ (@$ MyMacro)) ;; results in MyContC for example


;; change its operator

;; change its name

;; change domain

;; change predicates

```

Constructs of the language:

flow statements: goto statements on steroids
context: holds typing information with respect to some evaluation context.
abstraction: corresponds to procedures.
record: corresponds more or less structs in C-like languages.
macros: basic metaprogramming facilities

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

We adopt mostly simply typed lambda calculus as our typing system. Most of our
notions follow: 
Barendregt, H.P., Dekkers, W. and Statman, R. (2013) Lambda calculus with
types. Cambridge ; New York: Cambridge University Press (Perspectives in
logic).


Abstraction
------------

Abstractions are the usual ways of dealing with functions and variables.
Though we try to have functions rather than procedures, this may not be the
case at all times due to the flow statements discarding values. There is no
concept of variable in the sense of c or c++. Here is an example:

```clojure
(:int f1(int, int)) ;; declaration of abstraction
(:= f1(x, y).(/ (* x 2) (+ y 3))) ;; binding of the abstraction
```
Notice that the abstractions have two components:

- Declaration
- Binding

Declaration contains type information. Binding contains behavior of the
abstraction.
Read the above piece of code as the abstraction f1 binds subject
`a` and subject `b` whose predicates are `int` to a predicate int.

We can use other abstractions in both components

Using abstraction in a declaration means including abstraction's typing
judgement for example:

```clojure
(:int f2( (:int float, float), float, float) )
(:= f2(fn, a, b).(fn a b))
```
Here the `fn` is the abstraction which substitutes two floating
points with an integer. Notice the "," between arguments. The "," is
reserved for homogeneous separation, that is, it is reserved for separating
components that have more or less same meaning given a certain context. We
shall see other examples.

Here is a variable like abstraction:
```clojure
(:int a)
(:= a.(4))
```

Notice that there is no difference between an abstraction that binds some
arguments to an expression and this one which binds no arguments to a literal.

Abstractions are the building blocks of LambdaScript. They can either be free
or bounded. Bounded abstractions can appear inside records and typing contexts.

Record
-------

Record is essentially a struct. Meaning that it has some heterogeneous
fields with names. They can be declared with default values. They can be
declared in full at one go or in parts. Values can be bind to record fields.

Here is full declaration example with default values:

```clojure

(:# MyRecord.(
    (:int a.(4)),
    (:float b).(3.7),
    (:float(4) as.(5.7, 5.3, 81.0, -2.5)),
    (:int(4) bs.(5, 9, 70, 2)),
    (:str c.("string")),
    (:str(3) cs.("string" "m string" "n string"))
    )
)
```

Notice that the members of the struct are in fact bounded abstractions. They
work as procedures that output their associated value given the record.  They
work like the record syntax of Haskell. So in order to access the value `4`
for example, one should do: `(a MyRecord)` which would yield 4.

Partial declaration is also quite simple. Here is an example with default
values:

```clojure

;; suppose we want to have something like this at the end
(:# MyOtherRecord.(
    (:int a.(4)),
    (:float b.(3.7)),
    (:float(4) as.(5.7, 5.3, 81.0, -2.5)),
    (:int(4) bs.(5, 9, 70, 2)),
    (:str c.("string")),
    (:str(3) cs.("string" "m string" "n string"))
    )
)
;; we need to declare them as the following:
(:# MyOtherRecord(6) ;; a record with six slots for abstractions
)

(:int a) ;; some free abstraction bound to base context
(:= a.(4))

(:# MyOtherRecord.a)

(:# MyOtherRecord.(:float b.(3.7)))
(:# MyOtherRecord.(:float(4) as.(5.7, 5.3, 81.0, -2.5)))
(:# MyOtherRecord.(:int(4) bs.(5, 9, 70, 2)))
(:# MyOtherRecord.(:str c.("string")))
(:# MyOtherRecord.(:str(3) cs.("string", "m string", "n string")))
```

Notice that this is not a binding but a declaration so we use the record
declaration operator `:#` and not `:=`.

Partial declaration allows for attaching base context abstractions to multiple
records easily. It helps us to compose base context abstractions with records.
The example with `(:int a)` shows have an abstraction bound to a base
context is composed with `MyOtherRecord`.

Now let's see examples with binding. Here is a full declaration followed by
binding of abstractions to records:

```clojure
(:# MyRecA.(
    (:int a),
    (:int(4) bs),
    (:float b),
    (:float(4) as),
    (:str c),
    (:str(3) cs)
    (:int f(int, int))
    )
)

(:= a(MyRecA).(4), bs(MyRecA).(1,2,3,4), b(MyRecA).(4.2),
    as(MyRecA).(1.0,2.7,3.1,4.7), c(MyRecA).("my string"),
    cs(MyRecA).("my string", "is", "awesome"),
    f(MyRecA).((arg1: int, arg2: int).(+ (* arg1 arg1) arg2))
)
```

Notice that the abstraction `f` contains only bounded variables. Thus it is
closed. It is also possible to include free abstractions bounded to base
typing context, or the context in which the binding takes place.

We can also have default function space typed abstractions with default
values during the record declaration.

```clojure
(:# MyRecA.(
    (:int fn(arg: float, arg2: float).(floor (+ arg arg2)))
)
)
```
Here the bounded expression must use bounded variables. It must be a closed
expression, not containing any free variables.

Binding a record with a particular set of values to another abstraction is
done in the same way as in regular abstractions. Here is an example:

```clojure
(:# MyRecA.(
    (:int a),
    (:int(4) bs),
    (:float b),
    (:float(4) as),
    (:str c),
    (:str(3) cs)
    (:int f(int, int))
    )
)

(:MyRecA recAbs)

(:= recAbs.(:=
        a(MyRecA).(4), bs(MyRecA).(1,2,3,4), b(MyRecA).(4.2),
        as(MyRecA).(1.0,2.7,3.1,4.7), c(MyRecA).("my string"),
        cs(MyRecA).("my string", "is", "awesome"),
        f(MyRecA).((arg1, arg2).(+ (* arg1 arg1) arg2))
    )
)

(a recAbs) ;; would yield 4 even if the abstraction has a different default
           ;; value

```
In the case of records with fields who are themselves records. It would go
something like this:

```clojure
(:# MyRecA.(
    (:int f(int, int))
    (:float b)
    )
)

(:# MyRecB.(
    (:int g(int, int))
    (:float a)
    )
)
(:# MyRecC.(
    (:MyRecA mra)
    (:MyRecB mrb)
    )
)
(:MyRecA recA)

(:= recA.(:=
        f(MyRecA).((arg1, arg2).(+ (* arg1 arg1) arg2)),
        b(MyRecA).(4.3)
    )
)
(:MyRecB recB)

(:= recB.(:=
        g(MyRecB).((arg1, arg2).(+ (* arg1 arg1) arg2)),
        a(MyRecB).(4.3)
    )
)
(:MyRecC recC)

(:= recC.(:=
        mra(MyRecC).(recA),
        mrb(MyRecC).(recB)
    )
)

```

Contexts
--------

Typing contexts. These are used for specifying the typing basis of
abstractions. They can be used as type classes where typing judgements
provide prototypes for abstractions. They are the main extension mechanism for
adding new semantics to both free abstractions and records.

Here is an example of a context declaration:

```clojure

(|- Printable.(
    (:str print),
    (:str printNewLine),
    )
)

```
Notice that it is declared in fully in one go.

Typing context can be bound to abstractions and records.
Here is an example with an abstraction:

```clojure

(|- Printable.(
    (:str print),
    (:str printNewLine),
    )
)

(:int a)
(:= a.(4))
(:= Printable(a).(
    print.("a.(4)"),
    printNewLine.("a.(4)\n"),
    )
)

```

Here is an example with a slightly more complex context and an abstraction:

```clojure

(|- Interpolatable.(
    (:int(2) bounds),
    (:int interpolate(int, int))
    )
)

(:int a(int))
(:= a(arg).(* 2 arg))

(:= Rangeable(a).(
    bounds.(a (-4), a 4),
    interpolate(min, max).(+ min (* (a min) (- max min)))
    )
)

```
Notice that we specify the bounded abstraction once at the top of binding,
then we use its name inside the abstractions.

The same thing goes for records. For example:

```clojure

(|- Printable.(
    (:str print),
    (:str printNewLine)
    )
)

(:# MyRecA.(
    (:int f(int, int))
    (:float b)
    )
)

(:= Printable(MyRecA).(
        print.(+ (print (b MyRecA)) (print (f MyRecA))),
        printNewLine.(+ (print (b MyRecA)) (printNewLine (f MyRecA)))
    )
)

```
For record members which have function space types, if one wants to apply
context abstractions, one must do the following: 

- Declare the abstraction that is going to be a member of the record. 
- Bind the abstraction to the desired typing context
- Partially declare the record
- Bind the abstraction to the record


Flow Statements
----------------

Flows are bindings. They are essentially stitches that join computations.
There are 2 types of flow bindings:

- exclusive or bindings which are required to be sequential due to their
  dependence over the runtime output of the preceding function.

- and bindings which are not necessarily sequential and evaluated in parallel
  whenever possible. These functions do not depend the runtime output of the
  preceding function.

The transmission of values from one abstraction to another is done using
captures. If the value is used in the next abstraction, it is captured in the
next abstraction, if it is not used, it is not captured thus discarded. Here is
an example:

```clojure
(:int f1)
(:= f1.(4))

(:int f2)
(:= f2.(-1))

(:int f3)
(:= f3.(7))

;; exclusive-or flow binding
(|> f1.( 
        (4).(f2), ;; if f1 outputs 4 f2 is evaluated afterwards
        (_).(f3)  ;; otherwise f3 evaluated
    )
)
;; and flow binding
(&> f1.( 
        (f2), ;; evaluate both abstractions in parallel after f1 
        (f3)  ;;
    )
)
```

There are two manners for specifying flows:

- Implicit flow is when an abstraction calls another abstraction in its body
- Explicit is when an abstraction is tied to another one explicitly using a
  flow binding

The implicit flow is assumed to be an exclusive or flow, thus evaluated in a
sequential manner depending on the run time output of the preceding
abstraction.

The idea is that for values such as true, false, and other constants
required for deciding flow direction, the developer knows which branch goes
along with which value. If the developer does not know which a constant value
one can always recourse to wild card notation `_` for specifying the branch.

Flow bindings can introduce constants and other abstractions as arguments. For
example:

```clojure
(:int f1)
(:= f1.(4))

(:int f2(int, int))
(:= f2(arg1, arg2).(+ arg1 arg2))

(:int f3)
(:= f3.(7))

;; exclusive-or flow binding
(|> f1.( 
        (4).(f2(8, 1)), ;; if f1 outputs 4 f2 is evaluated afterwards
        (_).(f3)  ;; otherwise f3 evaluated
    )
)
;; and flow binding
(&> f1.( 
        (f2(4, 5)), ;; evaluate both abstractions in parallel after f1 
        (f3)  ;;
    )
)
```

In the case of abstractions, we can use the following:

```clojure
(:int f1)
(:= f1.(4))

(:int f2((:int (int, int)), int))
(:= f2(f, arg1, arg2).(+ (f arg1 arg1) arg2))

(:int f3)
(:= f3.(7))

(:int f4(int, int))
(:= f4(arg1, arg2).(+ arg1 arg2))

;; exclusive-or flow binding
(|> f1.( 
        (4).(f2(f4, 8, 1)), ;; if f1 outputs 4 f2 is evaluated afterwards
        (_).(f3)  ;; otherwise f3 evaluated
    )
)
;; and flow binding
(&> f1.( 
        (f2(f4, 4, 5)), ;; evaluate both abstractions in parallel after f1 
        (f3)  ;;
    )
)
```

Macros
-------

Macros are basic meta programming facilities of LambdaScript based on
substitution. Macro operators work at a token level, that is they can access
to each token that is validated by the lexer, except parenthesis. Here is an
example:

```clojure

;; let's declare a macro
@MyMacro(|- MyContC.( (:int a), (:int b), (:int c) ))

```
One can think of a macro as the following structure
`MyMacro = ["|-", ["MyContC", [[":", "int", "a"], [...], [...]]]]`

LambdaScript provides four operators for working with these lists:

- `@^`: let's you access to the head of the list.
- `@$`: let's you access to the last element of the list
- `@<`: let's you access to initial elements except the last element of the
  list
- `@>`: let's you access to the tail of the list.

Here are some usage examples:
```clojure

;; let's declare a macro
@MyMacro2(|- MyContC.( (:int a), (:int b), (:int c) ))

(@ MyMacro2) ;; results in (|- MyContC.( (:int a), (:int b), (:int c) ))
(@^ MyMacro2) ;; results in ( |- )
(@$ MyMacro2) ;; results in ( MyContC.( (:int a) (:int b) (:int c) ) ) 

```
Notice that both the result of `@^` and `@$` are syntactically incorrect. Thus
the compiler would generate an error message. How do we get rid of the
parenthesis ? By specifying the macro expansion environment.

```clojure

;; let's declare a macro
@MyMacro2(|- MyContC.( (:int a), (:int b), (:int c) ))

'(@^ MyMacro) ;; results in |-
'(@$ MyMacro) ;; results in MyContC.( (:int a) (:int b) (:int c) )

```
Notice the `'` mark at the beginning of the parenthesis. Inside the macro
expansion environment the expansion consumes only the outer parenthesis. So we
can safely nest expansions:

```clojure

;; let's declare a macro
@MyMacro2(|- MyContC.( (:int a), (:int b), (:int c) ))

'(@^ (@$ MyMacro2)) ;; results in MyContC for example

```

The nice thing is macro declarations can contain macro expansion environments
and function like macros that have constants as arguments are evaluated at the
compilation phase. For example let's say you want to declare records with
different number of multi valued types. You can do something like the
following:

```clojure

;; a function like macro
@MyInc((x: int).(+ x 1))

@MyRecAMac(:# MyRecA.( (:int'(@ MyInc(3)) a)))

@MyRecBMac(:# MyRecB.( (:int'(@ MyInc( '(@$ ( @> (@^ (@$ MyRecAMac)))) )) a)))

;; the expression (@$ ( @> (@^ (@$ MyRecAMac)))) results in 4

```

Function like macros can be very powerful as they permit to abstract behavior.
We could have also used an intermediary function like macro to make the above
expression more readable.


```clojure

;; a function like macro
@MyInc(:int (x: int).(+ x 1))

@MyRecAMac(:# MyRecA.( (:int'(@ MyInc(3)) a)))

@IncFromRecA('(@ MyInc( '(@$ ( @> (@^ (@$ MyRecAMac)))) )))

@MyRecBMac(:# MyRecB.( (:int(@ IncFromRecA) a)))

@IncFromRecB('(@ MyInc( '(@$ ( @> (@^ (@$ MyRecBMac)))) )))

@MyRecCMac(:# MyRec'(@ IncFromRecA).( (:int(@ IncFromRecB) a))) ;; results in 5

```

The main condition for expansion of macros is that they need to be previously
declared. Shadowing of free abstractions by macro expansion is a compile time
error.


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
