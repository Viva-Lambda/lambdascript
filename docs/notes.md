# Design Notes

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
    (:int x),
    (:float y),
    (:float(2) xs), ;; equals(float float)
    (:int(6) ys), ;; equals (int int int int int int)
    (:str z),
    (:str(3) zs), ;; equals (str str str)
    (:bool f(arg1: bool arg2: bool))
    )
)

;; once you declared a context, you can extend it by binding other contexts
;; which do not contain duplicates using bind statements

;; Let's define some contexts
(|- MyContextA.( (:int a), (:float b) ))
(|- MyContextB.( (:int c), (:float d) ))
(|- MyContextC.( (:int e), (:float f) ))
(|- MyContextD.( (:float a), (:float g) ))

;; the following is valid
(:= MyContextC.(MyContextA MyContextB))

;; the following is invalid
(:= MyContextD.(MyContextA MyContextB)) ;; since "a" is duplicate

;; record declaration. Records hold heterogeneous data bind to names

(:& MyRecord.(
    (:int a.(4)),
    (:float b.(3.7)),
    (:float(4) as.(5.7, 5.3, 81.0, -2.5))
    (:int(4) bs.(5, 9, 70, 2))
    (:str c.("string"))
    (:str(3) cs.("string", "m string", "n string"))
    ) 
)

;; this creates accessor functions automatically so we have
(a MyRecord) ;; gives 4
(as MyRecord) ;; gives (5.7, 5.3, 81.0, -2.5)

;; we can also declare records partially
(:& MyOtherRecord(6) ;; we have six slots for binding abstractions
)

;; binding abstractions to partially declared records is very easy
;; suppose we want to have something like the following at the end
(:& MyOtherRecord.(
    (:int a.(4)),
    (:float b.(3.7)),
    (:float(4) as.(5.7, 5.3, 81.0, -2.5)),
    (:int(4) bs.(5, 9, 70, 2)),
    (:str c.("string")),
    (:str(3) cs.("string", "m string", "n string"))
    )
)
;; we need to declare them as the following:
(:& MyOtherRecord(6)
)

(:int a.(4))
(:& MyOtherRecord.a)

(:& MyOtherRecord.(:float b.(3.7)))
(:& MyOtherRecord.(:float(4) as.(5.7,5.3,81.0, -2.5)))
(:& MyOtherRecord.(:int(4) bs.(5,9,70,2)))
(:& MyOtherRecord.(:str c.("string")))
(:& MyOtherRecord.(:str(4) cs.("string", "m string", "n string")))


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

;; if the record is previously not declared, trying to produce it would result
;; in compile time error

;; now let's see flow binding

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

```

Constructs of the language:

module: basic reusable blocks of lambda script. Module name must be specified
at the beginning of program. Along with exported functions.

flow statements: goto statements on steroids related operator `>`

context: holds typing information with respect to some evaluation context.
related operator: `|`

abstraction: corresponds to procedures. Related operator `:`

record: corresponds more or less structs in C-like languages.
array: corresponds to more or less arrays in C-like languages, monolithic
blocks of homogeneous data

macros: basic metaprogramming facilities


Operators
---------

Operators are the fundamental signals that is used by the compiler to parse
the source code.
Each construct of the language has an operator associated to it.

Abstractions use `:`:
For abstractions it is `:`. Once we see the column, we can assume that there
is something related to types and thus something related to abstractions over
those types. For example in `(:int f(int, float))` the column directly
signals us that this is an abstraction, `int` later on shows us that this is
not a record abstraction but a function space typed abstraction whose output
is an integer.
Whereas in `(:& MyRecA.((:int a.(4)), (:float b).(3.7) ))` the column shows us
again that this is an abstraction, `&` later on shows us that this is record
abstraction which has accessory abstractions that are obliged to use MyRecA
typed value as input for outputting their expressions. It is a product type.
One can read it as `MyRecA` is both `int` type **and** `float` type.

Context use `|`:

Macros use `@`:

Flow statements use `>`:

Module statements use `!`

Modules
-------

Modules are the building blocks of reusable code in LambdaScript. Their main
purpose is to show reachable abstractions of a given program. One can export
abstractions, records, and contexts.

Following module declarations assume the following file structure:

- src/
  - MyLambda
    - Module1
      - MyFileName.lambda
    - Module2
      - MySomeOtherFile.lambda

- config.json:
Contains current package name, author name, location of third party packages,
keyword mappings etc.


The module declaration happens as follows:

```clojure
! MyFileName (f, MyRecA, g)

(:int f(int, int))
(:= f(x, y).(/ (* x 2) (+ y 3)))

(:# MyRecA.(
    (:int a.(4)),
    (:float b).(3.7),
    (:float(4) as.(5.7, 5.3, 81.0, -2.5)),
    (:int(4) bs.(5, 9, 70, 2)),
    (:str c.("string")),
    (:str(3) cs.("string" "m string" "n string"))
    )
)

(:int g(int, int))
(:= g(x, y).(/ (* x 2) (+ y 3)))

```

If no abstraction is specified we assume that all abstractions are exported.

Importing abstractions from another module is done the following way:

```clojure
! MySomeOtherFile (h, MyRecB, m) ;; this file
!: /MyLambda/Module1/MyFileName (f, MyRecA) ;; import from local folder
!: SomePackage/ModuleX/Funcs ;; import from some third party package

(:int h(int, int))
(:= h(x, y).(f x y))

(:# MyRecB.(
    (:int a.(4)),
    (:float b).(3.7),
    (:float(4) as.(5.7, 5.3, 81.0, -2.5)),
    (:int(4) bs.(5, 9, 70, 2)),
    (:str c.("string")),
    (:str(3) cs.("string" "m string" "n string"))
    )
)

(:int m(int, int))
(:= m(x, y).(/ (* x 2) (+ y 3)))

```
If we don't specify anything after the imported module, we import whatever the
module is exporting.

One can use qualified imports for avoiding name clashes:

```clojure
! MySomeOtherFile (h, MyRecB, m) ;; this file
!: /MyLambda/Module1/MyFileName (!MyN(f, MyRecA)) ;; qualified import from local folder
!: SomePackage/ModuleX/Funcs (!MyOtherN) ;; qualified import from package

(:int h(int, int))
(:= h(x, y).(!MyN:f x y))

(:# MyRecB.(
    (:int a.(4)),
    (:float b).(3.7),
    (:float(4) as.(5.7, 5.3, 81.0, -2.5)),
    (:int(4) bs.(5, 9, 70, 2)),
    (:str c.("string")),
    (:str(3) cs.("string" "m string" "n string"))
    )
)

(:int m(int, int))
(:= m(x, y).(/ (* x 2) (+ y 3)))

```

Notice that we are not specifying any names after the third import statements
name declaration. This means that all functions exported by Funcs module needs
to use MyOtherN prefix in MySomeOtherFile module.


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
(:int f2( (:int (float, float)), float, float) )
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

Record is essentially a struct. Meaning that it has some heterogeneous fields
with names. Once the declaration took place values can be bind to record
fields through abstractions.

### Record Declaration

Here is a declaration example:

```clojure

;; a record with seven slots for abstractions

(:& MyRecord.(
        (:int a), ;; integer variable like abstraction
        (:float b), ;; float variable like abstraction
        (:float(4) as), ;; float array like abstraction
        (:int(4) bs), ;; int array like abstraction
        (:str c), ;; string abstraction
        (:str(3) cs), ;; string array like abstraction
        (:int f(int, int)) ;; function like abstraction
    )
)

```

Notice that the members of the record are in fact bounded abstractions. They
work as procedures that output their associated value given the record. They
work like the record syntax of Haskell. So in order to access the value `4`
for example, one should do: `(a MyRecord)` which would yield 4.

Notice that this is not a binding but a declaration so we use the record
declaration operator `:&` and not `:=`.

### Record Binding

Now let's see examples with binding. Binding a record with a particular set of
values to another abstraction is done in the same way as in regular
abstractions. Here is an example:

```clojure

;; this is our record
(:& MyRecA.(
    (:int a),
    (:int(4) bs),
    (:float b),
    (:float(4) as),
    (:str c),
    (:str(3) cs)
    (:int f(int, int))
    )
)

(:MyRecA recAbs) ;; recAbs is an abstraction like (:int f3)

(:int f1(int, int)) ;; declaration of abstraction

;; bind in one go
(:= a(recAbs).(4), bs(recAbs).(1,2,3,4), b(recAbs).(4.2),
    as(recAbs).(1.0,2.7,3.1,4.7), c(recAbs).("my string"),
    cs(recAbs).("my string", "is", "awesome"),
    f(recAbs).(f1)
)

(a recAbs) ;; would yield 4 even if the abstraction has a different default
           ;; value

(:= f1(x, y).(/ (* x 2) (+ y 3))) ;; binding of the abstraction

```

In the case of records with fields who are themselves records. It would go
something like this:

```clojure
(:& MyRecA.(
    (:int f(int, int))
    (:float b)
    )
)

(:& MyRecB.(
    (:int g(int, int))
    (:float a)
    )
)
(:& MyRecC.(
    (:MyRecA mra)
    (:MyRecB mrb)
    )
)
(:MyRecA recA)

(:= f(recA).((arg1, arg2).(+ (* arg1 arg1) arg2)),
    b(recA).(4.3)
)
(:MyRecB recB)

(:= g(recB).((arg1, arg2).(+ (* arg1 arg1) arg2)),
    a(recB).(4.3)
)
(:MyRecC recC)

(:= mra(recC).(recA),
    mrb(recC).(recB)
)

```

Notice that both in declaration and in binding, everything is done in one go.
It is trivial to implement a partial declaration system, or to implement
separated declarations however there is the risk of confusing it with an
override mechanism, which is simply not the case for both the declarations and
bindings. Separating bindings might introduce a decision problem about which
version is the intended version.  We can also separate declaration and
bindings and generate a compile error if two bindings and/or declarations
occur for the same abstraction in the record.


Contexts
--------

Typing contexts. These are used for specifying the typing basis of
abstractions. They can be used as type classes where typing judgements provide
prototypes for abstractions. They are the main extension mechanism for adding
new semantics to both free abstractions and records.

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

(:= Interpolatable(a).(
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
(>|| f1.( 
        (4).(f2), ;; if f1 outputs 4 f2 is evaluated afterwards
        (_).(f3)  ;; otherwise f3 evaluated
    )
)
;; and flow binding
(>&& f1.( 
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
(>|| f1.( 
        (4).(f2(8, 1)), ;; if f1 outputs 4 f2 is evaluated afterwards
        (_).(f3)  ;; otherwise f3 evaluated
    )
)
;; and flow binding
(>&& f1.( 
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
(>|| f1.( 
        (4).(f2(f4, 8, 1)), ;; if f1 outputs 4 f2 is evaluated afterwards
        (_).(f3)  ;; otherwise f3 evaluated
    )
)
;; and flow binding
(>&& f1.( 
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

@MyRecAMac(:# MyRecA.( (:int'((@ MyInc(3))) a)))

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


The new grammar in bnf like form:

first very basic tokens with single characters that help to organize the code
blocks. Macros are essentially ways to work with these. Parenthesis delimit
the block, separators split the contents of the block.

Separators:
```
leftpar := <unique-utf8-char> | (
rightpar := <unique-utf8-char> | )
list separator := <unique-utf8-char> | ,
bind separator := <unique-utf8-char> | .
type separator := <unique-utf8-char> | :
b-s := <bind separator>
rpar := <rightpar>
lpar := <leftpar>
l-s := <list separator>
t-s := <type separator>
```

module related constructs (DONE):
```
module statement := <module declaration> <module import>*
module declaration :=  <module declaration and export> | <module name declaration>
module declaration and export := <module operator> <module name> <export statement>
module name declaration := <module operator> <module name>
module operator := <unique-utf8-string> | !

export statement := <lpar> <exported-name> <rpar> | <export list>
export list := <export list start> <exported content>* <export list end>
export list start := <lpar>
exported content := <exported-name> <l-s>
exported list end := <exported-name> <rpar>

module import := <module import specific> | <module import general>
module import general := <module import operator> <module import path>
module import specific := <module import general> <import statement>
module import operator := <module operator> <type separator>
import statement := <import list> | <qualified import list> | <qualification statement>

import list := <import list start> <import list content>* <import list end>
import list start := <lpar>
import list content := <imported-name> <l-s>
import list end := <imported-name> <rpar>

qualified import list := <lpar> <module operator> <imported name> <import list> <rpar>
qualification statement := <lpar> <module operator> <imported name> <rpar>

module import path := <local import path> | <package import path>
local import path := <path separator> <path content>
package import path := <path content>
path content := <path content start>* <path content end>
path content start := <path name><path separator>
path content end := <path name>
path separator := <unique-utf8-char> | /
```

abstraction related syntax: TODO specify imported abstractions syntax,
qualified etc; abstraction body needs to be reworked.
```
abstraction declaration := <lpar> <abstraction type> <abstraction name> <arg list> <rpar>
abstraction type := <type operator> <type name>
type operator := <type separator>
t-o = <type operator>
arg list := <empty arg list> | <arg list start> <arg list content>* <arg list end>
arg list start := <lpar>
arg list content := <type indicator> <l-s>
arg list end := <type indicator> <rpar>

type indicator := <simple type indicator> 
                | <function space type indicator>

function space type indicator := <lpar> <abstraction type> <arg list> <rpar>
simple type indicator := <type name>
empty arg list := <null>

abs binding := <lpar> <type operator> <bind operator> <abstraction binding> <rpar>
abstraction binding := <abstraction argument> <b-s> <abstraction body>
abstraction argument := <abstraction name> <argument name list> | <empty arg list>
argument name list := <arg-n-start> <arg-n-content>* <arg-n-end>
arg-n-start := <lpar>
arg-n-content := <argument name> <l-s>
arg-n-end := <argument name> <rpar>

abstraction body := <application>+

```

Record related syntax: TODO rework the bounded abstractions

```
record declaration := <full record declaration> | <partial record declaration>

full record declaration := <full record start> <record name> <b-s> <record bind list> <rpar>
full record start := <type operator> <record signal>
record signal := <unique-utf8-char> | &
record bind list := <record list start> <record list content>* <record list end>
record list start := <lpar>
record list content := <bounded abstraction declaration> <l-s>
record list end := <bounded abstraction declaration> <rpar>
bounded abstraction declaration := <bounded declaration with default value> 
                                 | <bounded declaration>

bounded declaration with default value := <bounded named literal> 
                                        | <bounded abstraction value>

bounded named literal := <bounded array literal> 
                       | <bounded simple literal>
                       | <bounded record literal>

bounded array literal := <babstraction literal start> <array values> <rpar>
babstraction literal start := <lpar> <array type specifier> <array name> <b-s>
array type specifier := <type operator> <type name> <lpar> <number>+ <rpar>
array values := <array value start> <array value>* <array value end>
array value start := <lpar>
array value := <literal> <l-s>
array value end := <literal> <l-s> <literal> <rpar>

bounded simple literal := <type operator> <type name> <literal name> <b-s>
```

The rest: TODO rework the expression, half of that thing is unrelated to
application.

```
program := <module statement>+ <expression>+

expression := <declaration> | <binding> | <statement> | <application>
declaration := <abstraction declaration> | <record declaration>
             | <context declaration> | <macro declaration>
```
