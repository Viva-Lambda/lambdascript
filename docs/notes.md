# Design Notes

## Constructs of the language

module: basic reusable blocks of lambda script. Module name must be specified
at the beginning of program. Along with exported functions.

flow statements: goto statements on steroids related operator `>`

concept: holds typing information with respect to some evaluation context.
related operator: `:|`

abstraction: corresponds to procedures. Related operator `:$`

record: corresponds more or less structs in C-like languages. Related operator
`:&`

array: corresponds to more or less arrays in C-like languages, monolithic
blocks of homogeneous data, a syntactic sugar for records, see notes in
abstractions.

macros: basic metaprogramming facilities


### Operators

Operators are the fundamental signals that is used by the compiler to parse
the source code.
Each construct of the language has an operator associated to it.

Abstractions use `$`:

Records use `&`

Concepts use `|`:

Macros use `@`:

Flow statements use `>`:

Module statements use `!`

### Modules

Modules are the building blocks of reusable code in LambdaScript. Their main
purpose is to show reachable abstractions of a given program. One can export
abstractions, records, and concepts.

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

(:$ f(int, int) int)
(=: f(x, y) (/ (* x 2) (+ y 3)))

(:& MyRecB.(
    (:int a),
    (:float b),
    (:float(4) as),
    (:int(4) bs),
    (:str c),
    (:str(3) cs)
    )
)
(:$ g(int, int) int)
(=: g(x, y) (/ (* x 2) (+ y 3)))

```

If no abstraction is specified we assume that all abstractions are exported.

Importing abstractions from another module is done the following way:

```clojure
! MySomeOtherFile (h, MyRecB, m) ;; this file
!: /MyLambda/Module1/MyFileName (f, MyRecA) ;; import from local folder
!: SomePackage/ModuleX/Funcs ;; import from some third party package

(:$ h(int, int) int)
(=: h(x, y).(f x y))

(:& MyRecB.(
    (:int a),
    (:float b),
    (:float(4) as),
    (:int(4) bs),
    (:str c),
    (:str(3) cs)
    )
)

(:$ m(int, int) int)
(=: m(x, y).(/ (* x 2) (+ y 3)))

```
If we don't specify anything after the imported module, we import whatever the
module is exporting.

One can use qualified imports for avoiding name clashes:

```clojure
! MySomeOtherFile (h, MyRecB, m) ;; this file
!: /MyLambda/Module1/MyFileName (!MyN(f, MyRecA)) ;; qualified import from local folder
!: SomePackage/ModuleX/Funcs (!MyOtherN) ;; qualified import from package

(:$ h(int, int) int)
(=: h(x, y) (!MyN:f x y))

(:& MyRecB.(
    (:int a),
    (:float b),
    (:float(4) as),
    (:int(4) bs),
    (:str c),
    (:str(3) cs)
    )
)

(:$ m(int, int) int)
(=: m(x, y) (/ (* x 2) (+ y 3)))

```

Notice that we are not specifying any names after the third import statements
name declaration. This means that all functions exported by Funcs module needs
to use MyOtherN prefix in MySomeOtherFile module.


### Types

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


### Abstraction

Abstractions are the usual ways of dealing with functions and variables.
Though we try to have functions rather than procedures, this may not be the
case at all times due to the flow statements discarding values. There is no
concept of variable in the sense of c or c++. Here is an example:

```clojure
(:$ f1(int, int) int) ;; declaration of abstraction
(=: f1(x, y) (/ (* x 2) (+ y 3))) ;; binding of the abstraction
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
(:$ f2( (:int (float, float)), float, float) int)
(=: f2(fn, a, b) (fn a b))
```
Here the `fn` is the abstraction which substitutes two floating
points with an integer. Notice the "," between arguments. The "," is
reserved for homogeneous separation, that is, it is reserved for separating
components that have more or less same meaning given a certain context. We
shall see other examples.

Here is a variable like abstraction:
```clojure
(:$ a int)
(=: a (4))
```

Notice that there is no difference between an abstraction that binds some
arguments to an expression and this one which binds no arguments to a literal.

Here is an array like abstraction:

```clojure
(:$ a int(5))
(=: a (4, 4, 3, 7, 0))
```
One can access to the elements of an array like abstraction with regular
application expression:
```clojure
(:$ a int(5))
(=: a (4, 8, 3, 7, 0))
(1 a) ;; outputs 8
(0 a) ;; outputs 4
```
Array like abstractions are in fact records in disguise. You can think of an
array like abstraction as the following:
```clojure
(:& A (
        (:$ 0 int),
        (:$ 1 int),
        (:$ 2 int),
        (:$ 3 int),
        (:$ 4 int)
    )
)
(:$ a A)
(=: a ( 0(4), 1(8), 2(3), 3(7), 4(0)))
```

Abstractions are the building blocks of LambdaScript. They can either be free
or bounded. Bounded abstractions can appear inside records and typing
contexts.

### Record

Record is essentially a stateless struct. Meaning that it has some
heterogeneous fields with names. Once the declaration took place values can be
bind to record fields through abstractions.

#### Record Declaration

Here is a declaration example:

```clojure

;; a record with seven slots for abstractions

(:& MyRecord (
        (:$ a int), ;; integer variable like abstraction
        (:$ b float), ;; float variable like abstraction
        (:$ as float(4)), ;; float array like abstraction
        (:$ bs int(4)), ;; int array like abstraction
        (:$ c str), ;; string abstraction
        (:$ cs str(3)), ;; string array like abstraction
        (:$ f(int, int) int) ;; function like abstraction
    )
)

```

Notice that the members of the record are in fact bounded abstractions. They
work as procedures that output their associated value given the record. They
work like the record syntax of Haskell. So in order to access the value `4`
for example, one should do: `(a MyRecord)` which would yield 4.

Notice that this is not a binding but a declaration so we use the record
declaration operator `:&` and not `=:`.

#### Record Binding

Now let's see examples with binding. Binding a record with a particular set of
values to another abstraction is done in the same way as in regular
abstractions. Here is an example:

```clojure

;; this is our record
(:& MyRecA (
        (:$ a int),
        (:$ bs int(4)),
        (:$ b float),
        (:$ as float(4)),
        (:$ c str),
        (:$ cs str(3))
        (:$ f(int, int) int)
    )
)

(:$ recAbs MyRecA) ;; recAbs is an abstraction like (:int f3)

(:$ f1(int, int) int) ;; declaration of abstraction

;; bind in one go
(=: recAbs (
        a(4), bs(1,2,3,4), b(4.2),
        as(1.0, 2.7, 3.1, 4.7), c("my string"),
        cs("my string", "is", "awesome"),
        f(f1)
    )
)

(a recAbs) ;; would yield 4 even if the abstraction has a different default
           ;; value

(=: f1(x, y) (/ (* x 2) (+ y 3))) ;; binding of the abstraction

```

In the case of records with fields who are themselves records. It would go
something like this:

```clojure
(:& MyRecA (
    (:$ f(int, int) int),
    (:$ b float)
    )
)

(:& MyRecB (
    (:$ g(int, int) int),
    (:$ a float)
    )
)

(:& MyRecC (
    (:$ mra MyRecA),
    (:$ mrb MyRecB)
    )
)

(:$ recA MyRecA)

;;

(:$ f1(int, int) int)
(:$ f2(int, int) int)

(=: recA (
        f(f1), ;; lambda abstraction ??
        b(4.3)
    )
)
(:$ recB MyRecB)

(=: recB (
    g(f2),
    a(4.3)
    )
)
(:$ recC MyRecC)

(=: recC (
        mra(recA),
        mrb(recB)
    )
)

(=: f1(arg1, arg2) (+ (* arg1 arg1) arg2))
(=: f2(arg1, arg2) (* (+ arg1 arg1) arg2))
```
Notice that both in declaration and in binding, everything is done in one go.

### Concepts

Concepts describe what is doable with a type.

Here is an example for declaring a concept:

```clojure

;; declare a concept
(:| Printable(A) (
        (:$ toString(A) string),
        (:$ print string),
    )
)
```

The concept `Printable` takes a placeholder type name `A` as its argument,
signaling that what comes after it must be something that can be declared,
thus no flow statement or no concept. The idea is simple, you bind `toString`
method, you get `print` for free. If you want to bind `print` as well, you are
free to do so.

Here is a usage example:

```
(:| Printable(A) (
    (:$ toString(A) string),
    (:$ print string),
    )
)

(:& MyRecB (
        (:$ g(int, int) int),
        (:$ a float)
    )
)
(:$ myRecStr(MyRecB) string)

(=: Printable(MyRecB) (
        toString(myRecStr)
       ;; ,print(mrec).("Print my rec b") optional
    )
)

(=: myRecStr(myrec) (
                concat "MyRecB" (
                    concat (toString (g myrec)) (toString (a myrec)) 
                )
            )
)
```

Concepts can take constraints. For example:

```clojure

(:| Divisible(A(Number, Collection)) (
        (:$ divide(A) A)
    )
)

```

This indicates that whatever type `A` is, it must be bound to concepts
`Number` and `Collection`. Hence they indicate a constraint on the concept.


### Flow Statements

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
(:$ f1)
(=: f1 (4))

(:$ f2 int)
(=: f2 (-1))

(:$ f3 int)
(=: f3 (7))

;; exclusive-or flow binding
(>|| f1 ( 
        (=: 4 (f2)), ;; if f1 outputs 4 f2 is evaluated afterwards
        (=: _ (f3))  ;; otherwise f3 evaluated
    )
)
;; and flow binding
(>&& f1 ( 
        f2, ;; evaluate both abstractions in parallel after f1 
        f3  ;;
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
(:$ f1 int)
(=: f1 (4))

(:$ f2(int, int) int)
(=: f2(arg1, arg2) (+ arg1 arg2))

(:$ f3 int)
(=: f3 (7))

;; exclusive-or flow binding
(>|| f1 (
        (=: 4 (f2(8, 1))), ;; if f1 outputs 4 f2 is evaluated afterwards
        (=: _ (f3))  ;; otherwise f3 evaluated
    )
)
;; and flow binding
(>&& f1 (
        f2(4, 5), ;; evaluate both abstractions in parallel after f1 
        f3 ;;
    )
)
```

In the case of abstractions, we can use the following:

```clojure
(:$ f1 int)
(=: f1 (4))

(:$ f2((:int (int, int)), int) int)
(=: f2(f, arg1, arg2) (+ (f arg1 arg1) arg2))

(:$ f3 int)
(=: f3 (7))

(:$ f4(int, int) int)
(=: f4(arg1, arg2) (+ arg1 arg2))

;; exclusive-or flow binding
(>|| f1 (
        (=: 4 (f2(f4, 8, 1))), ;; if f1 outputs 4 f2 is evaluated afterwards
        (=: _ (f3))  ;; otherwise f3 evaluated
    )
)
;; and flow binding
(>&& f1 (
        f2(f4, 4, 5), ;; evaluate both abstractions in parallel after f1 
        f3  ;;
    )
)
```

### Macros

Macros are basic meta programming facilities of LambdaScript based on
substitution. Basically there are two macro types:

- Parametrized macros
- Basic macros

Here is an example for basic macros:

```clojure
;; let's declare a variable like macro
(:@ MyMacro 8)

;; let's declare an abstraction like macro
(:@ MyAbstraction (:$ f(int, float) float))

``` 
Here `:@` indicates that this is a macro declaration. `MyMacro` is the
name of the macro. `8` is the macro body which is considered as a single
element list.  Later on we shall see that we can work with these lists.

Here is parametric macro declaration:

```clojure

;; we can also define function like macros
(:@ MyFnLikeMacro(A, B) (+ A B))

```
This macro takes two parameters `A` and `B`. The macro body is `(+ A B)` is a
list with 3 members.

The macros are used like this:

```clojure
;; let's declare a variable like macro
'(MyMacro) ;; expands into 8

;; let's declare an abstraction like macro
'(MyAbstraction) ;; expands (:$ f(int, float) float)

;; we can also define function like macros
'(MyFnLikeMacro (arg1, arg3)) ;; expands into (+ arg1 arg3)
```

Now more advanced macros concern transformers.

LambdaScript provides four access operators for working with macro body:

- `@^`: let's you access to the head of the list.
- `@$`: let's you access to the last element of the list
- `@<`: let's you access to initial elements except the last element of the
  list
- `@>`: let's you access to the tail of the list.

See their usage:

```clojure
(:@ MyMacro4(A, B, fn) (fn ((@^ A) (@$ B))) )

(:$ f1(int) float)
(=: f1(a) (toFloat a))
(:$ nbsf int(4))
(:$ nbs float(4))
(=: nbs (0.1, 0.2, 0.3, 0.4))
(=: nbsf (1,2,3,4))

'(MyMacro4 ((0 nbsf), (1 nbs), f1))

;; expands into (f1 (0 nbs)) which is then evaluated as
;; toFloat 1

```
Notice that we only put a single `'` for signaling the macro expansion
environment.

LambdaScript provides a single merge operator for macro bodys:

- `@+`: let's you merge two macro lists

See its usage:

```
(:@ MyMacro1(A, B) (@+ (@< A) (@$ B)))

'(MyMacro1 (
        (:$ f int(5)),
        (:$ f2 float(6))
    ) 
) ;; expands into (:$ f float(6))

```

## Grammar

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
