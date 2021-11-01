module Expression.Expression where

import Lexer.Lexer
import Expression.Identifier
import Expression.Literal

{-
The new grammar of the language:

Constructs of the language:
flow
context
function

function takes a context as input, and outputs a context and a signal.
flow takes a function name and a signal as input, outputs a context
and a function name. The resulting function name and context is executed after
the function in the input if it outputs the associated signal.

Context is essentially a named map of types. 
Its keys can be either strings or integers and they hold values. Each pair is
associated to a type.

The implicit flow and the explicit flow:
The implicit flow is when a procedure calls another procedure in its body.
The explicit flow is when a procedure is tied to another procedure explicitly
using a flow statement.
The implicit declaration of explicit flow is when a procedure declares what is
to be follow or precede it in its body.

Couple of restrictions apply. implicit flows can not specify external
contexts. They are either empty context functions produced by some combinators
or functions that have the same context. The restriction does not apply to
implicit declaration of explicit flows.

An abstraction binds an input to an expression, 
L x.x**2 + 3 means f(x) = x**2 + 3
in our language this would be something like (f (x).(+ (** x 2) 3))

Notice the duo (abstractionName (VariableName).(Expression)).
This is the basis of our syntax for defining procedures.

Defining procedures for some typing context is more or less the same:
(ContextName abstractionName(VariableNameInContext).(Expression))


Defining flow statements is all about binding procedures to other procedures
with or without specifying the context. If the context is not specified we
assume that the binding argument inherits context from the binded procedure.

(:= procName(
        (signal).(proc1: Context) (signal).(proc2) (signal).(proc3: Context2)
    )
)

Here the proc2 inherits the context of procName.

The contexts actualize as states during the evaluation of programs.

Passing of value between procedures are done by & operator.
Let's suppose the following computation:
(f (x).(* x 2))

Suppose that this computation is bound to a context ex. (|- MyCon(x).f)

Let's suppose another computation:
(g (y).(* (+ y $) 3)) which is bound to another context ex. (|- MyOtherCon(y).g)

Let's suppose a flow such as:
(:= MyCon(f).(
        (2).(g: MyOtherCon) (4).(proc2) (_).(proc3)
    )
)
This is saying that if the procedure "f" outputs 2 as value go to "g" if it
outputs 4 go to "proc2", otherwise go to "proc3".

If "f" is followed by "g" the "$" would yield as "2" in the evaluation of g.

Let's suppose another case with 3 procedures:
(MyCon f(x).(* x 2))
(MyOtherCon g(y).(* y 3))
(MySomeCon k(z).(* z ((+ (& f) (& g)))))

Suppose a flow such as
(:= MyCon(f).(
        (2).(z: MySomeCon) (4).(proc2) (_).(proc3)
    )
)
(:= MyOtherCon(g).(
        (6).(z: MySomeCon) (4).(proc2) (_).(proc3)
    )
)

($ f) fetches the value computed in the procedure "f", and (& g) fetches the
value computed in the procedure "g" preceding the procedure "k".

Basically our grammar is something like the following:

program := <expression> <expression>*
expression := <get>
            | <abstraction-declaration> 

get := <constant>
     | <varname>
     | <application>

constant := <bool> | <number> | <string>
bool := true | false
number := <int> | <real>
int := 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
real := <int>+.<int>+
string := " any unicode character that does not break quotes "

varname := <alphabetic>+<numeric>* | alphabetic>+_*<numeric>*_*
alphabetic := [a-zA-Z]
numeric := <int>

abstraction-declaration := <pure-abstraction> 
                         | <context-declaration> 
                         | <bind-declaration> 

pure-abstraction := (<abstraction-name> (<identifier>*)<substitute-marker>(<expression>+))
abstraction-name := <varname>
identifier := <varname><in-marker> <typename>
in-marker := :
typename := <varname>
substitute-marker := <s-m>
s-m := .

bind-declaration := <context-bind> | <flow-bind>

context-bind := (:= <context-name>(<identifier-in-context>+)<s-m><abstraction-name>)
context-name := <varname>
identifier-in-context := <identifier>

flow-bind := (:= <context-name>(<abstraction-name>)<s-m>(<signaled-expression>+) )
signaled-expression := <explicit-context-declaration> 
                     | <implicit-context-declaration>

explicit-context-declaration := (<signal>)<s-m>(<abstraction-name> <context-name>)
implicit-context-declaration := (<signal>)<s-m>(<abstraction-name>)
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


Compiling occurs in several stages:
- Lexing
- Symbolic Tree building where we build the AST
- Symbolic Tree expansion, where we inject parts of AST to different places
  and maybe compute some compile time values.
- Parse Abstract Binding tree.
- Parse context definitions: Parse the resulting AST for definitions
- Parse procedure definitions: Parse the resulting AST for function definitions
- Parse flow statements: Parse the resulting ast for flow statements.
- Check if the resulting path from flow statements has a hole in it somewhere.
- Parse procedure applications:
- Type checking according to simply typed lambda calculus.


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

-- typed sequence for collections ? something like (seq identifier exprs*)

-}


data Expr = GExpr GetExpr
            | StmtExpr Statement
            | EndExpr


instance Show Expr where
    show (GExpr v) = show v
    show (StmtExpr a ) = show a
    show EndExpr = ""


instance Eq Expr where
    (GExpr a) == (GExpr b) = a == b
    (GExpr _) == _ = False
    (StmtExpr a ) == (StmtExpr b ) = a == b
    (StmtExpr _ ) == _ = False
    EndExpr == EndExpr = True
    EndExpr == _ = False

data GetExpr = GetVName VarName
               | GetLit Literal

instance Show GetExpr where
    show (GetVName v) = show v
    show (GetLit v) = show v

instance Eq GetExpr where
    (GetVName a) == (GetVName b) = a == b
    (GetVName _) == _ = False
    (GetLit a) == (GetLit b) = a == b
    (GetLit _) == _ = False

data ProcedureCall = Proc {op :: Operator, args :: Operand}

instance Show ProcedureCall where
    show (Proc o s) = "( " ++ show o ++ " (" ++ show s ++ ") )"

instance Eq ProcedureCall where
    (Proc o _) == (Proc p _) = o == p

data Operator = OpName VarName

instance Show Operator where
    show (OpName c) = show c

instance Eq Operator where
    (OpName c) == (OpName f) = c == f

data Operand = OprExpr [TypedExpr]

instance Show Operand where
    show (OprExpr exps) = "( " ++ (unwords $ map show exps) ++ " )"

instance Eq Operand where
    (OprExpr a) == (OprExpr b) = a == b

data TypedExpr = TypedLit Literal
               | TypedId IdentifierName
               | TypedCall ProcedureCall

instance Eq TypedExpr where
    (TypedLit a) == (TypedLit b) = a == b
    (TypedLit _) == _ = False
    (TypedId a) == (TypedId b) = a == b
    (TypedId _) == _ = False
    (TypedCall a) == (TypedCall b) = a == b
    (TypedCall _) == _ = False

instance Show TypedExpr where
    show (TypedLit a) = show a
    show (TypedId a) = show a
    show (TypedCall a) = show a

data Sequence = SeqExpr {parent :: Expr, child :: Expr}

instance Show Sequence where
    show SeqExpr {parent=e, child=f} = show e ++ " " ++ show f

instance Eq Sequence where
    SeqExpr {parent=e, child=f} == SeqExpr {parent=b, child=a} = 
        (e == b) && (f == a)

data Statement = AssignStmt Assign -- eval written
                | CondStmt Conditional -- eval written
                | ProcDefStmt ProcedureDefinition -- eval written
                | CallStmt ProcedureCall
                | LoopStmt Loop -- eval written
                | SeqStmt Sequence -- eval written

instance Show Statement where
    show (AssignStmt a) = show a
    show (CondStmt a) = show a
    show (LoopStmt a) = show a
    show (ProcDefStmt a) = show a
    show (SeqStmt a) = show a
    show (CallStmt a ) = show a


instance Eq Statement where
    (AssignStmt a) == (AssignStmt b) = a == b
    (AssignStmt _) == _ = False
    (CondStmt a) == (CondStmt b) = a == b
    (CondStmt _) == _ = False
    (LoopStmt a) == (LoopStmt b) = a == b
    (LoopStmt _) == _ = False
    (ProcDefStmt a) == (ProcDefStmt b) = a == b
    (ProcDefStmt _) == _ = False
    (SeqStmt a) == (SeqStmt b) = a == b
    (SeqStmt _) == _ = False
    (CallStmt a ) == (CallStmt b ) = a == b
    (CallStmt _ ) == _ = False


data Conditional = Cond {ctest :: ConditionTest,
                         consequent :: Sequence,
                         alternate :: Sequence}

instance Show Conditional where
    show Cond {ctest=a, consequent=b, alternate=c} =
        let msg = "Condition Test " ++ show a ++ " Consequent " ++ show b 
            msg2 = msg ++ " Alternate " ++ show c
        in msg2

instance Eq Conditional where
    (Cond {ctest=a, consequent=b, alternate=c}) == (Cond {ctest=d, consequent=e, alternate=f}) =
        let c1 = a == d
            c2 = b == e
            c3 = c == f
        in (c1 && (c2 && c3))

data ConditionTest = CTestLit Literal
                    | CTestProc ProcedureCall
                    | CTestVar VarName

instance Show ConditionTest where
    show (CTestLit li) = show li
    show (CTestProc li) = show li
    show (CTestVar li) = show li

instance Eq ConditionTest where
    (CTestLit a) == (CTestLit b) = a == b
    (CTestLit _) == _ = False
    (CTestProc a) == (CTestProc b) = a == b
    (CTestProc _) == _ = False
    (CTestVar a) == (CTestVar b) = a == b
    (CTestVar _) == _ = False


data Loop = Looper {ltest :: ConditionTest,
                    lconsequent :: Sequence}

instance Show Loop where
    show Looper {ltest=a, lconsequent=b} = show a ++ " " ++ show b

instance Eq Loop where
    Looper {ltest=a, lconsequent=b} == Looper {ltest=c, lconsequent=d} = 
        let c1 = a == c
            c2 = b == d
        in c1 && c2

data ProcedureDefinition = DefineProc {
        procname :: Identifier,
        arguments :: [Identifier],
        body :: Sequence
        }
{-
(do + (1 2) )
(fn toplam: int
    (sayi: int sayi2: int) 
    (seq (do + (sayi 3) ) (do - (sayi1 sayi2) ) )
)
-}

instance Show ProcedureDefinition where
    show DefineProc {procname=a, arguments=b, body=c} = 
        show a ++ " " ++ show b ++ " " ++ show c


instance Eq ProcedureDefinition where
    DefineProc {procname=a, arguments=b, body=c} == DefineProc {procname=d, arguments=e, body=f} =
        let c1 = a == d
            c2 = b == e
            c3 = c == f
        in (c1 && c2) && c3

data Assign = Assigner Identifier TypedExpr
              -- TypeAssigner Identifier ProcedureCall ?

instance Show Assign where
    show (Assigner aid e) = show aid ++ " " ++ show e


instance Eq Assign where
    (Assigner i1 e) == (Assigner i2 f) = (i1 == i2) && (e == f)

-- reduceExpr (e:es) = foldr SeqExpr EndExpr (e:es)

