module Expression.Expression where

import Lexer.Lexer
import Expression.Identifier
import Expression.Literal

{-
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

{-
struct Expr{
    ConstructorType t;
    Expr(GetExpr){
        t = GetExpr;
        }
    Expr(StmtExpr){
        t = StmtExpr;
        }
    Expr(EndExpr){}
};
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

