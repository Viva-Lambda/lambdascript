module Expression where

{-
Here is the grammar of the language

expression := <literal> | <identifier> | <procedure call>

statement := <conditional>
            | <assignment>
            | <procedure definition> 
            | <loop>
            | <sequence>


-- expressions
-- literals
literal := <boolean> | <number> | <string>

number := <digit>+ | <digit>+.<digit>+
boolean := true | false
string := "...any number of char"

identifier := <letter>+ <digit>*
letter := a | b | c | d | e | f | ... | A | B | ... | Z
digit := 0 | ... | 9

procedural call := (do/yap <operator> <operand>)
operator := opchar | <identifier>
opchar := + | - | * | / | % | < | > | & | \| | !
operand := (<expression>*)

-- (do + (1 3 9)); (do printMyVar (16))
-- -6.0 (do - (seq 6.0))


assignment := def/tanim <identifier> <expression>

conditional := (eger/if <test> <consequent> <alternate>)
test := (<literal>) | <procedural call> | (<identifier>)
consequent := (then/ise <sequence>)
alternate := (else/yoksa <sequence>)

loop := (loop/dongu <test> <consequent>)
-- (loop/dongu (do/yap < (1.6 6.0)) (then/ise fdsak,m))

procedure definition := (fn/edim <identifier> <arguments> <body>)
arguments := (<identifier>*)
body := <sequence>

sequence := ( seq/liste <expression>+ )

-}

type LineInfo = Int

data Identifier = IdExpr String LineInfo -- parsed - eval

instance Show Identifier where
    show (IdExpr a _) = a

instance Eq Identifier where
    (IdExpr a _) == (IdExpr b _) = a == b

debugIdentifier :: Identifier -> String
debugIdentifier (IdExpr a i) = "Identifier " ++ a ++ " at line " ++ show i

data Literal = BLit Bool
              | StrLit String
              | NumLit Double

instance Show Literal where
    show (BLit b) = if b
                    then "true"
                    else "false"
    show (StrLit s) = s
    show (NumLit s) = show s

instance Eq Literal where
    (BLit a) == (BLit b) = a == b
    (BLit a) == _ = False

    (StrLit a) == (StrLit b) = a == b
    (StrLit a) == _ = False

    (NumLit a) == (NumLit b) = a == b
    (NumLit a) == _ = False

data Expr = LiteralExpr Literal LineInfo -- eval written
            | SymbolicExpr Identifier
            | CallExpr ProcedureCall LineInfo
            | StmtExpr Statement LineInfo --
            | EndExpr

instance Show Expr where
    show (LiteralExpr a _) = show a
    show (SymbolicExpr a) = show a
    show (CallExpr a _) = show a
    show (StmtExpr a _) = show a
    show EndExpr = ""

debugExpr :: Expr -> String
debugExpr (LiteralExpr a i) = "LiteralExpr " ++ show a ++ " at line " ++ show i
debugExpr (SymbolicExpr a) = "SymbolicExpr " ++ debugIdentifier a
debugExpr (CallExpr a i) = "CallExpr " ++ debugProcCall a ++ " at line " ++ show i
debugExpr (StmtExpr a i) = "StmtExpr " ++ debugStatement a ++ " at line " ++ show i
debugExpr EndExpr = "EndExpr" 

instance Eq Expr where
    (LiteralExpr a _) == (LiteralExpr b _) = a == b
    (LiteralExpr a _) == _ = False
    (SymbolicExpr a) == (SymbolicExpr b) = a == b
    (SymbolicExpr a) == _ = False
    (CallExpr a _) == (CallExpr b _) = a == b
    (CallExpr a _) == _ = False
    (StmtExpr a _) == (StmtExpr b _) = a == b
    (StmtExpr a _) == _ = False
    EndExpr == EndExpr = True
    EndExpr == _ = False


data ProcedureCall = Proc {op :: Operator, args :: Operand}

instance Show ProcedureCall where
    show (Proc o s) = "( " ++ show o ++ " (" ++ show s ++ ") )"

instance Eq ProcedureCall where
    (Proc o s) == (Proc p k) = o == p

debugProcCall :: ProcedureCall -> String
debugProcCall (Proc o seq) = 
    "Procedure Call with Operator " ++ show o ++ " and Operand " ++ show seq

data Operator = OpName Identifier

instance Show Operator where
    show (OpName c) = show c

instance Eq Operator where
    (OpName c) == (OpName f) = c == f

data Operand = OprExpr [Expr]

instance Show Operand where
    show (OprExpr exps) = "( " ++ (unwords $ map show exps) ++ " )"

instance Eq Operand where
    (OprExpr a) == (OprExpr b) = a == b

data Sequence = SeqExpr {parent :: Expr, child :: Expr}

instance Show Sequence where
    show SeqExpr {parent=e, child=f} = show e ++ " " ++ show f

debugSequence :: Sequence -> String
debugSequence SeqExpr {parent=e, child=f} = "Sequence: \
\ parent: " ++ debugExpr e ++ " child: " ++ debugExpr f

instance Eq Sequence where
    SeqExpr {parent=e, child=f} == SeqExpr {parent=b, child=a} = 
        (e == b) && (f == a)

data Statement = AssignStmt Assign -- eval written
                | CondStmt Conditional -- eval written
                | ProcDefStmt ProcedureDefinition -- eval written
                | LoopStmt Loop -- eval written
                | SeqStmt Sequence -- eval written

instance Show Statement where
    show (AssignStmt a) = show a
    show (CondStmt a) = show a
    show (LoopStmt a) = show a
    show (ProcDefStmt a) = show a
    show (SeqStmt a) = show a


instance Eq Statement where
    (AssignStmt a) == (AssignStmt b) = a == b
    (AssignStmt a) == _ = False
    (CondStmt a) == (CondStmt b) = a == b
    (CondStmt a) == _ = False
    (LoopStmt a) == (LoopStmt b) = a == b
    (LoopStmt a) == _ = False
    (ProcDefStmt a) == (ProcDefStmt b) = a == b
    (ProcDefStmt a) == _ = False
    (SeqStmt a) == (SeqStmt b) = a == b
    (SeqStmt a) == _ = False

debugStatement :: Statement -> String
debugStatement (AssignStmt a) = "Assign statement " ++ show a
debugStatement (CondStmt a) = "Conditiional statement " ++ show a
debugStatement (LoopStmt a) = "Loop statement " ++ show a
debugStatement (ProcDefStmt a) = 
    "Procedural definition statement " ++ debugProcDef a
debugStatement (SeqStmt a) = "Sequence statement " ++ debugSequence a

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
                    | CTestId Identifier

instance Show ConditionTest where
    show (CTestLit li) = show li
    show (CTestProc li) = show li
    show (CTestId li) = show li

instance Eq ConditionTest where
    (CTestLit a) == (CTestLit b) = a == b
    (CTestLit a) == _ = False
    (CTestProc a) == (CTestProc b) = a == b
    (CTestProc a) == _ = False
    (CTestId a) == (CTestId b) = a == b
    (CTestId a) == _ = False


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

instance Show ProcedureDefinition where
    show DefineProc {procname=a, arguments=b, body=c} = 
        show a ++ " " ++ show b ++ " " ++ show c

debugProcDef :: ProcedureDefinition -> String
debugProcDef DefineProc {procname=a, arguments=b, body=c} =
    let msg = "Procedure Defintion: "
        msg2 = msg ++ "name: " ++ debugIdentifier a ++ " args: " ++ show b
        msg3 = msg2 ++ "body: " ++ debugSequence c
    in msg3

instance Eq ProcedureDefinition where
    DefineProc {procname=a, arguments=b, body=c} == DefineProc {procname=d, arguments=e, body=f} =
        let c1 = a == d
            c2 = b == e
            c3 = c == f
        in (c1 && c2) && c3


data Assign = Assigner Identifier Expr

instance Show Assign where
    show (Assigner id e) = show id ++ " " ++ show e


instance Eq Assign where
    (Assigner i1 e) == (Assigner i2 f) = (i1 == i2) && (e == f)
