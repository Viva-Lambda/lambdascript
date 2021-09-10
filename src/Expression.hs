module Expression where

import Lexer

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
-- (loop/dongu (do/yap < (1.6 6.0)) (then/ise fdsak,m))

procedure definition := (fn/edim <identifier> <arguments> <body>)
arguments := (<identifier>*)
body := <sequence>

sequence := ( seq/liste <expression>+ )

-}

type LineInfo = Int

data Identifier = IdExpr VarName TypeName LineInfo -- parsed - eval

instance Show Identifier where
    show (IdExpr a b _) = show a ++ ": " ++ show b

instance Eq Identifier where
    (IdExpr a b _) == (IdExpr c d _) = (a == c) && (b == d)

data VarName = VName String TokenInfo
data TypeName = TName VarName TokenInfo

instance Show VarName where
    show (VName v _) = v

instance Show TypeName where
    show (TName v _) = show v

instance Eq VarName where
    (VName v _) == (VName a _) = v == a

instance Eq TypeName where
    (TName v _) == (TName a _) = v == a

debugVarName :: VarName -> String
debugVarName (VName s i) =
    "{\"variable-name\": " ++ s ++ debugTokenInfo i ++ "}"

debugTypeName :: TypeName -> String
debugTypeName (TName (VName s _) j) =
    let key = "\"type-name\": "
        val = "\"" ++ s ++ "\","
        infokey = "\"info\": "
        infoval = debugTokenInfo j
    in "{" ++ key ++ val ++ infokey ++ infoval ++ "}"

debugIdentifier :: Identifier -> String

debugIdentifier (IdExpr a b line) = 
    let key = "{\"identifier\": {"
        varkey = "\"name\": "
        varval = debugVarName a
        typekey = ",\"type\": "
        typeval = debugTypeName b
        msg4 =  varkey ++ varval ++ typekey ++ typeval ++ ikeyval ++ "}"
        ikeyval = ", \"line\": " ++ show line ++ "}"
    in key ++ msg4 ++ ikeyval

data Literal = BLit Bool TokenInfo
              | StrLit String TokenInfo
              | NumLit Double TokenInfo

getLitTokenInfo :: Literal -> TokenInfo
getLitTokenInfo (BLit _ i) = i
getLitTokenInfo (StrLit _ i) = i
getLitTokenInfo (NumLit _ i) = i

instance Show Literal where
    show (BLit b _) = if b
                    then "true"
                    else "false"
    show (StrLit s _) = "\""++ s ++ "\""
    show (NumLit s _) = show s

instance Eq Literal where
    (BLit a _) == (BLit b _) = a == b
    (BLit _ _) == _ = False

    (StrLit a _) == (StrLit b _) = a == b
    (StrLit _ _) == _ = False

    (NumLit a _) == (NumLit b _) = a == b
    (NumLit _ _) == _ = False

debugLiteral :: Literal -> String
debugLiteral a =
    case a of
        (BLit b t) -> 
            let msg = "{\"literal\": " ++ show b ++ ", \"info\": "
            in dmsg msg t
        (NumLit b t) ->
            let msg = "{\"literal\": " ++ show b++ ", \"info\": "
            in dmsg msg t
        (StrLit b t) ->
            let msg = "{\"literal\": " ++ show b ++ ", \"info\": "
            in dmsg msg t
    where dmsg m info = 
            let msg = debugTokenInfo info
            in m ++ msg ++ "}"

data Expr = LiteralExpr Literal -- eval written
            | SymbolicExpr Identifier
            | CallExpr ProcedureCall TokenInfo
            | StmtExpr Statement TokenInfo --
            | EndExpr


instance Show Expr where
    show (LiteralExpr a ) = show a
    show (SymbolicExpr a) = show a
    show (CallExpr a _) = show a
    show (StmtExpr a _) = show a
    show EndExpr = ""

debugExpr :: Expr -> String
debugExpr (LiteralExpr a) = "LiteralExpr " ++ debugLiteral a
debugExpr (SymbolicExpr a) = "SymbolicExpr " ++ debugIdentifier a
debugExpr (CallExpr a i) = "CallExpr " ++ debugProcCall a ++ " at line " ++ show i
debugExpr (StmtExpr a i) = "StmtExpr " ++ debugStatement a ++ " at line " ++ show i
debugExpr EndExpr = "EndExpr" 

instance Eq Expr where
    (LiteralExpr a ) == (LiteralExpr b ) = a == b
    (LiteralExpr _ ) == _ = False
    (SymbolicExpr a) == (SymbolicExpr b) = a == b
    (SymbolicExpr _) == _ = False
    (CallExpr a _) == (CallExpr b _) = a == b
    (CallExpr _ _) == _ = False
    (StmtExpr a _) == (StmtExpr b _) = a == b
    (StmtExpr _ _) == _ = False
    EndExpr == EndExpr = True
    EndExpr == _ = False


data ProcedureCall = Proc {op :: Operator, args :: Operand}

instance Show ProcedureCall where
    show (Proc o s) = "( " ++ show o ++ " (" ++ show s ++ ") )"

instance Eq ProcedureCall where
    (Proc o _) == (Proc p _) = o == p

debugProcCall :: ProcedureCall -> String
debugProcCall (Proc o iseq) = 
    "Procedure Call with Operator " ++ show o ++ " and Operand " ++ show iseq

procCallInfo :: ProcedureCall -> TokenInfo
procCallInfo Proc {op=OpName (VName v i), args=a} = i

data Operator = OpName VarName

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
    (AssignStmt _) == _ = False
    (CondStmt a) == (CondStmt b) = a == b
    (CondStmt _) == _ = False
    (LoopStmt a) == (LoopStmt b) = a == b
    (LoopStmt _) == _ = False
    (ProcDefStmt a) == (ProcDefStmt b) = a == b
    (ProcDefStmt _) == _ = False
    (SeqStmt a) == (SeqStmt b) = a == b
    (SeqStmt _) == _ = False

debugStatement :: Statement -> String
debugStatement (AssignStmt a) = "Assign statement " ++ show a
debugStatement (CondStmt a) = "Conditiional statement " ++ show a
debugStatement (LoopStmt a) = "Loop statement " ++ show a
debugStatement (ProcDefStmt a) = 
    "Procedural definition statement " ++ debugProcDef a
debugStatement (SeqStmt a) = "Sequence statement " ++ debugSequence a

statementInfo :: Statement -> TokenInfo
statementInfo (AssignStmt (Assigner aid expr)) =
    let IdExpr (VName _ info) _ _ = aid
    in info
statementInfo (CondStmt (Cond {ctest = a, consequent = b, alternate = c})) =
    case a of
        (CTestLit d) -> getLitTokenInfo d
        (CTestProc d) -> procCallInfo d
        (CTestVar (VName _ info)) -> info

statementInfo (LoopStmt (Looper {ltest = a, lconsequent = b})) =
    case a of
        (CTestLit d) -> getLitTokenInfo d
        (CTestProc d) -> procCallInfo d
        (CTestVar (VName _ info)) -> info

statementInfo (SeqStmt (SeqExpr {parent = a, child = b})) = getExprInfo a

getExprInfo :: Expr -> TokenInfo
getExprInfo (LiteralExpr li) = getLitTokenInfo li
getExprInfo (SymbolicExpr (IdExpr (VName _ i) _ _)) = i
getExprInfo (CallExpr _ i) = i
getExprInfo (StmtExpr _ i) = i 
getExprInfo EndExpr = mkTokInfo (-1) (-1) "end of expression" ""




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
    show (Assigner aid e) = show aid ++ " " ++ show e


instance Eq Assign where
    (Assigner i1 e) == (Assigner i2 f) = (i1 == i2) && (e == f)

reduceExpr :: [Expr] -> Expr
reduceExpr [] = EndExpr
reduceExpr (e:es) =
    let children = reduceExpr es
        seq = SeqExpr {parent = e, child = children}
        stmt = SeqStmt seq
    in StmtExpr stmt (getExprInfo e)

fromExprToSeq :: [Expr] -> Sequence
fromExprToSeq [] = SeqExpr {parent = EndExpr, child = EndExpr}
fromExprToSeq (e:es) = SeqExpr {parent = e, child = reduceExpr es}

fromSeqToExprs :: Sequence -> [Expr]
-- end of sequence
fromSeqToExprs SeqExpr {parent = EndExpr, child=EndExpr} = []
-- skip parent sequence
fromSeqToExprs SeqExpr {parent = EndExpr, child=StmtExpr (SeqStmt s) i} = fromSeqToExprs s
-- skip child sequence
fromSeqToExprs SeqExpr {parent = StmtExpr (SeqStmt s) i, child=EndExpr} = fromSeqToExprs s
-- both are sequences
fromSeqToExprs SeqExpr {parent = StmtExpr (SeqStmt s) i, child=StmtExpr (SeqStmt a) j} = 
    fromSeqToExprs s ++ fromSeqToExprs a
-- only parent is a sequence
fromSeqToExprs SeqExpr {parent = StmtExpr (SeqStmt s) i, child=a} = 
    fromSeqToExprs s ++ [a]
-- only child is a sequence
fromSeqToExprs SeqExpr {parent = a, child=StmtExpr (SeqStmt s) i} = [a] ++ fromSeqToExprs s
-- both are not a sequence
fromSeqToExprs SeqExpr {parent = a, child = b} = [a] ++ [b]


-- reduceExpr (e:es) = foldr SeqExpr EndExpr (e:es)

