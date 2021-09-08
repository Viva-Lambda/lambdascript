-- implement a parser for simple language
module Parser where
import Lexer
import Expression
import ASTree


-- (while (< x 2) (set x (+ x 1)))

-- data ExprTree = ExprLeaf
                -- | ExprNode Expr ExprTree ExprTree

{-
Here is the grammar of the language

expression := <literal> | <procedure call> | <identifier> 

statement := <conditional> | <assignment> | <procedure definition> | <loop>


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
operand := ( <sequence> )
sequence := <expression>+


assignment := def/tanim <identifier> <expression>

conditional := (eger/if <test> <consequent> <alternate>)
test := (<literal>) | <procedural call> | (<identifier>)
consequent := (then/ise sequence)
alternate := (else/yoksa sequence)

loop := (loop/dongu <test> <consequent>)

procedure definition := (fn/edim <identifier> <arguments> <body>)
arguments := (<identifier>*)
body := (sequence)

-}

-- literal expressions 
toExprLit :: SLiteral -> Literal
toExprLit (BooleanLiteral b (TokInfo _ _)) = BLit b 
toExprLit (StringLiteral b (TokInfo _ _)) = StrLit b
toExprLit (NumericLiteral b (TokInfo _ _)) = NumLit b

parse :: STree -> Expr

-- match the complex patterns first

-- statements

-- if statement call expression test

parse (SList [Symbol "if" (TokInfo line _),
        SList (Symbol "do" (TokInfo ln col):a),
        SList [Symbol "then" _, SList (Symbol "seq" k:conseq)],
        SList [Symbol "else" _, SList (Symbol "seq" g:alter)]]) =
    let procTest = parse (SList (Symbol "do" (TokInfo ln col):a))
        (CallExpr procCall _) = procTest
        cseq = fromExprToSeq $ map parse conseq
        caltern = fromExprToSeq $ map parse alter
        condExpr = Cond {ctest=CTestProc procCall, 
                         consequent=cseq, 
                         alternate=caltern}
        stmt = CondStmt condExpr
    in StmtExpr stmt line

-- literal and symbolic variable test
parse (SList [Symbol "if" (TokInfo line _),
        SList [SLit lval],
        SList [Symbol "then" _, SList (Symbol "seq" k:conseq)],
        SList [Symbol "else" _, SList (Symbol "seq" g:alter)]]) =
    let (TokInfo _ _) = getSTreeTokInfo (SLit lval)
        cseq = fromExprToSeq $ map parse conseq
        caltern = fromExprToSeq $ map parse alter
        condition = Cond {ctest=ct, 
                          consequent=cseq, 
                          alternate=caltern}
        stmt = CondStmt condition
    in StmtExpr stmt line
        where ct = CTestLit (toExprLit lval)


parse (SList [Symbol "if" (TokInfo line _),
        SList [Symbol a (TokInfo ln _)],
        SList [Symbol "then" _, SList (Symbol "seq" k:conseq)],
        SList [Symbol "else" _, SList (Symbol "seq" g:alter)]]) =
    let ct = CTestId (IdExpr a ln)
        cseq = fromExprToSeq $ map parse conseq
        caltern = fromExprToSeq $ map parse alter
        condExp = Cond {ctest=ct, consequent=cseq, alternate=caltern}
        stmt = CondStmt condExp
    in StmtExpr stmt line

-- loop statement

-- call expression test
parse (SList [Symbol "loop" (TokInfo line c),
        SList (Symbol "do" (TokInfo ln col):a),
        SList [Symbol "then" _, SList (Symbol "seq" k:conseq)]]) =
    let (CallExpr procCall _) = parse (SList (Symbol "do" (TokInfo ln col):a) )
        cseq = fromExprToSeq $ map parse conseq
        loopExp = Looper {ltest=CTestProc procCall, lconsequent=cseq}
        stmt = LoopStmt loopExp
    in StmtExpr stmt line


parse (SList [Symbol "loop" (TokInfo line _),
        SList [SLit lval],
        SList [Symbol "then" _, SList (Symbol "seq" k:conseq)]]) =
    let cseq = fromExprToSeq $ map parse conseq
        loopExpr = Looper {ltest=ct, lconsequent=cseq}
        stmt = LoopStmt loopExpr
    in StmtExpr stmt line
        where ct = CTestLit (toExprLit lval)


parse (SList [Symbol "loop" (TokInfo line c),
        SList [Symbol a (TokInfo _ _)],
        SList [Symbol "then" _, SList (Symbol "seq" k:conseq)]]) =
    let ct = CTestId (IdExpr a line)
        cseq = fromExprToSeq $ map parse conseq
        loopExpr = Looper {ltest=ct, lconsequent=cseq}
        stmt = LoopStmt loopExpr
    in StmtExpr stmt line



-- procedural definition statement
parse (SList [
        Symbol "fn" (TokInfo line _),
        Symbol str (TokInfo ln _), -- function name
        SList (Symbol argname i : ars), -- function arguments
        SList (Symbol "seq" j:fnbody)]) =
        let procN = IdExpr str ln
            procArgs = mkIdentifiers (Symbol argname i : ars)
            procBody = fromExprToSeq $ map parse fnbody
            procDefExp = DefineProc {
                procname = procN, arguments = procArgs, body = procBody
                }
            stmt = ProcDefStmt procDefExp
        in StmtExpr stmt line

-- Assign statement

parse (SList [
    Symbol "def" (TokInfo line col),
    Symbol str (TokInfo ln c), a]) =
    let assignId = IdExpr str ln
        assignExpr = parse a
        assignEx = Assigner assignId assignExpr 
        stmt = AssignStmt assignEx
    in StmtExpr stmt line


-- sequence statement
parse (SList (Symbol "seq" i:a)) = reduceExpr $ map parse a 

-- match simpler expressions now

parse (SLit (BooleanLiteral s (TokInfo line _))) = LiteralExpr (BLit s) line
parse (SLit (StringLiteral s (TokInfo line _))) = LiteralExpr ( StrLit s) line
parse (SLit (NumericLiteral s (TokInfo line _))) = LiteralExpr (NumLit s) line

-- binary expressions
parse (SList [Symbol "do" (TokInfo line col), Symbol op (TokInfo ln _), 
        SList [b, c]]) =
    case op of
        "+" -> mkBinary op b c ln line
        "-" -> mkBinary op b c ln line
        "*" -> mkBinary op b c ln line
        "/" -> mkBinary op b c ln line
        "<" -> mkBinary op b c ln line
        ">" -> mkBinary op b c ln line
        "&" -> mkBinary op b c ln line
        "|" -> mkBinary op b c ln line
        "!" -> mkBinary op b c ln line
        "=" -> let mbin = mkBinary op b c ln line
               in mbin
        -- some other binary expression
        _ -> let procCall = Proc (OpName (IdExpr op ln)) (OprExpr [parse b, parse c])
             in CallExpr procCall line
    where mkBinary oper bexp cexp lln ll =
            let procCall = Proc (OpName (IdExpr oper lln)) (OprExpr [parse bexp, parse cexp])
            in CallExpr procCall ll


-- general procedure call expression
parse (SList [
    Symbol "do" (TokInfo line _), 
    Symbol s (TokInfo l _), 
    SList (a:callargs)
    ]) =
    let procCall = Proc (OpName (IdExpr s l)) (OprExpr $ map parse (a:callargs))
    in CallExpr procCall line

-- parse (SList (Symbol "begin" b):a) = parse a


-- symbolic expressions
parse (Symbol a (TokInfo line _)) = SymbolicExpr $ IdExpr a line

-- last match
-- parse (SList a) = reduceExpr $ map parse a

parse a = error $ "can not match stree " ++ show a

-- parse (SList a) = reduceExpr (map parse a)

reduceExpr :: [Expr] -> Expr
reduceExpr [] = EndExpr
reduceExpr (e:es) =
    let children = reduceExpr es
        seq = SeqExpr {parent = e, child = children}
        stmt = SeqStmt seq
    in StmtExpr stmt (getExprLine e)

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

getExprLine :: Expr -> LineInfo
getExprLine (LiteralExpr a line) = line
getExprLine (SymbolicExpr (IdExpr a line)) = line
getExprLine (CallExpr a line) = line
getExprLine (StmtExpr a line) = line
getExprLine EndExpr = -1

mkExpr :: String -> String


mkExpr toks =
  let tp = tokenize toks 0 0
      ptp = parseAll tp
      expTree = parse ptp
  in show expTree
-- let p = "(+ 1.05 4.3)"
