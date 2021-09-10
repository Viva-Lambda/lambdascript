-- evaluator module for our parser
module Evaluator where
import Lexer
-- import Parser
import CombinedParser
import Expression
import FnNumber
import FnBool
import ASTree
-- import Control.Monad.State
import qualified Data.Map as DMap
import Control.Monad.State.Lazy


type SymTab = DMap.Map String Expr

type Evaluator a = State SymTab a

-- evaluate :: STree -> Double

-- evaluate literals

lookUp :: String -> Int -> Evaluator Expr
lookUp str i = do
    symTab <- get
    case DMap.lookup str symTab of
        Just v -> return v
        Nothing -> error $ "undefined variable " ++ str ++ " at line " ++ show i

addSymbol :: String -> Expr -> Evaluator ()
addSymbol str expr = do
    symTab <- get
    put $ DMap.insert str expr symTab
    return ()


removeSymbol :: String -> Evaluator ()
removeSymbol str = do
    symTab <- get
    put $ DMap.delete str symTab
    return ()

addIdentifierEval :: Identifier -> Expr -> Evaluator Expr
addIdentifierEval (IdExpr (VName a _) _ _) expr = do
    value <- evaluate expr
    addSymbol a value
    return value

removeId :: Identifier -> Evaluator Expr
removeId (IdExpr (VName a _) _ _) = do
    removeSymbol a
    return EndExpr

removeIds :: [Identifier] -> Evaluator Expr
removeIds [] = return EndExpr
removeIds (i:ii) = do
    _ <- removeId i
    removeIds ii

addIdExpZipEval :: (Identifier, Expr) -> Evaluator Expr
addIdExpZipEval (i, e) = addIdentifierEval i e

addIdsExpZipEval :: [(Identifier, Expr)] -> Evaluator Expr
addIdsExpZipEval [] = return EndExpr
addIdsExpZipEval (ie:ies) = do 
    _ <- addIdExpZipEval ie 
    addIdsExpZipEval ies

-- check for expression types
isNumericExpr :: Expr -> Bool
isNumericExpr (LiteralExpr (NumLit _ _)) = True
isNumericExpr _ = False

isBoolExpr :: Expr -> Bool
isBoolExpr (LiteralExpr (BLit _ _) ) = True
isBoolExpr _ = False

-- default functions


foldlSequence :: Sequence -> Evaluator Expr
foldlSequence SeqExpr {parent = EndExpr, child = EndExpr} = return EndExpr
foldlSequence SeqExpr {parent = a, child = EndExpr} = evaluate a
foldlSequence SeqExpr {parent = EndExpr, child = a} = evaluate a

foldlSequence SeqExpr {parent = a, child = b} = do
    _ <- evaluate a
    inner <- evaluate b
    return inner

-- evaluation function
evaluate :: Expr -> Evaluator Expr

-- evaluate literals
evaluate (LiteralExpr (BLit a i) ) = return $ LiteralExpr (BLit a i)
evaluate (LiteralExpr (NumLit a i) ) = return $ LiteralExpr (NumLit a i)
evaluate (LiteralExpr (StrLit a i) ) = return $ LiteralExpr (StrLit a i)

{-
let p1 = "true"
let p2 = "false"
let p3 = "2.53"
let p4 = "\"dsklmkm\""
-}

-- evaluate assignment statement
evaluate (StmtExpr (AssignStmt a) _) =
    let (Assigner (IdExpr (VName aid _) _ _) aexp) = a
    in do
        expr <- evaluate aexp
        addSymbol aid expr
        return expr

-- evaluate sequential statement
evaluate (StmtExpr (SeqStmt a) _) = foldlSequence a

-- evaluate procedure definition statement
evaluate (StmtExpr (ProcDefStmt a) line) =
    let DefineProc {procname = (IdExpr (VName pname _) _ _),
                    arguments = _,
                    body = _} = a
    in do 
        addSymbol pname (StmtExpr (ProcDefStmt a) line)
        return EndExpr

-- evaluate conditional statement
evaluate (StmtExpr (CondStmt a) line) =
    let Cond {ctest = ct, 
              consequent = conseq,
              alternate = alter} = a
    in case ct of
        (CTestVar (VName ide (TokInfo ln col _ _))) -> do
            expr <- lookUp ide ln
            condVal <- evaluate expr
            case condVal of
                (LiteralExpr lit) -> eval lit conseq alter
                _ -> let msg = "Variable must evaluate to a literal value"
                         msg2 = msg ++ " at line " ++ show ln
                     in error msg2
        (CTestLit lit) -> eval lit conseq alter
        (CTestProc p) -> do
            result <- evaluate (CallExpr p line)
            case result of
                --
                (LiteralExpr lit) -> eval lit conseq alter
                _ -> let msg = "Procedure must evaluate to a literal value"
                         msg2 = msg ++ " at line " ++ show line
                     in error msg2
        where eval (BLit b _) co alt = if b
                                     then foldlSequence co
                                     else foldlSequence alt
              eval (StrLit str _) co alt = if (length str) > 0
                                         then foldlSequence co
                                         else foldlSequence alt
              eval (NumLit n _) co alt = if n > 0
                                       then foldlSequence co
                                       else foldlSequence alt

-- evaluate loop statement
evaluate (StmtExpr (LoopStmt loopE) line) =
    let Looper {ltest = ct,
                lconsequent = _} = loopE
    in
        case ct of
            -- evaluate loop condition
            -- loop condition is variable
            (CTestVar (VName ids (TokInfo ln col i j))) -> do
                expr <- lookUp ids ln
                case expr of
                    (LiteralExpr lit ) -> eval loopE lit
                    _ -> let msg = debugVarName (VName ids (TokInfo ln col i j))
                             msg2 = msg ++ " must evaluate to a literal value"
                         in error msg2
            (CTestLit lit) -> eval loopE lit
            (CTestProc p) -> do
                result <- evaluate (CallExpr p (procCallInfo p))
                case result of
                    --
                    (LiteralExpr lit) -> eval loopE lit
                    _ -> let msg = debugProcCall p
                             msg2 = msg ++ " at line " ++ debugTokenInfo (procCallInfo p)
                             msg3 = msg2 ++ " must evaluate to a literal value"
                         in error msg3
        where eval a (BLit b tok) =
                let loopExpr = StmtExpr (LoopStmt a) tok
                    Looper {ltest = _, lconsequent = lseq} = a
                in evalif b loopExpr lseq
              eval a (StrLit b tok) =
                let loopExpr = StmtExpr (LoopStmt a) tok
                    Looper {ltest = _, lconsequent = lseq} = a
                in evalif (length b > 0) loopExpr lseq
              eval a (NumLit b tok) =
                let loopExpr = StmtExpr (LoopStmt a) tok
                    Looper {ltest = _, lconsequent = lseq} = a
                in evalif (b > 0.0) loopExpr lseq
              evalif b lexp ss =
                if b
                then do
                    _ <- foldlSequence ss
                    evaluate lexp
                else evaluate EndExpr

-- evaluate function call expression
evaluate (CallExpr procCall i) = -- (+ 1 2) - (set var 456)
    -- type checking can be done here
    -- IdExpr has another field (TName) registering types of variables
    let (Proc (OpName (VName op info)) pseq) = procCall
    in
        case pseq of
            -- evaluate unary operations
            (OprExpr [exp1]) -> evalUn op exp1 info
            -- basic binary arithmetic operations
            (OprExpr [exp1, exp2]) -> do
                f <- evaluate exp1
                s <- evaluate exp2
                evalBin op f s info
            -- call expression of a defined function
            (OprExpr exps) -> evalNarg op info exps
    where evalNarg funcName tinfo es = do
            procExpr <- lookUp funcName (lineNumber tinfo)
            let (StmtExpr (ProcDefStmt a) _) = procExpr
                DefineProc { procname = _,
                             arguments = pargs,
                             body = pbody } = a
                -- decompose the sequence
                lexps = length es
                largs = length pargs
                largLexps = lexps /= largs
            case largLexps of
                True ->
                    let msg = debugProcCall procCall
                        msg2 = msg ++ " at line " ++ debugTokenInfo tinfo
                        msg3 = msg2 ++ " has incorrect number of arguments"
                        msg4 = msg3 ++ " given: " ++ show lexps
                        msg5 = msg4 ++ " defined: " ++ show largs
                    in error msg5
                False ->
                    -- set values to arguments
                    let argExps = zip pargs es
                    in do 
                        -- set values to arguments
                        _ <- addIdsExpZipEval argExps
                        -- evaluate the body sequence
                        rvalue <- foldlSequence pbody
                        -- remove local values
                        _ <- removeIds pargs
                        -- return computed value
                        return rvalue

          evalBin ch (LiteralExpr (NumLit f fln)) (LiteralExpr (NumLit s sln)) tinfo =
            let first = LiteralExpr (NumLit f fln)
                second = LiteralExpr (NumLit s sln)
            in 
                case ch of
                    "+" -> return $ add2Number first second
                    "-" -> return $ sub2Number first second
                    "*" -> return $ multiply2Number first second
                    "/" -> return $ divide2Number first second
                    "^" -> return $ power2Number first second
                    "<" -> return $ cmpBinExprFn (<) first second
                    ">" -> return $ cmpBinExprFn (>) first second
                    "=" -> return $ cmpBinExprFn (==) first second
                    "!" -> return $ cmpBinExprFn (/=) first second
                    _ -> evalNarg ch tinfo [first, second]
          evalBin ch (LiteralExpr (BLit f fln)) (LiteralExpr (BLit s sln)) tinfo =
            let first = LiteralExpr (BLit f fln)
                second = LiteralExpr (BLit s sln)
            in
                case ch of
                    "&" -> return $ andBool first second
                    "|" -> return $ orBool first second
                    "=" -> return $ boolBinExprFn (==) first second
                    "!" -> return $ boolBinExprFn (/=) first second
                    _ -> evalNarg ch tinfo [first, second]
          evalBin ch first second tinfo =
            let msgp ="arguments are not "
                msge = " expressions at line " ++ debugTokenInfo tinfo
            in case ch of
                "+" -> error $ msgp ++ "numeric" ++ msge
                "-" -> error $ msgp ++ "numeric" ++ msge
                "*" -> error $ msgp ++ "numeric" ++ msge
                "/" -> error $ msgp ++ "numeric" ++ msge
                "^" -> error $ msgp ++ "numeric" ++ msge
                "<" -> error $ msgp ++ "numeric" ++ msge
                ">" -> error $ msgp ++ "numeric" ++ msge
                "&" -> error $ msgp ++ "boolean" ++ msge
                "|" -> error $ msgp ++ "boolean" ++ msge
                "=" -> let lit = LiteralExpr (BLit (first == second) (getExprInfo first))
                       in return lit
                       -- in error $ "equal came: " ++ debugExpr first
                "!" -> return $ LiteralExpr (BLit (first /= second) (getExprInfo first))
                _ -> evalNarg ch tinfo [first, second]
          evalUn op e tinfo =
                let msgpref = "expression must be a "
                    msgend = " at line " ++ debugTokenInfo tinfo
                in case op of
                    "-" -> let isNumeric = isNumericExpr e
                           in if isNumeric
                              then let LiteralExpr (NumLit b j) = e
                                   in return $ LiteralExpr (NumLit (-b) j) 
                              else error $ msgpref ++ "number" ++ msgend
                    "~" -> let isBool = isBoolExpr e
                           in if isBool
                              then let LiteralExpr (BLit b j) = e
                                   in return $ LiteralExpr (BLit (not b) j)
                              else error $ msgpref ++ "boolean" ++ msgend
                    _ -> evalNarg op tinfo [e]

{-

Test numeric expressions

let p1 = "(+ 2.0 3.0)"
let p2 = "(+ 2.0 (+ 1.8 0.2))"
let p3 = "(+ 2.0 (- 1.8 0.2))"
let p4 = "(+ 2.0 (- 1.8 0.2))"

-}

-- evaluate get expression
evaluate (SymbolicExpr (IdExpr (VName str j) _ _)) = lookUp str (lineNumber j)

-- as per scheme specification
evaluate EndExpr = return $ LiteralExpr (StrLit "" $ mkTokInfo (-1) (-1) "" "")

-- eval not matched
evaluate a = error $ "the following expression is not matched: " ++ debugExpr a 

-- value reference

-- test functions
exprCheck :: String -> Expr -> Bool
exprCheck arg expected =
    let toks = tokenize arg 0 0
        pexps = parseExpr toks
    in
        case pexps of
            (Result res rem) -> let act = runState $ evaluate res
                                    (expr, _) = act (DMap.fromList [("f", EndExpr)])
                                    expb = expr == expected
                                in if expb
                                   then True
                                   else error $ "unexpected " ++ show expr
            (Error e) -> error $ "Error happened " ++ show e

runEval :: String -> Expr
runEval toks =
    let tp = tokenize toks 0 0
        pexps = parseExpr tp
    in
        case pexps of
            (Result res rem) -> let act = runState $ evaluate res
                                    (expr, _) = act (DMap.fromList [("f", EndExpr)])
                                in expr
            (Error e) -> error $ "Error: " ++ show e


peval :: String -> IO ()
peval toks = print $ runEval toks
-- (+ 1 2)
-- (set var1 (+ 1 564))
