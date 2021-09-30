module Parser.StatefulParser where

import Lexer.Lexer hiding (number)
import Prelude hiding (sequence)
import Expression.Expression
import Parser.ASTree

-- import Text.ParserCombinators.ReadP
import Control.Applicative
import qualified Data.Map as DMap
import qualified Data.List as DList
-- import Control.Monad.State.Lazy

type ParsingState = DMap.Map String String
type Keywords = DMap.Map String [String]

emptyState :: ParsingState
emptyState = DMap.empty

emptyKeywords :: Keywords
emptyKeywords = DMap.empty


data ParseResult a = PError String 
                     | PResult (Keywords, ParsingState, a)
                     deriving (Show)

-- keyword
iskey :: String -> String -> Keywords -> Bool
iskey key given keys =
    if DMap.member key keys
    then let klst = keys DMap.! key
         in given `elem` klst
    else False


result :: ParseResult a -> Either String a
result (PResult (_,_,a)) = Right a
result (PError s) = Left s

reduceResult :: [ParseResult a] -> [a]
reduceResult [] = []
reduceResult (t:ts) =
    case result t of
        (Right a) -> a : reduceResult ts
        (Left _) -> []

isParsed :: ParseResult a -> Bool
isParsed (PResult _) = True
isParsed (PError _) = False

instance Functor ParseResult where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (PResult (k, p, a)) = PResult (k, p, f a)
    fmap _ (PError err) = PError err



parseError :: STree -> String -> String
parseError t m = 
    let msg = "CAN NOT PARSE TOKEN(S): \n\
   \ " ++ show t ++ " AS\n\
   \ " ++ m ++ " AT\n "
        msg2 = msg ++ debugSTree t
    in msg2

type Input = (Keywords, ParsingState, STree)

-- top down recursive descent

expression :: Input -> ParseResult Expr

expression (k, p, (SName a b)) =
    case getExpression (k,p,(SName a b)) of
        (PResult (k2, p2, gexp)) -> PResult (k2, p2, GExpr gexp)
        (PError err) -> PError (err ++ ":in expression:")

expression (k, p, (SLit a)) =
    case getExpression (k, p, (SLit a)) of
        (PResult (k2, p2, gexp)) -> PResult (k2, p2, GExpr $ gexp)
        (PError err) -> PError (err ++ ":in expression:")

expression (k, p, (SList a)) =
    case statement (k, p, (SList a)) of
        (PResult (k2, p2, stmt)) -> PResult (k2, p2, StmtExpr $ stmt)
        (PError err) -> PError (err ++ ":in expression:")

expression (_, _, v) = PError $ parseError v "expression"

-- get expression
getExpression :: Input -> ParseResult GetExpr
getExpression (k, p, (SName a b)) = PResult (k,p,GetVName $ VName a b)
getExpression (k, p, (SLit (BoolLit a b))) = PResult (k, p, GetLit $ BLit a b)
getExpression (k, p, (SLit (StringLit a b))) = PResult (k, p, GetLit $ StrLit a b )
getExpression (k, p, (SLit (NumericLit a b))) = PResult (k, p, GetLit $ NumLit a b )
getExpression (_, _, v) = PError $ parseError v "get expression"

-- statement
statement :: Input -> ParseResult Statement
statement (k, p, SList (SName a b: c)) =
    let isIfWord = iskey "if" a k -- if condition
        isLoopWord = iskey "loop" a k -- loop
        isFnWord = iskey "fn" a k -- function definition
        isPCallWord = iskey "do" a k
        isDefWord = iskey "def" a k --
        isSeqWord = iskey "seq" a k -- seq
        toks = SList (SName a b: c)
        lnb = " :in statement at line " ++ show ( lineNumber b ) ++ " : "
    in if isIfWord
       then case conditional (k, p, toks) of 
                (PResult (k2, p2, cstmt)) -> PResult (k2, p2, CondStmt cstmt)
                (PError e) -> PError (e ++ lnb)
       else if isLoopWord
            then case loop (k, p, toks) of
                    (PResult (k2, p2, cstmt)) -> PResult (k2, p2, LoopStmt cstmt)
                    (PError e) -> PError (e ++ lnb)
            else if isFnWord
                 then case procedureDefinition (k, p, toks) of
                        (PResult (k2, p2, cstmt)) -> PResult (k2, p2, ProcDefStmt cstmt)
                        (PError e) -> PError (e ++ lnb)
                 else if isPCallWord
                      then case procedureCall (k, p, toks) of
                                (PResult (k2, p2, cstmt)) -> PResult (k2, p2, CallStmt cstmt b)
                                (PError e) -> PError (e ++ lnb)
                      else if isDefWord
                           then case assignment (k, p, toks) of
                                    (PResult (k2, p2, cstmt)) ->
                                        PResult (k2, p2, AssignStmt cstmt)
                                    (PError e) -> PError (e ++ lnb)
                           else if isSeqWord
                                then case sequence (k, p, toks) of
                                        (PResult (k2, p2, cstmt)) ->
                                            PResult (k2, p2, SeqStmt cstmt)
                                        (PError e) -> PError (e ++ lnb)
                                else PError lnb

statement (_, _, a) = PError $ parseError a "statement"

conditional :: Input -> ParseResult Conditional
conditional (k, p, (SList [SName _ b, SList c, SList (SName d e:f), SList (SName g h:m)])) =
    let testToks = SList c
        consToks = SList (SName d e:f)
        alterToks = SList (SName g h:m)
        lnb = " :in conditional at line " ++ show (lineNumber b) ++ " : "
    in case conditionTest (k, p, testToks) of
            (PResult (k2, p2, ct)) ->
                case cconsequent (k2, p2, consToks) of
                    (PResult (k3, p3, conseq)) ->
                        case calternate (k3, p3, alterToks) of
                            (PResult (k4, p4, alter)) ->
                                PResult (k4, p4, Cond {ctest = ct,
                                                       consequent = conseq,
                                                       alternate = alter})
                            (PError n) -> PError (n ++ lnb)
                    (PError n) -> PError (n ++ lnb)
            (PError n) -> PError (n ++ lnb)

conditional (_, _, a) = PError $ parseError a "conditional"
-- condition test
conditionTest :: Input -> ParseResult ConditionTest

-- condition test literal
conditionTest (k,p, SList [SLit (BoolLit a b)]) = PResult (k,p, CTestLit $ BLit a b)
conditionTest (k,p, SList [SLit (StringLit a b)]) = PResult (k, p, CTestLit $ StrLit a b)
conditionTest (k,p, SList [SLit (NumericLit a b)]) = PResult (k, p, CTestLit $ NumLit a b)

-- condition test variable name
conditionTest (k,p, SList [SName a b]) = PResult (k, p, CTestVar $ VName a b)

-- condition test procedural call
conditionTest (k,p, SList (SName a b: c)) =
    let procCall = procedureCall (k, p, SList (SName a b: c))
    in case procCall of
            (PResult (k2, p2, pc)) -> PResult (k2, p2, CTestProc pc)
            (PError e) -> PError e
conditionTest (_,_,_) = PError "condition test not matched"

-- consequent / alternate
cconseqAltern :: String -> String -> Input -> ParseResult Sequence
cconseqAltern key msg (k, p, SList [SName a b, SList (SName c d:e)]) =
    let isThenWord = iskey key a k
        lnb = msg ++ show (lineNumber b) ++ " : "
    in if isThenWord
       then case sequence (k, p, SList (SName c d: e)) of
                (PResult s) -> PResult s
                (PError f) -> PError (f ++ lnb)
       else PError lnb

cconseqAltern _ _ (_, _, _) = PError "can not match consequent or alternate"


cconsequent :: Input -> ParseResult Sequence
cconsequent (k, p, SList [SName a b, SList (SName c d:e)]) =
    let toks = (k, p, SList [SName a b, SList (SName c d:e)])
    in cconseqAltern "then" " :in consequent line " toks

cconsequent (_, _, a) = PError $ parseError a "consequent"
-- alternate
calternate :: Input -> ParseResult Sequence
calternate (k, p, SList [SName a b, SList (SName c d:e)]) =
    let toks = (k, p, SList [SName a b, SList (SName c d:e)])
    in cconseqAltern "else" " :in alternate line " toks

calternate (_, _, a) = PError $ parseError a "alternate"
-- loop
loop :: Input -> ParseResult Loop
loop (k, p, (SList [SName _ b, SList c, SList (SName d e:f)])) =
    let testToks = SList c
        consToks = SList (SName d e:f)
        lnb = " :in loop at line " ++ show (lineNumber b) ++ " : "
    in case conditionTest (k, p, testToks) of
            (PResult (k2, p2, ct)) ->
                case cconsequent (k2, p2, consToks) of
                    (PResult (k3, p3, conseq)) ->
                        PResult (k3, p3, Looper {ltest = ct,
                                                 lconsequent = conseq})
                    (PError m) -> PError (m ++ lnb)
            (PError m) -> PError (m ++ lnb)

loop (_, _, a) = PError $ parseError a "loop"
-- procedural definition
procedureDefinition :: Input -> ParseResult ProcedureDefinition
procedureDefinition (k, p, SList [SName _ b, SVar c d e, SList f, SList (SName g h :j)]) =
    let lnb = " :in procedural definition at line " ++ show ( lineNumber b ) ++ " : "
        idToks = SVar c d e
        argToks = SList f
        bToks = SList (SName g h :j)
    in case identifier (k, p, idToks) of
            (PResult (k2, p2, ident)) ->
                case fargs (k2, p2, argToks) of
                    (PResult (k3, p3, farg)) ->
                        case fbody (k3, p3, bToks) of
                            (PResult (k4, p4, fb)) ->
                                PResult (k4, p4,
                                    DefineProc {
                                        procname = ident,
                                        arguments = farg,
                                        body = fb
                                        }
                                    )
                            (PError m) -> PError (m ++ lnb ++ " body ")
                    (PError m) -> PError (m ++ lnb ++ " arguments ")
            (PError m) -> PError (m ++ lnb ++ " identifier ")

procedureDefinition (_, _, a) = PError $ parseError a "procedure definition"
-- arguments
parseIdentifier :: STree -> Identifier
parseIdentifier (SVar a b c) = IdExpr (VName a c) (TName b c) (lineNumber c)
parseIdentifier a = error $ parseError a "identifier"

isId :: STree -> Bool
isId (SVar _ _ _) = True
isId _ = False

isIds :: [STree] -> Bool
isIds [] = True
isIds (t:ts) = isId t && (isIds ts)

fargs :: Input -> ParseResult [Identifier]
fargs (k, p, (SList a)) =
    let allIds = isIds a
    in if allIds
       then PResult (k, p, map parseIdentifier a)
       else PError "arguments are not made up of identifiers"

fargs (_, _, _) = PError "function arguments must be provided as a list"

-- function body
fbody :: Input -> ParseResult Sequence
fbody i =
    case sequence i of
        (PResult j) -> PResult j
        (PError e) -> PError (e ++ " function body ")

-- assignment

assignment :: Input -> ParseResult Assign
assignment (k,p, (SList [SName _ b, SVar c d e, f])) =
    let ids = parseIdentifier (SVar c d e)
        lnb = " :in assignment line " ++ show (lineNumber b) ++ " : "
    in case expression (k, p, f) of
            (PResult (k2, p2, expr)) -> PResult (k2, p2, Assigner ids expr)
            (PError m) -> PError (m ++ lnb)

assignment (_, _, a) = PError $ parseError a "assignment"
-- identifier

identifier :: Input -> ParseResult Identifier
identifier (k, p, SVar a b c) = PResult (k, p, parseIdentifier (SVar a b c))
identifier (_, _, v) = PError $ parseError v "identifier"

-- procedure call
procedureCall :: Input -> ParseResult ProcedureCall
procedureCall (k, p, SList [SName _ b, SName c d, SList e]) =
    let lnb = " :in procedure call line " ++ show (lineNumber b) ++ " : "
    in case operator (k,p,(SName c d)) of
            (PResult (k2, p2, oper)) -> 
                case operand (k2, p2, (SList e)) of
                    (PResult (k3, p3, opera)) ->
                        PResult (k3, p3, Proc {op = oper, args = opera})
                    (PError m) -> PError (m ++ lnb)
            (PError m) -> PError (m ++ lnb)

procedureCall (_, _, a) = PError $ parseError a "procedure call"

-- operator
operator :: Input -> ParseResult Operator
operator (k, p, SName c d) = PResult (k,p, OpName (VName c d))
operator (_, _, _) = PError "does not match to operator"

-- operand
operand :: Input -> ParseResult Operand

isExpr :: Input -> Bool
isExpr i = case expression i of
                (PResult _) -> True
                (PError _) -> False

isExprAll :: (Keywords, ParsingState, [STree]) -> Bool
isExprAll (_, _, []) = True
isExprAll (k, p, (e:es)) = (isExpr (k,p, e)) && (isExprAll (k, p, es))

expressions :: Input -> [ParseResult Expr]

expressions (_,_, SList []) = []
expressions (k,p, SList (e:es)) = 
    (expression (k, p, e)) : (expressions (k, p, SList es))


expressions (_,_, a) = error $ parseError a "multiple expressions"

operand (k, p, SList a) =
    let allExpr = isExprAll (k, p, a)
    in if allExpr
       then PResult (k,p, OprExpr $ reduceResult (expressions (k, p, SList a)))
       else PError "operand contains tokens that can not be parsed as expression"

operand (_, _,  a) = PError $ parseError a "operand"
-- sequence

sequence :: Input -> ParseResult Sequence
sequence (k, p, SList (SName a b:e)) =
    let allExpr = isExprAll (k, p, e)
        exprInput = (k, p, SList e)
    in if allExpr
       then PResult (k,p, fromExprToSeq $ reduceResult (expressions exprInput))
       else let (parsable, unParsable) = DList.partition isParsed (expressions exprInput)
                msg = "sequence contains tokens that can not be parsed as expression"
                msg2 =  msg ++ " " ++ (show $ last unParsable) ++ " " ++ show b
            in PError msg2
            


sequence (_, _, a) = PError $ parseError a "sequence"
