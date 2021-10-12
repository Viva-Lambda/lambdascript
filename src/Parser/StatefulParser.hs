module Parser.StatefulParser where

import Prelude hiding (sequence)

import Lexer.Lexer hiding (number)
import Lexer.Keyword

import Expression.Expression
import Expression.Identifier
import Expression.Literal
import Expression.ExprUtils

-- parser
import Parser.ASTree
import Parser.ParseError
import Parser.ParsingState
import Parser.ParseResult
import Parser.ParseUtils hiding (Input)

-- runtime environment required for accessing operators
import RuntimeEnv.StdEnv

-- import Text.ParserCombinators.ReadP
import Control.Applicative
import qualified Data.Map as DMap
import qualified Data.List as DList
-- import Control.Monad.State.Lazy

instance Functor ParseResult where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (PResult (k, p, a)) = PResult (k, p, f a)
    fmap _ (PError err) = PError err


type Input = (Keywords, ParsingState, STree)
type InputNoState  = (Keywords, STree)

-- top down recursive descent

noSideExpression :: InputNoState -> StatelessParseResult Expr

noSideExpression (k, (SName a b)) =
    case noSideGetExpression (k, (SName a b)) of
        (StatelessPResult (k2, gexp)) -> StatelessPResult (k2, GExpr gexp)
        (StatelessPError err) ->
            let serr = show (OtherError (err ++ ":in expression:"))
            in StatelessPError serr

noSideExpression (k, (SLit a)) =
    case noSideGetExpression (k, (SLit a)) of
        (StatelessPResult (k2, gexp)) -> StatelessPResult (k2, GExpr $ gexp)
        (StatelessPError err) -> 
            StatelessPError $ show (OtherError (err ++ ":in expression:"))

noSideExpression (k, (SList a)) =
    case noSideStatement (k, (SList a)) of
        (StatelessPResult (k2, stmt)) ->
            StatelessPResult (k2, StmtExpr $ stmt)
        (StatelessPError err) ->
            StatelessPError $ show (OtherError (err ++ ":in expression:"))

noSideExpression (_, v) = StatelessPError $ show (MatchError v "expression")

expression :: Input -> ParseResult Expr

expression (k, p, (SName a b)) =
    case noSideExpression (k, (SName a b)) of
        (StatelessPError err) -> PError err
        (StatelessPResult (k2, a)) -> PResult (k2, p, a)

expression (k, p, (SLit a)) =
    case noSideExpression (k, (SLit a)) of
        (StatelessPError err) -> PError err
        (StatelessPResult (k2, b)) -> PResult (k2, p, b)

expression (k, p, (SList a)) =
    case noSideExpression (k, (SList a)) of
        (StatelessPError err) -> PError err
        (StatelessPResult (k2, b)) -> PResult (k2, p, b)

expression (k, p, v) =
    case noSideExpression (k, v) of
        (StatelessPError err) -> PError err
        (StatelessPResult (k2, b)) -> PResult (k2, p, b)


-- get expression
noSideGetExpression :: InputNoState -> StatelessParseResult GetExpr

noSideGetExpression (k, (SName a b)) =
    StatelessPResult (k, GetVName $ VName a b)

noSideGetExpression (k, (SLit (BoolLit a b))) =
    StatelessPResult (k, GetLit $ BLit a b)

noSideGetExpression (k, (SLit (StringLit a b))) =
    StatelessPResult (k, GetLit $ StrLit a b )

noSideGetExpression (k, (SLit (NumericLit a b))) =
    StatelessPResult (k, GetLit $ NumLit a b )

noSideGetExpression (_, v) = StatelessPError $ show (MatchError v "get expression")

getExpression :: Input -> ParseResult GetExpr

getExpression (k, p, (SName a b)) =
    case noSideGetExpression (k, (SName a b)) of
        (StatelessPError err) -> PError err
        (StatelessPResult (k2, c)) -> PResult (k2, p, c)

getExpression (k, p, (SLit a)) =
    case noSideGetExpression (k, (SLit a)) of
        (StatelessPError err) -> PError err
        (StatelessPResult (k2, c)) -> PResult (k2, p, c)

getExpression (k, p, a) =
    case noSideGetExpression (k, a) of
        (StatelessPError err) -> PError err
        (StatelessPResult (k2, c)) -> PResult (k2, p, c)

-- statement

noSideStatement :: InputNoState -> StatelessParseResult Statement

noSideStatement (k, SList (SName a b: c)) =
    let isIfWord = iskey "if" a k -- if condition
        isLoopWord = iskey "loop" a k -- loop
        isFnWord = iskey "fn" a k -- function definition
        isPCallWord = iskey "do" a k
        isDefWord = iskey "def" a k --
        isSeqWord = iskey "seq" a k -- seq
        toks = SList (SName a b: c)
        lnb = " :in statement at line " ++ show ( lineNumber b ) ++ " : "
    in if isIfWord
       then case noSideConditional (k, toks) of
                (StatelessPResult (k2, cstmt)) ->
                    StatelessPResult (k2, CondStmt cstmt)
                (StatelessPError e) ->
                    StatelessPError $ show (OtherError (e ++ lnb))
       else if isLoopWord
            then case noSideLoop (k, toks) of
                    (StatelessPResult (k2, cstmt)) ->
                        StatelessPResult (k2, LoopStmt cstmt)
                    (StatelessPError e) -> 
                        StatelessPError $ show (OtherError (e ++ lnb) )
            else if isFnWord
                 then case noSideProcedureDefinition (k, toks) of
                        (StatelessPResult (k2, cstmt)) ->
                            StatelessPResult (k2, ProcDefStmt cstmt)
                        (StatelessPError e) ->
                            StatelessPError $ show (OtherError (e ++ lnb))
                 else if isPCallWord
                      then case noSideProcedureCall (k, toks) of
                                (StatelessPResult (k2, cstmt)) ->
                                    StatelessPResult (k2, CallStmt cstmt)
                                (StatelessPError e) ->
                                    StatelessPError $ show (OtherError (e ++ lnb))
                      else if isDefWord
                           then case noSideAssignment (k, toks) of
                                    (StatelessPResult (k2, cstmt)) ->
                                        StatelessPResult (k2, AssignStmt cstmt)
                                    (StatelessPError e) -> 
                                        StatelessPError $ show (OtherError (e ++ lnb))
                           else if isSeqWord
                                then case noSideSequence (k, toks) of
                                        (StatelessPResult (k2, cstmt)) ->
                                            StatelessPResult (k2, SeqStmt cstmt)
                                        (StatelessPError e) -> 
                                            StatelessPError $ show (OtherError (e ++ lnb))
                                else StatelessPError $ show (OtherError lnb)

noSideStatement (_, _, a) = StatelessPError $ show (MatchError a "statement")

statement :: Input -> ParseResult Statement
statement (k, p, SList (SName a b: c)) =
    case noSideStatement (k, SList (SName a b: c)) of
        (StatelessPResult (k2, d)) -> PResult (k2, p, d)
        (StatelessPError e) -> PError e

statement (k, p, a) =
    case noSideStatement (k, a) of
        (StatelessPResult (k2, d)) -> PResult (k2, p, d)
        (StatelessPError e) -> PError e

-- condition
noSideConditional :: InputNoState -> StatelessParseResult Conditional
noSideConditional 
    (k, 
     (SList [SName _ b, SList c, SList (SName d e:f), SList (SName g h:m)])
    ) =
    let testToks = SList c
        consToks = SList (SName d e:f)
        alterToks = SList (SName g h:m)
        lnb = " :in conditional at line " ++ show (lineNumber b) ++ " : "
    in case noSideConditionTest (k, testToks) of
            (StatelessPResult (k2, ct)) ->
                case noSideCConsequent (k2, consToks) of
                    (StatelessPResult (k3, conseq)) ->
                        case noSideCAlternate (k3, alterToks) of
                            (StatelessPResult (k4, alter)) ->
                                StatelessPResult (k4, Cond {
                                    ctest = ct,
                                    consequent = conseq,
                                    alternate = alter})
                            (StatelessPError n) ->
                                StatelessPError $ show (OtherError (n ++ lnb))
                    (StatelessPError n) ->
                        StatelessPError $ show (OtherError (n ++ lnb))
            (StatelessPError n) ->
                StatelessPError $ show (OtherError (n ++ lnb))

noSideConditional (_, _, a) = StatelessPError $ show (MatchError a "conditional")


conditional :: Input -> ParseResult Conditional
conditional (k, p, (SList [SName _ b, SList c, SList (SName d e:f), SList (SName g h:m)])) =
    let a = SList [SName _ b, SList c, SList (SName d e:f), SList (SName g h:m)]
    in case noSideConditional (k, a) of
            (StatelessPResult (k2, b)) -> PResult (k2, p, b)
            (StatelessPError err) -> PError err

conditional (k, p, a) =
    case noSideConditional (k, a) of
        (StatelessPResult (k2, b)) -> PResult (k2, p, b)
        (StatelessPError err) -> PError err

-- condition test
noSideConditionTest :: InputNoState -> StatelessParseResult ConditionTest

noSideConditionTest (k, SList [SLit (BoolLit a b)]) =
    StatelessPResult (k, CTestLit $ BLit a b)

noSideConditionTest (k, SList [SLit (StringLit a b)]) =
    StatelessPResult (k, CTestLit $ StrLit a b)

noSideConditionTest (k, SList [SLit (NumericLit a b)]) =
    StatelessPResult (k, CTestLit $ NumLit a b)

noSideConditionTest (k, SList [SName a b]) =
    StatelessPResult (k, CTestVar $ VName a b)

noSideConditionTest (k, SList (SName a b: c)) =
    let procCall = noSideProcedureCall (k, SList (SName a b: c))
    in case procCall of
            (StatelessPResult (k2, pc)) ->
                StatelessPResult (k2, CTestProc pc)
            (StatelessPError e) -> StatelessPError $ show (OtherError e)

noSideConditionTest (_,_,_) = 
    StatelessPError $ show (OtherError "condition test not matched")


conditionTest :: Input -> ParseResult ConditionTest

-- condition test literal
conditionTest (k, p, SList [SLit a]) =
    case noSideConditionTest (k, SList [SLit a]) of
        (StatelessPResult (k2, pc)) -> PResult (k2, p, pc)
        (StatelessPError e) -> PError e


-- condition test variable name
conditionTest (k,p, SList [SName a b]) =
    case noSideConditionTest (k, SList [SName a b]) of
        (StatelessPResult (k2, pc)) -> PResult (k2, p, pc)
        (StatelessPError e) -> PError e

conditionTest (k, p, SList (SName a b: c)) =
    case noSideConditionTest (k, SList (SName a b: c)) of
        (StatelessPResult (k2, pc)) -> PResult (k2, p, pc)
        (StatelessPError e) -> PError e


-- condition test procedural call

conditionTest (_,_,_) = PError $ show (OtherError "condition test not matched")

-- consequent / alternate
cconseqAltern :: String -> String -> Input -> ParseResult Sequence
cconseqAltern key msg (k, p, SList [SName a b, SList (SName c d:e)]) =
    let isThenWord = iskey key a k
        lnb = msg ++ show (lineNumber b) ++ " : "
    in if isThenWord
       then case sequence (k, p, SList (SName c d: e)) of
                (PResult s) -> PResult s
                (PError f) -> PError $ show (OtherError (f ++ lnb) )
       else PError $ show (OtherError lnb)

cconseqAltern _ _ (_, _, _) = 
    PError $ show (OtherError "can not match consequent or alternate")


cconsequent :: Input -> ParseResult Sequence
cconsequent (k, p, SList [SName a b, SList (SName c d:e)]) =
    let toks = (k, p, SList [SName a b, SList (SName c d:e)])
    in cconseqAltern "then" " :in consequent line " toks

cconsequent (_, _, a) = PError $ show (MatchError a "consequent")
-- alternate
calternate :: Input -> ParseResult Sequence
calternate (k, p, SList [SName a b, SList (SName c d:e)]) =
    let toks = (k, p, SList [SName a b, SList (SName c d:e)])
    in cconseqAltern "else" " :in alternate line " toks

calternate (_, _, a) = PError $ show (MatchError a "alternate")
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
                    (PError m) -> PError $ show (OtherError (m ++ lnb))
            (PError m) -> PError $ show (OtherError (m ++ lnb))

loop (_, _, a) = PError $ show (MatchError a "loop")
-- procedural definition
procedureDefinition :: Input -> ParseResult ProcedureDefinition
procedureDefinition (k, p, SList [SName _ b, SVar c d e, SList f, SList (SName g h :j)]) =
    let lnb = " :in procedural definition at line " ++ show ( lineNumber b ) ++ " : "
        idToks = SVar c d e
        argToks = SList f
        bToks = SList (SName g h :j)
    in case noSideidentifier (k, p, idToks) of
            (PResult (k2, p2, ident)) ->
                case fargs (k2, p2, argToks) of
                    (PResult (k3, p3, farg)) ->
                        case fbody (k3, p3, bToks) of
                            (PResult (k4, p4, fb)) ->
                                let pdef = DefineProc {
                                        procname = ident,
                                        arguments = farg,
                                        body = fb
                                        }
                                    np = addProcDef2PState pdef p4
                                    -- add procedure definition to
                                    -- parsing state
                                in PResult (k4, np, pdef)
                            (PError m) -> PError $ show (OtherError (m ++ lnb ++ " body "))
                    (PError m) -> PError $ show (OtherError (m ++ lnb ++ " arguments "))
            (PError m) -> PError $ show (OtherError (m ++ lnb ++ " identifier "))

procedureDefinition (_, _, a) = PError $ show (MatchError a "procedure definition")
-- arguments
parseIdentifier :: STree -> Identifier
parseIdentifier (SVar a b c) = IdExpr (VName a c) (VName b c) (lineNumber c)
parseIdentifier a = error $ show (MatchError a "identifier")

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
       else PError $ show (OtherError "arguments are not made up of identifiers")

fargs (_, _, _) = PError $ show (OtherError "function arguments must be provided as a list")

-- function body
fbody :: Input -> ParseResult Sequence
fbody i =
    case sequence i of
        (PResult j) -> PResult j
        (PError e) -> PError $ show (OtherError (e ++ " function body "))

-- assignment

assignment :: Input -> ParseResult Assign
assignment (k,p, (SList [SName _ b, SVar c d e, f])) =
    let ids = parseIdentifier (SVar c d e)
        lnb = " :in assignment line " ++ show (lineNumber b) ++ " : "
    in case typedExpression (k, p, f) of
            (PResult (k2, p2, expr)) -> PResult (k2, p2, Assigner ids expr)
            (PError m) -> PError $ show (OtherError (m ++ lnb) )

assignment (_, _, a) = PError $ show (MatchError a "assignment")
-- identifier

identifier :: Input -> ParseResult Identifier
identifier (k, p, SVar a b c) = identifierParse True (k, p, SVar a b c)
identifier (_, _, v) = PError $ show (MatchError v "identifier")

noSideidentifier :: Input -> ParseResult Identifier
noSideidentifier (k, p, SVar a b c) = identifierParse True (k, p, SVar a b c)

identifierParse :: Bool -> Input -> ParseResult Identifier
identifierParse hasSideEffect (k, p, SVar a b c) =
    let idf = parseIdentifier (SVar a b c)
    in if hasSideEffect
       then PResult (k, addIde2PState idf p, idf)
       else PResult (k, p, idf)



-- procedure call
procedureCall :: Input -> ParseResult ProcedureCall
procedureCall (k, p, SList [SName _ b, SName c d, SList e]) =
    let lnb = " :in procedure call line " ++ show (lineNumber b) ++ " : "
    in case noSideOperator (k,p,(SName c d)) of
            (PResult (k2, p2, oper)) -> 
                case operand (k2, p2, (SList e)) of
                    (PResult (k3, p3, opera)) ->
                        PResult (k3, p3, Proc {op = oper, args = opera})
                    (PError m) -> PError $ show (OtherError (m ++ lnb))
            (PError m) -> PError $ show (OtherError (m ++ lnb))

procedureCall (_, _, a) = PError $ show (MatchError a "procedure call")

procedureCallParse :: Bool -> Input -> ParseResult ProcedureCall
procedureCallParse hasSideEffect (k, p, SList [SName _ b, SName c d, SList e]) =
    let lnb = " :in procedure call line " ++ show (lineNumber b) ++ " : "
        -- prs = --
    in case noSideOperator (k,p,(SName c d)) of
            (PResult (k2, p2, oper)) -> 
                case operand (k2, p2, (SList e)) of
                    (PResult (k3, p3, opera)) ->
                        PResult (k3, p3, Proc {op = oper, args = opera})
                    (PError m) -> PError $ show (OtherError (m ++ lnb) )
            (PError m) -> PError $ show (OtherError (m ++ lnb))


-- operator

noSideOperator :: Input -> ParseResult Operator
noSideOperator (k, p, SName c d) = PResult (k,p, OpName (VName c d))
noSideOperator (_, _, _) = PError $ show (OtherError "does not match to operator")

-- noSideOperator (_, _, _) = PError "does not match to operator"

operatorCheck :: Operator -> ParsingState -> Bool
operatorCheck (OpName (VName c _)) p =
    if c `elem` stdOps
    then True
    else let hasOperator = lookUpPState c p
         in case hasOperator of
                Nothing -> True
                Just _ -> False

-- operand
operand :: Input -> ParseResult Operand

isTypeExpr :: Input -> Bool
isTypeExpr a = case typedExpression a of
                    (PResult _) -> True
                    (PError _) -> False

isTypeExprAll :: (Keywords, ParsingState, [STree]) -> Bool
isTypeExprAll (k, s, []) = True
isTypeExprAll (k, s, (b:bs)) = if isTypeExpr (k, s, b)
                               then isTypeExprAll (k, s, bs)
                               else False

typedExpression :: Input -> ParseResult TypedExpr
typedExpression (k, p, a) =
    case variableName (k, p, a) of
        (PResult (k2, p2, tid)) -> PResult (k2, p2, TypedId tid)
        (PError _) ->
            case literal (k, p, a) of
                (PResult (k2, p2, tl)) -> PResult (k2, p2, TypedLit tl)
                (PError _) ->
                    case procedureCall (k, p, a) of
                        (PResult (k2, p2, tc)) -> PResult (k2, p2, TypedCall tc)
                        (PError e) ->
                            let err = MatchError a ("procedural call in typed expression " ++ e)
                            in PError $ show err
                (PError e) ->
                    PError $ show (MatchError a ("literal in typed expression " ++ e))
        (PError e) ->
            PError $ show (MatchError a ("identifier in typed expression " ++ e))

operand (k, p, SList a) =
    let allExpr = isTypeExprAll (k, p, a)
        linput = (k, p, SList a)
    in if allExpr
       then PResult (k,p, OprExpr $ mapReducePResult typedExpression linput)
       else let msg = "operand contains tokens that can not be parsed "
                msg1 = msg ++ "as typed expressions"
            in PError $ show (OtherError msg1)

operand (_, _,  a) = PError $ show (MatchError a "operand" )

operandCheck :: Operand -> ParsingState -> Bool
operandCheck o p = False --

-- sequence
isExpr :: Input -> Bool
isExpr i = case expression i of
                (PResult _) -> True
                (PError _) -> False

isExprAll :: (Keywords, ParsingState, [STree]) -> Bool
isExprAll (_, _, []) = True
isExprAll (k, p, (e:es)) = (isExpr (k,p, e)) && (isExprAll (k, p, es))

expressions :: Input -> [ParseResult Expr]
expressions a = mapPResult expression a
expressions (_,_, a) = error $ show (MatchError a "multiple expressions")

sequence :: Input -> ParseResult Sequence
sequence (k, p, SList (SName _ b:e)) =
    let allExpr = isExprAll (k, p, e)
        exprInput = (k, p, SList e)
    in if allExpr
       then PResult (k,p, fromExprToSeq $ reducePResult (expressions exprInput))
       else let (_, unParsable) = DList.partition isParsed (expressions exprInput)
                msg = "sequence contains tokens that can not be parsed as expression"
                msg2 =  msg ++ " " ++ (show $ last unParsable) ++ " " ++ show b
            in PError $ show (OtherError msg2)

sequence (_, _, a) = PError $ show (MatchError a "sequence")

-- literal and identifier name
literal :: Input -> ParseResult Literal
literal (k, p, SLit a) =
    let litval = case a of
                    (StringLit b c) -> StrLit b c
                    (BoolLit b c) ->  BLit b c
                    (NumericLit b c) -> NumLit b c
        np = addLit2PState litval p
    in PResult (k, np, litval)

literal (_, _, a) = PError $ show (MatchError a "literal")

-- variable name
variableName :: Input -> ParseResult VarName
variableName (k, p, SName a b) = PResult (k, p, VName a b)
variableName (_, _, a) = PError $ show (MatchError a "variable name")

typeName :: Input -> ParseResult TypeName
typeName = variableName

moduleName :: Input -> ParseResult ModuleName
moduleName = variableName

-- TODO change after implementing state
identifierName :: Input -> ParseResult IdentifierName
identifierName = variableName
