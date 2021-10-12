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
import Parser.ParseUtils hiding (Input, InputNoState)

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

expression a = parseWithSideEffects noSideExpression a

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

getExpression a = parseWithSideEffects noSideGetExpression a

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

noSideStatement (_, a) = StatelessPError $ show (MatchError a "statement")

statement :: Input -> ParseResult Statement
statement a = parseWithSideEffects noSideStatement a

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

noSideConditional (_, a) = StatelessPError $ show (MatchError a "conditional")


conditional :: Input -> ParseResult Conditional
conditional a = parseWithSideEffects noSideConditional a

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

noSideConditionTest (_, _) = 
    StatelessPError $ show (OtherError "condition test not matched")


conditionTest :: Input -> ParseResult ConditionTest

-- condition test literal
conditionTest a = parseWithSideEffects noSideConditionTest a

-- consequent / alternate
noSideCConseqAltern :: String -> String -> InputNoState -> StatelessParseResult Sequence
noSideCConseqAltern key msg (k, SList [SName a b, SList (SName c d:e)]) =
    let isThenWord = iskey key a k
        lnb = msg ++ show (lineNumber b) ++ " : "
    in if isThenWord
       then case noSideSequence (k, SList (SName c d: e)) of
                (StatelessPResult s) ->
                    StatelessPResult s
                (StatelessPError f) ->
                    StatelessPError $ show (OtherError (f ++ lnb) )
       else StatelessPError $ show (OtherError lnb)

noSideCConseqAltern _ _ (_, _) =
    StatelessPError $ show (OtherError "can not match consequent or alternate")


cconseqAltern :: String -> String -> Input -> ParseResult Sequence
cconseqAltern key msg (k, p, a) = 
    case noSideCConseqAltern key msg (k, a) of
        (StatelessPError err) -> PError err
        (StatelessPResult (k2, d)) -> PResult (k2, p, d)


noSideCConsequent :: InputNoState -> StatelessParseResult Sequence

noSideCConsequent (k, SList [SName a b, SList (SName c d:e)]) =
    let toks = (k, SList [SName a b, SList (SName c d:e)])
    in noSideCConseqAltern "then" " :in consequent line " toks

noSideCConsequent (k, a) =
    StatelessPError $ show (OtherError "can not match consequent")

cconsequent :: Input -> ParseResult Sequence
cconsequent a = parseWithSideEffects noSideCConsequent a


-- alternate
noSideCAlternate :: InputNoState -> StatelessParseResult Sequence
noSideCAlternate (k, SList [SName a b, SList (SName c d:e)]) =
    let toks = (k, SList [SName a b, SList (SName c d:e)])
    in noSideCConseqAltern "else" " :in alternate line " toks

noSideCAlternate (_, a) = StatelessPError $ show (MatchError a "alternate")

calternate :: Input -> ParseResult Sequence
calternate a = parseWithSideEffects noSideCAlternate a

-- loop
noSideLoop :: InputNoState -> StatelessParseResult Loop
noSideLoop (k, (SList [SName _ b, SList c, SList (SName d e:f)])) =
    let testToks = SList c
        consToks = SList (SName d e:f)
        lnb = " :in loop at line " ++ show (lineNumber b) ++ " : "
    in case noSideConditionTest (k, testToks) of
            (StatelessPResult (k2, ct)) ->
                case noSideCConsequent (k2, consToks) of
                    (StatelessPResult (k3, conseq)) ->
                        StatelessPResult (k3, Looper {ltest = ct,
                                                 lconsequent = conseq})
                    (StatelessPError m) -> StatelessPError $ show (OtherError (m ++ lnb))
            (StatelessPError m) -> StatelessPError $ show (OtherError (m ++ lnb))

noSideLoop (_, a) = StatelessPError $ show (MatchError a "loop")

loop :: Input -> ParseResult Loop
loop a = parseWithSideEffects noSideLoop a

-- procedural definition
noSideProcedureDefinition :: InputNoState -> StatelessParseResult ProcedureDefinition
noSideProcedureDefinition (k, SList [SName _ b, SVar c d e, SList f, SList (SName g h :j)]) =
    let lnb = " :in procedural definition at line " ++ show ( lineNumber b ) ++ " : "
        idToks = SVar c d e
        argToks = SList f
        bToks = SList (SName g h :j)
    in case noSideIdentifier (k, idToks) of
            (StatelessPResult (k2, ident)) ->
                case noSideFargs (k2, argToks) of
                    (StatelessPResult (k3, farg)) ->
                        case noSideFbody (k3, bToks) of
                            (StatelessPResult (k4, fb)) ->
                                let pdef = DefineProc {
                                        procname = ident,
                                        arguments = farg,
                                        body = fb
                                        }
                                in StatelessPResult (k4, pdef)
                            (StatelessPError m) -> StatelessPError $ show (OtherError (m ++ lnb ++ " body "))
                    (StatelessPError m) -> StatelessPError $ show (OtherError (m ++ lnb ++ " arguments "))
            (StatelessPError m) -> StatelessPError $ show (OtherError (m ++ lnb ++ " identifier "))

noSideProcedureDefinition (_, a) = 
    StatelessPError $ show (MatchError a "procedure definition")

procedureDefinition :: Input -> ParseResult ProcedureDefinition
procedureDefinition a = parseWithSideEffects noSideProcedureDefinition a

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

noSideFargs :: InputNoState -> StatelessParseResult [Identifier]
noSideFargs (k, (SList a)) =
    let allIds = isIds a
    in if allIds
       then StatelessPResult (k, map parseIdentifier a)
       else StatelessPError $ show (OtherError "arguments are not made up of identifiers")

noSideFargs (_, _) = StatelessPError $ show (OtherError "function arguments must be provided as a list")

fargs :: Input -> ParseResult [Identifier]
fargs a = parseWithSideEffects noSideFargs a

-- function body
noSideFbody :: InputNoState -> StatelessParseResult Sequence
noSideFbody i =
    case noSideSequence i of
        (StatelessPResult j) -> StatelessPResult j
        (StatelessPError e) -> StatelessPError $ show (OtherError (e ++ " function body "))

fbody :: Input -> ParseResult Sequence
fbody a = parseWithSideEffects noSideFbody a

-- assignment

noSideAssignment :: InputNoState -> StatelessParseResult Assign
noSideAssignment (k, (SList [SName _ b, SVar c d e, f])) =
    let ids = parseIdentifier (SVar c d e)
        lnb = " :in assignment line " ++ show (lineNumber b) ++ " : "
    in case noSideTypedExpression (k, f) of
            (StatelessPResult (k2, expr)) -> StatelessPResult (k2, Assigner ids expr)
            (StatelessPError m) -> StatelessPError $ show (OtherError (m ++ lnb) )

noSideAssignment (_, a) = StatelessPError $ show (MatchError a "assignment")

assignment :: Input -> ParseResult Assign
assignment a = parseWithSideEffects noSideAssignment a

-- identifier
noSideIdentifier :: InputNoState -> StatelessParseResult Identifier
noSideIdentifier (k, SVar a b c) =
    let idf = parseIdentifier (SVar a b c)
    in StatelessPResult (k, idf)

noSideIdentifier (_, v) = StatelessPError $ show (MatchError v "identifier")


identifier :: Input -> ParseResult Identifier
identifier a = parseWithSideEffects noSideIdentifier a

-- procedure call
noSideProcedureCall :: InputNoState -> StatelessParseResult ProcedureCall
noSideProcedureCall (k, SList [SName _ b, SName c d, SList e]) =
    let lnb = " :in procedure call line " ++ show (lineNumber b) ++ " : "
    in case noSideOperator (k, (SName c d)) of
            (StatelessPResult (k2, oper)) -> 
                case noSideOperand (k2, (SList e)) of
                    (StatelessPResult (k3, opera)) ->
                        StatelessPResult (k3, Proc {op = oper, args = opera})
                    (StatelessPError m) -> 
                        StatelessPError $ show (OtherError (m ++ lnb))
            (StatelessPError m) -> StatelessPError $ show (OtherError (m ++ lnb))


noSideProcedureCall (_, a) = StatelessPError $ show (MatchError a "procedure call")
procedureCall :: Input -> ParseResult ProcedureCall
procedureCall a = parseWithSideEffects noSideProcedureCall a

-- operator

noSideOperator :: InputNoState -> StatelessParseResult Operator
noSideOperator (k, SName c d) = StatelessPResult (k, OpName (VName c d))
noSideOperator (_, _) = StatelessPError $ show (OtherError "does not match to operator")

-- noSideOperator (_, _, _) = PError "does not match to operator"

{-

operatorCheck :: Operator -> ParsingState -> Bool
operatorCheck (OpName (VName c _)) p =
    if c `elem` stdOps
    then True
    else let hasOperator = lookUpPState c p
         in case hasOperator of
                Nothing -> True
                Just _ -> False
-}

-- operand
noSideOperand :: InputNoState -> StatelessParseResult Operand

isTypeExprNoSide :: InputNoState -> Bool
isTypeExprNoSide a = case noSideTypedExpression a of
                    (StatelessPResult _) -> True
                    (StatelessPError _) -> False

isTypeExprAllNoSide :: (Keywords, [STree]) -> Bool
isTypeExprAllNoSide (k, []) = True
isTypeExprAllNoSide (k, (b:bs)) = if isTypeExprNoSide (k, b)
                                  then isTypeExprAllNoSide (k, bs)
                                  else False

noSideTypedExpression :: InputNoState -> StatelessParseResult TypedExpr
noSideTypedExpression (k, a) =
    case noSideVariableName (k, a) of
        (StatelessPResult (k2, tid)) -> StatelessPResult (k2, TypedId tid)
        (StatelessPError _) ->
            case noSideLiteral (k, a) of
                (StatelessPResult (k2, tl)) -> StatelessPResult (k2, TypedLit tl)
                (StatelessPError _) ->
                    case noSideProcedureCall (k, a) of
                        (StatelessPResult (k2, tc)) -> StatelessPResult (k2, TypedCall tc)
                        (StatelessPError e) ->
                            let err = MatchError a ("procedural call in typed expression " ++ e)
                            in StatelessPError $ show err
                (StatelessPError e) ->
                    StatelessPError $ show (MatchError a ("literal in typed expression " ++ e))
        (StatelessPError e) ->
            StatelessPError $ show (MatchError a ("identifier in typed expression " ++ e))

typedExpression :: Input -> ParseResult TypedExpr
typedExpression a = parseWithSideEffects noSideTypedExpression a

noSideOperand (k, SList a) =
    let allExpr = isTypeExprAllNoSide (k, a)
        linput = (k, SList a)
    in if allExpr
       then StatelessPResult (k, OprExpr $ noSideMapReducePResult noSideTypedExpression linput)
       else let msg = "operand contains tokens that can not be parsed "
                msg1 = msg ++ "as typed expressions"
            in StatelessPError $ show (OtherError msg1)

noSideOperand (_,  a) = StatelessPError $ show (MatchError a "operand" )

operandCheck :: Operand -> ParsingState -> Bool
operandCheck o p = False --

-- sequence
noSideIsExpr :: InputNoState -> Bool
noSideIsExpr i = case noSideExpression i of
                    (StatelessPResult _) -> True
                    (StatelessPError _) -> False

noSideIsExprAll :: (Keywords, [STree]) -> Bool
noSideIsExprAll (_, []) = True
noSideIsExprAll (k, (e:es)) = (noSideIsExpr (k, e)) && (noSideIsExprAll (k, es))

noSideExpressions :: InputNoState -> [StatelessParseResult Expr]
noSideExpressions a = noSideMapPResult noSideExpression a
noSideExpressions (_, a) = error $ show (MatchError a "multiple expressions")

noSideSequence :: InputNoState -> StatelessParseResult Sequence
noSideSequence (k, SList (SName _ b:e)) =
    let allExpr = noSideIsExprAll (k, e)
        exprInput = (k, SList e)
    in if allExpr
       then let ns = noSideReducePResult (noSideExpressions exprInput)
            in StatelessPResult (k, fromExprToSeq $ ns)
       else let nexprs = noSideExpressions exprInput
                (_, unParsable) = DList.partition noSideIsParsed nexprs
                msg = "sequence contains tokens that can not be parsed as expression"
                msg2 =  msg ++ " " ++ (show $ last unParsable) ++ " " ++ show b
            in StatelessPError $ show (OtherError msg2)

noSideSequence (_, a) = StatelessPError $ show (MatchError a "sequence")

sequence :: Input -> ParseResult Sequence
sequence a = parseWithSideEffects noSideSequence a

-- literal and identifier name
noSideLiteral :: InputNoState -> StatelessParseResult Literal
noSideLiteral (k, SLit a) =
    let litval = case a of
                    (StringLit b c) -> StrLit b c
                    (BoolLit b c) ->  BLit b c
                    (NumericLit b c) -> NumLit b c
        -- np = addLit2PState litval p
    in StatelessPResult (k, litval)

noSideLiteral (_, a) = StatelessPError $ show (MatchError a "literal")

literal :: Input -> ParseResult Literal
literal a = parseWithSideEffects noSideLiteral a

-- variable name
noSideVariableName :: InputNoState -> StatelessParseResult VarName
noSideVariableName (k, SName a b) = StatelessPResult (k, VName a b)
noSideVariableName (_, a) = StatelessPError $ show (MatchError a "variable name")

variableName :: Input -> ParseResult VarName
variableName a = parseWithSideEffects noSideVariableName a

noSideTypeName :: InputNoState -> StatelessParseResult TypeName
noSideTypeName = noSideVariableName

typeName :: Input -> ParseResult TypeName
typeName = variableName

noSideModuleName :: InputNoState -> StatelessParseResult ModuleName
noSideModuleName = noSideVariableName

moduleName :: Input -> ParseResult ModuleName
moduleName = variableName

-- TODO change after implementing state
identifierName :: Input -> ParseResult IdentifierName
identifierName = variableName
