module StatefulParser where

import Lexer hiding (number)
import Expression
import ASTree

-- import Text.ParserCombinators.ReadP
import Control.Applicative
import qualified Data.Map as DMap
-- import Control.Monad.State.Lazy

type ParsingState = DMap.Map String String
type Keywords = DMap.Map String [String]

emptyState :: ParsingState
emptyState = DMap.empty

emptyKeywords :: Keywords
emptyKeywords = DMap.empty


data ParseResult a = PError String 
                     | PResult (Keywords, ParsingState, a)

result :: ParseResult a -> Either String a
result (PResult (_,_,a)) = Right a
result (PError s) = Left s

reduceResult :: [ParseResult a] -> [a]
reduceResult [] = []
reduceResult (t:ts) =
    case result t of
        (Right a) -> a : reduceResult ts
        (Left _) -> []

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

lookAHead :: [Token] -> Token
accept :: [Token] -> [Token]

lookAHead [] = TokEnd
lookAHead (t:_) = t

accept [] = error "nothing to accept"
accept (_:ts) = ts

type Input = (Keywords, ParsingState, STree)
type Output = (ParsingState, Expr)

moreInput :: Input -> [Input]
moreInput (_, _, SList []) = []
moreInput (p, k, SList (t:ts)) = (p, k, t) : moreInput (p, k, SList ts)
moreInput (p, k, a) = [(p, k, a)]

parseResultAll :: (Input -> ParseResult a) -> Input -> [ParseResult a]
parseResultAll f i = map f (moreInput i)

parseLit :: Input -> ParseResult Literal
parseLit (k, p, SLit v) = PResult (k, p, mklit v)
    where mklit (BoolLit b i) = BLit b i
          mklit (StringLit b i) = StrLit b i
          mklit (NumericLit b i) = NumLit b i
parseLit (_, _, v) = PError $ parseError v "literal"

parseVarName :: Input -> ParseResult VarName
parseVarName (k, p, SName v i) = PResult (k, p, VName v i)
parseVarName (_, _, v) = PError $ parseError v "variable name"

parseIdentifier :: Input -> ParseResult Identifier
parseIdentifier (k, p, SVar v a i) = 
    PResult $ (k,p,IdExpr (VName v i) (TName (VName a i) i) (lineNumber i))
parseIdentifier (_, _, v) = PError $ parseError v "identifier"

-- operator
parseOperator :: Input -> ParseResult Operator
parseOperator (k, p, SName v i) = PResult (k, p, OpName (VName v i))
parseOperator (_, _, v) = PError $ parseError v "operator"

-- operand
parseOperand :: Input -> ParseResult Operand
parseOperand (k, p, SList a) =
    case parseAllExpr (k, p, SList a) of
        (PError _) -> PError $ parseError (SList a) "operand"
        (PResult (k2, p2, exprs)) -> PResult (k2, p2, OprExpr exprs)

parseOperand (_, _, v) =PError $ parseError v "operand"
-- keyword
iskey :: String -> String -> Input -> Bool
iskey key given (keys, _, _) =
    if DMap.member key keys
    then let klst = keys DMap.! key
         in given `elem` klst
    else False

parseProcedureCall :: Input -> ParseResult ProcedureCall
parseProcedureCall (k, p, SList [SName s i, SName d j, SList a]) =
    let isDo = iskey "do" s (k, p, SName s i)
    in if isDo
       then case parseOperator (k, p, SName d j) of
                (PResult (k2, p2, oper)) ->
                    case parseOperand (k2, p2, SList a) of
                        (PResult (k3, p3, opera)) ->
                            PResult (k3, p3, Proc {op = oper, args = opera})
                        (PError _) -> PError $ parseError (SList a) "operand"
                (PError _) -> PError $ parseError (SList a) "operator"
       else PError $ parseError (SName s i) "procedure call marker"

parseProcedureCall (_, _, v) = PError $ parseError v "procedure call"
-- assignment statement
parseAssignment :: Input -> ParseResult Assign
parseAssignment (k, p, SList [SName s i, SVar a b j, expr]) =
    let isDef = iskey "def" s (k, p, SName s i)
    in if isDef
       then case parseIdentifier (k, p, SVar a b j) of
                (PResult (k2, p2, ids)) -> 
                    case parseExpr (k2, p2, expr) of
                        (PResult (k3, p3, e)) ->
                            PResult (k3, p3, Assigner ids e)
                        (PError _) -> PError $ parseError expr "assignment expression"
                (PError _) -> PError $ parseError expr "assignment identifier"
       else PError $ parseError (SName s i) "assignment marker"

parseAssignment (_, _, v) = PError $ parseError v "assignment"

-- sequence
parseSequence :: Input -> ParseResult Sequence
parseSequence (k, p, SList (SName s i: a)) =
    let isSeq = iskey "seq" s (k, p, SName s i)
    in if isSeq
       then case parseAllExpr (k, p, SList a) of
                (PResult (k2, p2, es)) -> 
                    case null es of
                        False -> PResult (k2, p2, fromExprToSeq es)
                        True -> PError $ parseError (SList a) "empty sequence expression"
                (PError _) -> PError $ parseError (SList a) "sequence expression"
       else PError $ parseError (SName s i) ("sequence marker in keys :" ++
           show k)

parseSequence (_, _, v) = PError $ parseError v "sequence"

-- conditional test
parseConditionTest :: Input -> ParseResult ConditionTest

-- conditional test procedure call
parseConditionTest (p, k, SList [SName s i, SName d j, SList a]) =
    let cToks = SList [SName s i, SName d j, SList a]
    in case parseProcedureCall (p, k, cToks) of
            (PResult (p2, k2, pcall)) -> PResult (p2, k2, CTestProc pcall)
            (PError _) -> PError $ parseError cToks "condition test procedural call"

parseConditionTest (p, k, SList [SLit v]) =
    case parseLit (p, k, SLit v) of
        (PResult (p2, k2, lval)) -> PResult (p2, k2, CTestLit lval)
        (PError _) -> PError $ parseError (SLit v) "condition test literal"

parseConditionTest (p, k, SList [SName s i]) =
    case parseVarName (p, k, SName s i) of
        (PResult (p2, k2, vname)) -> PResult (p2, k2, CTestVar vname)
        (PError _) -> PError $ parseError (SName s i) "condition test variable name"

parseConditionTest (_, _, v) = PError $ parseError v "condition test"

-- 

parseConsequent :: Input -> ParseResult Sequence
parseConsequent (k, p, SList [SName s i, SList (SName d j: a)]) =
    let isThen = iskey "then" s (k, p, SName s i)
        sToks = SList (SName d j:a)
    in if isThen
       then case parseSequence (k, p, sToks) of
                (PResult (k2, p2, pseq)) -> PResult (k2, p2, pseq)
                (PError _) -> PError $ parseError (SName s i) "consequence sequence"
       else PError $ parseError (SName s i) "consequence marker"


parseConsequent (_, _, v) = PError $ parseError v "consequent"


parseAlternate :: Input -> ParseResult Sequence
parseAlternate (k, p, SList [SName s i, SList (SName d j:a)]) =
    let isElse = iskey "else" s (k, p, SName s i)
        aToks = SList (SName d j:a)
    in if isElse
       then case parseSequence (k, p, aToks) of
                (PResult (k2, p2, pseq)) -> PResult (k2, p2, pseq)
                (PError _) -> PError $ parseError aToks "alternate sequence"
       else PError $ parseError (SName s i) "alternate marker"

parseAlternate (_, _, v) = PError $ parseError v "alternate"

-- parse conditional

parseConditional :: Input -> ParseResult Conditional
parseConditional 
    (k, p,
     SList [SName a b, -- if
            SList c, -- test
            SList [SName d e, SList (SName f g: h)], -- consequence
            SList [SName i j, SList (SName l m: n)] -- alternate
     ]) =
    let ifWordToks = SName a b
        isIfWord = iskey "if" a (k, p, ifWordToks)
        testToks = SList c
        consToks = SList [SName d e, SList (SName f g: h)]
        altToks = SList [SName i j, SList (SName l m: n)]
    in if isIfWord
       then 
            case parseConditionTest (k, p, testToks) of
                (PResult (k2, p2, cTest)) ->
                    case parseConsequent (k2, p2, consToks) of
                        (PResult (k3, p3, cseq)) ->
                            case parseAlternate (k3, p3, altToks) of
                                (PResult (k4, p4, calt)) ->
                                    PResult (k4, p4, 
                                        Cond {ctest = cTest,
                                              consequent = cseq,
                                              alternate = calt})
                                (PError _) -> PError $ parseError altToks "conditional alternate"
                        (PError _) -> PError $ parseError consToks "conditional consequent"
                (PError _) -> PError $ parseError testToks "conditional test"
       else PError $ parseError ifWordToks "conditional marker"

parseConditional (_, _, ts) = PError $ parseError ts "conditional"

-- loop
parseLoop :: Input -> ParseResult Loop
parseLoop 
    (k, p, 
     SList [SName a b, -- loop
            SList c, -- test
            SList [SName d e, SList [SName f g, SList h]] -- consequence
     ]) =
    let loopWordToks = SName a b
        isLoopWord = iskey "loop" a (k, p, loopWordToks)
        testToks = SList c
        consToks = SList [SName d e, SList [SName f g, SList h]]
    in if isLoopWord
       then case parseConditionTest (k, p, testToks) of
                (PResult (k2, p2, cTest)) ->
                    case parseConsequent (k2, p2, consToks) of
                        (PResult (k3, p3, cseq)) -> 
                            PResult (k3, p3, Looper {ltest = cTest,
                                                   lconsequent = cseq})
                        (PError _) -> PError $ parseError consToks "loop consequent"
                (PError _) -> PError $ parseError testToks "loop test"
       else PError $ parseError loopWordToks "loop"

parseLoop (_, _, v) = PError $ parseError v "loop"

-- procedure definition
parseProcedureDef :: Input -> ParseResult ProcedureDefinition
parseProcedureDef (k,p, 
                   SList [
                   SName a b, -- fn keyword
                   SVar c d e,
                   SList (SVar f g h:vars),
                   SList (SName i j: m)
                   ]
                   ) =
    let fnToks = SName a b
        idToks = SVar c d e
        argToks = (SVar f g h : vars)
        bodyToks = SList (SName i j: m)
        isFnWord = iskey "fn" a (k, p, fnToks)
    in if isFnWord
       then
            case parseIdentifier (k, p, idToks) of
                (PResult (k2, p2, ident)) ->
                    let farg = parseResultAll parseIdentifier (k2, p2, SList argToks)
                        fargs = reduceResult farg
                    in case parseSequence (k2, p2, bodyToks) of
                            (PResult (k3, p3, fbody)) ->
                                PResult (k3, p3, 
                                       DefineProc { procname = ident,
                                                    arguments = fargs,
                                                    body = fbody})
                            (PError _) -> PError $ parseError bodyToks "procedure definition body"
                (PError _) -> PError $ parseError idToks "procedure definition name"
       else PError $ parseError bodyToks "procedure definition marker"

parseProcedureDef (_,_,v) = PError $ parseError v "procedure definition"

-- statement

parseStatement :: Input -> ParseResult Statement
parseStatement (k, p, SList (SName n i: b)) =
    let sToks = (SName n i: b)
    in case parseAssignment (k, p, SList sToks) of
        (PResult (k2, p2, a)) -> PResult (k2, p2, AssignStmt a)
        (PError _) ->
            case parseConditional (k, p, SList sToks) of
                (PResult (k2, p2, a)) -> PResult (k2, p2, CondStmt a)
                (PError _) ->
                    case parseProcedureDef (k, p, SList sToks) of
                        (PResult (k2, p2, a)) -> PResult (k2, p2, ProcDefStmt a)
                        (PError _) ->
                            case parseLoop (k,p, SList sToks) of
                                (PResult (k2, p2, a)) -> PResult (k2, p2, LoopStmt a)
                                (PError _) ->
                                    case parseSequence (k, p, SList sToks) of
                                        (PResult (k2, p2, a)) -> PResult (k2, p2, SeqStmt a)
                                        (PError e) ->
                                            PError $ parseError (SList sToks) "statement sequence in " ++ e 

parseStatement (_, _, t) = PError $ parseError t "statement"

-- expression
parseExprAll :: Input -> [ParseResult Expr]
parseExprAll (_, _, SList []) = []
parseExprAll (k, p, SList ts) = parseResultAll parseExpr (k,p, SList ts)
parseExprAll (k, p,v) = error $ "can not have multiple expressions with " ++ show v

parseAllExpr :: Input -> ParseResult [Expr]
parseAllExpr (k,p, SList a) = PResult (k,p, reduceResult $ parseExprAll (k,p,SList a))
parseAllExpr (_,_, v) = PError $ parseError v "mulitple expression"

-- get expression
parseGetExpression :: Input -> ParseResult GetExpr
parseGetExpression (k, p, SName a i) =
    case parseVarName (k,p, SName a i) of
        (PResult (k2, p2, vname)) -> PResult (k2, p2, GetVName vname)
        (PError _) -> PError $ parseError (SName a i) "get expression variable name"

parseGetExpression (k, p, SLit v) =
    case parseLit (k, p, SLit v) of
        (PResult (k2, p2, lval)) -> PResult (k2, p2, GetLit lval)
        (PError _) -> PError $ parseError (SLit v) "get expression literal"

parseGetExpression (_, _, v) = PError $ parseError v "get expression"


parseExpr :: Input -> ParseResult Expr
-- procedure call
parseExpr (k,p, SList [SName s i, SName d j, SList a]) =
    let procToks = SList [SName s i, SName d j, SList a]
    in case parseProcedureCall (k, p, procToks) of
            (PResult (k2, p2, pcall)) -> 
                PResult (k2, p2, CallExpr pcall i)
            (PError _) -> PError $ parseError procToks "expression proc call"

-- parse Statements
parseExpr (k, p, SList (SName b j:a)) =
    let sToks = SList (SName b j:a)
        i = getSTreeTokInfo sToks
    in case parseStatement (k, p, sToks) of
            (PResult (k2, p2, stmt)) -> PResult (k2, p2, StmtExpr stmt i)
            (PError e) -> 
                PError $ parseError sToks "expression statement in " ++ e

parseExpr (k, p, SName t a) =
    case parseGetExpression (k, p, SName t a) of
            (PResult (k2, p2, gexpr)) -> PResult (k2, p2, GExpr gexpr)
            (PError _) -> PError $ parseError (SName t a) "expression get"

parseExpr (k, p, SLit t ) =
    case parseGetExpression (k, p, SLit t) of
            (PResult (k2, p2, gexpr)) -> PResult (k2, p2, GExpr gexpr)
            (PError _) -> PError $ parseError (SLit t) "expression get"

parseExpr (k, p, v) = PResult (k, p, EndExpr)

