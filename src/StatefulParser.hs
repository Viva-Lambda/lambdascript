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


data ParseResult a = Either String (Keywords, ParsingState, a)

result :: ParseResult a -> Either String a
result (Right (_,_,a)) = Right a
result (Left s) = Left s

reduceResult :: [ParseResult a] -> [a]
reduceResult [] = []
reduceResult (t:ts) =
    case result t of
        (Right a) -> a : reduceResult ts
        (Left err) -> []

instance Functor ParseResult where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (Right (k, p, a, t)) = Right (k, p, f a, t)
    fmap f (Left err) = Left err


parseError :: STree -> String -> String
parseError t m = 
    let msg = "can not parse token(s) " ++ show t ++ " as " ++ m ++ " at "
        msg2 = msg ++ debugSTree t
    in msg2

lookAHead :: [Token] -> Token
accept :: [Token] -> [Token]

lookAHead [] = TokEnd
lookAHead (t:ts) = t

accept [] = error "nothing to accept"
accept (t:ts) = ts

type Input = (Keywords, ParsingState, STree)
type Output = (ParsingState, Expr)

parseExpr :: Input -> ParseResult Expr

parseExprAll :: Input -> [ParseResult Expr]
parseExprAll (p, k, SList a) = map parseExpr a
parseExprAll (p, k, a) = [parseExpr a]
parseAllExpr :: Input -> ParseResult [Expr]
parseAllExpr i = reduceResult $ parseExprAll i

parseLit :: Input -> ParseResult Literal
parseLit (k, p, SLit v) = Right (k, p, mklit v)
    where mklit (BoolLit b i) = BLit b i
          mklit (StringLit b i) = StrLit b i
          mklit (NumericLit b i) = NumLit b i
parseLit (k, p, v) = Left $ parseError v "literal"

parseVarName :: Input -> ParseResult VarName
parseVarName (k, p, SName v i) = Right (k, p, VName v i)
parseVarName (_, _, v) = Left $ parseError v "variable name"

parseIdentifier :: Input -> ParseResult Identifier
parseIdentifier (k, p, SVar v a i) = 
    IdExpr (VName v i) (TName (VName a i) i) (lineNumber i)
parseIdentifier (_, _, v) = Left $ parseError v "identifier"

-- operator
parseOperator :: Input -> ParseResult Operator
parseOperator (k, p, SName v i) = Right (k, p, OpName (VName v i))
parseOperator (k, p, v) = Left $ parseError v "operator"

-- keyword
iskey :: String -> String -> Input -> Bool
iskey key given (keys, p, v) =
    if member key keys
    then let klst = keys DMap.! key
         in given `elem` klst
    else False

parseProcedureCall :: Input -> ParseResult ProcedureCall
parseProcedureCall (k, p, SList [SName s i, SName d j, SList a]) =
    let isDo = iskey "do" s (k, p, SName s i)
        (k2, p2, oper) = parseOperator (k, p, SName d j)
        (k3, p3, opera) = parseOperand (k2, p2, a)
    in if isDo
       then Right (k3, p3, Proc {op = oper, args = opera})
       else Left $ parseError (SName s i) "call expression marker"

-- assignment statement
parseAssignment :: Input -> ParseResult Assign
parseAssignment (k, p, SList [SName s i, SVar a b j, expr]) =
    let isDef = iskey "def" s (k, p, SName s i)
        (k2, p2, ids) = parseIdentifier (k, p, SVar a b j)
        (k3, p3, e) = parseExpr (k2, p2, expr)
    in if isDef
       then Right (k3, p3, Assigner ids e)
       else Left $ parseError (SName s i) "set expression marker"

-- sequence
parseSequence :: Input -> ParseResult Sequence
parseSequence (p, k, SList [SName s i, SList a]) =
    let isSeq = iskey "seq" s (k, p, SName s i)
    in if isSeq
       then let (k2, p2, es) = parseAllExpr $ map parseExpr a
            in Right (k2, p2, reduceExpr es)
       else Left $ parseError (SName s i) "sequence"


-- conditional test
parseConditionTest :: Input -> ParseResult ConditionTest

parseConditionTest (p, k, SList [SName s i, SName d j, SList a]) =
    let (p2, k2, pcall) = parseProcedureCall (p, k, SList [SName s i, SName d j, SList a])
    in Right (p2, k2, CTestProc pcall)

parseConditionTest (p, k, SList [SLit v]) =
    let (p2, k2, lval) = parseLit (p, k, SLit v)
    in Right (p2, k2, CTestLit lval)

parseConditionTest (p, k, SList [SName s i]) =
    let (p2, k2, vname) = parseVarName (p, k, SName s i)
    in Right (p2, k2, CTestVar vname)

parseConditionTest (p, k, v) = Left $ parseError v "condition test"

-- 
