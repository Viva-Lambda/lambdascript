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


data ParseResult a = Either String (Keywords, ParsingState, a, [Token])

result :: ParseResult a -> Either String a
result (Right (_,_,a,_)) = Right a
result (Left s) = Left s

resultTokens :: ParseResult a -> Either String (a, [Token])
resultTokens (Right (_,_,a,t)) = Right (a,t)
resultTokens (Left s) = Left s

reduceResult :: [ParseResult a] -> [a]
reduceResult [] = []
reduceResult (t:ts) =
    case result t of
        (Right a) -> a : reduceResult ts
        (Left err) -> []

reduceResultTokens :: [ParseResult a] -> [(a, [Token])]
reduceResultTokens [] = []
reduceResultTokens (t:ts) =
    case resultTokens t of
        (Right a) -> a : reduceResultTokens ts
        (Left _) -> []


instance Functor ParseResult where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (Right (k, p, a, t)) = Right (k, p, f a, t)
    fmap f (Left err) = Left err


parseError :: Token -> String -> String
parseError t m = 
    let msg = "can not parse token(s) " ++ show t ++ " as " ++ m ++ " at "
        msg2 = msg ++ debugTokenInfo (getTokenInfo t)
    in msg2


lookAHead :: [Token] -> Token
accept :: [Token] -> [Token]

lookAHead [] = TokEnd
lookAHead (t:ts) = t

accept [] = error "nothing to accept"
accept (t:ts) = ts

type Input = (Keywords, ParsingState, STree)
type Output = (ParsingState, Expr)

