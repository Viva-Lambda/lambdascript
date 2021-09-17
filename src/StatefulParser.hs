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

