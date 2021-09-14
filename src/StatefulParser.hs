module StatefulParser where

import Lexer hiding (number)
import Expression

-- import Text.ParserCombinators.ReadP
import Control.Applicative
import qualified Data.Map as DMap
import Control.Monad.State.Lazy


type Input = [Token]
type Rest = Input

type ParsingState = DMap.Map String [String]

emptyState :: ParsingState
emptyState = DMap.empty


newtype Parser a = P { parse :: Input -> ParseResult a}

newtype StatefulParser a = StParser (ParsingState -> (Parser a, ParsingState))

data ParseResult a = Error ParseError
                   | Result a Rest 
                   deriving Eq

data ParseError =
    UnexpectedEof -- hit end of file when we expected more input
  | ExpectedEof Rest -- should have successfully parsed everything but there's more!
  | UnexpectedToken Token
  | UnexpectedInput Input
  deriving (Eq, Show)

