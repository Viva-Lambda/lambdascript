-- parse result module
module Parser.ParseResult where

import Lexer.Keyword
import Parser.ParsingState

data ParseResult a = PError String 
                     | PResult (Keywords, ParsingState, a)
                     deriving (Show)
