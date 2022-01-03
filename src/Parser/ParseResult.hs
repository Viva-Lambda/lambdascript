-- parse result module
module Parser.ParseResult where

import Lexer.Keyword
import Parser.ParsingState
import Parser.ParseError

data StatelessParseResult a = StatelessPError String
                            | StatelessPResult (Keywords, a)
                            deriving (Show)

data ParseResult a = PError String
                     | PResult (Keywords, ParsingState, a)
                     deriving (Show)
