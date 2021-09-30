-- parser utility functions
module Parser.ParseUtils where

import Parser.ParseResult
import Parser.ASTree
import Parser.ParseError
import Lexer.Keyword
import Parser.ParsingState

type Input = (Keywords, ParsingState, STree)

result :: ParseResult a -> Either String a
result (PResult (_,_,a)) = Right a
result (PError s) = Left s

mapPResult :: (Input -> ParseResult a) -> Input -> [ParseResult a]
mapPResult _ (_, _, SList []) = []
mapPResult f (a, b, SList (e:es)) = (f (a, b, e)) : mapPResult f (a, b, SList es)
mapPResult _ (_, _, a) = error $ parseError a "mapping function to given tokens"

mapReducePResult :: (Input -> ParseResult a) -> Input -> [a]
mapReducePResult f a = reducePResult $ mapPResult f a

reducePResult :: [ParseResult a] -> [a]
reducePResult [] = []
reducePResult (t:ts) =
    case result t of
        (Right a) -> a : reducePResult ts
        (Left _) -> []

isParsed :: ParseResult a -> Bool
isParsed (PResult _) = True
isParsed (PError _) = False
