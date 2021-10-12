-- parser utility functions
module Parser.ParseUtils where

import Parser.ParseResult
import Parser.ASTree
import Parser.ParseError
import Lexer.Keyword
import Parser.ParsingState

type Input = (Keywords, ParsingState, STree)
type InputNoState = (Keywords, STree)

parseWithSideEffects :: (InputNoState -> StatelessParseResult a) -> Input -> ParseResult a
parseWithSideEffects f (k, p, a) =
    case f (k, a) of
        (StatelessPError e) -> PError e
        (StatelessPResult (k2, b)) -> PResult (k2, p, b)


noSideResult :: StatelessParseResult a -> Either String a
noSideResult (StatelessPResult (_, a)) = Right a
noSideResult (StatelessPError s) = Left s

result :: ParseResult a -> Either String a
result a = case a of
               (PError e) -> Left e
               (PResult (_, _, b)) -> Right b

noSideMapPResult :: (InputNoState -> StatelessParseResult a) -> InputNoState -> [StatelessParseResult a]
noSideMapPResult _ (_, SList []) = []
noSideMapPResult f (a, SList (e:es)) = 
    (f (a, e)) : noSideMapPResult f (a, SList es)

noSideMapPResult _ (_, a) = error $ parseError a "mapping function to given tokens"

mapPResult :: (Input -> ParseResult a) -> Input -> [ParseResult a]
mapPResult _ (_, _, SList []) = []
mapPResult f (a, b, SList (e:es)) = (f (a, b, e)) : mapPResult f (a, b, SList es)
mapPResult _ (_, _, a) = error $ parseError a "mapping function to given tokens"

mapReducePResult :: (Input -> ParseResult a) -> Input -> [a]
mapReducePResult f a = reducePResult $ mapPResult f a

noSideMapReducePResult :: (InputNoState -> StatelessParseResult a) -> InputNoState -> [a]

noSideMapReducePResult f a = noSideReducePResult $ noSideMapPResult f a

noSideReducePResult :: [StatelessParseResult a] -> [a]
noSideReducePResult [] = []
noSideReducePResult (t:ts) =
    case noSideResult t of
        (Right a) -> a : noSideReducePResult ts
        (Left _) -> []

reducePResult :: [ParseResult a] -> [a]
reducePResult [] = []
reducePResult (t:ts) =
    case result t of
        (Right a) -> a : reducePResult ts
        (Left _) -> []

isParsed :: ParseResult a -> Bool
isParsed (PResult _) = True
isParsed (PError _) = False

noSideIsParsed :: StatelessParseResult a -> Bool
noSideIsParsed (StatelessPResult _) = True
noSideIsParsed (StatelessPError _) = False
