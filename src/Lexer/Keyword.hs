-- keyword module
module Lexer.Keyword where

import qualified Data.Map as DMap

type Keywords = DMap.Map String [String]

emptyKeywords :: Keywords
emptyKeywords = DMap.empty

defaultKWords :: Keywords
defaultKWords = DMap.fromList [
                            ("do", ["do", "yap"]),
                            ("seq", ["seq", "list"]),
                            ("fn", ["fn", "edim"]),
                            ("if", ["if", "eger"]),
                            ("loop", ["loop", "dongu"]),
                            ("then", ["then", "ise"]),
                            ("else", ["else", "yoksa"]),
                            ("def", ["def", "tanim"]),
                            ("(", ["("]),
                            (")", [")"])
                ]

iskey :: String -> String -> Keywords -> Bool
iskey key given keys =
    if DMap.member key keys
    then let klst = keys DMap.! key
         in given `elem` klst
    else False

