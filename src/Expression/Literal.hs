-- literal module
module Expression.Literal where

import Lexer.Lexer

data Literal = BLit Bool TokenInfo
              | StrLit String TokenInfo
              | NumLit Double TokenInfo

instance Show Literal where
    show (BLit b _) = if b
                      then "true"
                      else "false"
    show (StrLit s _) = "\""++ s ++ "\""
    show (NumLit s _) = show s

instance Eq Literal where
    (BLit a _) == (BLit b _) = a == b
    (BLit _ _) == _ = False

    (StrLit a _) == (StrLit b _) = a == b
    (StrLit _ _) == _ = False

    (NumLit a _) == (NumLit b _) = a == b
    (NumLit _ _) == _ = False
