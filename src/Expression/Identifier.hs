-- identifier module
module Expression.Identifier where

import Lexer.Lexer

type LineInfo = Int

data Identifier = IdExpr IdentifierName TypeName LineInfo -- parsed - eval

instance Show Identifier where
    show (IdExpr a b _) = show a ++ ": " ++ show b

instance Eq Identifier where
    (IdExpr a b _) == (IdExpr c d _) = (a == c) && (b == d)

data VarName = VName String TokenInfo

type TypeName = VarName
type IdentifierName = VarName
type ModuleName = VarName

instance Show VarName where
    show (VName v _) = v

instance Eq VarName where
    (VName v _) == (VName a _) = v == a

