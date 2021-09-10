-- standard boolean expression functions
module FnBool where

import Lexer
import Expression

boolBinExprFn :: (Bool -> Bool -> Bool) -> Expr -> Expr -> Expr
boolBinExprFn fn (LiteralExpr (BLit a i)) (LiteralExpr (BLit b j)) =
    let val = a `fn` b
        TokInfo {lineNumber = ta, colNumber = tc,
                 tokDescription = td, tokContext = tcon} = joinTokInfo i j
        ndescr = "line: " ++ show ta ++ " " ++ td
        info = mkTokInfo ta tc ndescr tcon
    in LiteralExpr ( BLit val info)

boolBinExprFn _ a b =
    let linea = debugExpr a
        lineb = debugExpr b
        msg = "Expressions: " ++ linea ++ " " ++ lineb
        msg2 = msg ++ " must be boolean expressions such as true, false"
    in error msg2

andBool :: Expr -> Expr -> Expr
andBool a b = boolBinExprFn (&&) a b

orBool :: Expr -> Expr -> Expr
orBool a b = boolBinExprFn (||) a b
