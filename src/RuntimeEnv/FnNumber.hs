-- standard number expression functions
module RuntimeEnv.FnNumber where


import Expression.Expression
import Expression.Identifier
import Expression.Literal
import Expression.Debug
import Lexer.Lexer

-- numeric expression function
numBinExprFn :: (Double -> Double -> Double) -> Expr -> Expr -> Expr
numBinExprFn fn (GExpr (GetLit (NumLit a i))) (GExpr (GetLit (NumLit b _))) =
    let val = a `fn` b
    in GExpr $ GetLit (NumLit val i)

numBinExprFn _ a b = 
    let linea = debugExpr a
        lineb = debugExpr b
        msg = "Expressions: " ++ linea ++ " " ++ lineb
        msg2 = msg ++ " must be numeric expressions such as 1.0, 3.38 etc"
    in error msg2

cmpBinExprFn :: (Double -> Double -> Bool) -> Expr -> Expr -> Expr
cmpBinExprFn fn (GExpr (GetLit (NumLit a i))) (GExpr (GetLit (NumLit b _))) = 
    let val = a `fn` b
    in GExpr $ GetLit (BLit val i)

cmpBinExprFn _ a b = 
    let linea = debugExpr a
        lineb = debugExpr b
        msg = "Expressions: " ++ linea ++ " " ++ lineb
        msg2 = msg ++ " must be numeric expressions such as 1.0, 3.38 etc"
    in error msg2

-- basic arithmetic

add2Number :: Expr -> Expr -> Expr
add2Number a b = numBinExprFn (+) a b

sub2Number :: Expr -> Expr -> Expr
sub2Number a b = numBinExprFn (-) a b

multiply2Number :: Expr -> Expr -> Expr
multiply2Number a b = numBinExprFn (*) a b

divide2Number :: Expr -> Expr -> Expr
divide2Number a (GExpr (GetLit (NumLit 0.0 i))) = 
    error $ "zero division at line " ++ show i
divide2Number a b = numBinExprFn (/) a b

power2Number :: Expr -> Expr -> Expr
power2Number a b = numBinExprFn (**) a b

numericOps :: [String]
numericOps = ["+", "-", "*", "/", "^"]
