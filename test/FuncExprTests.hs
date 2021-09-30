module FuncExprTests(fntests) where

import Lexer.Lexer
import Parser
import Eval.Evaluator
import Expression.Expression
import Test.HUnit

import qualified Data.Map as DMap

fnstr :: String
fnstr = "(seq (fn threearg: float \n\
                \ (var1: float var2: float var3: float) \n\ 
                \ (seq (do * ((do / (var1 var2)) var3)))\
            \ )\
            \ ( do threearg (4.0 2.0 3.0) )\
           \ )"

test1 :: Test
test1 = TestCase ( 
        assertEqual ("for seq expression " ++ fnstr) True 
        (exprCheck fnstr (GExpr (GetLit (NumLit 6.0 (mkTokInfo 0 0 "" "")))))
    )


fnstr2 :: String
fnstr2 = "(seq (fn two: float \n\
                \ (var1: float var2: float) \n\ 
                \ (seq (def var3: float (do * (var1 var1))) \
                \      (def var4: float (do + (var3 var2))) \
                \      var4 \
                \ )\
            \ )\
            \ ( do two (2.0 1.0) )\
           \ )"

test2 :: Test
test2 = TestCase ( 
        assertEqual ("for fn expression " ++ fnstr2) True 
        (exprCheck fnstr2 (GExpr (GetLit (NumLit 5.0 (mkTokInfo 0 0 "" "")))))
    )


fntests :: Test
fntests = TestList [
    TestLabel "fn test 1" test1,
    TestLabel "fn test 2" test2
    ]
