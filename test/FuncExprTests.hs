module FuncExprTests(fntests) where

import Lexer
import Parser
import Evaluator
import Expression
import Test.HUnit

import qualified Data.Map as DMap

fnstr :: String
fnstr = "(seq (fn threearg \n\
                \ (var1 var2 var3)\n\ 
                \ (seq (do * ((do / (var1 var2)) var3)))\
            \ )\
            \ (do threearg (4.0 2.0 3.0)))"

test1 :: Test
test1 = TestCase ( 
        assertEqual ("for seq expression " ++ fnstr) True 
        (exprCheck fnstr (LiteralExpr (NumLit 6.0) 0))
    )


fntests :: Test
fntests = TestList [
    TestLabel "fn test 1" test1
    ]
