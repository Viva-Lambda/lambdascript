module LoopExprTests(looptests) where

import Lexer.Lexer
-- import Parser
--import CombinedParser
import Eval.Evaluator
--
import Expression.Expression
import Expression.Identifier
import Expression.Literal
import Eval.RunEval
import Test.HUnit

-- import qualified Data.Map as DMap

loopstr :: String
loopstr = "(seq (def x: float 0.0)\
              \ (loop (do < (x 10.0))\
                    \ (then (seq (def x: float (do + (x 1.0)))))\
              \ )\
              \ x \
         \ )"

test1 :: Test
test1 = TestCase ( 
        assertEqual ("for loop expression " ++ loopstr) True 
        (exprCheck 
            loopstr 
            (GExpr (GetLit (NumLit 10.0 (mkTokInfo 0 0 "" "" ""))))
        )
    )


looptests :: Test
looptests = TestList [
    TestLabel "loop test 1" test1
    ]
