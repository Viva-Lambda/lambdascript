module SeqExprTests(seqtests) where

import Lexer
-- import Parser
import CombinedParser
import Evaluator
import Expression
import Test.HUnit

import qualified Data.Map as DMap

seqString :: String
seqString = "(seq (def var1: float 2.5)\n (do * (var1 2.0))\n)"

test1 :: Test
test1 = TestCase (
    assertEqual
    ("for seq expression " ++ seqString)
    True (exprCheck seqString (LiteralExpr (NumLit 5.0 (mkTokInfo 0 0 "" ""))))
    )

seqString2 :: String
seqString2 = "(seq (def var1: float 2.5)\n\
                 \ (def var1: float (do * (var1 2.0)))\n\
                 \ (do / (var1 2.5)) )"

test2 :: Test
test2 = TestCase (
    assertEqual
    ("for seq expression " ++ seqString2)
    True (exprCheck seqString2 (LiteralExpr (NumLit 2.0 (mkTokInfo 0 0 "" ""))))
    )

seqtests :: Test
seqtests = TestList [
    TestLabel "seq test 1" test1,
    TestLabel "seq test 2" test2
    ]
