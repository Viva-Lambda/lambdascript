module Main where

import Test.HUnit
import IfExprTests
import SeqExprTests
import FuncExprTests
import LoopExprTests


main :: IO Counts
main = do
    let (TestList iftlist) = iftests
        (TestList sqtlist) = seqtests
        (TestList fns) = fntests
        (TestList loops) = looptests
    let tests = TestList (iftlist ++ sqtlist ++ fns ++ loops)
    runTestTT tests 

