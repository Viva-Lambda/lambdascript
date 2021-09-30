module IfExprTests(iftests) where

import Lexer.Lexer
-- import Parser
import Eval.Evaluator
import Expression.Expression
import Test.HUnit

import qualified Data.Map as DMap

ifString :: String
ifString = "(if (do < (1.0 3.4) ) (then (seq \"Merhaba\")) (else (seq \"Dunya\")) )"

ifString2 :: String
ifString2 = "(if (do < (6.0 3.4) ) (then (seq \"Merhaba\")) (else (seq \"Dunya\")) )"

ifBoolExpr1 :: String
ifBoolExpr1 = "(if (do < (1.0 3.4) ) (then (seq false)) (else (seq true)) )"

ifBoolExpr2 :: String
ifBoolExpr2 = "(if (do < (6.0 3.4) ) (then (seq false)) (else (seq true)) )"

ifNumExpr1 :: String
ifNumExpr1 = "(if (do < (1.0 3.4) ) (then (seq 1.05)) (else (seq 0.5)) )"

ifNumExpr2 :: String
ifNumExpr2 = "(if (do < (6.0 3.4) ) (then (seq 1.05)) (else (seq 0.5)) )"

ifUnaryExpr1 :: String
ifUnaryExpr1 = "(if (do < (1.0 3.4) )\
              \ (then (seq (do ~ (false))))\
              \ (else (seq (do ~ (true)) ) ) )"

ifUnaryExpr2 :: String
ifUnaryExpr2 = "(if (do < (6.0 3.4) )\
              \ (then (seq (do ~ (false)))) \
              \ (else (seq (do ~ (true)) )))"

ifUnaryExpr3 :: String
ifUnaryExpr3 = "(if (do < (1.0 3.4) )\
              \ (then (seq (do - (1.0))))\ 
              \ (else (seq (do - (3.0)))) )"

ifUnaryExpr4 :: String
ifUnaryExpr4 = "(if (do < (6.0 3.4) )\
              \ (then (seq (do - (1.0))))\ 
              \ (else (seq (do - (3.0)))) )"

ifBinaryExpr1 :: String
ifBinaryExpr1 = 
    "(if (do < (1.0 3.4) ) \
    \ (then (seq (do - (1.0 5.0)))) \
    \ (else (seq (do + (3.0 6.0) ) ) ))"

ifBinaryExpr2 :: String
ifBinaryExpr2 = "(if (do < (6.0 3.4))\ 
               \ (then (seq (do - (1.0 5.0)))) \ 
               \ (else (seq (do + (3.0 6.0)) ) ) )"


ifIfExpr1 :: String
ifIfExpr1 = "(if (do < (1.0 3.4))\n\ 
           \     (then (seq (if (do > (1.0 0.5))\n\ 
           \                    (then (seq (do + (3.0 6.0))))\n\ 
           \                    (else (seq (do * (2.0 4.0))))\
                          \ )\
           \            )\
           \     )\
               \ (else (seq (do / (4.0 2.0)) ) )\
           \ ) "

ifIfExpr2 :: String
ifIfExpr2 =  "(if (do < (6.0 3.4) )\
            \     (then (seq (if (do > (1.0 0.5))\
            \            (then (seq (do + (3.0 6.0))))\
            \            (else (seq (do * (2.0 4.0))))\
            \            )))\
            \     (else (seq (do / (4.0 2.0)))) )"


ifExprCheck :: String -> Expr -> Bool
ifExprCheck = exprCheck
    

test1 :: Test
test1 = TestCase ( assertEqual 
    ("for if expression: " ++ ifString)
    True (ifExprCheck ifString (GExpr ( GetLit  (StrLit "\"Merhaba\""
    (mkTokInfo 0 0 "" "")))) ))

test2 :: Test
test2 = TestCase ( assertEqual 
    ("for if expression: " ++ ifString2)
    True (ifExprCheck ifString2 (GExpr (GetLit  (StrLit "\"Dunya\"" (mkTokInfo
    0 0 "" "")))) ))

test3 :: Test
test3 = TestCase ( 
    assertEqual
    ("for if expression: " ++ ifBoolExpr1)
    True (ifExprCheck ifBoolExpr1 (GExpr (GetLit  (BLit False (mkTokInfo 0 0 "" ""))))
    ))

test4 :: Test
test4 = TestCase ( 
    assertEqual 
    ("for if expression: " ++ ifBoolExpr2)
    True (ifExprCheck ifBoolExpr2 (GExpr (GetLit  (BLit True (mkTokInfo 0 0 "" ""))))
    ))
 
test5 :: Test
test5 = TestCase ( 
    assertEqual 
    ("for if expression: " ++ ifNumExpr1)
    True (ifExprCheck ifNumExpr1 (GExpr (GetLit  (NumLit 1.05 (mkTokInfo 0 0 "" "")) ))
    ))

test6 :: Test
test6 = TestCase ( 
    assertEqual 
    ("for if expression: " ++ ifNumExpr2)
    True (ifExprCheck ifNumExpr2 (GExpr (GetLit  (NumLit 0.5 (mkTokInfo 0 0 "" "")) ))
    ))

test7 :: Test
test7 = TestCase (
    assertEqual 
    ("for if expression: " ++ ifUnaryExpr1)
    True (ifExprCheck ifUnaryExpr1 (GExpr (GetLit  (BLit True (mkTokInfo 0 0 "" "")) ))
    ))

test8 :: Test
test8 = TestCase (
    assertEqual 
    ("for if expression: " ++ ifUnaryExpr2)
    True (ifExprCheck ifUnaryExpr2 (GExpr (GetLit  (BLit False (mkTokInfo 0 0 "" "")) ))
    ))

test9 :: Test
test9 = TestCase (
    assertEqual 
    ("for if expression: " ++ ifUnaryExpr3)
    True (ifExprCheck ifUnaryExpr3 (GExpr (GetLit  (NumLit (-1.0) (mkTokInfo 0 0 "" ""))))
    ))

test10 :: Test
test10 = TestCase (
    assertEqual 
    ("for if expression: " ++ ifUnaryExpr4)
    True (ifExprCheck ifUnaryExpr4 (GExpr (GetLit  (NumLit (-3.0) (mkTokInfo 0 0 "" ""))))
    ))

test11 :: Test
test11 = TestCase (
    assertEqual 
    ("for if expression: " ++ ifBinaryExpr1) 
    True (ifExprCheck ifBinaryExpr1 (GExpr (GetLit  (NumLit (-4.0) (mkTokInfo 0 0 "" ""))))
    ))

test12 :: Test
test12 = TestCase (
    assertEqual 
    ("for if expression: " ++ ifBinaryExpr2)
    True (ifExprCheck ifBinaryExpr2 (GExpr (GetLit  (NumLit (9.0) (mkTokInfo 0 0 "" ""))))
    ))

test13 :: Test
test13 = TestCase (
    assertEqual 
    ("for if expression: " ++ ifIfExpr1)
    True (ifExprCheck ifIfExpr1 (GExpr (GetLit  (NumLit (9.0) (mkTokInfo 0 0 "" ""))))
    ))

test14 :: Test
test14 = TestCase (
    assertEqual 
    ("for if expression: " ++ ifIfExpr2)
    True (ifExprCheck ifIfExpr2 (GExpr (GetLit  (NumLit (2.0) (mkTokInfo 0 0
    "" "")))))
    )


iftests :: Test
iftests = TestList [
    TestLabel "if test 1" test1,
    TestLabel "if test 2" test2,
    TestLabel "if test 3" test3,
    TestLabel "if test 4" test4,
    TestLabel "if test 5" test5,
    TestLabel "if test 6" test6,
    TestLabel "if test 7" test7,
    TestLabel "if test 8" test8,
    TestLabel "if test 9" test9,
    TestLabel "if test 10" test10,
    TestLabel "if test 11" test11,
    TestLabel "if test 12" test12,
    TestLabel "if test 13" test13,
    TestLabel "if test 14" test14
    ]
