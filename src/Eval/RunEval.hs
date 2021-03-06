-- start evaluator
module Eval.RunEval where

import Lexer.Lexer
import Lexer.Keyword
import Parser.ASTree
import Parser.ParsingState
import Parser.StatefulParser
import Parser.ParseResult
import Expression.Expression
import Expression.ExprUtils
import Expression.Debug
import Eval.Evaluator
import qualified Data.Map as DMap
import Control.Monad.State.Lazy

-- test functions
exprCheck :: String -> Expr -> Bool
exprCheck arg expected =
    let kws = defaultKWords
        lefts = kws DMap.! "("
        rights = kws DMap.! ")"
        toks = tokenize "" (lefts, rights) arg 0 0
        pexps = expression (defaultKWords, emptyState, parseAll toks)
    in
        case pexps of
            (PResult (_, _, res)) ->
                let act = runState $ evaluate res
                    (expr, _) = act (DMap.fromList [("k", EndExpr)])
                    expb = expr == expected

                in if expb
                   then True
                   else let msg = "unexpected " ++ debugExpr expr
                        in error $ msg ++ " parsed: " ++ debugExpr res
            (PError e) -> error $ "Error: " ++ e


runEval2 :: String -> String -> Keywords -> Expr
runEval2 fp toks kws =
    let lefts = kws DMap.! "("
        rights = kws DMap.! ")"
        tp = tokenize fp (lefts, rights) toks 0 0
        stree = parseAll tp
        pexps = expression $ (kws, emptyState, stree)
    in
        case pexps of
            (PResult (_, _, res)) -> let act = runState $ evaluate res
                                         (expr, _) = act (DMap.fromList [("f", EndExpr)])
                                in expr
            (PError e) -> error $ "Error: " ++ show e


runEval :: String -> String -> Expr
runEval fp toks = runEval2 fp toks defaultKWords
    

peval :: String -> String -> IO ()
peval fp toks = print $ runEval fp toks
peval2 :: String -> String -> Keywords -> IO ()
peval2 fp toks kws = print $ runEval2 fp toks kws
-- (+ 1 2)
-- (set var1 (+ 1 564))
