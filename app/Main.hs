module Main (main) where

import Eval.Evaluator
import System.Environment
import Data.List
import qualified Data.Map as DMap

import Parser.StatefulParser
import Eval.RunEval
import Lexer.Keyword

readKeywords :: String -> Keywords
readKeywords fileContents =
    let fileLines = lines fileContents
        (ln:lns) = map words fileLines
    in DMap.fromList (f (ln:lns))
    where f [] = []
          f (s:ss) = (head s, s): f ss

countItem :: Eq a => a -> [a] -> Int
countItem t ts = length (filter (== t) ts)
countItems :: Eq a => [a] -> [Int]
countItems [] = []
countItems (s:ss) = countItem s ss : countItems ss

hasUniqueKeywords :: Keywords -> Bool
hasUniqueKeywords ks =
    let keywordElements = DMap.elems ks
        nbElements = filter (> 1) $ countItems keywordElements
    in null nbElements


checkCommandArgs :: [String] -> Bool
checkCommandArgs [inpa, outp] = 
    let fCheck = ".lambda" `isSuffixOf` inpa
        oCheck = ".lambda" `isSuffixOf` outp
        oCheck1 = "stdout" == outp
    in fCheck && (oCheck || oCheck1)

checkCommandArgs [kfile, inpa, outp] = 
    checkKeywordFile kfile && checkCommandArgs [inpa, outp]

checkCommandArgs _ = False

checkKeywordFile :: String -> Bool
checkKeywordFile "keywords.lambda.txt" = True
checkKeywordFile _ = False

getKeywords :: String -> Keywords
getKeywords fcontents =
    let kwords = readKeywords fcontents
        hasUnique = hasUniqueKeywords kwords
    in if hasUnique
       then kwords
       else error $ "keywords file contains duplicate keywords:\n " ++ show kwords

evalCArgs2 :: (String, String) -> IO ()
evalCArgs2 (inpath, outpath) = do contents <- readFile inpath
                                  case outpath of
                                    "stdout" -> peval contents
                                    _ -> let expr = runEval contents
                                         in writeFile outpath (show expr)

evalCArgs :: (String,String, String) -> IO ()
evalCArgs (kwordsPath, inpath, outpath) = 
    do contents <- readFile inpath;
       kfcontents <- readFile kwordsPath;
       let kws = getKeywords kfcontents in case outpath of
                                                "stdout" -> peval2 contents kws
                                                _ -> let expr = runEval2 contents kws
                                                     in writeFile outpath (show expr)




main :: IO ()
main = do
    commandArgs <- getArgs
    case length commandArgs of
        2 -> let cargs = commandArgs
                 fCheck = checkCommandArgs cargs
             in if fCheck
                then let [inpa, outpath] = cargs
                     in evalCArgs2 (inpa, outpath)
                else emsg
        3 -> let cargs = commandArgs
                 fCheck = checkCommandArgs cargs
             in if fCheck
                then let [kws, inpa, outpath] = cargs
                     in evalCArgs (kws, inpa, outpath)
                else emsg
        _ -> error "must have 2 (or 3) arguments keyword file (optional), input file and output file"

    where emsg = let msg = "input file does not have the correct extension"
                     msg2 = msg ++ " .lambda"
                 in error msg2
            
