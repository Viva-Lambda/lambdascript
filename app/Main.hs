module Main (main) where

import Evaluator
import System.Environment
import Data.List
import qualified Data.Map as DMap

readKeywords :: String -> DMap.Map String [String]
readKeywords fileContents =
    let fileLines = lines fileContents
        (ln:lns) = map words fileLines
    in DMap.fromList (f (ln:lns))
    where f [] = []
          f (s:ss) = (head s, tail s): f ss


checkCommandArgs :: [String] -> Bool
checkCommandArgs [_,inpa,outp] = 
    let fCheck = ".lambda" `isSuffixOf` inpa
        oCheck = ".lambda" `isSuffixOf` outp
        oCheck1 = "stdout" == outp
    in fCheck && (oCheck || oCheck1)
checkCommandArgs _ = False

checkKeywordFile :: String -> Bool
checkKeywordFile "keywords.lambda.txt" = True
checkKeywordFile _ = False



main :: IO ()
main = do
    commandArgs <- getArgs
    case length commandArgs of
        2 -> let [inpath, outpath] = commandArgs
                 fCheck = ".lambda" `isSuffixOf` inpath
             in
                case fCheck of
                    False ->
                        let msg = "input file does not have the correct extension"
                            msg2 = msg ++ " .lambda"
                        in error msg2
                    True -> do
                        contents <- readFile inpath
                        case outpath of
                                "stdout" -> peval contents
                                _ -> let expr = runEval contents
                                     in writeFile outpath (show expr)

        _ -> error "must have two arguments input file and output file"
            
