module Main (main) where

import Evaluator
import System.Environment
import Data.List


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
            
