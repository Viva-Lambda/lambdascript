-- lexer module
module Lexer where

import Data.Char
import Text.Read
import Data.List
import Prelude    hiding ( span )

{-
List of tokens a subset of scheme from
https://schemers.org/Documents/Standards/R5RS/r5rs.pdf

token := <identifier> | <boolean> | <number>
         | <string> | ( | )
         | + | - | * | /


-}
type LineNumber = Int
type ColumnNumber = Int

data TokenInfo = TokInfo LineNumber ColumnNumber

instance Show TokenInfo where
    show (TokInfo l c) = "line: " ++ show l ++ " " ++ "column: " ++ show c

instance Eq TokenInfo where
    (TokInfo l c) == (TokInfo l2 c2) = (l == l2) && (c == c2)

data Token = TokLPar TokenInfo -- (
           | TokRPar TokenInfo -- )
           | TokSymbol String TokenInfo -- <letter>+<digit>*
           | TokOp Char TokenInfo -- +*/-&|~
           | TokSep Char TokenInfo -- +*/-&|~
           -- datum
           | TokBool Bool TokenInfo -- T, F
           | TokString String TokenInfo -- "fdsl"
           | TokNumber Double TokenInfo -- 1 1.054
           | TokEnd
           deriving (Show, Eq)


-- span function for multi character tokens


tokenize :: String -> Int -> Int -> [Token]

tokenize [] a b = []

tokenize (x:xs) line col
  -- x is ,
  | x == ',' || x == ':' = TokSep x (TokInfo line col) : tokenize xs line (col + 1)
  -- x marks the beginning of a comment
  | x == ';' = skipComments x xs line col
  -- x is a paranthesis
  | x == '(' = TokLPar (TokInfo line col) : tokenize xs line (col + 1)
  | x == ')' = TokRPar (TokInfo line col) : tokenize xs line (col + 1)
  -- x is an operator +-*/
  | x `elem` "-+*/&|~!<>=" = TokOp x (TokInfo line col) : tokenize xs line (col +1)
  -- x is a symbol
  | isAlpha x = symbols x xs line col
  -- x is a start of string
  | x == '"' = stringToken x xs line col
  -- x token is number or not
  | isDigit x = number x xs line col
  | x == '\n' = tokenize xs (line + 1) 0
  | isSpace x = tokenize xs line (col + 1)
  | otherwise = let msg = show line ++ " column " ++ show col
                in error $ [x] ++ " can not be tokenized at line " ++ msg


symbolStr :: String -> (String, String)
symbolStr = span isAlphaNum

symbols :: Char -> String -> Int -> Int -> [Token]
symbols s str line col
  | s == 't' && isPrefixOf "rue" str =
    let nstr = drop (length "rue") str
        lentrue = length "true"
    in TokBool True (TokInfo line col) : tokenize nstr line (col + lentrue) 
  | s == 'f' && isPrefixOf "alse" str =
    let nstr = drop (length "alse") str
        lenfalse = length "false"
    in TokBool False (TokInfo line col) : tokenize nstr line (col + lenfalse) 
  | otherwise =
    let (symStr, st) = symbolStr (s:str)
        lensym = length symStr
    in TokSymbol symStr (TokInfo line col) : tokenize st line (col + lensym)


spanUpToQuote :: String -> (String, String)
spanUpToQuote = span (/= '"')

stringToken :: Char -> String -> Int -> Int -> [Token]
stringToken s str line col =
  let (inquote, q:qs) = spanUpToQuote str
  in
    if null (q:qs)
    then error "Strings must terminate with a quote \" "
    else
      let sinquote = s:inquote
          sinq = sinquote++[q]
          lensinq = length sinq
      in TokString sinq (TokInfo line col) : tokenize qs line (col + lensinq)

number :: Char -> String -> Int -> Int -> [Token]
number c cs line col =
  let (digs, dot:c_s) = span isDigit (c:cs)
  in
    case dot of
      '.' -> let (rest, noDigit) = span isDigit c_s
                 flist = (digs++[dot]++rest)
             in
               case (readMaybe flist :: Maybe Double) of
                 Just f -> let tnb = TokNumber f (TokInfo line col)
                               lenflst = length flist
                           in tnb: tokenize noDigit line (col + lenflst)
                 Nothing -> error ("expecting floating number in " ++ flist)
      _ -> let tnb = TokNumber (read (c: digs)) (TokInfo line col)
               lendigs = length (c:digs)
           in tnb: tokenize (dot:c_s) line (col + lendigs)

spanUpToNewLine :: String -> (String, String)
spanUpToNewLine = span (/= '\n')

skipComments :: Char -> String -> Int -> Int -> [Token]
skipComments s str line col =
    let (skipChars, newlineChar:qs) = spanUpToNewLine str
    in 
        let comments = s:skipChars
            lencoms = length comments
        in tokenize qs (line + 1) 0
-- Example program adapted from Norvig
-- program = "(begin (define r T) (& pi (| r r)))"
{-
let p4 = "(var6 (var3 (T F |) idi) idi)"
let p3 = "(var6 (var (1 0.52 +) idi) idi)"
let p2 = "(var6 (var3 \"benim degiskenim\" idi) idi)"
let p1 = "(aro (var1 var2) (+ (- var1 5) var2) eder)"
-}

toString :: [Token] -> String
toString s = unwords (map show s)
