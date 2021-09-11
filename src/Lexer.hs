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
type TokenDescription = String

data TokenInfo = TokInfo {
        lineNumber :: Int,
        colNumber :: Int,
        tokDescription :: String,
        tokContext :: String
        }

mkTokInfo :: Int -> Int -> String -> String -> TokenInfo
mkTokInfo i j d c = TokInfo {lineNumber = i, colNumber = j,
                             tokDescription = d, tokContext = c}

joinTokInfo :: TokenInfo -> TokenInfo -> TokenInfo
joinTokInfo tinfo1 tinfo2 =
    let (TokInfo {lineNumber = a, colNumber = b,
              tokDescription = _, tokContext = _}) = tinfo1
        descr = "info created by joining tokens: " ++ show tinfo1 ++ "," ++ show tinfo2
        contxt = "[" ++ debugTokenInfo tinfo1 ++ "," ++ debugTokenInfo tinfo2
        contxt2 = contxt ++ "]"
    in mkTokInfo a b descr contxt2
            

instance Show TokenInfo where
    show (TokInfo ln col _ _) = "line: " ++ show ln ++ " " ++ "column: " ++ show col

instance Eq TokenInfo where
    (TokInfo ln col _ _) == (TokInfo l2 col2 _ _) = 
        (ln == l2) && (col == col2)

debugTokenInfo :: TokenInfo -> String
debugTokenInfo TokInfo {lineNumber=ln, 
                        colNumber=col, 
                        tokDescription=descr,
                        tokContext=cntxt} =
    let msg = "{\"token-information\": { " 
        msg2 = "\"postion\": {" ++ "\"line\": " ++ show ln ++ ", " 
        msg3 = "\"column\": " ++ show col ++ "},"
        msg4 = "\"description\": \"" ++ show descr ++ "\""
        msg5 = "\"context\": [" ++ show cntxt ++ "]"
        msg6 = "}}"
    in msg ++ msg2 ++ msg3 ++ msg4 ++ msg5 ++ msg6

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
getTokenInfo :: Token -> TokenInfo
getTokenInfo (TokBool _ i) = i
getTokenInfo (TokLPar i) = i
getTokenInfo (TokRPar i) = i
getTokenInfo (TokSymbol _ i) = i
getTokenInfo (TokOp _ i) = i
getTokenInfo (TokSep _ i) = i
getTokenInfo (TokString _ i) = i
getTokenInfo (TokNumber _ i) = i
getTokenInfo (TokEnd) = mkTokInfo (-1) (-1) "end token" ""


tokenize :: String -> Int -> Int -> [Token]

tokenize [] _ _ = []

tokenize (x:xs) line col
  -- x marks the beginning of a comment
  | x == ';' = skipComments x xs line col
  -- x is a paranthesis
  | x == '(' = TokLPar (mkTokInfo line col "left parenthesis" "") : tokenize xs line (col + 1)
  | x == ')' = TokRPar (mkTokInfo line col "right parenthesis" "") : tokenize xs line (col + 1)
  -- x is an operator +-*/
  | x `elem` "-+*/&|~!<>=" = let info = mkTokInfo line col ("operator " ++ [x]) ""
                                 tok = TokOp x info
                             in tok: tokenize xs line (col +1)
  -- x is a symbol
  | isAlpha x || x == ':' = symbols x xs line col
  -- x is a start of string
  | x == '"' = stringToken x xs line col
  -- x token is number or not
  | isDigit x = number x xs line col
  | x == '\n' = tokenize xs (line + 1) 0 -- set column counter back to zero
  | isSpace x = tokenize xs line (col + 1) -- skip space
  | otherwise = let msg = show line ++ " column " ++ show col
                in error $ [x] ++ " can not be tokenized at line " ++ msg


varNameStr :: String -> (String, String)
varNameStr = span isAlphaNum

removeSpace :: String -> Int -> Int -> (String, Int, Int)
removeSpace [] a b = ([], a, b)
removeSpace (x:xs) line col
    | x == '\n' = removeSpace xs (line+1) 0
    | isSpace x = removeSpace xs line (col + 1)
    | otherwise = ((x:xs), line, col)

symbols :: Char -> String -> Int -> Int -> [Token]
symbols s str line col
  | s == 't' && isPrefixOf "rue" str =
    let nstr = drop (length "rue") str
        lentrue = length "true"
        info = mkTokInfo line col "boolean literal true" ""
    in TokBool True info : tokenize nstr line (col + lentrue)
  | s == 'f' && isPrefixOf "alse" str =
    let nstr = drop (length "alse") str
        lenfalse = length "false"
        info = mkTokInfo line col "boolean literal false" ""
    in TokBool False info : tokenize nstr line (col + lenfalse) 
  | s == ':' = 
    let ((x:xs), nline, ncol) = removeSpace str line col
    in case isAlpha x of
            False -> let msg = "typename seperator : must be followed by "
                         msg2 = " typename (int, double, str, etc) at "
                         msg3 = "line " ++ show nline ++ " column "
                         msg4 = msg ++ msg2 ++ msg3 ++ show ncol
                         msg5 = " but it is followed by " ++ [x]
                     in error $ msg4 ++ msg5
            True -> let info = mkTokInfo line col ("symbol " ++ [x]) ""
                    in TokSep s info : (symbols x xs nline ncol)
  | otherwise =
    let (symStr, st) = varNameStr (s:str)
        lensym = length symStr
        info = mkTokInfo line col ("variable name symbol " ++ symStr) ""
    in TokSymbol symStr info : tokenize st line (col + lensym)


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
          info = mkTokInfo line col "string literal" ""
      in TokString sinq info : tokenize qs line (col + lensinq)

number :: Char -> String -> Int -> Int -> [Token]
number c cs line col =
  let (digs, dot:c_s) = span isDigit (c:cs)
  in
    case dot of
      '.' -> let (rest, noDigit) = span isDigit c_s
                 flist = (digs++[dot]++rest)
             in
               case (readMaybe flist :: Maybe Double) of
                 Just f -> let lenflst = length flist
                               nmlit = "numeric literal " ++ show f
                               info = mkTokInfo line col nmlit ""
                               tnb = TokNumber f info
                           in tnb: tokenize noDigit line (col + lenflst)
                 Nothing -> error $ "expecting floating number in " ++ flist
      _ -> let info = mkTokInfo line col ("numeric literal " ++ (c:digs)) ""
               lendigs = length (c:digs)
               tnb = TokNumber (read (c: digs)) info
           in tnb: tokenize (dot:c_s) line (col + lendigs)

spanUpToNewLine :: String -> (String, String)
spanUpToNewLine = span (/= '\n')

skipComments :: Char -> String -> Int -> Int -> [Token]
skipComments _ str line _ =
    let (_, _:qs) = spanUpToNewLine str
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
