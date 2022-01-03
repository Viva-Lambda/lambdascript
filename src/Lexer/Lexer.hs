-- lexer module
module Lexer.Lexer where

import Data.Char
import Text.Read
import Data.List
import Prelude    hiding ( span, FilePath)

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
type TokenContext = String
type FilePath = String

data TokenInfo = TokInfo {
        lineNumber :: LineNumber,
        colNumber :: ColumnNumber,
        tokDescription :: TokenDescription,
        tokContext :: TokenContext,
        tokFilePath :: FilePath
    }


mkTokInfo :: LineNumber -> ColumnNumber -> TokenDescription -> TokenContext -> FilePath -> TokenInfo
mkTokInfo lnb cnb tokd tokc tokfname = TokInfo {
        lineNumber = lnb, colNumber = cnb,
        tokDescription = tokd, tokContext = tokc,
        tokFilePath = tokfname
        }

instance Show TokenInfo where
    show (TokInfo ln col _ _ f) =
        "line: " ++ show ln ++ " " ++ "column: " ++ show col

instance Eq TokenInfo where
    (TokInfo ln col _ _ _) == (TokInfo l2 col2 _ _ _) = 
        (ln == l2) && (col == col2)

debugTokenInfo :: TokenInfo -> String
debugTokenInfo TokInfo {lineNumber=ln, 
                        colNumber=col, 
                        tokDescription=descr,
                        tokContext=cntxt,
                        tokFilePath=tpath} =
    let msg = "{\"token-information\": { " 
        msg2 = "\"postion\": {" ++ "\"line\": " ++ show ln ++ ", " 
        msg3 = "\"column\": " ++ show col ++ "},"
        msg4 = "\"description\": \"" ++ descr ++ "\""
        msg5 = "\"context\": [" ++ cntxt ++ "]"
        msg6 = "\"filepath\": \"" ++ tpath ++ "\""
        msg7 = "}}"
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
getTokenInfo (TokEnd) = mkTokInfo (-1) (-1) "end token" "" ""

type LeftParChars = [String]
type RightParChars = [String]

tokenize :: FilePath -> (LeftParChars, RightParChars) -> String -> Int -> Int -> [Token]

tokenize _ (_,_) [] _ _ = []

tokenize fp (lefts, rights) (x:xs) line col
  -- x marks the beginning of a comment
  | x == ';' = skipComments fp (lefts,rights) x xs line col
  -- x is a paranthesis
  | [x] `elem` lefts = TokLPar (mkTokInfo line col "left parenthesis" "" fp) : tokenize fp (lefts, rights) xs line (col + 1)
  | [x] `elem` rights = TokRPar (mkTokInfo line col "right parenthesis" "" fp) : tokenize fp (lefts, rights) xs line (col + 1)
  -- x is an operator +-*/
  | x `elem` "-+*/&|~!<>=" = let info = mkTokInfo line col ("operator " ++ [x]) "" fp
                                 tok = TokOp x info
                             in tok: tokenize fp (lefts, rights) xs line (col +1)
  -- x is a symbol
  | isAlpha x || x == ':' = symbols fp (lefts,rights) x xs line col
  -- x is a start of string
  | x == '"' = stringToken fp (lefts,rights) x xs line col
  -- x token is number or not
  | isDigit x = number fp (lefts,rights) x xs line col
  | x == '\n' = tokenize fp (lefts, rights) xs (line + 1) 0 -- set column counter back to zero
  | isSpace x = tokenize fp (lefts, rights) xs line (col + 1) -- skip space
  | otherwise = let msg = "line " ++ show line ++ " column " ++ show col
                    msg2 = "file " ++ fp
                in error $ [x] ++ " can not be tokenized at " ++ msg ++ msg2


varNameStr :: String -> (String, String)
varNameStr = span isAlphaNum

removeSpace :: String -> Int -> Int -> (String, Int, Int)
removeSpace [] a b = ([], a, b)
removeSpace (x:xs) line col
    | x == '\n' = removeSpace xs (line+1) 0
    | isSpace x = removeSpace xs line (col + 1)
    | otherwise = ((x:xs), line, col)

symbols :: FilePath -> (LeftParChars, RightParChars) -> Char -> String -> Int -> Int -> [Token]
symbols fp (l,r) s str line col
  | s == 't' && isPrefixOf "rue" str =
    let nstr = drop (length "rue") str
        lentrue = length "true"
        info = mkTokInfo line col "boolean literal true" "" fp
    in TokBool True info : tokenize fp (l,r) nstr line (col + lentrue)
  | s == 'f' && isPrefixOf "alse" str =
    let nstr = drop (length "alse") str
        lenfalse = length "false"
        info = mkTokInfo line col "boolean literal false" "" fp
    in TokBool False info : tokenize fp (l, r) nstr line (col + lenfalse) 
  | s == ':' = 
    let ((x:xs), nline, ncol) = removeSpace str line col
    in case isAlpha x of
            False -> let msg = "typename seperator : must be followed by "
                         msg2 = " typename (int, double, str, etc) at "
                         msg3 = "line " ++ show nline ++ " column "
                         msg4 = msg ++ msg2 ++ msg3 ++ show ncol
                         msg5 = " but it is followed by " ++ [x]
                     in error $ msg4 ++ msg5
            True -> let info = mkTokInfo line col ("symbol " ++ [x]) "" fp
                    in TokSep s info : (symbols fp (l,r) x xs nline ncol)
  | otherwise =
    let (symStr, st) = varNameStr (s:str)
        lensym = length symStr
        info = mkTokInfo line col ("variable name symbol " ++ symStr) "" fp
    in TokSymbol symStr info : tokenize fp (l,r) st line (col + lensym)


spanUpToQuote :: String -> (String, String)
spanUpToQuote = span (/= '"')

stringToken :: FilePath -> (LeftParChars, RightParChars) -> Char -> String -> Int -> Int -> [Token]
stringToken fp (l,r) s str line col =
  let (inquote, q:qs) = spanUpToQuote str
  in
    if null (q:qs)
    then error "Strings must terminate with a quote \" "
    else
      let sinquote = s : inquote
          sinq = sinquote ++ [q]
          lensinq = length sinq
          info = mkTokInfo line col "string literal" "" fp
      in TokString sinq info : tokenize fp (l,r) qs line (col + lensinq)

number :: FilePath -> (LeftParChars, RightParChars) -> Char -> String -> Int -> Int -> [Token]
number fp (l,r) c cs line col =
  let (digs, dot:c_s) = span isDigit (c:cs)
  in
    case dot of
      '.' -> let (rest, noDigit) = span isDigit c_s
                 flist = (digs++[dot]++rest)
             in
               case (readMaybe flist :: Maybe Double) of
                 Just f -> let lenflst = length flist
                               nmlit = "numeric literal " ++ show f
                               info = mkTokInfo line col nmlit "" fp
                               tnb = TokNumber f info
                           in tnb: tokenize fp (l,r) noDigit line (col + lenflst)
                 Nothing -> error $ "expecting floating number in " ++ flist
      _ -> let info = mkTokInfo line col ("numeric literal " ++ (c:digs)) "" fp
               lendigs = length (c:digs)
               tnb = TokNumber (read (c: digs)) info
           in tnb: tokenize fp (l,r) (dot:c_s) line (col + lendigs)

spanUpToNewLine :: String -> (String, String)
spanUpToNewLine = span (/= '\n')

skipComments :: FilePath -> (LeftParChars, RightParChars) -> Char -> String -> Int -> Int -> [Token]
skipComments fp (l,r) _ str line _ =
    let (_, _:qs) = spanUpToNewLine str
    in tokenize fp (l,r) qs (line + 1) 0
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
