-- abstract syntax tree
module ASTree where

import Lexer
import Expression

-- 
data SLiteral = BooleanLiteral Bool TokenInfo
                | StringLiteral String TokenInfo
                | NumericLiteral Double TokenInfo

instance Show SLiteral where
    show (BooleanLiteral b _) = show b
    show (StringLiteral b _) = show b
    show (NumericLiteral b _) = show b


instance Eq SLiteral where
    (BooleanLiteral b _) == (BooleanLiteral a _) = b == a
    (BooleanLiteral b _) == _ = False
    (StringLiteral b _) == (StringLiteral a _) = b == a
    (StringLiteral b _) == _ = False
    (NumericLiteral b _) == (NumericLiteral a _) = b == a
    (NumericLiteral b _) == _ = False


data STree = SList [STree]
           | SLit SLiteral
           | Symbol String TokenInfo
           deriving (Eq, Show)

isSLit :: STree -> Bool
isSLit (SLit a) = True
isSLit _ = False
isNumSLit :: STree -> Bool
isNumSLit (SLit (NumericLiteral _ _)) = True
isNumSLit _ = False 

isBoolSLit :: STree -> Bool
isBoolSLit (SLit (BooleanLiteral _ _)) = True
isBoolSLit _ = False 

isStrSLit :: STree -> Bool
isStrSLit (SLit (StringLiteral _ _)) = True
isStrSLit _ = False

isSymbol :: STree -> Bool
isSymbol (Symbol a b) = True
isSymbol _ = False

isSList :: STree -> Bool
isSList (SList a) = True
isSList _ = False

getSTreeTokInfo :: STree -> TokenInfo
getSTreeTokInfo (Symbol _ b) = b
getSTreeTokInfo (SLit (BooleanLiteral _ b)) = b
getSTreeTokInfo (SLit (StringLiteral _ b)) = b
getSTreeTokInfo (SLit (NumericLiteral _ b)) = b
getSTreeTokInfo (SList []) = TokInfo (-1) (-1)
getSTreeTokInfo (SList (a:_)) = getSTreeTokInfo a

mkIdentifiers :: [STree] -> [Identifier]
mkIdentifiers [] = []
mkIdentifiers (Symbol a (TokInfo i j) : aa) = IdExpr a i : mkIdentifiers aa
mkIdentifiers (a:_) =
    let (TokInfo line col) = getSTreeTokInfo a
        msg = "Can not make identifiers with tokens of line "
        msg2 = msg ++ show line ++ " column " ++ show col
    in error msg2


parseOne :: [Token] -> ([STree], [Token])
parseMany :: [STree] -> [Token] -> ([STree], [Token])

parseOne [] = ([], [])
parseOne [TokEnd] = ([], [])

parseOne (TokLPar tinfo: toks) =
  let (st, ts) = parseMany [] toks
  in ([SList st], ts)

parseOne (TokSymbol s a: tokens) = ([Symbol s a], tokens)
parseOne (TokOp s a: tokens) = ([Symbol [s] a], tokens)
parseOne (TokBool b a: tokens) = ([SLit $ BooleanLiteral b a], tokens)
parseOne (TokNumber b a: tokens) = ([SLit $ NumericLiteral b a], tokens)
parseOne (TokString b a: tokens) = ([SLit $ StringLiteral b a], tokens)
parseOne (TokRPar a: tokens) = ([], tokens)
parseOne (TokEnd: tokens) = ([], tokens)

parseMany previous tokens =
  case parseOne tokens of
    -- parenthesis
    ([], toks) -> (previous, toks)
    -- boolean/number/string/symbol token
    (ns, toks) -> parseMany (previous ++ ns) toks

parseAll :: [Token] -> STree
parseAll tokens =
  case parseMany [] tokens of
    (ns, []) -> SList $ Symbol "seq" (TokInfo 0 0) : ns
    _ -> error ("unexpected content: " ++ toString tokens)

 -- helper functions

debugUnknownSymbol :: String -> String -> Int -> Int -> String
debugUnknownSymbol str context line col = 
    let msg = "Unknown symbol " ++ str ++ " at line " ++ show line
        msg2 = msg ++ " column " ++ show col ++ " during " ++ context
    in msg2


-- check if given token contains special keywords
checkKeywords :: Token -> Bool
checkKeywords (TokSymbol str _)
  -- condition
  | str == "if" = True
  | str == "eger" = True
  -- assignment/get
  | str == "def" = True
  | str == "tanim" = True
  | str == "get" = True
  | str == "al" = True
  -- function definition
  | str == "fn" = True
  | str == "eder" = True
  | str == "quote" = True
  | str == "sabit" = True
  -- while definition
  | str == "while" = True
  | str == "surece" = True
  | otherwise = False

checkKeywords _ = False
