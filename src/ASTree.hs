-- abstract syntax tree
module ASTree where

import Lexer
import Data.List

-- 
data SLiteral = BLit Bool TokenInfo
                | StrLit String TokenInfo
                | NumLit Double TokenInfo

instance Show SLiteral where
    show (BLit b _) = show b
    show (StrLit b _) = show b
    show (NumLit b _) = show b


instance Eq SLiteral where
    (BLit b _) == (BLit a _) = b == a
    (BLit _ _) == _ = False
    (StrLit b _) == (StrLit a _) = b == a
    (StrLit _ _) == _ = False
    (NumLit b _) == (NumLit a _) = b == a
    (NumLit _ _) == _ = False

type Annotation = String
type Symbol = String


data STree = SList [STree]
           | SLit SLiteral
           | SName Symbol TokenInfo
           | SVar Symbol Annotation TokenInfo
           deriving (Eq, Show)

debugSTree :: STree -> String

debugSTree s = 
    case s of
        (SLit (BLit b info)) -> 
            let msg = "{ \"slit-bool\": " ++ show b ++ ", \"info\": "
            in dmsg msg info
        (SLit (StrLit b info)) ->
            let msg = "{ \"slit-string\": " ++ show b ++ ", \"info\": "
            in dmsg msg info
        (SLit (NumLit b info)) -> 
            let msg = "{ \"slit-number\": " ++ show b ++ ", \"info\": "
            in dmsg msg info
        (SName b info) ->
            let msg = "{ \"slit-symbol\": \"" ++ show b ++ "\", \"info\": "
            in dmsg msg info
        (SVar a b info) ->
            let msg = "{ \"slit-variable\": " 
                msg2 = "{" ++ "\"name\": \"" ++ show a ++ "\","
                msg3 =  "\"annotation\": \"" ++ show b ++ "\"}"
                msg4 = msg ++ msg2 ++ msg3 ++ ", \"info\": "
            in dmsg msg4 info
        (SList a) ->
            let msg = "{ \"slist\": [ "
                msg2 = intercalate ", " $ map debugSTree a
                msg3 = " ]}"
            in msg ++ msg2 ++ msg3
    where dmsg m i = m ++ debugTokenInfo i ++ " }"


isSLit :: STree -> Bool
isSLit (SLit _) = True
isSLit _ = False
isNumSLit :: STree -> Bool
isNumSLit (SLit (NumLit _ _)) = True
isNumSLit _ = False 

isBoolSLit :: STree -> Bool
isBoolSLit (SLit (BLit _ _)) = True
isBoolSLit _ = False 

isStrSLit :: STree -> Bool
isStrSLit (SLit (StrLit _ _)) = True
isStrSLit _ = False

isSName :: STree -> Bool
isSName (SName _ _) = True
isSName _ = False

isSList :: STree -> Bool
isSList (SList _) = True
isSList _ = False

getSTreeTokInfo :: STree -> TokenInfo
getSTreeTokInfo (SName _ b) = b
getSTreeTokInfo (SVar _ _ b) = b
getSTreeTokInfo (SLit (BLit _ b)) = b
getSTreeTokInfo (SLit (StrLit _ b)) = b
getSTreeTokInfo (SLit (NumLit _ b)) = b
getSTreeTokInfo (SList []) = TokInfo (-1) (-1) "" ""
getSTreeTokInfo (SList (a:_)) = getSTreeTokInfo a


parseOne :: [Token] -> ([STree], [Token])
parseMany :: [STree] -> [Token] -> ([STree], [Token])

parseOne [] = ([], [])
parseOne [TokEnd] = ([], [])

parseOne (TokLPar _: toks) =
  let (st, ts) = parseMany [] toks
  in ([SList st], ts)

-- identifier
parseOne (TokSymbol a ainfo : TokSep _ _: TokSymbol c _ :tokens) = 
    ([SVar c a ainfo], tokens)

parseOne (TokSep _ (TokInfo line col _ _): TokSymbol _ _ :_) = 
    let msg = "typename seperator must be preceded by a variable name"
        msg2 = msg ++ " at line " ++ show line ++ " column " ++ show col
    in error msg2

parseOne (TokSep _ (TokInfo line col _ _): _) = 
    let msg = "typename seperator must be preceded by a variable name"
        msg2 = msg ++ " and precede a typename "
        msg3 = msg2 ++ " at line " ++ show line ++ " column " ++ show col
    in error msg3

-- variable or operator name
parseOne (TokSymbol s a: tokens) = ([SName s a], tokens)
parseOne (TokOp s a: tokens) = ([SName [s] a], tokens)
parseOne (TokBool b a: tokens) = ([SLit $ BLit b a], tokens)
parseOne (TokNumber b a: tokens) = ([SLit $ NumLit b a], tokens)
parseOne (TokString b a: tokens) = ([SLit $ StrLit b a], tokens)
parseOne (TokRPar _: tokens) = ([], tokens)
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
    (ns, []) -> SList $ SName "seq" (TokInfo 0 0 "" "") : ns
    _ -> error ("unexpected content: " ++ toString tokens)

 -- helper functions

debugUnknownSymbol :: String -> String -> Int -> Int -> String
debugUnknownSymbol str context line col = 
    let msg = "Unknown symbol " ++ str ++ " at line " ++ show line
        msg2 = msg ++ " column " ++ show col ++ " during " ++ context
    in msg2
