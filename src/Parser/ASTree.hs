-- abstract syntax tree
module Parser.ASTree where

import Lexer.Lexer
import Data.List

-- 
data SLiteral = BoolLit Bool TokenInfo
                | StringLit String TokenInfo
                | NumericLit Double TokenInfo

instance Show SLiteral where
    show (BoolLit b _) = show b
    show (StringLit b _) = show b
    show (NumericLit b _) = show b


instance Eq SLiteral where
    (BoolLit b _) == (BoolLit a _) = b == a
    (BoolLit _ _) == _ = False
    (StringLit b _) == (StringLit a _) = b == a
    (StringLit _ _) == _ = False
    (NumericLit b _) == (NumericLit a _) = b == a
    (NumericLit _ _) == _ = False

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
        (SLit (BoolLit b info)) -> 
            let msg = "{ \"slit-bool\":\n " ++ show b ++ ", \"info\": "
            in dmsg msg info
        (SLit (StringLit b info)) ->
            let msg = "{ \"slit-string\":\n " ++ show b ++ ", \"info\": "
            in dmsg msg info
        (SLit (NumericLit b info)) -> 
            let msg = "{ \"slit-number\":\n " ++ show b ++ ", \"info\": "
            in dmsg msg info
        (SName b info) ->
            let msg = "{ \"slit-symbol\":\n \"" ++ show b ++ "\", \"info\": "
            in dmsg msg info
        (SVar a b info) ->
            let msg = "{ \"slit-variable\":\n " 
                msg2 = "{" ++ "\"name\": \"" ++ show a ++ "\","
                msg3 =  "\"annotation\": \"" ++ show b ++ "\"}"
                msg4 = msg ++ msg2 ++ msg3 ++ ", \"info\": "
            in dmsg msg4 info
        (SList a) ->
            let msg = "{ \"slist\": [\n "
                msg2 = intercalate ", " $ map debugSTree a
                msg3 = " ]}"
            in msg ++ msg2 ++ msg3
    where dmsg m i = m ++ debugTokenInfo i ++ " }"


isSLit :: STree -> Bool
isSLit (SLit _) = True
isSLit _ = False
isNumSLit :: STree -> Bool
isNumSLit (SLit (NumericLit _ _)) = True
isNumSLit _ = False 

isBoolSLit :: STree -> Bool
isBoolSLit (SLit (BoolLit _ _)) = True
isBoolSLit _ = False 

isStrSLit :: STree -> Bool
isStrSLit (SLit (StringLit _ _)) = True
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
getSTreeTokInfo (SLit (BoolLit _ b)) = b
getSTreeTokInfo (SLit (StringLit _ b)) = b
getSTreeTokInfo (SLit (NumericLit _ b)) = b
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
parseOne (TokSymbol sym ainfo : TokSep _ _: TokSymbol anno _ :tokens) = 
    ([SVar sym anno ainfo], tokens)

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
parseOne (TokBool b a: tokens) = ([SLit $ BoolLit b a], tokens)
parseOne (TokNumber b a: tokens) = ([SLit $ NumericLit b a], tokens)
parseOne (TokString b a: tokens) = ([SLit $ StringLit b a], tokens)
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
