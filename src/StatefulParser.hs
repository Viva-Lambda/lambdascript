module StatefulParser where

import Lexer hiding (number)
import Expression

-- import Text.ParserCombinators.ReadP
import Control.Applicative
import qualified Data.Map as DMap
-- import Control.Monad.State.Lazy


type Input = [Token]
type Rest = Input

type ParsingState = DMap.Map String Expr
type Keywords = DMap.Map String [String]

emptyState :: ParsingState
emptyState = DMap.empty

emptyKWords :: Keywords
emptyKWords = DMap.empty

newtype StatefulResult a = StResult (ParsingState -> (a, ParsingState))

type ParseResult a = Either String (Keywords, StatefulResult a, [Token])

parseExpr :: Keywords -> [Token] -> ParseResult Expr

parseExprAll :: Keywords -> [Token] -> ParseResult [Expr]

parseError :: Token -> String -> String
parseError t m = 
    let msg = "can not parse token(s) " ++ show t ++ " as " ++ m ++ " at "
        msg2 = msg ++ debugTokenInfo (getTokenInfo t)
    in msg2


lookAHead :: [Token] -> Token
accept :: [Token] -> [Token]

lookAHead [] = TokEnd
lookAHead (t:ts) = t

accept [] = error "nothing to accept"
accept (t:ts) = ts

parseLit :: Keywords -> [Token] -> ParseResult Literal
parseLit k ts = 
    case lookAHead ts of
        (TokBool b i) -> Right (k, mkStResult (TokBool b i), accept ts)
        (TokString b i) -> Right (k, mkStResult (TokString b i), accept ts)
        (TokNumber b i) -> Right (k, mkStResult (TokNumber b i), accept ts)
        t -> Left $ parseError t "literal"
    where mkStResult (TokBool b i) = let srfn = \sr -> (BLit b i, sr)
                                     in StResult srfn

          mkStResult (TokNumber b i) = let srfn = \sr -> (NumLit b i, sr)
                                       in StResult srfn

          mkStResult (TokString b i) = let srfn = \sr -> (StrLit b i, sr)
                                       in StResult srfn

parseVarName :: Keywords -> [Token] -> ParseResult VarName
parseVarName k ts = 
    case lookAHead ts of
        (TokSymbol b i) -> Right (k, mkStResult (TokSymbol b i), accept ts)
        t -> Left $ parseError t "variable name"

    where mkStResult (TokSymbol b i) = let srfn = \sr -> (VName b i, sr)
                                       in StResult srfn

parseTypeName :: Keywords -> [Token] -> ParseResult TypeName
parseTypeName k ts = 
    case lookAHead ts of
        (TokSep b i) -> mkStResult k (TokSep b i) (accept ts)
        t -> Left $ parseError t "typename name"

    where mkStResult k (TokSep _ i) nts = 
            let (stateVname, toks) = parseVarName nts
                stfn = \sr -> let (vname, stres) = stateVname sr
                                  tname = TName vname i
                              in (tname, stres)
            in Right (k, StResult stfn, toks)

parseIdentifier :: Keywords -> [Token] -> ParseResult Identifier
parseIdentifier k ts =
    case lookAHead ts of
        (TokSymbol b i) -> mkStResult k (TokSymbol b i) (accept ts)
        t -> Left $ parseError t "identifier"

    where mkStResult k (TokSymbol b i) nts = 
            let (stateVname, toks) = parseVarName ((TokSymbol b i):nts)
                (stateTypename, tok_s) = parseTypeName toks
                srfn = \sr -> let (vname, stvname) = stateVname sr
                                  (tname, sttname) = stateTypename stvname
                              in (IdExpr vname tname (lineNumber i), sttname)
            in Right (k, StResult srfn, tok_s)


parseOperator :: Keywords -> [Token] -> ParseResult Operator
parseOperator k ts = 
    case lookAHead ts of
        (TokSymbol b i) -> mkStResult k ts
        t -> Left $ parseError t "operator"

    where mkStResult k nts = 
            let (stateVname, toks) = parseVarName nts
                srfn = \sr -> let (vname, vsr) = stateVname sr
                              in (OpName vname, vsr)
            in Right (k, StResult srfn, toks)

parseOperand :: Keywords -> [Token] -> ParseResult Operand
parseOperand k ts =
    case lookAHead ts of
        (TokLPar i) -> 
            let (kk, stexpr, tt) = parseExprAll k (accept ts)
            in case lookAHead tt of
                   (TokRPar j) -> 
                        let {stRes = \st -> let (exprs, nStRes) = stexpr st
                                            in (OprExpr exprs, nStRes) }
                        in Right (k, StResult stRes, accept tt)
                   t -> Left $ parseError t "operand"
        t -> Left $ parseError t "operand"


iskey :: String -> String -> Keywords -> Bool
iskey key str ks =
    let mems = DMap.member key ks
    in if mems
       then elem str (DMap.! ks key)
       else False

parseKeyword :: String -> String -> Keywords -> [Token] -> ParseResult Token
parseKeyword key msg kwords ts =
    case lookAHead ts of
        (TokSymbol b i) ->
            let isK = iskey key b kwords
            in if isK
               then let stRes = \st -> (b, st)
                    in Right (kwords, StResult stRes, accept ts)
               else let msg = parseError (TokSymbol b i) msg
                        msg2 = msg ++  " because it is not found in keyword"
                        msg3 = msg2 ++ " database."
                    in Left msg3
        t -> Left $ parseError t msg


parseDo :: Keywords -> [Token] -> ParseResult Token
parseDo kwords ts = parseKeyword "do" "procedural call marker" kwords ts 

parseProcedureCall :: Keywords -> [Token] -> ParseResult ProcedureCall

parseProcedureCall k ts =
    case lookAHead ts of
        (TokLPar i) ->
            case parseDo k (accept ts) of
                (Left r) -> Left r
                (Right (kk, stexpr, toks)) ->
                    case parseOperator kk toks of
                        (Left popr) -> Left popr
                        (Right (kk2, operSt, tok_s)) ->
                            case parseOperand kk2 tok_s of
                                (Left pks2) -> Left pks2
                                (Right (kk3, opandSt, toks2)) ->
                                    case lookAHead toks2 of
                                       (TokRPar j) ->
                                            let {
        stRes = \st -> let { (oper, st2) = operSt st;
                             (operargs, st3) = opandSt st2;
                           }
                       in (Proc {op = oper, args = operargs}, st3)
                                                }
                                            in Right (k, StResult stRes, accept toks2)
                t -> Left $ parseError t "procedure call"
        t -> Left $ parseError t "procedure call"

-- assignment

parseDef :: Keywords -> [Token] -> ParseResult Token
parseDef kwords ts = parseKeyword "def" "assignment marker" kwords ts 

parseAssignment :: Keywords -> [Token] -> ParseResult Assign
parseAssignment k ts =
    case lookAHead ts of
        (TokLPar i) ->
            case parseDef k (accept ts) of
                (Left err) -> Left err
                -- defStateVal :: (ParsingState -> (a, ParsingState))
                (Right (k2, defStateVal, ts2)) ->
                    case parseIdentifier k2 ts2 of
                        (Left err) -> Left err
                        (Right (k3, idStateVal, ts3)) ->
                            case parseExpr k3 ts3 of
                                (Left err) -> Left err
                                (Right (k4, exprStateVal, ts4)) ->
                                    case lookAHead ts4 of
                                        (TokRPar j) ->
                                            let {
            stfn = \stVal -> let (defWord, stateDef) = defStateVal stVal;
                                 (identif, stateId) = idStateVal stateDef;
                                 (expr, stateExpr) = exprStateVal stateId;
                             in (Assigner identif expr, stateExpr)
                                                }
                                            in Right (k4, StResult stfn, accept ts4)
                                        t -> Left $ parseError t "assignment"
        t -> Left $ parseError t "assignment expression"

-- sequence
-- parse seq keyword
parseSeq :: Keywords -> [Token] -> ParseResult Token
parseSeq kwords ts = parseKeyword "seq" "sequence marker" kwords ts 

parseSequence :: Keywords -> [Token] -> ParseResult Sequence
parseSequence k ts =
    case lookAHead ts of
        (TokLPar i) ->
            case parseSeq k (accept ts) of
                (Left err) -> Left err
                (Right (k2, seqStateVal, ts2)) ->
                    case parseExprAll k2 ts2 of
                        (Left err) -> Left err
                        (Right (k3, exprStateVal, ts3)) ->
                            case lookAHead ts3 of
                                (TokRPar j) ->
                                    let {
                                        stfn = \stVal ->
                                        let (seqWord, stateSeq) = seqStateVal stVal
                                            (exprs, stateExpr) = exprStateVal stateSeq
                                        in (fromExprToSeq exprs, stateExpr)
                                        }
                                    in Right (k3, StResult stfn, accept ts3)
                                t -> Left $ parseError t "sequence"
        t -> Left $ parseError t "sequence"

-- loop
-- parse loop keyword
parseLoopWord :: Keywords -> [Token] -> ParseResult Token
parseLoopWord kwords ts = parseKeyword "loop" "loop marker" kwords ts 

parseTest :: Keywords -> [Token] -> ParseResult ConditionTest

parseTest k ts =
    case parseProcedureCall k ts of
        (Right (k2, procState, ts2)) ->
            let {
        stfn = \stVal -> let (procVal, stateProc) = procState stVal
                         in (CTestProc procVal, stateProc)
                }
            in Right (k2, StResult stfn, ts2)
        (Left _) ->
            case lookAHead ts of
                (TokLPar i) ->
                    case parseLit k (accept ts) of
                        (Right (k2, litState, ts2)) ->
                            let {
        stfn = \stVal -> let (litval, stateLit) = litState stVal
                         in (CTestLit litval, stateLit);
        ts3 = accept ts2;
                                }
                            in case lookAHead ts3 of
                                    (TokRPar j) -> Right (k2, StResult stfn, accept ts3)
                                    t -> Left $ parseError t "literal condition test"
                        (Left _) ->
                            case parseVarName k (accept ts) of
                                (Right (k2, varState, ts2) ) ->
                                    let {
                stfn = \stVal -> let (varval, stateVar) = varState stVal
                                 in (CTestVar varval, stateVar);
                ts3 = accept ts2;
                                        }
                                    in case lookAHead ts3 of
                                            (TokRPar j) -> Right (k2, StResult stfn, accept ts3)
                                            t -> Left $ parseError t "condition test"
                t -> Left $ parseError t "condition test"

                                                    
-- consequent

parseThen :: Keywords -> [Token] -> ParseResult Token
parseThen kwords ts = parseKeyword "then" "consequence marker" kwords ts 

parseConsequent :: Keywords -> [Token] -> ParseResult Sequence

parseConsequent kwords ts =
    case lookAHead ts of
        (TokLPar i) ->
            case parseThen kwords (accept ts) of
                (Left e) -> Left e 
                (Right (k2, thenState, ts2)) ->
                    case parseSequence k2 ts2 of
                        (Left e) -> Left e
                        (Right (k3, seqState, ts3)) ->
                            case lookAHead ts3 of
                                (TokRPar j) ->
                                    let {
            stfn = \stVal -> let (thenWord, stateThen) = thenState stVal
                                 (seqVal, stateSeq) = seqState stateThen
                             in (seqVal, stateSeq);
                                        }
                                    in (k3, StResult stfn, accept ts3)
                                t -> Left $ parseError t "consequence marker"
        t -> Left $ parseError t "consequence marker"


parseLoop :: Keywords -> [Token] -> ParseResult Loop
parseLoop k ts =
    case lookAHead ts of
        (TokLPar i) ->
            case parseLoopWord k (accept ts) of
                (Left e) -> Left e
                (Right (k2, lwordState, ts2)) ->
                    case parseTest k2 ts2 of
                        (Left e) -> Left e
                        (Right (k3, ltestState, ts3)) ->
                            case parseConsequent k3 ts3 of
                                (Left e) -> Left e
                                (Right (k4, lconseqState, ts4)) ->
                                    case lookAHead ts4 of
                                        (TokRPar j) ->
                                            let {
        stfn = \stVal -> let (lword, stateLword) = lwordState stVal
                             (lTest, stateLTest) = ltestState stateLword
                             (lCons, stateLConseq) = lconseqState stateLTest
                         in (Looper {ltest = lTest, lconsequent = lCons}, stateLConseq)
                                                }
                                            in Right (k4, StResult stfn, accept ts4)
                                        t -> Left $ parseError t "loop marker"
        t -> Left $ parseError t "loop marker"


--

parseElse :: Keywords -> [Token] -> ParseResult Token
parseElse kwords ts = parseKeyword "else" "alternate marker" kwords ts 


parseAlternate :: Keywords -> [Token] -> ParseResult Sequence
parseAlternate kwords ts =
    case lookAHead ts of
        (TokLPar i) ->
            case parseElse kwords (accept ts) of
                (Left e) -> Left e 
                (Right (k2, elseState, ts2)) ->
                    case parseSequence k2 ts2 of
                        (Left e) -> Left e
                        (Right (k3, seqState, ts3)) ->
                            case lookAHead ts3 of
                                (TokRPar j) ->
                                    let {
            stfn = \stVal -> let (thenWord, stateElse) = elseState stVal
                                 (seqVal, stateSeq) = seqState stateElse
                             in (seqVal, stateSeq);
                                        }
                                    in (k3, StResult stfn, accept ts3)
                                t -> Left $ parseError t "alternate marker"
        t -> Left $ parseError t "alternate marker"

-- loop
