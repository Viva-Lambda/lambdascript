module StatefulParser where

import Lexer hiding (number)
import Expression

-- import Text.ParserCombinators.ReadP
import Control.Applicative
import qualified Data.Map as DMap
import Control.Monad.State.Lazy


type Input = [Token]
type Rest = Input

type ParserState = DMap.Map String [String]

emptyState :: ParserState
emptyState = DMap.empty


newtype Parser a = P { parse :: Input -> ParseResult a}

data StatefulParser a = StParser {parserState :: ParserState,
                                  parser :: (Parser a)}

data ParseResult a = Error ParseError
                   | Result a Rest 
                   deriving Eq

data ParseError =
    UnexpectedEof -- hit end of file when we expected more input
  | ExpectedEof Rest -- should have successfully parsed everything but there's more!
  | UnexpectedToken Token
  | UnexpectedInput Input
  deriving (Eq, Show)


instance Show a => Show (ParseResult a) where
  show (Result r rest) = "Remaining >" ++ toString rest ++ "< Result > " ++ show r
  show (Error UnexpectedEof) = "Unexpected end of stream"
  show (Error (UnexpectedToken c)) = "Unexpected token: " ++ show c
  show (Error (UnexpectedInput s)) = "Unexpected input: " ++ toString s
  show (Error (ExpectedEof ss))      =
    "Expected end of stream, but got >" ++ toString ss ++ "<"

-- functor is that which provides a definition for fmap
-- which has a signature like fmap :: (a -> b) -> f a -> f b
instance Functor ParseResult where
  fmap f (Result result rest) = Result (f result) rest
  fmap _ (Error e) = Error e


instance Functor Parser where
  fmap f pr = 
    let pfn = parse pr -- accessing P {parse = f} field
        mapfn = \toks ->
                        let aResult = pfn toks
                            bResult = fmap f aResult
                        in bResult
    in P {parse = mapfn}

-- applicative is an intermediate structure between a functor and a monad
-- an applicative needs to define two functions (<*>) for sequencing
-- and `pure` for lifting values. 
instance Applicative Parser where
  -- pure :: a -> f a
  pure x = let pfn = \toks -> Result x toks -- write an anonyme function
           in P {parse = pfn}

  -- (<*>) :: f (a -> b) -> f a -> f b
  -- basically very close to fmap
  (<*>) fAToB fToA = do
        aToB <- fAToB
        toA <- fToA
        pure $ aToB toA


-- alternative is a monoid on applicative functors
-- it formalizes the notion of choice
instance Alternative Parser where
    -- empty :: f a
    -- empty always fails whatever the token is
    empty = let pf = \toks -> Error $ UnexpectedInput toks
            in P { parse = pf }


    -- choice function, apply first if fails, try the other one
    -- if succeed return first one
    -- (<|>) :: f a -> f a -> f a
    p <|> q =
        let pfn = parse p -- accessing P {parse = f} field
            qfn = parse q
            resultfn = \toks ->
                case pfn toks of
                    (Error _) -> qfn toks
                    (Result _ _) -> pfn toks
        in P {parse = resultfn}

instance Alternative StatefulParser where
    -- empty :: f a
    -- empty always fails whatever the token is
    empty = let pf = \toks -> Error $ UnexpectedInput toks
                prsr = P {parse = pf}
            in StParser{parser = prsr, parserState = emptyState}

    p <|> q = let pprsr = parser p
                  qprsr = parser q
                  rprsr = pprsr <|> qprsr
                  pstate = parserState p
                  qstate = parserState q
                  nonempty = if (DMap.size pstate) == 0
                             then qstate
                             else pstate
              in StParser {parser = rprsr, parserState = nonempty}
                   

-- bind one function to another
instance Monad Parser where
    return = pure

    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (>>=) (P {parse = toksToA}) fn = 
        let rfn = \toks -> 
                    case toksToA toks of
                        (Result a rest) -> let parserb = fn a -- gives Parser b
                                               -- accessing the P {parse = f} field
                                               bfn = parse parserb -- ([Token]->b)
                                               bresult = bfn rest
                                            in bresult
                        (Error a) -> (Error a)
        in P {parse = rfn}


instance Functor StatefulParser where
    fmap f StParser {parserState = a, parser = b} = 
        let fb = fmap f b
        in StParser {parserState = a, parser = fb}

instance Applicative StatefulParser where
    pure x = StParser{parserState = DMap.empty,
                      parser =P{parse = \toks -> Result x toks}}

    -- (<*>) :: f (a -> b) -> f a -> f b
    -- basically very close to fmap
    (<*>) fAToB fToA = 
        let atobkeys = parserState fAToB
            toakeys = parserState fToA 
            nonempty = if (DMap.size atobkeys) == 0
                       then toakeys
                       else atobkeys
        in do aToB <- fAToB; toA <- fToA; 
                let stprs = pure ( aToB toA )
                    StParser {parserState = a, parser = b} = stprs
                in StParser {parserState = nonempty ,parser = b}

instance Monad StatefulParser where
    --
    -- (>>=) :: StatefulParser a -> (a -> StatefulParser b) -> StatefulParser b
    sta >>= f = 
        let pstate = parserState sta
            prsr = parser sta
            pfn = parse prsr
            bfn = \toks -> case pfn toks of
                                (Error perr) -> Error perr
                                (Result r rest) -> let bparser = f r
                                                       bprsr = parser bparser
                                                       bfn = parse bprsr
                                                   in bfn rest
            parserB = P {parse = bfn}
        in StParser {parser = parserB, parserState = pstate}
    


-- lookAhead simply creates a parser for the next character
-- it permits us to obtain the next character to parse without consuming it
lookAhead :: StatefulParser Token
lookAhead = StParser {parser = P {parse = parseit}, parserState=emptyState}
  where parseit [] = Error UnexpectedEof
        parseit (c:s) = Result c s


satisfy :: (Token -> Bool) -> StatefulParser Token
satisfy predicate = do 
    presult <- lookAhead
    if predicate presult
    then pure presult -- return c | liftA2 c
    else empty

is :: Token -> StatefulParser Token
is c = satisfy (== c)

-- parsing basic tokens
isSymbol :: Token -> Bool
isSymbol (TokSymbol _ _) = True
isSymbol _ = False
isSep :: Token -> Bool
isSep (TokSep _ _) = True
isSep _ = False

separator :: StatefulParser Token
separator = satisfy isSep

-- parenthesis parser
isLPar :: Token -> Bool
isLPar (TokLPar _) = True
isLPar _ = False
lpar :: StatefulParser Token
lpar = satisfy isLPar


isRPar :: Token -> Bool
isRPar (TokRPar _) = True
isRPar _ = False
rpar :: StatefulParser Token
rpar = satisfy isRPar



-- expression parser

expression :: StatefulParser Expr

-- parse literals first

-- run a parser on some input 

isBoolLit :: Token -> Bool
isBoolLit (TokBool _ _) = True
isBoolLit _ = False

boolLit :: StatefulParser Token
boolLit = satisfy isBoolLit

numberLit :: StatefulParser Token
numberLit = satisfy isNumLit

isNumLit :: Token -> Bool
isNumLit (TokNumber _ _) = True
isNumLit _ = False

strLit ::  StatefulParser Token
strLit = satisfy isStrLit

isStrLit :: Token -> Bool
isStrLit (TokString _ _) = True
isStrLit _ = False

literal :: StatefulParser Literal
literal = do tok <- boolLit; let (TokBool b i) = tok in return (BLit b i)
          <|>
          do ntok <- numberLit; let (TokNumber n j) = ntok in return $ NumLit n j
          <|>
          do stok <- strLit; let (TokString s k) = stok in return $ StrLit s k

-- now parser identifiers


varname :: StatefulParser VarName
varname = do 
   (TokSymbol s i) <- satisfy isSymbol
   return $ VName s i

typename :: StatefulParser TypeName
typename = 
    do v <- varname; let (VName _ info) = v in return $ TName v info

identifier :: StatefulParser Identifier
identifier = do v <- varname; _ <- separator; t <- typename; 
                    let (VName _ (TokInfo ln _ _ _)) = v
                    in return $ IdExpr v t ln

-- procedural call parser
doparser :: StatefulParser Token
isDo :: Token -> Bool
doWords :: [String]
doWords = ["do", "yap"]
isDo (TokSymbol b _) = b `elem` doWords
isDo _ = False

doparser = satisfy isDo

isTokOp (TokOp i _) = True
isTokOp _ = False
tokop = satisfy isTokOp

operator :: StatefulParser Operator
operator = do v <- varname; return $ OpName v
           <|> 
           do (TokOp p t) <- tokop; return $ OpName (VName [p] t)

operand :: StatefulParser Operand
operand = do 
    _ <- lpar
    exps <- many expression
    _ <- rpar
    return $ OprExpr exps

pcall :: StatefulParser ProcedureCall
pcall = do
    _ <- lpar
    _ <- doparser
    opt <- operator
    opargs <- operand
    _ <- rpar
    return $ Proc { op = opt, args = opargs }

-- assignment statement parser
assign :: StatefulParser Assign

assignWords :: [String]
assignWords = ["def", "tanim"]
isAssign :: Token -> Bool
isAssign (TokSymbol b _) = b `elem` assignWords
isAssign _ = True

assignWord :: StatefulParser Token
assignWord = satisfy isAssign

assign = do
    _ <- lpar
    _ <- assignWord
    idf <- identifier
    expr <- expression
    _ <- rpar
    return $ Assigner idf expr

-- condition test statement parser

condtest :: StatefulParser ConditionTest
condtest = do _ <- lpar; lit <- literal; _ <- rpar; return $ CTestLit lit
           <|>
           do procCall <- pcall; return $ CTestProc procCall
           <|>
           do _ <- lpar; cvar <- varname; _ <- rpar; return $ CTestVar cvar

-- sequence parser

seqWords :: [String]
seqWords = ["liste", "seq"]

isSeqWord :: Token -> Bool
isSeqWord (TokSymbol b _) = b `elem` seqWords
isSeqWord _ = False

seqword :: StatefulParser Token
seqword = satisfy isSeqWord

ssequence :: StatefulParser Sequence
ssequence = do
    _ <- lpar
    _ <- seqword
    exps <- some expression
    _ <- rpar
    return $ fromExprToSeq exps

-- parse consequent

conseqWords :: [String]
conseqWords = ["then", "ise"]

isConseqWord :: Token -> Bool
isConseqWord (TokSymbol b _) = b `elem` conseqWords
isConseqWord _ = False

conseqword :: StatefulParser Token
conseqword = satisfy isConseqWord

consequence :: StatefulParser Sequence
consequence = do
    _ <- lpar
    _ <- conseqword
    seqv <- ssequence
    _ <- rpar
    return $ seqv

-- alternate parser
alterWords :: [String]
alterWords = ["else", "yoksa"]

isAlterWord :: Token -> Bool
isAlterWord (TokSymbol b _) = b `elem` alterWords
isAlterWord _ = False

alterWord :: StatefulParser Token
alterWord = satisfy isAlterWord

alternance :: StatefulParser Sequence
alternance = do
    _ <- lpar
    _ <- alterWord
    seqv <- ssequence
    _ <- rpar
    return $ seqv

-- loop statement parser
loopWords :: [String]
loopWords = ["loop", "dongu"]

isLoopWord :: Token -> Bool
isLoopWord (TokSymbol b _) = b `elem` loopWords
isLoopWord _ = False

loopWord :: StatefulParser Token
loopWord = satisfy isLoopWord

loopStmt :: StatefulParser Loop
loopStmt = do
    _ <- lpar
    _ <- loopWord
    ct <- condtest
    cs <- consequence
    _ <- rpar
    return $ Looper {ltest=ct, lconsequent=cs}

-- conditional statement parser

ifWords :: [String]
ifWords = ["if", "eger"]

isIfWord :: Token -> Bool
isIfWord (TokSymbol b _) = b `elem` ifWords
isIfWord _ = False

ifWord :: StatefulParser Token
ifWord = satisfy isIfWord

condStmt :: StatefulParser Conditional
condStmt = do
    _ <- lpar
    _ <- ifWord
    ct <- condtest
    cs <- consequence
    as <- alternance
    _ <- rpar
    return $ Cond {ctest=ct, consequent=cs, alternate = as}

-- procedure definition
procdefWords :: [String]
procdefWords = ["fn", "edim"]

isProcdefWord :: Token -> Bool
isProcdefWord (TokSymbol b _) = b `elem` procdefWords
isProcdefWord _ = False

procdefWord :: StatefulParser Token
procdefWord = satisfy isProcdefWord

farguments :: StatefulParser [Identifier]
farguments = do
    _ <- lpar
    as <- many identifier
    _ <- rpar
    return as


procDefStmt :: StatefulParser ProcedureDefinition
procDefStmt = do
    _ <- lpar
    _ <- procdefWord
    fnid <- identifier
    fargs <- farguments
    fbody <- ssequence
    _ <- rpar
    return $ DefineProc {procname = fnid, arguments = fargs, body = fbody}

-- Statement parser

statement :: StatefulParser Statement

statement = do astmt <- assign; return $ AssignStmt astmt
            <|>
            do conds <- condStmt; return $ CondStmt conds
            <|>
            do procd <- procDefStmt; return $ ProcDefStmt procd
            <|>
            do loopst <- loopStmt; return $ LoopStmt loopst
            <|>
            do seqt <- ssequence; return $ SeqStmt seqt

-- now we can define an expression
isEndExpr :: Expr -> Bool
isEndExprTok :: Token -> Bool
isEndExpr EndExpr = True
isEndExpr _ = False
isEndExprTok TokEnd =  True
isEndExprTok _ =  False
endexpr :: StatefulParser Token
endexpr = satisfy isEndExprTok

expression = do lit <- literal; return $ LiteralExpr lit
            <|> 
            do ids <- varname; return $ GetExpr ids
            <|> 
            do procCall <- pcall; 
                let procInfo = procCallInfo procCall
                in return $ CallExpr procCall procInfo
            <|>
            do stmt <- statement; 
                let sinf = statementInfo stmt 
                in return $ StmtExpr stmt sinf
            <|>
            do e <-endexpr; return EndExpr 

parseExpr :: [Token] -> ParseResult Expr
parseExpr kd = let sprsr = expression -- accessing P{parse = f} field
                   expParser = parser sprsr
                   expfn = parse expParser
               in expfn kd
