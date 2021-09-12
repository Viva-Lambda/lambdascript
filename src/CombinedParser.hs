-- combined parser for ASTree
module CombinedParser where
import Lexer hiding (number)
import Expression

-- import Text.ParserCombinators.ReadP
import Control.Applicative
import qualified Data.Map as DMap
import Control.Monad.State.Lazy


{-
Here is the grammar of the language

expression := <literal> | <varname> | <procedure call>

statement := <conditional>
            | <assignment>
            | <procedure definition> 
            | <loop>
            | <sequence>


-- expressions
-- literals
literal := <boolean> | <number> | <string>

number := <digit>+ | <digit>+.<digit>+
boolean := true | false
string := "...any number of char"

identifier := <varname> <typename>
varname := <letter>+ <digit>*
letter := a | b | c | d | e | f | ... | A | B | ... | Z
digit := 0 | ... | 9
typename := : <varname>

procedural call := (do/yap <operator> <operand>)
operator := opchar | <varname>
opchar := + | - | * | / | % | < | > | & | \| | !
operand := (<expression>*)

assignment := def/tanim <identifier> <expression>

conditional := (eger/if <test> <consequent> <alternate>)
test := (<literal>) | <procedural call> | (<varname>)
consequent := (then/ise <sequence>)
alternate := (else/yoksa <sequence>)

loop := (loop/dongu <test> <consequent>)
-- (loop/dongu (do/yap < (1.6 6.0)) (then/ise fdsak,m))

procedure definition := (fn/edim <identifier> <arguments> <body>)
arguments := (<identifier>*)
body := <sequence>
sequence := ( seq/liste <expression>+ )
-}

-- a parser for everything

type KeywordDatabase = DMap.Map String [String]

type Input = [Token]
type Rest = Input

-- from https://tgdwyer.github.io/parsercombinators/#creating-a-parse-tree
-- and http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf
-- https://www.epicc.fc.up.pt/~pbv/aulas/tapf/handouts/parsing.html

newtype Parser a = P { parse :: Input -> ParseResult a}

type StatefulParser a = State KeywordDatabase (Parser a)


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
        pure $ aToB toA in if (keywords fAToB) == (DMap.fromList [("",[])])
                                    then P {parse = parse pr, keywords = keywords fToA}
                                    else P {parse = parse pr, keywords = keywords fAToB}


-- alternative is a monoid on applicative functors
-- it formalizes the notion of choice
instance Alternative Parser where
    -- empty :: f a
    -- empty always fails whatever the token is
    empty = let pf = \toks -> Error $ UnexpectedInput toks
            in P { parse = pf, keywords =  DMap.fromList [("",[])]}


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
        in 
            case (keywords q) == (DMap.fromList [("", [])]) of
                True -> P {parse = resultfn, keywords = keywords p}
                False -> P {parse = resultfn, keywords = keywords q}
        


-- bind one function to another
instance Monad Parser where
    return = pure

    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (>>=) (P {parse = toksToA, keywords = kwords}) fn = 
        let rfn = \toks -> 
                    case toksToA toks of
                        (Result a rest) -> let parserb = fn a -- gives Parser b
                                               -- accessing the P {parse = f} field
                                               bfn = parse parserb -- ([Token]->b)
                                               bresult = bfn rest
                                            in bresult
                        (Error a) -> (Error a)
        in P {parse = rfn, keywords = kwords}
        

-- lookAhead simply creates a parser for the next character
-- it permits us to obtain the next character to parse without consuming it
lookAhead :: Parser Token
lookAhead = P {parse = parseit, keywords = DMap.fromList [("", [])]}
  where parseit [] = Error UnexpectedEof
        parseit (c:s) = Result c s


satisfy :: (Token -> Bool) -> Parser Token
satisfy predicate = do 
    presult <- lookAhead
    if predicate presult
    then pure presult -- return c | liftA2 c
    else empty

is :: Token -> Parser Token
is c = satisfy (== c)

iskey :: String -> String -> Parser a -> Bool
iskey str kword p =
    let kwords = keywords p
        isMem = DMap.member kword kwords
    in if isMem
       then str `elem` (kwords DMap.! kword)
       else False
-- parsing basic tokens
isSymbol :: Token -> Bool
isSymbol (TokSymbol _ _) = True
isSymbol _ = False
isSep :: Token -> Bool
isSep (TokSep _ _) = True
isSep _ = False

separator :: Parser Token
separator = satisfy isSep

-- parenthesis parser
isLPar :: Token -> Bool
isLPar (TokLPar _) = True
isLPar _ = False
lpar :: Parser Token
lpar = satisfy isLPar


isRPar :: Token -> Bool
isRPar (TokRPar _) = True
isRPar _ = False
rpar :: Parser Token
rpar = satisfy isRPar



-- expression parser

expression :: Parser Expr

-- parse literals first

-- run a parser on some input 

isBoolLit :: Token -> Bool
isBoolLit (TokBool _ _) = True
isBoolLit _ = False

boolLit :: Parser Token
boolLit = satisfy isBoolLit

numberLit :: Parser Token
numberLit = satisfy isNumLit

isNumLit :: Token -> Bool
isNumLit (TokNumber _ _) = True
isNumLit _ = False

strLit ::  Parser Token
strLit = satisfy isStrLit

isStrLit :: Token -> Bool
isStrLit (TokString _ _) = True
isStrLit _ = False

literal :: Parser Literal
literal = do tok <- boolLit; let (TokBool b i) = tok in return (BLit b i)
          <|>
          do ntok <- numberLit; let (TokNumber n j) = ntok in return $ NumLit n j
          <|>
          do stok <- strLit; let (TokString s k) = stok in return $ StrLit s k

-- now parser identifiers


varname :: Parser VarName
varname = do 
   (TokSymbol s i) <- satisfy isSymbol
   return $ VName s i

typename :: Parser TypeName
typename = 
    do v <- varname; let (VName _ info) = v in return $ TName v info

identifier :: Parser Identifier
identifier = do v <- varname; _ <- separator; t <- typename; 
                    let (VName _ (TokInfo ln _ _ _)) = v
                    in return $ IdExpr v t ln

-- procedural call parser
doparser :: Parser Token
isDo :: Token -> Bool
doWords :: [String]
doWords = ["do", "yap"]
isDo (TokSymbol b _) = b `elem` doWords
isDo _ = False

isDo2 :: Token -> Parser a -> Bool
isDo2 (TokSymbol b _) pr = iskey b "do" pr
isDo2 _ _ = False

doparser = satisfy isDo
doparser2 :: Parser Token -> Parser Token
doparser2 a = let kwords = keywords a in do
                                            tok <- lookAhead
                                            case isDo2 tok a of
                                                True -> let pr = pure tok
                                                        in P {parse = parse pr,
                                                              keywords = kwords}
                                                False -> empty

isTokOp (TokOp i _) = True
isTokOp _ = False
tokop = satisfy isTokOp

operator :: Parser Operator
operator = do v <- varname; return $ OpName v
           <|> 
           do (TokOp p t) <- tokop; return $ OpName (VName [p] t)

operand :: Parser Operand
operand = do 
    _ <- lpar
    exps <- many expression
    _ <- rpar
    return $ OprExpr exps

pcall :: Parser ProcedureCall
pcall = do
    _ <- lpar
    _ <- doparser
    opt <- operator
    opargs <- operand
    _ <- rpar
    return $ Proc { op = opt, args = opargs }

-- assignment statement parser
assign :: Parser Assign

assignWords :: [String]
assignWords = ["def", "tanim"]
isAssign :: Token -> Bool
isAssign (TokSymbol b _) = b `elem` assignWords
isAssign _ = True

assignWord :: Parser Token
assignWord = satisfy isAssign

assign = do
    _ <- lpar
    _ <- assignWord
    idf <- identifier
    expr <- expression
    _ <- rpar
    return $ Assigner idf expr

-- condition test statement parser

condtest :: Parser ConditionTest
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

seqword :: Parser Token
seqword = satisfy isSeqWord

ssequence :: Parser Sequence
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

conseqword :: Parser Token
conseqword = satisfy isConseqWord

consequence :: Parser Sequence
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

alterWord :: Parser Token
alterWord = satisfy isAlterWord

alternance :: Parser Sequence
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

loopWord :: Parser Token
loopWord = satisfy isLoopWord

loopStmt :: Parser Loop
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

ifWord :: Parser Token
ifWord = satisfy isIfWord

condStmt :: Parser Conditional
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

procdefWord :: Parser Token
procdefWord = satisfy isProcdefWord

farguments :: Parser [Identifier]
farguments = do
    _ <- lpar
    as <- many identifier
    _ <- rpar
    return as


procDefStmt :: Parser ProcedureDefinition
procDefStmt = do
    _ <- lpar
    _ <- procdefWord
    fnid <- identifier
    fargs <- farguments
    fbody <- ssequence
    _ <- rpar
    return $ DefineProc {procname = fnid, arguments = fargs, body = fbody}

-- Statement parser

statement :: Parser Statement

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
endexpr :: Parser Token
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
                let sinf = statementInfo stmt in return $ StmtExpr stmt sinf
            <|>
            do e <-endexpr; return $ EndExpr

parseExpr :: [Token] -> ParseResult Expr
parseExpr = parse expression -- accessing P{parse = f} field
