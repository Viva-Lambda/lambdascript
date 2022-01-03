-- parse error functions
module Parser.ParseError where
import Parser.ASTree
import Lexer.Lexer

data ParseError = MatchError STree String
                | OtherError String
                | UndefinedError String TokenInfo

instance Show ParseError where
    show (MatchError a s) = "ParseError :: MatchError :: " ++ parseError a s
    show (OtherError s) = "ParseError :: OtherError :: " ++ s
    show (UndefinedError s t) =
        let msg1 = "ParseError :: UndefinedError :: Element "
            msg2 = msg1 ++ s ++ " in " ++ show t
            msg3 = " is not defined"
        in msg2 ++ msg3

parseError :: STree -> String -> String
parseError t m = 
    let msg = "CAN NOT PARSE TOKEN(S): \n\
   \ " ++ show t ++ " AS\n\
   \ " ++ m ++ " AT\n "
        msg2 = msg ++ debugSTree t
    in msg2
