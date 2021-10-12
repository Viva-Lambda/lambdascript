-- parse error functions
module Parser.ParseError where
import Parser.ASTree

data ParseError = MatchError STree String
                | OtherError String

instance Show ParseError where
    show (MatchError a s) = "ParseError :: MatchError :: " ++ parseError a s
    show (OtherError s) = "ParseError :: OtherError :: " ++ s

parseError :: STree -> String -> String
parseError t m = 
    let msg = "CAN NOT PARSE TOKEN(S): \n\
   \ " ++ show t ++ " AS\n\
   \ " ++ m ++ " AT\n "
        msg2 = msg ++ debugSTree t
    in msg2
