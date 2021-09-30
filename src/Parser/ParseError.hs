-- parse error functions
module Parser.ParseError where
import Parser.ASTree

parseError :: STree -> String -> String
parseError t m = 
    let msg = "CAN NOT PARSE TOKEN(S): \n\
   \ " ++ show t ++ " AS\n\
   \ " ++ m ++ " AT\n "
        msg2 = msg ++ debugSTree t
    in msg2
