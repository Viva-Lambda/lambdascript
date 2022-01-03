-- debugging module for expression
module Expression.Debug where

import Expression.Expression
import Expression.Identifier
import Expression.Literal
import Lexer.Lexer

debugGetExpr :: GetExpr -> String
debugGetExpr (GetVName v) = debugVarName v
debugGetExpr (GetLit v) = debugLiteral v

debugExpr :: Expr -> String
debugExpr (GExpr a) = "GetExpr " ++ debugGetExpr a
debugExpr (StmtExpr a) = "StmtExpr " ++ debugStatement a
debugExpr EndExpr = "EndExpr" 

debugLiteral :: Literal -> String
debugLiteral a =
    case a of
        (BLit b t) -> 
            let msg = "{\"literal\": " ++ show b ++ ", \"info\": "
            in dmsg msg t
        (NumLit b t) ->
            let msg = "{\"literal\": " ++ show b++ ", \"info\": "
            in dmsg msg t
        (StrLit b t) ->
            let msg = "{\"literal\": " ++ show b ++ ", \"info\": "
            in dmsg msg t
    where dmsg m info = 
            let msg = debugTokenInfo info
            in m ++ msg ++ "}"

getLitTokenInfo :: Literal -> TokenInfo
getLitTokenInfo (BLit _ i) = i
getLitTokenInfo (StrLit _ i) = i
getLitTokenInfo (NumLit _ i) = i

debugVarName :: VarName -> String
debugVarName (VName s i) =
    "{\"variable-name\": " ++ s ++ debugTokenInfo i ++ "}"

debugTypeName :: TypeName -> String
debugTypeName (VName s j) =
    let key = "\"type-name\": "
        val = "\"" ++ s ++ "\","
        infokey = "\"info\": "
        infoval = debugTokenInfo j
    in "{" ++ key ++ val ++ infokey ++ infoval ++ "}"

debugIdentifier :: Identifier -> String

debugIdentifier (IdExpr a b line) = 
    let key = "{\"identifier\": {"
        varkey = "\"name\": "
        varval = debugVarName a
        typekey = ",\"type\": "
        typeval = debugTypeName b
        msg4 =  varkey ++ varval ++ typekey ++ typeval ++ ikeyval ++ "}"
        ikeyval = ", \"line\": " ++ show line ++ "}"
    in key ++ msg4 ++ ikeyval

debugProcCall :: ProcedureCall -> String
debugProcCall (Proc o iseq) = 
    "Procedure Call with Operator " ++ show o ++ " and Operand " ++ show iseq

procCallInfo :: ProcedureCall -> TokenInfo
procCallInfo Proc {op=OpName (VName _ i), args=_} = i

debugSequence :: Sequence -> String
debugSequence SeqExpr {parent=e, child=f} = "Sequence: \
\ parent: " ++ debugExpr e ++ " child: " ++ debugExpr f

debugStatement :: Statement -> String
debugStatement (AssignStmt a) = "Assign statement " ++ show a
debugStatement (CondStmt a) = "Conditiional statement " ++ show a
debugStatement (LoopStmt a) = "Loop statement " ++ show a
debugStatement (ProcDefStmt a) = 
    "Procedural definition statement " ++ debugProcDef a
debugStatement (SeqStmt a) = "Sequence statement " ++ debugSequence a
debugStatement (CallStmt a ) = "Call statement " ++ debugProcCall a

statementInfo :: Statement -> TokenInfo
statementInfo (AssignStmt (Assigner aid _)) =
    let IdExpr (VName _ info) _ _ = aid
    in info
statementInfo (CondStmt (Cond {ctest = a, consequent = _, alternate = _})) =
    case a of
        (CTestLit d) -> getLitTokenInfo d
        (CTestProc d) -> procCallInfo d
        (CTestVar (VName _ info)) -> info

statementInfo (LoopStmt (Looper {ltest = a, lconsequent = _})) =
    case a of
        (CTestLit d) -> getLitTokenInfo d
        (CTestProc d) -> procCallInfo d
        (CTestVar (VName _ info)) -> info

statementInfo (SeqStmt (SeqExpr {parent = a, child = _})) = getExprInfo a
statementInfo (ProcDefStmt pdef) =
    let IdExpr (VName _ info) _ _ = procname pdef
    in info

statementInfo (CallStmt a) = procCallInfo a

getExprInfo :: Expr -> TokenInfo
getExprInfo (GExpr (GetVName (VName _ i))) = i
getExprInfo (GExpr (GetLit i) ) = getLitTokenInfo i
getExprInfo (StmtExpr i) = statementInfo i 
getExprInfo EndExpr = mkTokInfo (-1) (-1) "end of expression" "" ""

debugProcDef :: ProcedureDefinition -> String
debugProcDef DefineProc {procname=a, arguments=b, body=c} =
    let msg = "Procedure Defintion: "
        msg2 = msg ++ "name: " ++ debugIdentifier a ++ " args: " ++ show b
        msg3 = msg2 ++ "body: " ++ debugSequence c
    in msg3

