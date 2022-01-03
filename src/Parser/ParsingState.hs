-- parsing state module
module Parser.ParsingState where

import qualified Data.Map as DMap

-- Type system
import TypeSystem.TypeIO
import TypeSystem.TypeExpression
import TypeSystem.TypeError

-- Expression
import Expression.Literal
import Expression.Identifier
import Expression.Expression

-- Parser
import Parser.ParseError

-- type ParsingState = DMap.Map String TypeScope
type ParsingState = TypeScope

emptyState :: ParsingState
emptyState = DMap.empty

addLit2PState :: Literal -> String -> ParsingState -> ParsingState 
addLit2PState lit scopeName pstate =
    let (ValueType s tname) = fromLiteral2ValueType lit
        tval = ValueTyper (ValueType s tname)
        ps = DMap.insert s tval pstate
    in ps

addIde2PState :: Identifier -> String -> ParsingState -> ParsingState 
addIde2PState ids scopeName pstate =
    let (ValueType s tname) = fromIdentifier2ValueType ids
        tval = ValueTyper (ValueType s tname)
    in case DMap.lookup s pstate of
            Nothing -> DMap.insert s tval pstate
            Just t -> error $ show ( NameAlreadyBound s tname t)
            
addProcDef2PState :: ProcedureDefinition -> String -> ParsingState -> ParsingState
addProcDef2PState pd scopeName pstate =
    let (IdExpr (VName pname _) tn _) = procname pd
        ptype = FunctionTyper $ fromProcDef2FnType pd
    in case DMap.lookup scopeName pstate of
            Nothing -> DMap.insert pname ptype pstate
            Just t -> error $ show ( NameAlreadyBound pname tn t)

addAssign2PState :: Assign -> String -> ParsingState -> ParsingState
addAssign2PState (Assigner ids texpr) scopeName pstate =
    let idtype = fromIdentifier2ValueType ids
        (ValueType s tname) = idtype
        tval = ValueTyper (ValueType s tname)
        isDecl =  DMap.member s pstate
    in if isDecl
       then error $! show (NameAlreadyBound s tname tval)
       else case texpr of
                -- literal
                (TypedLit lit) ->
                    let vtype = fromLiteral2ValueType lit
                        (ValueType slit stname) = vtype
                        isWellTyped = vtype == idtype
                    in if isWellTyped
                       then DMap.insert s tval pstate
                       else error $! show (WrongType (show tname) (show stname) tval)
                -- identifier
                (TypedId idname) ->
                    -- find the previously declared type and
                    -- check if it corresponds to current type
                    let (VName prevIdName t) = idname 
                    in case DMap.lookup prevIdName pstate of
                            Nothing -> error $! show ( UndefinedError prevIdName t)
                            Just t -> let isWellTyped = t == tval
                                      in if isWellTyped
                                         then DMap.insert s tval pstate
                                         else error $! show (WrongType (show tname) (show idtype) tval)

                -- procedure call


-- addModuleDef2PState :: Module
-- addAssign2PState :: Assign -> ParsingState -> ParsingState

lookUpPState :: String -> String -> ParsingState -> Maybe Typed
lookUpPState s scopeName ps = DMap.lookup s ps
