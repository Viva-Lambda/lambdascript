-- parsing state module
module Parser.ParsingState where

import qualified Data.Map as DMap
import TypeSystem.TypeIO
import TypeSystem.TypeChecker
import TypeSystem.TypeRules
import TypeSystem.TypeError

import Expression.Literal
import Expression.Identifier
import Expression.Expression

type ParsingState = DMap.Map String Typed

emptyState :: ParsingState
emptyState = DMap.empty

addLit2PState :: Literal -> ParsingState -> ParsingState 
addLit2PState lit pstate =
    let (ValueType s tname) = fromLiteral2ValueType lit
        tval = ValueTyper (ValueType s tname)
        ps = DMap.insert s tval pstate
    in ps

addIde2PState :: Identifier -> ParsingState -> ParsingState 
addIde2PState ids pstate =
    let (ValueType s tname) = fromIdentifier2ValueType ids
        tval = ValueTyper (ValueType s tname)
    in case DMap.lookup s pstate of
            Nothing -> DMap.insert s tval pstate
            Just t -> error $ show ( NameAlreadyBound s tname t)
            
addProcDef2PState :: ProcedureDefinition -> ParsingState -> ParsingState
addProcDef2PState pd pstate =
    let (IdExpr (VName pname _) tn _) = procname pd
        ptype = FunctionTyper $ fromProcDef2FnType pd
    in case DMap.lookup pname pstate of
            Nothing -> DMap.insert pname ptype pstate
            Just t -> error $ show ( NameAlreadyBound pname tn t)

