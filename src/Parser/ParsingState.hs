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

-- addModuleDef2PState :: Module
-- addAssign2PState :: Assign -> ParsingState -> ParsingState

lookUpPState :: String -> String -> ParsingState -> Maybe Typed
lookUpPState s scopeName ps = DMap.lookup s ps
