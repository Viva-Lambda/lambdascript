-- input output interface for types
module TypeSystem.TypeIO where

import Expression.Expression
import Expression.Literal
import Expression.Identifier
import Utils

-- typing etc
import TypeSystem.TypeRules

fromLiteral2ValueType :: Literal -> TypedValue
fromLiteral2ValueType (BLit a t) = ValueType (show a) (VName "bool" t)
fromLiteral2ValueType (StrLit a t) = ValueType (show a) (VName "string" t)
fromLiteral2ValueType (NumLit a t) = ValueType (show a) (VName "double" t)

fromIdentifier2ValueType :: Identifier -> TypedValue
fromIdentifier2ValueType (IdExpr (VName s t) tyName _) = ValueType s tyName

fromProcDef2FnType :: ProcedureDefinition -> TypeFunction
fromProcDef2FnType (DefineProc {procname = (IdExpr _ a _), arguments = b, body = _}) =
    let codom = NList a []
        dom = map fromIdentifier2ValueType b
    in FuncType {domain = dom, codomain = codom}
