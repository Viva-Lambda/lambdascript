-- parsing state module
module TypeSystem.TypeError where

import TypeSystem.TypeExpression
import Expression.Identifier

data TypeError = NameAlreadyBound String TypeName Typed

instance Show TypeError where
    show (NameAlreadyBound idn (VName tn info) t) =
        let info2 = getTypeInfo t
            msg1 = "TypeError :: Name already bound to another type"
            msg2 = " \nName: " ++ idn
            msg3 = " \nPrevious declaration: " ++ show info2
            msg4 = " \nCurrent declaration: " ++ show info
        in error $ msg1 ++ msg2 ++ msg3 ++ msg4

