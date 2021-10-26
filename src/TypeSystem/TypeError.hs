-- parsing state module
module TypeSystem.TypeError where

import TypeSystem.TypeExpression
import Expression.Identifier

type WrongT = String
type CorrectT = String

data TypeError = NameAlreadyBound String TypeName Typed
               | WrongType CorrectT WrongT Typed

instance Show TypeError where
    show (NameAlreadyBound idn (VName _ info) t) =
        let info2 = getTypeInfo t
            msg1 = "TypeError :: Name already bound to another type"
            msg2 = " \nName: " ++ idn
            msg3 = " \nPrevious declaration: " ++ show info2
            msg4 = " \nCurrent declaration: " ++ show info
        in error $ msg1 ++ msg2 ++ msg3 ++ msg4

    show (WrongType a b t) =
        let info = getTypeInfo t
            msg1 = "TypeError :: Expression " ++ show info
            msg2 = " \nrequires type: " ++ a
            msg3 = " \nbut provided type: " ++ b
        in msg1 ++ msg2 ++ msg3
