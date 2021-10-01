-- typing rules for the type system
module TypeSystem.TypeRules where

import Expression.Identifier
import qualified Data.Map as DMap

data TypedValue = ValueType String TypeName

instance Eq TypedValue where
    (ValueType _ a) == (ValueType _ b) = a == b

data TypeFunction = FuncType { 
    domain :: [TypedValue], codomain :: [TypedValue]
    }

instance Eq TypeFunction where
    (FuncType {domain = a,
               codomain = b}) == (FuncType {domain = c,
                                            codomain = d}) =
        (a == c) && (b == d)

data TypeProduct = ProductType [TypedValue]

instance Eq TypeProduct where
    (ProductType a) == (ProductType b) = a == b

data TypeIntersection = IntersectionType [TypedValue]

instance Eq TypeIntersection where
    (IntersectionType a) == (IntersectionType b) = a == b

data TypeUnion = UnionType [TypedValue]

instance Eq TypeUnion where
    (UnionType a) == (UnionType b) = a == b

data TypeRecord = RecordType String [TypedValue]

instance Eq TypeRecord where
    (RecordType _ a) == (RecordType _ b) = a == b

data Typed = ValueTyper TypedValue
            | FunctionTyper TypeFunction
            | ProductTyper TypeProduct
            | IntersectionTyper TypeIntersection
            | UnionTyper TypeUnion
            | RecordTyper TypeRecord

instance Eq Typed where
    (ValueTyper v) ==  (ValueTyper a) = v == a
    (ValueTyper _) == _ = False

    (FunctionTyper v) ==  (FunctionTyper a) = v == a
    (FunctionTyper _) == _ = False

    (ProductTyper v) ==  (ProductTyper a) = v == a
    (ProductTyper _) == _ = False

    (IntersectionTyper v) ==  (IntersectionTyper a) = v == a
    (IntersectionTyper _) == _ = False

    (UnionTyper v) ==  (UnionTyper a) = v == a
    (UnionTyper _) == _ = False

    (RecordTyper v) ==  (RecordTyper a) = v == a
    (RecordTyper _) == _ = False
