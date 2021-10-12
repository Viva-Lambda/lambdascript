-- typing rules for the type system
module TypeSystem.TypeExpression where

import Expression.Identifier
import Lexer.Lexer
import qualified Data.Map as DMap
import Utils



data TypeValue = ValueType String TypeName

instance Eq TypeValue where
    (ValueType _ a ) == (ValueType _ b ) = a == b

instance Show TypeValue where
    show (ValueType a _) = show a

data TypeFunction = FuncType {
    domain :: [TypeValue], 
    codomain :: (NonEmptyList TypeName)
    }

instance Eq TypeFunction where
    a == b =
        let (FuncType {domain = c, codomain = NList d e}) = a
            (FuncType {domain = f, codomain = NList g h}) = b
        in (c == f) && (d == g) && (e == h)

instance Show TypeFunction where
    show (FuncType {domain = a, codomain = NList b c}) =
        let msg = unwords (map show a)
            msg2 = show b ++ " " ++ (unwords $! map show c)
        in msg ++ msg2

-- for function arguments
data TypeProduct = ProductType [TypeValue] TokenInfo

instance Eq TypeProduct where
    (ProductType a _) == (ProductType b _) = a == b

instance Show TypeProduct where
    show (ProductType a _) = unwords $! (map show a)

-- for function body
data TypeIntersection = IntersectionType (NonEmptyList TypeValue)

instance Eq TypeIntersection where
    (IntersectionType (NList a b)) == (IntersectionType (NList c d)) = (a:b) == (c:d)

instance Show TypeIntersection where
    show (IntersectionType (NList a b)) =
        let msg = show a ++ " " ++ (unwords $! map show b)
        in msg

-- for combining types
data TypeUnion = UnionType (NonEmptyList TypeValue)

instance Eq TypeUnion where
    (UnionType (NList a b)) == (UnionType (NList c d)) = (a:b) == (c:d)

instance Show TypeUnion where
    show (UnionType (NList a b)) =
        let msg = show a ++ " " ++ (unwords $! map show b)
        in msg

-- for heterogeneous objects
type TRecordMap = DMap.Map String TypeName

data TypeRecord = RecordType String TRecordMap TokenInfo

instance Eq TypeRecord where
    (RecordType _ a _) == (RecordType _ b _) = a == b

instance Show TypeRecord where
    show (RecordType a b _) = a ++ " " ++ (show b)

data Typed = ValueTyper TypeValue
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

instance Show Typed where
    show (ValueTyper v) = show v
    show (FunctionTyper v) = show v
    show (ProductTyper v) = show v
    show (IntersectionTyper v) = show v
    show (UnionTyper v) = show v
    show (RecordTyper v) = show v

getTypeInfo :: Typed -> TokenInfo
getTypeInfo (ValueTyper (ValueType _ (VName _ t))) = t
getTypeInfo (FunctionTyper (FuncType {domain=_, codomain=NList (VName _ a) _})) = a
getTypeInfo (ProductTyper (ProductType _ t) ) = t
getTypeInfo (IntersectionTyper (IntersectionType (NList (ValueType _ (VName _ t)) _) ) ) = t
getTypeInfo (UnionTyper (UnionType (NList (ValueType _ (VName _ t)) _) ) ) = t
getTypeInfo (RecordTyper (RecordType _ _ t) ) = t

type TypeScope = DMap.Map String Typed
