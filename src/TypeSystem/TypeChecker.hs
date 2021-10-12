-- module for dealing with type checking
module TypeSystem.TypeChecker where

import TypeSystem.TypeExpression
import Expression.Identifier

typeListEquivalence :: Eq a => [a] -> [a] -> Bool
typeListEquivalence a b = 
     if (length a) /= (length b)
    then False
    else let ziped = zipWith (==) a b
         in foldl1 (&&) ziped

{-
function rule: 
if f has type f: (s1,s2,s3) -> m and n1, n2, n3 have types s1, s2, s3
then f(n1, n2, n3) produces a value in type m
-}

checkFunctionType :: TypeFunction -> TypeFunction -> Bool
checkFunctionType a b = a == b

{-
product rule:
if f has type m x n and f(k,p) is such that k has type m 
    then p has type n
-}

{-
intersection rule:
f has type m ^ n and f(k) is such that k is both m and n
-}

{-
union rule:
f has type m ^ n and f(k) is such that k is both m and n
-}

{-
record rule:
if f has a type <x : t> then f has a member that has a type t
    struct MyS {
        int a;
        };
    struct MyS {
        float a;
        }; ?
-}
checkRecordType :: TypeRecord -> TypeRecord -> Bool
checkRecordType (RecordType a _ _) (RecordType b _ _) = a == b

checkTyped :: Typed -> Typed -> Bool
checkTyped (ValueTyper a) (ValueTyper b) = a == b
checkTyped (ValueTyper a) _ = False
checkTyped (ProductTyper _) (ProductTyper _) =
    error "NotImplemented: Product type checking is not implemented yet"
checkTyped (ProductTyper _) _ = False

checkTyped (FunctionTyper a) (FunctionTyper b) = checkFunctionType a b
checkTyped (FunctionTyper a) _ = False

checkTyped (IntersectionTyper a) (IntersectionTyper b) =
    error "NotImplemented: IntersectionType checking is not implemented yet"

checkTyped (IntersectionTyper a) _ = False

checkTyped (UnionTyper a) (UnionTyper b) =
    error "NotImplemented: UnionType checking is not implemented yet"

checkTyped (UnionTyper a) _ = False

checkTyped (RecordTyper a ) (RecordTyper b ) = checkRecordType a b
checkTyped (RecordTyper a ) _ = False

