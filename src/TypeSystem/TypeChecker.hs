-- module for dealing with type checking
module TypeSystem.TypeChecker where

import TypeSystem.TypeRules

{-
function rule: 
if f has type f: (s1,s2,s3) -> m and n1, n2, n3 have types s1, s2, s3
then f(n1, n2, n3) produces a value in type m
-}

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
-}
