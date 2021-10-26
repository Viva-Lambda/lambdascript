-- typing context/basis module the Gamma in most books
module TypeSystem.TypeBasis where

import Lexer.Lexer
import Utils

{-
Type atoms from: 
Barendregt, H.P., Dekkers, W. and Statman, R. (2013) Lambda calculus with
types, p. 7
-}
data TypeAtom = AtStr String
              | AtDouble Double
              | AtInt Int
              | AtBool Bool
              deriving (Show)

instance Eq TypeAtom where
    (AtStr a) == (AtStr b) = a == b
    (AtStr _) == _ = False

    (AtDouble a) == (AtDouble b) = a == b
    (AtDouble _) == _ = False

    (AtInt a) == (AtInt b) = a == b
    (AtInt _) == _ = False

    (AtBool a) == (AtBool b) = a == b
    (AtBool _) == _ = False

{-
Set of simple types
Barendregt, H.P. (2013) Lambda calculus with types, p. 7
-}

data SimpleType = StAtom TypeAtom
                 | StFunc {inputType :: SimpleType, outputType :: SimpleType}

{-
Type statements
Barendregt, H.P. (2013) Lambda calculus with types, p. 8
-}

type Subject = (String, TokenInfo)

data TypeStatement = TStatement { subject :: Subject,
                                  predicate :: SimpleType
                                }
{-
Typing context consists of hypothesis of the form G  x: t which reads as x has
the type t in the context
-}
type TypeBasis = NonEmptyList TypeStatement

basisDomain :: TypeBasis -> NonEmptyList Subject
basisDomain (NList a ts) = NList (subject a) (map subject ts)

{-
Type statement rules from
Barendregt, H.P. (2013) Lambda calculus with types, p. 8

-}
-- eliminate :: TypeStatement -> TypeStatement
-- axiom :: TypeStatement -> TypeBasis -> TypeStatement
-- introduce :: TypeStatement -> TypeBasis -> TypeStatement
