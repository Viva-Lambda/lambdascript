-- parsing state module
module Parser.ParsingState where

import qualified Data.Map as DMap
import TypeSystem.TypeIO
import TypeSystem.TypeChecker
import TypeSystem.TypeRules

type ParsingState = DMap.Map String Typed

emptyState :: ParsingState
emptyState = DMap.empty

-- addLit2PState :: Literal -> ParsingState -> ParsingState 
-- addLit2PState lit pstate =
--    let (ValueType s tname info) = fromLiteral2ValueType lit
