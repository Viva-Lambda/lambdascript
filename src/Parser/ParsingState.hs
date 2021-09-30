-- parsing state module
module Parser.ParsingState where

import qualified Data.Map as DMap

type ParsingState = DMap.Map String String

emptyState :: ParsingState
emptyState = DMap.empty
