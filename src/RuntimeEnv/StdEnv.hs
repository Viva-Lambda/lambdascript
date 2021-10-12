-- standard runtime environment providing standard functions and operators
module RuntimeEnv.StdEnv where

import RuntimeEnv.FnNumber
import RuntimeEnv.FnBool


stdOps :: [String]
stdOps = boolOps ++ numericOps ++ ["!", "=", "<", ">"]
