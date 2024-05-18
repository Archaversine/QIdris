module Util

import Data.Vect

export
unlines : Vect n String -> String
unlines [] = ""
unlines (x :: xs) = x ++ "\n" ++ unlines xs