module Data.String.Strip (strip)  where

import Data.Char

strip :: String -> String
strip = trim . reverse . trim . reverse
    where trim = dropWhile isSpace
