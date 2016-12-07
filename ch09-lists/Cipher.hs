module Cipher where

import Data.Char

shift :: Int -> String -> String
shift _ "" = ""
shift n (' ':xs) =
  ' ' : shift n xs
shift n (x:xs) = chr (code `mod` 26 + offset) : shift n xs
  where offset = if isUpper x then 65 else 97
        code = ord x - offset + n



encrypt :: Int -> String -> String
encrypt = shift

decrypt :: Int -> String -> String
decrypt n = shift (-n)
