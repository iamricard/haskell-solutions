module Ciphers where

import Data.Char

vigenere :: String -> String -> String
vigenere key str = go str (cycle key)
  where go "" _ = ""
        go xs "" = xs
        go (' ':xs) ys = ' ' : go xs ys
        go (x:xs) (y:ys) = replaced : go xs ys
          where
            base = if isLower y then ord 'a' else ord 'A'
            baseLetter = if isLower x then ord 'a' else ord 'A'
            code = (ord y - base) + (ord x - baseLetter)
            replaced = chr (code `mod` 26 + baseLetter)

encrypt :: String -> String -> String
encrypt = vigenere

decrypt :: String -> String -> String
decrypt =
  vigenere . generateKey
  where mapper c = chr (26 - (ord c - base) + base)
          where base = if isLower c then ord 'a' else ord 'A'
        generateKey = map mapper
