module Exercises where

import Data.Char

onlyUpper :: String -> String
onlyUpper "" = ""
onlyUpper s = filter isUpper s

capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = toUpper x : xs

uppercase :: String -> String
uppercase "" = ""
uppercase (x:xs) = toUpper x : uppercase xs

-- This could break!
initial :: String -> Char
initial = toUpper . head
