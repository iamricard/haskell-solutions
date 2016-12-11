module Main where

main :: IO ()
main = putStrLn "Chapter 10: Folding Lists!"

-- 10.5

-- 1) b & c

-- 2) (flip (*) (flip (*) (flip (*) 1 1) 2) 3)

-- 3) c

-- 4) a

-- 5)
-- a)
a :: String
a = foldr (++) "" ["woot", "WOOT", "woot"]

-- b)
b :: Char
b = foldr max minBound "fear is the little death"

-- c)
c :: Bool
c = foldr (&&) True [False, True]

-- d)
-- This one is more subtle than the previous. Can it ever
-- return a different answer?
d :: Bool
d = foldr (||) False [False, True]

-- e)
e :: String
e = foldr ((++) . show) "" ([1..5] :: [Integer])

-- f)
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- const :: a -> b -> a
-- flip const :: b -> a -> a == (a -> b -> b)
f :: Char
f = foldl const 'a' ([1..5] :: [Integer])

-- g)
g :: Integer
g = foldl const 0 "tacos"

-- h)
h :: Integer
h = foldl const 0 "burritos"

-- i)
i :: Char
i = foldl const 'z' ([1..5] :: [Integer])
