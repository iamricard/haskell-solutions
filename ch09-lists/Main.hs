module Main where

main :: IO ()
main = putStrLn "Chapter 9: Lists!"


-- 9.5
myEnumFromTo :: Enum a => a -> a -> [a]
myEnumFromTo start end
  | startPos > endPos = []
  | startPos == endPos = [start]
  | otherwise = start : myEnumFromTo (succ start) end
  where
    startPos = fromEnum start
    endPos = fromEnum end

eftBool :: Bool -> Bool -> [Bool]
eftBool = myEnumFromTo

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = myEnumFromTo

eftInt :: Int -> Int -> [Int]
eftInt = myEnumFromTo

eftChar :: Char -> Char -> String
eftChar = myEnumFromTo


-- 9.6

myWords :: String -> [String]
myWords "" = []
myWords (' ':str) = myWords str
myWords str = word : myWords rest
  where
    word = takeWhile (/= ' ') str
    rest = dropWhile (/= ' ') str


-- 9.7

-- mySqr :: [Integer]
-- mySqr = [x ^ 2 | x <- [1 .. 5]] -- [1, 4, 9, 16, 25]

-- [x | x <- mySqr, rem x 2 == 0]
-- [4, 16]

-- [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
-- []

-- take 5 [ (x, y) | x <- mySqr
--        , y <- mySqr, x < 50, y > 50 ]
-- []

-- mySqr' :: [Integer]
-- mySqr' = [x ^ 2 | x <- [1 .. 5]]
--
-- myCube :: [Integer]
-- myCube = [y ^ 3 | y <- [1 .. 5]]

-- both :: [(Integer, Integer)]
-- both = [(x, y) | x <- mySqr', y <- myCube]
--
-- bothUnderFifty :: [(Integer, Integer)]
-- bothUnderFifty = [(x, y) | x <- mySqr', y <- myCube, x < 50, y < 50]
--
-- numberOfTuples :: Int
-- numberOfTuples = length bothUnderFifty


-- 9.8

-- Will it blow up?
-- 1. Yes
-- 2. No
-- 3. Yes
-- 4. No
-- 5. Yes
-- 6. No
-- 7. Yes
-- 8. No
-- 9. No
-- 10. Yes

-- WHNF or NF or None?
-- 1. NF
-- 2. WHNF
-- 3. WHNF
-- 4. NF
-- 5. NF
-- 6. WHNF
-- 7. WHNF


-- 9.9
-- 1. ⊥
-- 2. [2]
-- 3. ⊥
-- 4. Goes through a String and returns an array where each character is True
--    or False depending or whether or not the character is a vowel. E.g.:
--    "Hello" -> [False, True, False, False, True]
-- 5.
--    a) [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
--    b) [1, 10, 20]
--    c) [15, 15, 15]


-- 9.10
multiplesOfThree :: [Int]
multiplesOfThree = filter (\x -> rem x 3 == 0) [1..30]

multiplesOfThree' :: [Int]
multiplesOfThree' = [x | x <- [1..30], rem x 3 == 0]

howManyMultiplesOfThree :: Int
howManyMultiplesOfThree = length multiplesOfThree

removeArticles :: String -> [String]
removeArticles = filter (`notElem` ["the", "am", "a"]) . words


-- 9.11

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

zip'' :: [a] -> [b] -> [(a, b)]
zip'' = zipWith' (\x y -> (x, y))
