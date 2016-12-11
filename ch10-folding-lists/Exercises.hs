module Exercises where

main :: IO ()
main = putStrLn "Chapter 10: Folding Lists! (Exercises)"

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combinations :: [(Char, Char, Char)]
combinations =
  [(x, y, z) | x <- stops, y <- vowels, z <- stops]

startWithP :: [(Char, Char, Char)]
startWithP =
  [(x, y, z) | x <- stops, y <- vowels, z <- stops, x == 'p']

seekritFunc :: String -> Int
seekritFunc x =
  div (sum (map length (words x)))
           (length (words x))

seekritFunc' :: Fractional a => String -> a
seekritFunc' x = wsLength / wsCount
  where ws = words x
        wsLength = fromIntegral $ sum $ map length ws
        wsCount = fromIntegral $ length ws

-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
or' :: [Bool] -> Bool
-- or' b = foldr (||) False b
or' = foldr (||) False

any' :: (a -> Bool) -> [a] -> Bool
-- any' f xs = foldr ((||) . f) False xs
any' f = foldr ((||) . f) False

elem' :: Eq a => a -> [a] -> Bool
-- elem' el xs = foldr ((||) . (== el)) False xs
elem' el = foldr ((||) . (== el)) False

elem'' :: Eq a => a -> [a] -> Bool
elem'' el = any' (== el)

reverse' :: [a] -> [a]
-- reverse' xs = foldl f [] xs
--   where f acc x = x : acc
reverse' = foldl f []
  where f acc x = x : acc

map' :: (a -> b) -> [a] -> [b]
-- map' f xs = foldr ((:) . f) [] xs
map' f = foldr ((:) . f) []

filter' :: (a -> Bool) -> [a] -> [a]
-- filter' f xs = foldr f' [] xs
--   where f' x acc = if f x then x : acc else acc
filter' f = foldr f' []
  where f' x acc = if f x then x : acc else acc

squish :: [[a]] -> [a]
-- squish xs = foldr (++) [] xs
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
-- squishMap f xs = foldr ((++) . f) [] xs
squishMap f = foldr ((++) . f) []

greater :: (a -> a -> Ordering) -> a -> a -> a
greater f x y
  | f x y == GT = x
  | otherwise = y

maximumBy' :: (a -> a -> Ordering) -> [a] -> a
maximumBy' f = foldr1 (greater f)

minimumBy' :: (a -> a -> Ordering) -> [a] -> a
minimumBy' f = foldr1 (greater $ flip f)
