module Std where

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = x || or' xs

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' f (x:xs) = f x || any' f xs

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x' (x:xs) = x == x' || elem' x' xs

elem'' :: Eq a => a -> [a] -> Bool
elem'' x = any' (== x)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

squish' :: [[a]] -> [a]
squish' [] = []
squish' (x:xs) = x ++ squish' xs

squishMap' :: (a -> [b]) -> [a] -> [b]
squishMap' _ [] = []
squishMap' f (x:xs) = f x ++ squishMap' f xs

squish'' :: [[a]] -> [a]
squish'' = squishMap' id

maximumBy' :: (a -> a -> Ordering) -> [a] -> a
maximumBy' _ [] = undefined
maximumBy' _ [x] = x
maximumBy' f (x:y:xs) = maximumBy' f (greater : xs)
  where greater = if f x y == GT then x else y

minimumBy' :: (a -> a -> Ordering) -> [a] -> a
minimumBy' _ [] = undefined
minimumBy' _ [x] = x
minimumBy' f (x:y:xs) = minimumBy' f (greater : xs)
  where greater = if f x y == LT then x else y

maximum' :: Ord a => [a] -> a
maximum' = maximumBy' compare

minimum' :: Ord a => [a] -> a
minimum' = minimumBy' compare
