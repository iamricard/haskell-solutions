module Main where

main :: IO ()
main = putStrLn "Chapter 8: Recursion!"



-- 8.2

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 _ b = b
applyTimes n f b = f . applyTimes (n - 1) f $ b

-- Write out the evaluation of the following. It might be a little less noisy if
-- you do so with the form that didn’t use (.).

-- applyTimes 5 (+1) 5

-- applyTimes 5 (+1) 5
-- (+1) . applyTimes 4 (+1) $ 5
-- (+1) . (+1) . applyTimes 3 (+1) $ 5
-- (+1) . (+1) . (+1) . applyTimes 2 (+1) $ 5
-- (+1) . (+1) . (+1) . (+1) . applyTimes 1 (+1) $ 5
-- (+1) . (+1) . (+1) . (+1) . (+1) . applyTimes 0 (+1) $ 5
-- (+1) . (+1) . (+1) . (+1) . (+1) $ 5



-- 8.6

-- Review of types

-- 1.
-- d) [[Bool]]

-- 2.
-- b) [[3 == 3], [6 > 5], [3 < 4]]

-- 3.
-- d) all of the above

-- 4.
-- b) func "Hello" "World"



-- Reviewing currying

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

-- 1. What is the value of appedCatty "woohoo!" ? Try to determine the answer
--    for yourself, then test in the REPL.
exOne :: Bool
exOne = appedCatty "woohoo!" == "woops mrow woohoo!"

-- 2. frappe "1"
exTwo :: Bool
exTwo = frappe "1" == "1 mrow haha"

-- 3. frappe (appedCatty "2")
exThree :: Bool
exThree = frappe (appedCatty "2") == "woops mrow 2 mrow haha"

-- 4. appedCatty (frappe "blue")
exFour :: Bool
exFour = appedCatty (frappe "blue") == "woops mrow blue mrow haha"

-- 5. cattyConny (frappe "pink")
--               (cattyConny "green" (appedCatty "blue"))
exFive :: Bool
exFive =
  let
    result = cattyConny (frappe "pink") (cattyConny "green" $ appedCatty "blue")
  in
    result == "pink mrow haha mrow green mrow woops mrow blue"

-- 6. cattyConny (flippy "Pugs" "are") "awesome"
exSix :: Bool
exSix = cattyConny (flippy "Pugs" "are") "awesome" == "are mrow Pugs mrow awesome"



-- Recursion

-- dividedBy :: Integral a => a -> a -> (a, a)
-- dividedBy num denom = go num denom 0
--   where go n d count
--          | n < d = (count, n)
--          | otherwise = go (n - d) d (count + 1)

-- 1. dividedBy 15 2
-- go 15 2 0
-- go 13 2 1
-- go 11 2 2
-- go 9 2 3
-- go 7 2 4
-- go 5 2 5
-- go 3 2 6
-- go 1 2 7
-- (7, 1)



-- 2.

sumToN :: (Eq a, Num a) => a -> a
sumToN = go 0
  where go acc count
         | count == 0 || signum count == -1 = acc
         | otherwise = go (acc + count) (count - 1)



-- 3.
flipSign :: Integral a => a -> a -> Bool
flipSign x y =
  signum x /= signum y

multiply :: Integral a => a -> a -> a
multiply x y = go 0 x' y'
  where
    x' = abs x
    y' = abs y
    go acc _ 0 = if flipSign x y then (-acc) else acc
    go acc left right = go (acc + left) left (right - 1)



-- Fixing dividedBy

dividedBy :: Integral a => a -> a -> Maybe (a, a)
dividedBy _ 0 = Nothing
dividedBy num denom = go num' denom' 0
  where
    num' = abs num
    denom' = abs denom
    go n d count
     | n < d = Just (if flipSign num denom then (-count) else count, n)
     | otherwise = go (n - d) d (count + 1)



-- McCarthy 91 function

mc91 :: (Ord a, Num a) => a -> a
mc91 x
  | x > 100 = x - 10
  | otherwise = 91
