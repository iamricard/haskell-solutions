module Main where

main :: IO ()
main = putStrLn "Chapter 12: Signaling Adversity"

-- 12.5 Chapter Exercises
-- 1) :k a :: *
-- 2) :k a :: *
--    :k f * -> *

notThe :: String -> Maybe String
notThe s
  | s == "the" = Nothing
  | otherwise = Just s

replaceThe :: String -> String
replaceThe = unwords . map (replace . notThe) . words
  where replace Nothing = "a"
        replace (Just s) = s

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"

startsWithVowel :: Maybe String -> Maybe Bool
startsWithVowel = fmap (isVowel . head)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go 0 . map (startsWithVowel . notThe) . words
  where go count [] = count
        go count [_] = count
        go count (Just _ : _ : xs) = go count xs
        go count (Nothing : Nothing : xs) = go count xs
        go count (Nothing : Just True : xs) = go (count + 1) xs
        go count (Nothing : Just False : xs) = go count xs

countVowels :: String -> Integer
countVowels = fromIntegral . length . filter id . fmap isVowel

newtype Word' =
  Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord w = if vowels > consonants
           then Nothing
           else Just (Word' w)
           where vowels = length $ filter isVowel w
                 consonants = length $ filter (not . isVowel) w

data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat 0 = Just Zero
integerToNat number
  | number > 0 = Just $ go number
  | otherwise = Nothing
  where go 0 = Zero
        go n = Succ $ go $ n - 1

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ fn (Just x) = fn x
mayybee fallback _ _ = fallback

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe fallback _ = fallback

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList _ = []

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just x : xs) = x : catMaybes xs
catMaybes (Nothing : xs) = catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe xs = foldr fn (Just []) xs
  where fn _ Nothing = Nothing
        fn Nothing _ = Nothing
        fn (Just x) (Just acc) = Just (x : acc)

lefts' :: [Either a b] -> [a]
lefts' [] = []
lefts' xs = foldr fn [] xs
  where fn (Right _) acc = acc
        fn (Left x) acc = x : acc

rights' :: [Either a b] -> [b]
rights' [] = []
rights' xs = foldr fn [] xs
  where fn (Left _) acc = acc
        fn (Right x) acc = x : acc

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers [] = ([], [])
partitionEithers xs = foldr fn ([], []) xs
  where fn (Left x) (ls, rs) = (x:ls, rs)
        fn (Right x) (ls, rs) = (ls, x:rs)

eitherMaybe :: (b -> c) -> Either a b -> Maybe c
eitherMaybe _ (Left _) = Nothing
eitherMaybe fn (Right x) = Just $ fn x

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fn _ (Left x) = fn x
either' _ fn (Right x) = fn x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' fn = either' (const Nothing) (Just . fn)

iterate' :: (a -> a) -> a -> [a]
iterate' fn x = x : iterate' fn (fn x)

unfoldr' :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr' fn x = go (fn x) []
  where go Nothing ys = ys
        go (Just (y, y')) ys = y : go (fn y') ys

iterate'' :: (a -> a) -> a -> [a]
iterate'' f = unfoldr' (\y -> Just (y, f y))

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfoldbt :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfoldbt fn x =
  case fn x of
    Nothing -> Leaf
    Just (l, n, r) -> Node (unfoldbt fn l) n (unfoldbt fn r)

treeBuild :: Integer -> BinaryTree Integer
treeBuild x = unfoldbt f 0
  where f y
          | y == x = Nothing
          | otherwise = Just (y + 1, y, y + 1)
