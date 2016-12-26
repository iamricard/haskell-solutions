module Main where

import Data.Char (toUpper)

main :: IO ()
main =
  putStrLn "Chapter 11: Algebraic datatypes!"

-- 11.5
-- 1) Type constructor
-- 2) Doggies :: * -> *
-- 3) Doggies String :: *
-- 4) Husky 10 :: Num a => Doggies a
-- 5) Husky (10 :: Integer) :: Doggies Integer
-- 6) Mastiff "Scooby Doo" :: Doggies String
-- 7) Both!
-- 8) DogueDeBordeaux :: DogueDeBordeaux a
-- 9) DogueDeBordeaux "doggie!" :: DogueDeBordeaux String

-- 11.8 Cardinality
-- 1) 1
-- 2) 3
-- 3) 65535
-- 4) Int would be Int64
--    Integer is not Bounded
-- 5) Int8 --> 2 ^ 8 --> 256

-- Simple datatypes with nullary data constructors
-- 1) MakeExample :: Example
-- 2) Yes --> instance Show Example
-- 3) It's like a function now ArgumentType -> Type

-- 11.10 Sum types
-- 1) 4
-- 2) 258

-- 11.13 Constructing and deconstructing values

data OperatingSystem
  = GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgrammingLanguage
  = Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgrammingLanguage }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgrammingLanguage]
allLanguages =
  [ Haskell
  , Agda
  , Idris
  , PureScript
  ]

allProgrammers :: [Programmer]
allProgrammers =
  [ Programmer o l | o <- allOperatingSystems, l <- allLanguages ]


-- 11.4 Function type is exponential

-- 1) 4 + 4 = 8, because Either is a sum type.
-- 2) 4 * 4 = 16
-- 3) 4 ^ 4 = 256, a -> b, b^a where a = 4, b = 4
-- 4) 2 * 2 * 2 = 8
-- 5) (2 ^ 2) ^ 2 == 2 ^ (2 * 2) = 16
-- 6) (c ^ b) ^ a, where a = 2, b = 4, c = 4
--    c ^ (b * a)
--    4 ^ (4 * 2)
--    4 ^ 8 = 65536


-- 11.17 Binary Tree
data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)


-- insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
-- insert' b Leaf = Node Leaf b Leaf
-- insert' b (Node left a right)
--   | b == a = Node left a right
--   | b < a = Node (insert' b left) a right
--   | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree fn (Node left x right) =
  Node (mapTree fn left) (fn x) (mapTree fn right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected =
  Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay :: IO ()
mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left x right) =
  [x] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left x right) =
  inorder left ++ [x] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left x right) =
  postorder left ++ postorder right ++ [x]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "postorder failed check"

toListOK :: IO ()
toListOK = do
  testPreorder
  testInorder
  testPostorder

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ identity Leaf = identity
foldTree fn identity (Node left x right) =
  fn x (foldTree fn (foldTree fn identity left) right)


-- 11.8 Chapter exercises
-- 1) a
-- 2) c
-- 3) b
-- 4) c


-- As-patterns
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf ax@(x:xs) (y:ys)
  | x == y = isSubsequenceOf xs ys
  | otherwise = isSubsequenceOf ax ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords = go . words
  where go = map (\s@(c:cs) -> (toUpper c : cs, s))


-- Language exercises
capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (c:cs) = toUpper c : cs

capitalizeParagraph :: String -> String
capitalizeParagraph = go True
  where go capitalize (' ':cs) = ' ' : go capitalize cs
        go _ ('.':cs) = '.' : go True cs
        go _ "" = ""
        go True (c:cs) = toUpper c : go False cs
        go False (c:cs) = c : go False cs
