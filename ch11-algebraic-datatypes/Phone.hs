module Phone where

import Data.Char (toLower, isUpper)

type Digit = Char
type Presses = Int

data Button = Button Digit String
data Phone = Phone [Button]

phone :: Phone
phone =
  Phone [ Button '1' "", Button '2' "abc", Button '3' "def"
        , Button '4' "ghi", Button '5' "jkl", Button '6' "mno"
        , Button '7' "pqrs", Button '8' "tuv", Button '9' "wxyz"
        , Button '*' "^", Button '0' "+ ", Button '#' ".,"
        ]

idx :: Eq a => a -> [a] -> Int
idx _ [] = -1
idx x ys = go x ys 0
  where go _ [] _ = -1
        go x' (y:ys') count
          | x' == y = count
          | otherwise = go x' ys' (count + 1)

reverseTaps :: Phone -> Char -> [(Digit, Presses)]
reverseTaps (Phone []) _ = [(' ', 0)]
reverseTaps (Phone (Button digit characters : btns)) c
  | c == digit = [(digit, length characters + 1)]
  | toLower c `elem` characters = if isUpper c
                                  then [('*', 1), (digit, idx (toLower c) characters + 1)]
                                  else [(digit, idx c characters + 1)]
  | otherwise = reverseTaps (Phone btns) c


cellPhonesDead :: Phone -> String -> [(Digit, Presses)]
cellPhonesDead = concatMap . reverseTaps

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol lol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Haha thanks just making sure rofl ur turn"
  ]
