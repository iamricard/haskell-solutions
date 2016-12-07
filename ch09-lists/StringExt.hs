module StringExt (mySplitAt) where

mySplitAt :: Char -> String -> [String]
mySplitAt _ "" = []
mySplitAt del (hd:tl)
  | hd == del = mySplitAt del tl
  | otherwise = chunk : mySplitAt del rest
  where
    chunk = takeWhile (/= del) (hd:tl)
    rest = dropWhile (/= del) (hd:tl)
