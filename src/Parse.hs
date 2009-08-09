module Parse (split) where

split :: Eq a => a -> [a] -> [[a]]
split char =
  map (takeWhile (/= char)) .
  takeWhile (not . null) .
  iterate (drop 1 . dropWhile (/= char))

