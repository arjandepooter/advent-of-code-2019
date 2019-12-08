module AOC.Utils
  ( splitOn
  , chunksOf
  ) where

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn sep = foldr folder []
  where
    folder cur [] =
      if cur == sep
        then [[]]
        else [[cur]]
    folder cur (curPart:rest) =
      if cur == sep
        then [] : curPart : rest
        else (cur : curPart) : rest

chunksOf :: Int -> [a] -> [[a]]
chunksOf size l =
  if length l <= size
    then [l]
    else (take size l : chunksOf size (drop size l))
