import           AOC.Utils     (chunksOf)
import           Control.Monad (join)
import           Data.Char     (digitToInt)
import           Data.List     (intersperse, transpose)
import           Data.Monoid

data Pixel
  = Black
  | White
  | Transparent

instance Semigroup Pixel where
  Transparent <> other = other
  other <> Transparent = other
  first <> _ = first

instance Monoid Pixel where
  mempty = Transparent

instance Show Pixel where
  show Black = "█"
  show White = "░"
  show _     = " "

intToPixel :: Int -> Pixel
intToPixel n =
  case n of
    0 -> Black
    1 -> White
    _ -> Transparent

count :: Eq a => a -> [a] -> Int
count needle [] = 0
count needle (hd:rest)
  | needle == hd = 1 + count needle rest
  | otherwise = count needle rest

decodeImage :: [[Int]] -> [Pixel]
decodeImage = fmap mconcat . transpose . fmap (fmap intToPixel)

showImage :: Int -> [Pixel] -> String
showImage w = join . intersperse "\n" . fmap (show =<<) . chunksOf w

solve1 :: [Int] -> Int
solve1 = snd . foldl f (150, 0) . chunksOf (25 * 6)
  where
    f :: (Int, Int) -> [Int] -> (Int, Int)
    f (mn, result) layer =
      if zeroCount < mn
        then (zeroCount, count 1 layer * count 2 layer)
        else (mn, result)
      where
        zeroCount = count 0 layer

solve2 :: [Int] -> String
solve2 = showImage 25 . decodeImage . chunksOf (25 * 6)

main :: IO ()
main = do
  img <- fmap digitToInt <$> getLine
  print $ solve1 img
  putStr $ solve2 img
