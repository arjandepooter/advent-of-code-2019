import           AOC.Utils     (chunksOf)
import           Control.Monad (join)
import           Data.Char     (digitToInt)
import           Data.Foldable (minimumBy)
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
solve1 =
  snd .
  minimumBy (\(c1, _) (c2, _) -> compare c1 c2) .
  fmap (\layer -> (count 0 layer, count 1 layer * count 2 layer)) .
  chunksOf (25 * 6)

solve2 :: [Int] -> String
solve2 = showImage 25 . decodeImage . chunksOf (25 * 6)

main :: IO ()
main = do
  img <- fmap digitToInt <$> getLine
  print $ solve1 img
  putStr $ solve2 img
