import           Data.Set (Set, delete, fromList, map)
import           Prelude  hiding (map)

type Coordinate = (Int, Int)

type Vector = (Int, Int)

type AsteroidMap = Set Coordinate

normalizeVector :: Vector -> Vector
normalizeVector (x, y) =
  let g = gcd x y
   in (x `div` g, y `div` g)

vector :: Coordinate -> Coordinate -> Vector
vector (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

solve1 :: AsteroidMap -> Int
solve1 m =
  maximum $ map (\c -> length $ map (normalizeVector . vector c) (delete c m)) m

solve2 :: AsteroidMap -> Int
solve2 = undefined

parseInput :: [String] -> AsteroidMap
parseInput =
  fromList .
  concatMap f .
  zip [0 ..] . fmap (fmap fst . filter ((== '#') . snd) . zip [0 ..])
  where
    f :: (Int, [Int]) -> [(Int, Int)]
    f (y, xs) = fmap ((,) y) xs

main :: IO ()
main = do
  input <- parseInput . lines <$> getContents
  print $ solve1 input
  print $ solve2 input
