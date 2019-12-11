import           Data.Foldable (maximumBy)
import           Data.List     (sort)
import           Data.Set      (Set, delete, fromList, map, toAscList)
import           Debug.Trace   (trace)
import           Prelude       hiding (map)

type Coordinate = (Int, Int)

data Quadrant
  = TopRight
  | BottomRight
  | BottomLeft
  | TopLeft
  deriving (Eq, Ord)

data Vector =
  Vector Int Int
  deriving (Show, Eq)

quadrant :: Vector -> Quadrant
quadrant (Vector x y)
  | x >= 0 && y > 0 = TopRight
  | x > 0 && y <= 0 = BottomRight
  | x <= 0 && y < 0 = BottomLeft
  | x < 0 && y >= 0 = TopLeft

type AsteroidMap = Set Coordinate

instance Ord Vector where
  compare v1 v2
    | v1 == v2 = EQ
    | q1 /= q2 = compare q1 q2
    | y1 == 0 && y2 == 0 = compare x1 x2
    | y1 == 0 = LT
    | y2 == 0 = GT
    | otherwise =
      let l = lcm y1 y2
       in case compare (l * x1 `div` y1) (l * x2 `div` y2) of
            EQ -> compare (x1 * x1 + y1 * y1) (x2 * x2 + y2 * y2)
            n  -> n
    where
      Vector x1 y1 = v1
      Vector x2 y2 = v2
      q1 = quadrant (Vector x1 y1)
      q2 = quadrant (Vector x2 y2)

normalizeVector :: Vector -> Vector
normalizeVector (Vector x y) =
  let g = gcd x y
   in Vector (x `div` g) (y `div` g)

multiply :: Vector -> Vector -> Vector
multiply (Vector x1 y1) (Vector x2 y2) = Vector (x1 * x2) (y1 * y2)

vector :: Coordinate -> Coordinate -> Vector
vector (x1, y1) (x2, y2) = Vector (x2 - x1) (y2 - y1)

asteroidVectors :: Coordinate -> AsteroidMap -> Set Vector
asteroidVectors c = map (vector c) . delete c

findBestAsteroid :: AsteroidMap -> (Coordinate, Int)
findBestAsteroid m =
  maximumBy (\(_, l1) (_, l2) -> compare l1 l2) $
  map (\c -> (c, length $ map (normalizeVector . vector c) (delete c m))) m

shootLaser :: [(Vector, [Coordinate])] -> [Coordinate]
shootLaser []                 = []
shootLaser ((vec, []):rest)   = shootLaser rest
shootLaser ((vec, c:cs):rest) = c : shootLaser (rest ++ [(vec, cs)])

solve1 :: AsteroidMap -> Int
solve1 = snd . findBestAsteroid

solve2 :: AsteroidMap -> Int
solve2 m =
  (\(x, y) -> x * 100 + y) .
  flip (!!) 199 .
  shootLaser .
  foldr reducer [] .
  fmap (coordWithNVector . transposeY) . toAscList . map transposeY $
  vectors
  where
    (cx, cy) = (fst . findBestAsteroid) m
    vectors = asteroidVectors (cx, cy) m
    transposeY = multiply (Vector 1 (-1))
    coordWithNVector (Vector x y) =
      (normalizeVector (Vector x y), (x + cx, y + cy))
    reducer ::
         (Vector, Coordinate)
      -> [(Vector, [Coordinate])]
      -> [(Vector, [Coordinate])]
    reducer (vec, c) [] = [(vec, [c])]
    reducer (vec, c) ((prev, cs):rest) =
      if vec == prev
        then (vec, c : cs) : rest
        else (vec, [c]) : (prev, cs) : rest

parseInput :: [String] -> AsteroidMap
parseInput =
  fromList .
  concatMap f .
  zip [0 ..] . fmap (fmap fst . filter ((== '#') . snd) . zip [0 ..])
  where
    f :: (Int, [Int]) -> [(Int, Int)]
    f (y, xs) = fmap (flip (,) y) xs

main :: IO ()
main = do
  input <- parseInput . lines <$> getContents
  print $ solve1 input
  print $ solve2 input
