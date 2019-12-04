import           Control.Monad      (liftM2)
import           Data.Maybe         (fromJust)
import           Debug.Trace        (trace)
import           Text.Parsec        hiding (Line, between)
import           Text.Parsec.String (Parser)

--- Types
type Position = (Int, Int)

type Length = Int

data Orientation
  = Horizontal
  | Vertical
  deriving (Eq, Show)

data Line =
  Line Position Position
  deriving (Show, Eq)

data Direction
  = Up
  | Rgt
  | Dwn
  | Lft
  deriving (Show, Eq)

data Segment =
  Segment Direction Length
  deriving (Show)

--- Parse input
directionFromChar :: Char -> Direction
directionFromChar c =
  case c of
    'U' -> Up
    'R' -> Rgt
    'D' -> Dwn
    _   -> Lft

stepParser :: Parser Segment
stepParser = do
  direction <- directionFromChar <$> oneOf "URDL"
  length <- read <$> many1 digit
  return $ Segment direction length

parseLine :: String -> String -> Either ParseError [Segment]
parseLine = parse (many (stepParser <* optional (char ',')) <* eof)

main :: IO ()
main = do
  wire1 <- parseLine "line 1" <$> getLine
  wire2 <- parseLine "line 2" <$> getLine
  let inp = (,) <$> wire1 <*> wire2
  case inp of
    Left err -> print err
    Right n -> do
      print $ solve1 n
      print $ solve2 n

--- Solutions
solve1 :: ([Segment], [Segment]) -> Int
solve1 (w1, w2) =
  minimum . filter (> 0) . fmap (uncurry (+) . fromJust) . filter (not . null) $
  intersections
  where
    intersections =
      [ l1 `intersect` l2
      | l1 <- linesFromSegments w1
      , l2 <- linesFromSegments w2
      ]

solve2 :: ([Segment], [Segment]) -> Int
solve2 (w1, w2) =
  minimum .
  filter (> 0) .
  fmap (intersectLength . fmap fromJust) . filter (not . null . snd) $
  intersections
  where
    intersections =
      [ (((ls1, l1), (ls2, l2)), l1 `intersect` l2)
      | (ls1, l1) <- linesWithLength w1
      , (ls2, l2) <- linesWithLength w2
      ]
    intersectLength :: (((Int, Line), (Int, Line)), Position) -> Int
    intersectLength (((l1, Line start1 _), (l2, Line start2 _)), pos) =
      l1 + l2 + distance start1 pos + distance start2 pos

distance :: Position -> Position -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

between :: Ord a => a -> a -> a -> Bool
between n1 n2 n = (n1 <= n && n <= n2) || (n2 <= n && n <= n1)

intersect :: Line -> Line -> Maybe Position
intersect (Line (v1, w1) (v2, w2)) (Line (x1, y1) (x2, y2))
  | v1 == v2 && y1 == y2 && between x1 x2 v1 && between w1 w2 y1 = Just (v1, y1)
  | x1 == x2 && w1 == w2 && between v1 v2 x1 && between y1 y2 w1 = Just (x1, w1)
  | otherwise = Nothing

lineFromSegment :: Position -> Segment -> Line
lineFromSegment (x, y) (Segment d l) = Line (x, y) (x + dx, y + dy)
  where
    dx =
      case d of
        Rgt -> l
        Lft -> -l
        _   -> 0
    dy =
      case d of
        Up  -> l
        Dwn -> -l
        _   -> 0

linesFromSegments :: [Segment] -> [Line]
linesFromSegments = reverse . snd . foldl f ((0, 0), [])
  where
    f :: (Position, [Line]) -> Segment -> (Position, [Line])
    f (pos, lines) s =
      let Line start end = lineFromSegment pos s
       in (end, Line start end : lines)

linesWithLength :: [Segment] -> [(Int, Line)]
linesWithLength segs = zip (scanl f 0 segs) (linesFromSegments segs)
  where
    f n (Segment _ l) = n + l
