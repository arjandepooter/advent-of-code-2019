import           AOC.IntComputer
import           Control.Monad.State
import           Data.Bifunctor      (first)
import           Data.List           (intercalate)
import           Data.Set            as S

type Coord = (Int, Int)

data Direction
  = North
  | East
  | South
  | West
  deriving (Show, Enum, Eq)

type Maze = Set Coord

type Path = [Coord]

directionToInput :: Direction -> Int
directionToInput d =
  case d of
    North -> 1
    East  -> 4
    South -> 2
    West  -> 3

nextDirection :: Direction -> Direction
nextDirection = toEnum . (`mod` 4) . (+ 1) . fromEnum

prevDirection :: Direction -> Direction
prevDirection = toEnum . (`mod` 4) . subtract 1 . fromEnum

move :: Coord -> Direction -> Coord
move (x, y) direction =
  let (dx, dy) =
        case direction of
          North -> (0, 1)
          East  -> (1, 0)
          South -> (0, -1)
          West  -> (-1, 0)
   in (x + dx, y + dy)

buildMaze :: Program -> (Maze, Coord)
buildMaze =
  (\(_, position, maze) -> (maze, position)) .
  evalState (buildMaze' (East, (0, 0), empty)) . initialize
  where
    buildMaze' ::
         (Direction, Coord, Maze) -> State Runtime (Direction, Coord, Maze)
    buildMaze' (direction, position, maze') = do
      let maze = insert position maze'
      let input = (directionToInput . prevDirection) direction
      let newPosition = move position (prevDirection direction)
      setInput [input]
      runUntilInput
      result <- head <$> flushOutput
      case result of
        0 -> buildMaze' (nextDirection direction, position, maze)
        1 -> buildMaze' (prevDirection direction, newPosition, maze)
        2 -> return (direction, newPosition, insert newPosition maze)

findLongestEndpoint :: Maze -> Coord -> Int
findLongestEndpoint maze from
  | not (from `S.member` maze) = 0
  | otherwise =
    (+ 1) . maximum . fmap (findLongestEndpoint (delete from maze) . move from) $
    [North ..]

renderMaze :: Coord -> Coord -> Maze -> String
renderMaze start finish maze =
  (intercalate "\n" .
   fmap (\y -> concatMap (\x -> renderCell (x, y)) [minX .. maxX]))
    [minY .. maxY]
  where
    minX = (findMin . S.map fst) maze
    maxX = (findMax . S.map fst) maze
    minY = (findMin . S.map snd) maze
    maxY = (findMax . S.map snd) maze
    renderCell :: Coord -> String
    renderCell (x, y)
      | (x, y) == start = "S"
      | (x, y) == finish = "F"
      | (x, y) `S.member` maze = "#"
      | otherwise = " "

solve1 :: Program -> String
solve1 = uncurry (flip $ renderMaze (0, 0)) . buildMaze

solve2 :: Program -> Int
solve2 = subtract 1 . uncurry findLongestEndpoint . buildMaze

main :: IO ()
main = do
  program <- parseInput <$> getLine
  putStr $ solve1 program ++ "\n"
  print $ solve2 program
