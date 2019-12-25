import           AOC.IntComputer
import           Control.Monad.State
import           Data.Char           (chr)
import qualified Data.Map            as M
import           Prelude             hiding (Left, Right)

data Direction
  = Up
  | Right
  | Down
  | Left
  deriving (Show, Enum, Eq)

data Tile
  = Scaffold
  | Open
  | Robot Direction
  deriving (Show, Eq)

type Coord = (Int, Int)

type Map = M.Map Coord Tile

fetchMap :: Program -> Map
fetchMap = M.fromList . parseOutput (0, 0) . reverse . runProgram []
  where
    parseOutput :: Coord -> [Int] -> [(Coord, Tile)]
    parseOutput _ [] = []
    parseOutput (x, y) (10:ts) = parseOutput (0, y + 1) ts
    parseOutput c@(x, y) (t:ts) =
      let tile =
            case chr t of
              '#' -> Scaffold
              '.' -> Open
              '>' -> Robot Right
              '<' -> Robot Left
              'v' -> Robot Down
              '^' -> Robot Up
       in (c, tile) : (parseOutput (x + 1, y) ts)

neighbours :: Coord -> [Coord]
neighbours (x, y) = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

isIntersection :: Map -> Coord -> Bool
isIntersection m c = all (flip M.member m) (neighbours c)

solve1 :: Program -> Int
solve1 prg =
  let map = M.filter (== Scaffold) . fetchMap $ prg
   in sum .
      fmap (uncurry (*)) .
      M.keys . M.filterWithKey (\c _ -> isIntersection map c) $
      map

solve2 :: Program -> Int
solve2 = undefined

main :: IO ()
main = do
  program <- parseInput <$> getLine
  print $ solve1 program
  print $ solve2 program
