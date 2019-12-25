import           AOC.IntComputer
import           Control.Monad.State
import           Data.Char           (chr, ord)
import           Data.List           (intercalate)
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
  deriving (Eq)

instance Show Tile where
  show Scaffold      = "#"
  show (Robot Left)  = "<"
  show (Robot Right) = ">"
  show (Robot Up)    = "^"
  show (Robot Down)  = "v"
  show _             = "."

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

showMap :: Map -> String
showMap m =
  intercalate "\n" $
  fmap
    (\y ->
       concatMap (\x -> (show $ M.findWithDefault Open (x, y) m)) [minX .. maxX])
    [minY .. maxY]
  where
    minX = (minimum . fmap fst . M.keys) m
    maxX = (maximum . fmap fst . M.keys) m
    minY = (minimum . fmap snd . M.keys) m
    maxY = (maximum . fmap snd . M.keys) m

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
solve2 (p:ps) = head . runProgram input $ (2 : ps)
  where
    input =
      fmap ord $
      (intercalate
         "\n"
         [ "A,A,B,C,C,A,C,B,C,B"
         , "L,4,L,4,L,6,R,10,L,6"
         , "L,12,L,6,R,10,L,6"
         , "R,8,R,10,L,6"
         , "n"
         ]) ++
      "\n"

main :: IO ()
main = do
  program <- parseInput <$> getLine
  putStrLn . showMap . fetchMap $ program
  print $ solve1 program
  print $ solve2 program
