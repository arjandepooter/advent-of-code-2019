import           AOC.IntComputer
import           Control.Monad.State
import           Data.List           (intersperse)
import           Data.Map            as M
import qualified Data.Set            as S
import           Prelude             hiding (Left, Right)

data Direction
  = Up
  | Right
  | Down
  | Left
  deriving (Show, Enum, Eq)

setInput :: Int -> State Runtime ()
setInput i = get >>= (\rt -> put rt {inputs = [i]})

moveDirection :: (Int, Int) -> Direction -> (Int, Int)
moveDirection (x, y) direction = (x + dx, y + dy)
  where
    (dx, dy) =
      case direction of
        Up    -> (0, 1)
        Right -> (1, 0)
        Down  -> (0, -1)
        Left  -> (-1, 0)

updatePosition :: Int -> (Int, Int) -> Direction -> ((Int, Int), Direction)
updatePosition turn (x, y) direction =
  (moveDirection (x, y) newDirection, newDirection)
  where
    newDirection :: Direction
    newDirection = toEnum $ (fromEnum direction + (2 * turn - 1)) `mod` 4

runPaintJob :: Map (Int, Int) Int -> Program -> Map (Int, Int) Int
runPaintJob initial prg =
  evalState (runPaintJob' initial ((0, 0), Up)) (initialize prg)
  where
    runPaintJob' ::
         Map (Int, Int) Int
      -> ((Int, Int), Direction)
      -> State Runtime (Map (Int, Int) Int)
    runPaintJob' panel ((x, y), direction) = do
      let currentColor = findWithDefault 0 (x, y) panel
      setInput currentColor
      (color, _) <- runUntilOutput
      (turn, finished) <- runUntilOutput
      if finished
        then return panel
        else do
          let updatedPanel = alter (const $ Just color) (x, y) panel
          let updatedPosition = updatePosition turn (x, y) direction
          runPaintJob' updatedPanel updatedPosition

showPaintJob :: S.Set (Int, Int) -> String
showPaintJob p =
  concatMap
    (\y ->
       '\n' :
       fmap
         (\x ->
            if (x, y) `S.member` p
              then '█'
              else ' ')
         hBounds)
    vBounds
  where
    xs = S.map fst p
    ys = S.map snd p
    hBounds = [S.findMin xs .. S.findMax xs]
    vBounds = reverse [S.findMin ys .. S.findMax ys]

solve1 :: Program -> Int
solve1 = length . runPaintJob empty

solve2 :: Program -> String
solve2 =
  showPaintJob . keysSet . M.filter (== 1) . runPaintJob (singleton (0, 0) 1)

main :: IO ()
main = do
  program <- parseInput <$> getLine
  print $ solve1 program
  putStr $ solve2 program
