{-# LANGUAGE NamedFieldPuns #-}

import           AOC.IntComputer
import           AOC.Utils           (chunksOf)
import           Control.Monad       (when)
import           Control.Monad.State
import           Data.List           (intercalate)
import qualified Data.Map            as M

type Coord = (Int, Int)

type Score = Int

data Tile
  = Empty
  | Wall
  | Block
  | Paddle
  | Ball
  deriving (Enum, Eq)

type Screen = M.Map Coord Int

instance Show Tile where
  show tile =
    case tile of
      Empty  -> " "
      Wall   -> "█"
      Block  -> "░"
      Paddle -> "_"
      Ball   -> "◯"

resolution :: Screen -> (Int, Int)
resolution screen = (maxX, maxY)
  where
    maxX = maximum . map fst . M.keys $ screen
    maxY = maximum . map snd . M.keys $ screen

renderScreen :: Screen -> String
renderScreen screenData =
  intercalate "\n" $
  fmap
    (\y ->
       concatMap
         (show . (\x -> M.findWithDefault Empty (x, y) screen))
         [0 .. maxX])
    [0 .. maxY]
  where
    screen = M.map toEnum screenData
    (maxX, maxY) = resolution screenData

getTiles :: State Runtime [(Coord, Int)]
getTiles = do
  rt@Runtime {outputs} <- get
  put rt {outputs = []}
  return ((\(x:y:tile:_) -> ((x, y), tile)) <$> chunksOf 3 (reverse outputs))

replaceInput :: Int -> State Runtime ()
replaceInput n = get >>= (\rt -> put rt {inputs = [n]})

runGame :: Screen -> State Runtime Screen
runGame screen = do
  (coord, tile) <- getTile
  let updatedScreen = M.insert coord tile screen
  Runtime {finished} <- get
  if finished
    then return updatedScreen
    else runGame updatedScreen

insertCoin :: State Runtime ()
insertCoin = writeTarget 0 2

playGame :: Screen -> State Runtime Screen
playGame screen = do
  insertCoin
  loopGame screen

setJoystickInput :: Screen -> State Runtime ()
setJoystickInput screenData = do
  let ballX = fst . head . M.keys . M.filter (== fromEnum Ball) $ screenData
  let paddleX = fst . head . M.keys . M.filter (== fromEnum Paddle) $ screenData
  replaceInput (signum $ ballX - paddleX)

loopGame :: Screen -> State Runtime Screen
loopGame screen = do
  runUntilInput
  tiles <- getTiles
  let updatedScreen =
        foldl (\screen (coord, tile) -> M.insert coord tile screen) screen tiles
  setJoystickInput updatedScreen
  Runtime {finished} <- get
  if finished
    then return updatedScreen
    else loopGame updatedScreen

solve1 :: Program -> Int
solve1 program =
  let screenData = evalState (runGame M.empty) (initialize program)
   in length . M.filter (== Block) . M.map toEnum $ screenData

solve2 :: Program -> Int
solve2 program =
  let screen = evalState (playGame M.empty) (initialize program)
   in (M.!) screen (-1, 0)

main :: IO ()
main = do
  program <- parseInput <$> getContents
  print $ solve1 program
  print $ solve2 program
