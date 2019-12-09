{-# LANGUAGE RecordWildCards #-}

import           AOC.IntComputer
import           AOC.Utils           (splitOn)
import           Control.Monad.State (runState)
import           Data.List           (permutations)

runAmplifier :: Program -> [Int] -> Int
runAmplifier prg = foldl (\p s -> head $ runProgram [s, p] prg) 0

setInput :: Runtime -> [Int] -> Runtime
setInput Runtime {..} ips =
  Runtime memory pointer relativeBase (inputs ++ ips) outputs

runAmplifierLoop :: Program -> [Int] -> Int
runAmplifierLoop prg codes =
  (\(op, _, _) -> op) .
  last .
  takeWhile (\(_, finished, _) -> not finished) . scanl f (0, False, runtimes) $
  [0 ..]
  where
    runtimes = fmap (initialize prg . return) codes
    f :: (Int, Bool, [Runtime]) -> a -> (Int, Bool, [Runtime])
    f (input, _, rt:rts) _ =
      let (finished, Runtime {..}) =
            runState runUntilOutput (setInput rt [input])
       in (head outputs, finished, rts ++ [Runtime memory pointer 0 [] []])

solve1 :: Program -> Int
solve1 prg = maximum $ fmap (runAmplifier prg) (permutations [0 .. 4])

solve2 :: Program -> Int
solve2 prg = maximum $ fmap (runAmplifierLoop prg) (permutations [5 .. 9])

main :: IO ()
main = do
  prog <- parseInput <$> getContents
  print $ solve1 prog
  print $ solve2 prog
