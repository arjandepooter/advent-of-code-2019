import           AOC.IntComputer
import           AOC.Utils           (splitOn)
import           Control.Monad.State (runState)
import           Data.List           (permutations)

runAmplifier :: Program -> [Int] -> Int
runAmplifier prg = foldl (\p s -> head $ runProgram [s, p] prg) 0

setInput :: Runtime -> [Int] -> Runtime
setInput rt ips = rt {inputs = inputs rt ++ ips}

runAmplifierLoop :: Program -> [Int] -> Int
runAmplifierLoop prg codes =
  (\(op, _, _) -> op) .
  last . takeWhile (\(_, finished, _) -> not finished) . iterate f $
  (0, False, runtimes)
  where
    runtimes = fmap (initialize prg . return) codes
    f :: (Int, Bool, [Runtime]) -> (Int, Bool, [Runtime])
    f (input, _, rt:rts) =
      let ((output, finished), rt') =
            runState runUntilOutput (setInput rt [input])
       in (output, finished, rts ++ [rt' {outputs = [], inputs = []}])

solve1 :: Program -> Int
solve1 prg = maximum $ fmap (runAmplifier prg) (permutations [0 .. 4])

solve2 :: Program -> Int
solve2 prg = maximum $ fmap (runAmplifierLoop prg) (permutations [5 .. 9])

main :: IO ()
main = do
  prog <- parseInput <$> getContents
  print $ solve1 prog
  print $ solve2 prog
