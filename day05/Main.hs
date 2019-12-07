import           AOC.IntComputer (Program, parseInput, runProgram)
import           AOC.Utils       (splitOn)

solve1 :: Program -> Int
solve1 = head . runProgram [1]

solve2 :: Program -> Int
solve2 = head . runProgram [5]

main :: IO ()
main = do
  prog <- parseInput <$> getContents
  print $ solve1 prog
  print $ solve2 prog
