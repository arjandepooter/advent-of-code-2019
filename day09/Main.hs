import           AOC.IntComputer
import           Data.Sequence   (fromList)

solve1 :: Program -> Int
solve1 = head . runProgram [1]

solve2 :: Program -> Int
solve2 = head . runProgram [2]

main :: IO ()
main = do
  prg <- parseInput <$> getContents
  print $ solve1 prg
  print $ solve2 prg
