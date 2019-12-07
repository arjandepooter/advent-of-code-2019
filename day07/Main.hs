import           AOC.IntComputer (Program, parseInput, runProgram)
import           AOC.Utils       (splitOn)
import           Data.List       (permutations)

runAmplifier :: Program -> [Int] -> Int
runAmplifier prg = foldl (\p s -> head $ runProgram [s, p] prg) 0

solve1 :: Program -> Int
solve1 prg = maximum $ fmap (runAmplifier prg) (permutations [0 .. 4])

solve2 :: Program -> Int
solve2 = undefined

main :: IO ()
main = do
  prog <- parseInput <$> getContents
  print $ solve1 prog
  print $ solve2 prog
