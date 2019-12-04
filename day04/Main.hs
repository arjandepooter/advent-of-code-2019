import           Data.Char   (digitToInt)
import           Data.Monoid

intToDigits :: Int -> [Int]
intToDigits = fmap digitToInt . show

validateLength :: Int -> Bool
validateLength n = n >= 100000 && n <= 999999

validatePair :: Int -> Bool
validatePair n = any (uncurry (==)) pairs
  where
    pairs = zip (intToDigits n) (tail $ intToDigits n)

validateIncreasing :: Int -> Bool
validateIncreasing = validateIncreasing' 0 . intToDigits
  where
    validateIncreasing' _ [] = True
    validateIncreasing' cur (next:rest)
      | next >= cur = validateIncreasing' next rest
      | otherwise = False

isValid :: Int -> Bool
isValid n = getAll . mconcat . fmap (($ n) . (All .)) $ preds
  where
    preds = [validateLength, validatePair, validateIncreasing]

solve1 :: Int -> Int -> Int
solve1 start end = length . filter isValid $ [start .. end]

main :: IO ()
main = do
  p1 <- read <$> getLine
  p2 <- read <$> getLine
  print $ solve1 p1 p2
