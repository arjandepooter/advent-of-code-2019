import           Data.Char  (digitToInt, intToDigit)
import           Test.Hspec

type Signal = [Int]

type Pattern = [Int]

basePattern :: Pattern
basePattern = [0, 1, 0, -1]

phase :: Signal -> Signal
phase signal = fmap (applyPattern . uncurry (flip const)) (zip signal [1 ..])
  where
    applyPattern :: Int -> Int
    applyPattern step =
      (`mod` 10) . abs . sum . fmap (uncurry (*)) . zip signal $
      (tail . cycle $ pattern' step)
    pattern' :: Int -> Pattern
    pattern' step = basePattern >>= replicate step

solve1 :: Signal -> Int
solve1 =
  read . fmap intToDigit . take 8 . last . take 100 . tail . iterate phase

solve2 :: Signal -> Int
solve2 = undefined

tests :: IO ()
tests =
  hspec $ do
    describe "phase" $ do
      it "example1" $ do
        let signal = [1, 2, 3, 4, 5, 6, 7, 8]
        phase signal `shouldBe` [4, 8, 2, 2, 6, 1, 5, 8]
      it "example2" $ do
        let signal = [4, 8, 2, 2, 6, 1, 5, 8]
        phase signal `shouldBe` [3, 4, 0, 4, 0, 4, 3, 8]
      it "example3" $ do
        let signal = [3, 4, 0, 4, 0, 4, 3, 8]
        phase signal `shouldBe` [0, 3, 4, 1, 5, 5, 1, 8]
    describe "solve1" $
      it "example1" $ do
        let signal = fmap digitToInt "80871224585914546619083218645595"
        solve1 signal `shouldBe` 24176176

main :: IO ()
main = do
  signal <- fmap digitToInt <$> getLine
  print $ solve1 signal
  print $ solve2 signal
