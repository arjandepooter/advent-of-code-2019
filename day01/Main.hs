calculateFuel :: Int -> Int
calculateFuel = flip (-) 2 . (`div` 3)

solve1 :: [Int] -> Int
solve1 = sum . fmap calculateFuel

solve2 :: [Int] -> Int
solve2 = sum . fmap calculateFuel'
  where
    calculateFuel' :: Int -> Int
    calculateFuel' w =
      let extraFuel = calculateFuel w
       in if extraFuel >= 0
            then extraFuel + calculateFuel' extraFuel
            else 0

main :: IO ()
main = do
  modules <- fmap read . lines <$> getContents
  print $ solve1 modules
  print $ solve2 modules
