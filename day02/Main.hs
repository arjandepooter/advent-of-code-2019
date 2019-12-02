splitOn :: Eq a => a -> [a] -> [[a]]
splitOn sep = foldr folder []
  where
    folder cur [] =
      if cur == sep
        then [[]]
        else [[cur]]
    folder cur (curPart:rest) =
      if cur == sep
        then [] : curPart : rest
        else (cur : curPart) : rest

replace :: Int -> a -> [a] -> [a]
replace idx n l = take idx l ++ [n] ++ drop (idx + 1) l

runProgram :: (Int, Int) -> [Int] -> [Int]
runProgram (noun, verb) = runProgram' 0 . replace 1 noun . replace 2 verb
  where
    runProgram' :: Int -> [Int] -> [Int]
    runProgram' pos prog =
      let currentOp = prog !! pos
       in case currentOp of
            1 ->
              runProgram' (pos + 4) $
              replace
                (prog !! (pos + 3))
                ((prog !! (prog !! (pos + 1))) + (prog !! (prog !! (pos + 2))))
                prog
            2 ->
              runProgram' (pos + 4) $
              replace
                (prog !! (pos + 3))
                ((prog !! (prog !! (pos + 1))) * (prog !! (prog !! (pos + 2))))
                prog
            _ -> prog

solve1 :: [Int] -> Int
solve1 = head . runProgram (12, 2)

solve2 :: [Int] -> Int
solve2 prog =
  case foldl
         folder
         Nothing
         [(noun, verb) | noun <- [0 .. 99], verb <- [0 .. 99]] of
    Just (noun, verb) -> noun * 100 + verb
    _                 -> undefined
  where
    folder :: Maybe (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
    folder (Just x) _ = Just x
    folder _ n =
      let result = runProgram n prog
       in if head result == 19690720
            then Just n
            else Nothing

parseInput :: String -> [Int]
parseInput = fmap read . splitOn ','

main :: IO ()
main = do
  prog <- parseInput <$> getContents
  print $ solve1 prog
  print $ solve2 prog
