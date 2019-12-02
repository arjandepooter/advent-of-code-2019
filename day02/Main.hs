type IntProgram = [Int]

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

getPosition :: Int -> IntProgram -> Int
getPosition = flip (!!)

getPointer :: Int -> IntProgram -> Int
getPointer idx prog = getPosition (getPosition idx prog) prog

runProgram :: (Int, Int) -> IntProgram -> IntProgram
runProgram (noun, verb) = runProgram' 0 . replace 1 noun . replace 2 verb
  where
    runProgram' :: Int -> IntProgram -> IntProgram
    runProgram' pos prog =
      let currentOp = prog !! pos
       in case currentOp of
            1 ->
              runProgram' (pos + 4) $
              replace
                (getPosition (pos + 3) prog)
                (getPointer (pos + 1) prog + getPointer (pos + 2) prog)
                prog
            2 ->
              runProgram' (pos + 4) $
              replace
                (getPosition (pos + 3) prog)
                (getPointer (pos + 1) prog * getPointer (pos + 2) prog)
                prog
            _ -> prog

solve1 :: IntProgram -> Int
solve1 = head . runProgram (12, 2)

solve2 :: IntProgram -> Int
solve2 prog =
  case foldr
         folder
         Nothing
         [(noun, verb) | noun <- [0 .. 99], verb <- [0 .. 99]] of
    Just (noun, verb) -> noun * 100 + verb
    _                 -> error "No solution found"
  where
    folder :: (Int, Int) -> Maybe (Int, Int) -> Maybe (Int, Int)
    folder _ (Just x) = Just x
    folder n _ =
      let result = runProgram n prog
       in if head result == 19690720
            then Just n
            else Nothing

parseInput :: String -> IntProgram
parseInput = fmap read . splitOn ','

main :: IO ()
main = do
  prog <- parseInput <$> getContents
  print $ solve1 prog
  print $ solve2 prog
