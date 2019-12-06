import           Data.Map.Lazy (empty, findWithDefault, fromListWith)
import           Data.Tree     (Tree, levels, unfoldTree)
import           Debug.Trace   (trace)

buildTree :: [(String, String)] -> Tree String
buildTree l = unfoldTree f "COM"
  where
    mapping = fromListWith (++) (fmap (fmap (: [])) l)
    f node = (node, findWithDefault [] node mapping)

solve1 :: [(String, String)] -> Int
solve1 =
  sum . fmap (uncurry (*)) . zip [0 ..] . fmap length . levels . buildTree

parseLine :: String -> (String, String)
parseLine = (\(s1:s2:_) -> (s1, s2)) . splitOn ')'

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

main :: IO ()
main = do
  lns <- fmap parseLine . lines <$> getContents
  print $ solve1 lns
