{-# LANGUAGE NamedFieldPuns #-}

import           Control.Applicative ((<|>))
import           Data.Map.Lazy       (empty, findWithDefault, fromListWith)
import           Data.Maybe          (fromMaybe)
import           Data.Tree           (Tree (..), levels, unfoldTree)

buildTree :: [(String, String)] -> Tree String
buildTree l = unfoldTree f "COM"
  where
    mapping = fromListWith (++) (fmap (fmap (: [])) l)
    f node = (node, findWithDefault [] node mapping)

findPath :: Eq a => a -> Tree a -> Maybe [a]
findPath needle Node {rootLabel, subForest} =
  if needle == rootLabel
    then Just []
    else (rootLabel :) <$> foldr (<|>) Nothing (findPath needle <$> subForest)

stripEqual :: Eq a => [a] -> [a] -> ([a], [a])
stripEqual a [] = (a, [])
stripEqual [] b = ([], b)
stripEqual (a:as) (b:bs) =
  if a == b
    then stripEqual as bs
    else (a : as, b : bs)

solve1 :: [(String, String)] -> Int
solve1 =
  sum . fmap (uncurry (*)) . zip [0 ..] . fmap length . levels . buildTree

solve2 :: [(String, String)] -> Int
solve2 ls = length . uncurry (++) $ stripEqual path1 path2
  where
    tree = buildTree ls
    path1 = fromMaybe [] $ findPath "YOU" tree
    path2 = fromMaybe [] $ findPath "SAN" tree

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
  print $ solve2 lns
