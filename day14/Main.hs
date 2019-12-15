import           Data.Bifunctor     (second)
import           Data.Map           as M hiding (foldr)
import           Test.Hspec
import           Text.Parsec
import           Text.Parsec.String (Parser)

type Chemical = String

type Product = (Int, Chemical)

data Reaction =
  Reaction
    { input  :: [Product]
    , output :: Product
    }
  deriving (Show)

type ReactionMap = Map Chemical Reaction

type Leftovers = Map Chemical Int

reactionMap :: [Reaction] -> ReactionMap
reactionMap = M.fromList . fmap (\r@(Reaction _ (_, chem)) -> (chem, r))

consumeLeftover :: Product -> Leftovers -> (Product, Leftovers)
consumeLeftover (needed, chem) leftovers = (updatedProduct, updatedMap)
  where
    available = M.findWithDefault 0 chem leftovers
    willTake = min needed available
    updatedProduct = (needed - willTake, chem)
    updatedMap = M.update updater chem leftovers
    updater c
      | n == 0 = Nothing
      | otherwise = Just n
      where
        n = c - willTake

neededOreWithLeftovers ::
     ReactionMap -> Product -> Leftovers -> (Int, Leftovers)
neededOreWithLeftovers _ (0, _) leftovers = (0, leftovers)
neededOreWithLeftovers _ (amount, "ORE") leftovers = (amount, leftovers)
neededOreWithLeftovers m (amount, chem) leftovers =
  second (insertWith (+) chem newLeftovers) . foldr f (0, updatedLeftovers) $
  inputs
  where
    ((neededAmount, _), updatedLeftovers) -- consume leftovers
     = consumeLeftover (amount, chem) leftovers
    r@(Reaction inputs (rAmount, _)) = m ! chem -- find the reaction to create the current chem
    batches = (neededAmount `div` rAmount) + signum (neededAmount `mod` rAmount) -- calculate number of batches of inputs needed
    newLeftovers -- calculate how many leftovers we will have for this chem
     =
      let n = (neededAmount `mod` rAmount)
       in if n == 0
            then 0
            else rAmount - n
    f :: Product -> (Int, Leftovers) -> (Int, Leftovers)
    f (amount, chem) (cAmount, leftovers) = (cAmount + rAmount, rLeftovers)
      where
        (rAmount, rLeftovers) =
          neededOreWithLeftovers m (batches * amount, chem) leftovers

binarySearch :: (Integral a, Ord b) => (a -> b) -> a -> a -> b -> a
binarySearch f low high threshold
  | (high - low) <= 1 || result == threshold = low
  | result > threshold = binarySearch f low trial threshold
  | otherwise = binarySearch f trial high threshold
  where
    trial = low + (high - low) `div` 2
    result = f trial

solve1 :: ReactionMap -> Int
solve1 m = fst $ neededOreWithLeftovers m (1, "FUEL") M.empty

solve2 :: ReactionMap -> Int
solve2 m = binarySearch partial 1 10000000 1000000000000
  where
    partial n = fst $ neededOreWithLeftovers m (n, "FUEL") M.empty

chemicalParser :: Parser Product
chemicalParser = do
  amount <- read <$> many1 digit
  _ <- space
  chemical <- many1 (oneOf ['A' .. 'Z'])
  return (amount, chemical)

reactionParser :: Parser Reaction
reactionParser = do
  input <- chemicalParser `sepBy` string ", "
  _ <- string " => "
  output <- chemicalParser
  _ <- optional endOfLine
  return (Reaction input output)

parseInput :: String -> Either ParseError ReactionMap
parseInput = parse ((reactionMap <$> many reactionParser) <* eof) ""

tests :: IO ()
tests =
  hspec $ do
    describe "consumeLeftover" $
      it "uses maximum what is needed" $ do
        let product = (10, "ORE")
        let leftOvers = M.fromList [("ORE", 15)]
        consumeLeftover product leftOvers `shouldBe`
          ((0, "ORE"), M.fromList [("ORE", 5)])
    describe "Examples" $ do
      it "example1" $ do
        let (Right d) =
              parseInput
                "10 ORE => 10 A\
                \1 ORE => 1 B\
                \7 A, 1 B => 1 C\
                \7 A, 1 C => 1 D\
                \7 A, 1 D => 1 E\
                \7 A, 1 E => 1 FUEL"
        solve1 d `shouldBe` 31
      it "example2" $ do
        let (Right d) =
              parseInput
                "9 ORE => 2 A\
                \8 ORE => 3 B\
                \7 ORE => 5 C\
                \3 A, 4 B => 1 AB\
                \5 B, 7 C => 1 BC\
                \4 C, 1 A => 1 CA\
                \2 AB, 3 BC, 4 CA => 1 FUEL"
        solve1 d `shouldBe` 165

main :: IO ()
main = do
  parseResult <- parseInput <$> getContents
  case parseResult of
    Left err -> print err
    Right reactions -> do
      print $ solve1 reactions
      print $ solve2 reactions
