import           Data.Bifunctor
import           Test.Hspec
import           Text.Parsec
import           Text.Parsec.String (Parser)

type Coord = Int

type Moon = (Vector, Vector)

data Vector =
  Vector
    { x :: Coord
    , y :: Coord
    , z :: Coord
    }
  deriving (Show, Eq)

empty :: Vector
empty = Vector 0 0 0

vecSum :: Vector -> Int
vecSum (Vector x y z) = abs x + abs y + abs z

vecAdd :: Vector -> Vector -> Vector
vecAdd (Vector x1 y1 z1) (Vector x2 y2 z2) =
  Vector (x1 + x2) (y1 + y2) (z1 + z2)

lcm' :: [Int] -> Int
lcm' = foldr lcm 1

updateVelocity :: [Moon] -> Moon -> Moon
updateVelocity moons moon = foldl f moon moons
  where
    f :: Moon -> Moon -> Moon
    f (velocity, pos@(Vector x1 y1 z1)) (_, Vector x2 y2 z2) =
      ( vecAdd velocity (Vector (offset x1 x2) (offset y1 y2) (offset z1 z2))
      , pos)
    offset :: Coord -> Coord -> Coord
    offset from to = signum (to - from)

updatePosition :: Moon -> Moon
updatePosition (velocity, position) = (velocity, vecAdd position velocity)

step :: [Moon] -> [Moon]
step moons = fmap (updatePosition . updateVelocity moons) moons

steps :: [Moon] -> [[Moon]]
steps = iterate step

potentialEnergy :: Moon -> Int
potentialEnergy = uncurry (*) . bimap vecSum vecSum

stepCoords :: (Vector -> Coord) -> [Moon] -> [Coord]
stepCoords coord = concatMap ((\(a, b) -> [a, b]) . bimap coord coord)

coordFrequency :: (Vector -> Coord) -> [Moon] -> Int
coordFrequency coord moons =
  (+ 1) .
  length . takeWhile (/= toMatch) . fmap (stepCoords coord) . tail . steps $
  moons
  where
    toMatch = stepCoords coord moons

frequency :: [Moon] -> Int
frequency moons = lcm' $ fmap (`coordFrequency` moons) [x, y, z]

solve1 :: [Moon] -> Int
solve1 = sum . fmap potentialEnergy . (!! 1000) . steps

solve2 :: [Moon] -> Int
solve2 = frequency

coordParser :: Parser Coord
coordParser = do
  _ <- oneOf "xyz"
  _ <- char '='
  fmap read $ (:) <$> (char '-' <|> digit) <*> many digit

vectorParser :: Parser Vector
vectorParser = do
  (x:y:z:_) <- between (char '<') (char '>') (sepBy coordParser (string ", "))
  _ <- optional endOfLine
  return $ Vector x y z

parseInput :: String -> Either ParseError [Vector]
parseInput = parse (many vectorParser <* eof) ""

tests :: IO ()
tests =
  hspec $ do
    let example1 =
          [ (empty, Vector (-1) 0 2)
          , (empty, Vector 2 (-10) (-7))
          , (empty, Vector 4 (-8) 8)
          , (empty, Vector 3 5 (-1))
          ]
    describe "potentialEnergy" $
      it "should calculate the right amount" $ do
        let moon = (Vector (-3) (-2) 1, Vector 2 1 (-3))
        potentialEnergy moon `shouldBe` 36
    describe "step" $
      it "should return the example output" $ do
        let expectedOutput =
              [ (Vector 3 (-1) (-1), Vector 2 (-1) 1)
              , (Vector 1 3 3, Vector 3 (-7) (-4))
              , (Vector (-3) 1 (-3), Vector 1 (-7) 5)
              , (Vector (-1) (-3) 1, Vector 2 2 0)
              ]
        step example1 `shouldBe` expectedOutput
    describe "frequency" $
      it "should return the correct value for example1" $
      frequency example1 `shouldBe` 2772

main :: IO ()
main = do
  parseResult <- parseInput <$> getContents
  case parseResult of
    Left err -> print err
    Right vectors -> do
      let moons = zip (repeat empty) vectors
      print $ solve1 moons
      print $ solve2 moons
