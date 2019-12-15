{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

{- HLINT ignore -}
import           AOC.IntComputer
import           Control.Monad       (foldM)
import           Control.Monad.State (evalState, runState)
import           Data.IntMap         (toList, (!))
import           Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "IntComputer" $ do
      describe "initialize" $ do
        it "should return a computer in initial state" $ do
          let Runtime {..} = initialize [1, 2, 3]
          inputs `shouldBe` []
          outputs `shouldBe` []
      describe "getTarget should get the argument based on the argmode" $ do
        it "should return the address of the arg if the mode is immediate" $ do
          let rt = initialize [101, 1, 2, 0]
          evalState (getTarget 0) rt `shouldBe` 1
        it
          "should return the address where the arg points to if the mode is position" $ do
          let rt = initialize [1, 5, 2, 0]
          evalState (getTarget 0) rt `shouldBe` 5
        it
          "should return the argpointer value + the relativeBase as the address" $ do
          let rt = initialize [201, 7, 0, 0]
          evalState (getTarget 0) rt {relativeBase = 7} `shouldBe` 14
      describe "writeTarget" $ do
        it "should update the target pointer with given value" $ do
          let rt = initialize [0, 0, 0, 0, 0, 0, 0]
          let (_, Runtime {memory}) = runState (writeTarget 5 100) rt
          memory ! 5 `shouldBe` 100
      describe "mathOperation" $ do
        it
          "should calculate the operation between arg1 and arg2 and write it as result" $ do
          let rt = initialize [1101, 10, 5, 3]
          let (_, Runtime {memory}) = runState runCommand rt
          memory ! 3 `shouldBe` 15
    describe "Examples from problems" $ do
      describe "Day 2" $ do
        it "should run the examples" $ do
          let rt = initialize [1, 1, 1, 4, 99, 5, 6, 0, 99]
          let (_, Runtime {memory}) = runState runUntilFinished rt
          (fmap snd . toList) memory `shouldBe` [30, 1, 1, 4, 2, 5, 6, 0, 99]
      describe "Day 9" $ do
        it "should run example 1" $ do
          let program =
                [ 109
                , 1
                , 204
                , -1
                , 1001
                , 100
                , 1
                , 100
                , 1008
                , 100
                , 16
                , 101
                , 1006
                , 101
                , 0
                , 99
                ]
          let rt = initialize program
          evalState runUntilFinished rt `shouldBe` reverse program
        it "should run example 2" $ do
          let rt = initialize [1102, 34915192, 34915192, 7, 4, 7, 99, 0]
          (length . show . head) (evalState runUntilFinished rt) `shouldBe` 16
        it "should run example 3" $ do
          let program = [104, 1125899906842624, 99]
          let rt = initialize program
          (head $ evalState runUntilFinished rt) `shouldBe` program !! 1
