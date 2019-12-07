{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module AOC.IntComputer
  ( Program
  , runProgram
  , parseInput
  ) where

import           AOC.Utils           (splitOn)
import           Control.Monad.State
import           Data.Char           (digitToInt)
import           Data.Sequence       (Seq (..), fromList, index, update)
import           Prelude             hiding (compare)

type Pointer = Int

type Opcode = Int

type Program = Seq Int

data ArgMode
  = Position
  | Immediate

data Runtime =
  Runtime
    { program :: Program
    , pointer :: Pointer
    , inputs  :: [Int]
    , outputs :: [Int]
    }
  deriving (Show)

(-->) :: Program -> Pointer -> Int
(-->) = index

infixl 2 -->

(*->) :: Program -> Pointer -> Int
prg *-> ptr = prg --> (prg --> ptr)

infixl 2 *->

parseCommand :: Int -> (Opcode, [ArgMode])
parseCommand =
  (\(arg3:arg2:arg1:opcode) ->
     (read opcode, fmap charToArgMode [arg1, arg2, arg3])) .
  (\l -> replicate (5 - length l) '0' ++ l) . show
  where
    charToArgMode :: Char -> ArgMode
    charToArgMode '1' = Immediate
    charToArgMode _   = Position

argOp :: ArgMode -> (Program -> Pointer -> Int)
argOp Immediate = (-->)
argOp Position  = (*->)

getArg :: [ArgMode] -> Int -> State Runtime Int
getArg argModes offset = do
  Runtime {program, pointer} <- get
  let argPointer = pointer + 1 + offset
  let argMode = argModes !! offset
  let op = argOp argMode
  return (program `op` argPointer)

mathOperation :: (Int -> Int -> Int) -> [ArgMode] -> State Runtime Bool
mathOperation op argModes = do
  Runtime {..} <- get
  arg1 <- getArg argModes 0
  arg2 <- getArg argModes 1
  let target = program --> (pointer + 3)
  let newProgram = update target (arg1 `op` arg2) program
  put (Runtime newProgram (pointer + 4) inputs outputs)
  return False

exit :: [ArgMode] -> State Runtime Bool
exit = const (return True)

input :: [ArgMode] -> State Runtime Bool
input argModes = do
  Runtime {..} <- get
  let (ip:ips) = inputs
  let target = program --> (pointer + 1)
  let newProgram = update target ip program
  put (Runtime newProgram (pointer + 2) ips outputs)
  return False

output :: [ArgMode] -> State Runtime Bool
output argModes = do
  Runtime {..} <- get
  value <- getArg argModes 0
  put (Runtime program (pointer + 2) inputs (value : outputs))
  return False

jumpIf :: Bool -> [ArgMode] -> State Runtime Bool
jumpIf cmp argModes = do
  Runtime {..} <- get
  arg1 <- getArg argModes 0
  arg2 <- getArg argModes 1
  let newPointer =
        if (arg1 /= 0) == cmp
          then arg2
          else pointer + 3
  put (Runtime program newPointer inputs outputs)
  return False

compare :: (Int -> Int -> Bool) -> [ArgMode] -> State Runtime Bool
compare cmp argModes = do
  Runtime {..} <- get
  arg1 <- getArg argModes 0
  arg2 <- getArg argModes 1
  let target = program --> pointer + 3
  let newProgram =
        update
          target
          (if arg1 `cmp` arg2
             then 1
             else 0)
          program
  put (Runtime newProgram (pointer + 4) inputs outputs)
  return False

opcodeCommand :: Opcode -> ([ArgMode] -> State Runtime Bool)
opcodeCommand opcode =
  case opcode of
    1  -> mathOperation (+)
    2  -> mathOperation (*)
    3  -> input
    4  -> output
    5  -> jumpIf True
    6  -> jumpIf False
    7  -> compare (<)
    8  -> compare (==)
    99 -> exit
    _  -> error $ "Unknown opcode found: " ++ show opcode

runCommand :: State Runtime Bool
runCommand = do
  Runtime {program, pointer} <- get
  let cmd = program --> pointer
  let (opcode, argModes) = parseCommand cmd
  let command = opcodeCommand opcode
  command argModes

runUntilFinished :: State Runtime [Int]
runUntilFinished = do
  finished <- runCommand
  if finished
    then do
      Runtime {outputs} <- get
      return outputs
    else runUntilFinished

runProgram :: [Int] -> Program -> [Int]
runProgram inputs prg = evalState runUntilFinished (Runtime prg 0 inputs [])

parseInput :: String -> Program
parseInput = fromList . fmap read . splitOn ','
