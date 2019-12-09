{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module AOC.IntComputer
  ( Program
  , Runtime(..)
  , getArg
  , getTarget
  , runCommand
  , runProgram
  , runUntilOutput
  , runUntilFinished
  , parseInput
  , initialize
  , setRelativeBase
  , writeTarget
  ) where

import           AOC.Utils           (splitOn)
import           Control.Monad.State
import           Data.Char           (digitToInt)
import           Data.IntMap         (IntMap, alter, findWithDefault, fromList)
import           Prelude             hiding (compare)

type Pointer = Int

type Opcode = Int

type Operation = State Runtime Bool

type Program = [Int]

type Memory = IntMap Int

data ArgMode
  = Position
  | Immediate
  | Relative

data Command =
  Command
    { operation :: Operation
    , argModes  :: [ArgMode]
    }

data Runtime =
  Runtime
    { memory       :: Memory
    , pointer      :: Pointer
    , relativeBase :: Int
    , inputs       :: [Int]
    , outputs      :: [Int]
    }
  deriving (Show)

(-->) :: Memory -> Pointer -> Int
(-->) = flip $ findWithDefault 0

infixl 2 -->

(*->) :: Memory -> Pointer -> Int
prg *-> ptr = prg --> (prg --> ptr)

infixl 2 *->

parseCommand :: Int -> Command
parseCommand =
  (\(arg3:arg2:arg1:opcode) ->
     Command
       (opcodeCommand . read $ opcode)
       (fmap charToArgMode [arg1, arg2, arg3])) .
  (\l -> replicate (5 - length l) '0' ++ l) . show
  where
    charToArgMode :: Char -> ArgMode
    charToArgMode '1' = Immediate
    charToArgMode '2' = Relative
    charToArgMode _   = Position

getArg :: Int -> State Runtime Int
getArg offset = do
  Runtime {memory, pointer, relativeBase} <- get
  Command {argModes} <- getCommand
  let argPointer = pointer + 1 + offset
  let argMode = argModes !! offset
  return $
    case argMode of
      Immediate -> memory --> argPointer
      Position  -> memory *-> argPointer
      Relative  -> memory --> (relativeBase + (memory --> argPointer))

getTarget :: Int -> State Runtime Int
getTarget offset = do
  Runtime {memory, pointer, relativeBase} <- get
  Command {argModes} <- getCommand
  let argPointer = pointer + 1 + offset
  let argMode = argModes !! offset
  return $
    case argMode of
      Relative -> relativeBase + (memory --> argPointer)
      _        -> memory --> argPointer

writeTarget :: Int -> Int -> State Runtime ()
writeTarget target value = do
  Runtime {memory} <- get
  let newMemory = alter (const $ Just value) target memory
  updateMemory newMemory
  return ()

movePointer :: Int -> State Runtime ()
movePointer newPointer = do
  Runtime {..} <- get
  put (Runtime memory newPointer relativeBase inputs outputs)
  return ()

updateMemory :: Memory -> State Runtime ()
updateMemory newMemory = do
  Runtime {..} <- get
  put (Runtime newMemory pointer relativeBase inputs outputs)

popInput :: State Runtime Int
popInput = do
  Runtime {..} <- get
  let (ip:ips) = inputs
  put (Runtime memory pointer relativeBase ips outputs)
  return ip

addOutput :: Int -> State Runtime ()
addOutput o = do
  Runtime {..} <- get
  put (Runtime memory pointer relativeBase inputs (o : outputs))
  return ()

mathOperation :: (Int -> Int -> Int) -> Operation
mathOperation op = do
  Runtime {memory, pointer} <- get
  arg1 <- getArg 0
  arg2 <- getArg 1
  target <- getTarget 2
  writeTarget target (arg1 `op` arg2)
  movePointer (pointer + 4)
  return False

exit :: Operation
exit = return True

input :: Operation
input = do
  Runtime {pointer, memory} <- get
  ip <- popInput
  target <- getTarget 0
  writeTarget target ip
  movePointer (pointer + 2)
  return False

output :: Operation
output = do
  Runtime {pointer} <- get
  value <- getArg 0
  addOutput value
  movePointer (pointer + 2)
  return False

jumpIf :: Bool -> Operation
jumpIf cmp = do
  Runtime {pointer} <- get
  arg1 <- getArg 0
  arg2 <- getArg 1
  let newPointer =
        if (arg1 /= 0) == cmp
          then arg2
          else pointer + 3
  movePointer newPointer
  return False

compare :: (Int -> Int -> Bool) -> Operation
compare cmp = do
  Runtime {pointer, memory} <- get
  arg1 <- getArg 0
  arg2 <- getArg 1
  target <- getTarget 2
  writeTarget
    target
    (if arg1 `cmp` arg2
       then 1
       else 0)
  movePointer (pointer + 4)
  return False

setRelativeBase :: Operation
setRelativeBase = do
  arg1 <- getArg 0
  Runtime {..} <- get
  put (Runtime memory pointer (relativeBase + arg1) inputs outputs)
  movePointer (pointer + 2)
  return False

opcodeCommand :: Opcode -> Operation
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
    9  -> setRelativeBase
    99 -> exit
    _  -> error $ "Unknown opcode found: " ++ show opcode

getCommand :: State Runtime Command
getCommand = do
  Runtime {memory, pointer} <- get
  let cmd = memory --> pointer
  return $ parseCommand cmd

runCommand :: State Runtime Bool
runCommand = do
  Runtime {memory, pointer} <- get
  let cmd = memory --> pointer
  let Command {operation} = parseCommand cmd
  operation

runUntilFinished :: State Runtime [Int]
runUntilFinished = do
  finished <- runCommand
  if finished
    then do
      Runtime {outputs} <- get
      return outputs
    else runUntilFinished

runUntilOutput :: State Runtime Bool
runUntilOutput = do
  finished <- runCommand
  Runtime {outputs} <- get
  if finished
    then return True
    else if null outputs
           then runUntilOutput
           else return False

initialize :: Program -> [Int] -> Runtime
initialize prg = flip (Runtime (fromList $ zip [0 ..] prg) 0 0) []

runProgram :: [Int] -> Program -> [Int]
runProgram inputs prg = evalState runUntilFinished (initialize prg inputs)

parseInput :: String -> Program
parseInput = fmap read . splitOn ','
