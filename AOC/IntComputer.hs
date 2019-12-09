{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module AOC.IntComputer
  ( Program
  , Runtime(..)
  , runCommand
  , runProgram
  , runUntilOutput
  , parseInput
  , initialize
  ) where

import           AOC.Utils           (splitOn)
import           Control.Monad.State
import           Data.Char           (digitToInt)
import           Data.IntMap         (IntMap, alter, findWithDefault, fromList)
import           Prelude             hiding (compare)

type Pointer = Int

type Opcode = Int

type Program = [Int]

type Memory = IntMap Int

data ArgMode
  = Position
  | Immediate
  | Relative

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

parseCommand :: Int -> (Opcode, [ArgMode])
parseCommand =
  (\(arg3:arg2:arg1:opcode) ->
     (read opcode, fmap charToArgMode [arg1, arg2, arg3])) .
  (\l -> replicate (5 - length l) '0' ++ l) . show
  where
    charToArgMode :: Char -> ArgMode
    charToArgMode '1' = Immediate
    charToArgMode '2' = Relative
    charToArgMode _   = Position

getArg :: [ArgMode] -> Int -> State Runtime Int
getArg argModes offset = do
  Runtime {memory, pointer} <- get
  let argPointer = pointer + 1 + offset
  let argMode = argModes !! offset
  case argMode of
    Immediate -> return (memory --> argPointer)
    Position -> return (memory *-> argPointer)
    Relative -> do
      Runtime {relativeBase} <- get
      return (memory --> (relativeBase + (memory --> argPointer)))

writeTarget :: [ArgMode] -> Int -> Int -> State Runtime ()
writeTarget argModes offset value = do
  Runtime {memory, pointer, relativeBase} <- get
  let targetPointer = memory --> pointer + 1 + offset
  let argMode = argModes !! offset
  let target =
        case argMode of
          Relative -> relativeBase + targetPointer
          _        -> targetPointer
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

mathOperation :: (Int -> Int -> Int) -> [ArgMode] -> State Runtime Bool
mathOperation op argModes = do
  Runtime {memory, pointer} <- get
  arg1 <- getArg argModes 0
  arg2 <- getArg argModes 1
  let target = memory --> (pointer + 3)
  writeTarget argModes 2 (arg1 `op` arg2)
  movePointer (pointer + 4)
  return False

exit :: [ArgMode] -> State Runtime Bool
exit = const (return True)

input :: [ArgMode] -> State Runtime Bool
input argModes = do
  Runtime {pointer, memory} <- get
  ip <- popInput
  let target = memory --> (pointer + 1)
  writeTarget argModes 0 ip
  movePointer (pointer + 2)
  return False

output :: [ArgMode] -> State Runtime Bool
output argModes = do
  Runtime {pointer} <- get
  value <- getArg argModes 0
  addOutput value
  movePointer (pointer + 2)
  return False

jumpIf :: Bool -> [ArgMode] -> State Runtime Bool
jumpIf cmp argModes = do
  Runtime {pointer} <- get
  arg1 <- getArg argModes 0
  arg2 <- getArg argModes 1
  let newPointer =
        if (arg1 /= 0) == cmp
          then arg2
          else pointer + 3
  movePointer newPointer
  return False

compare :: (Int -> Int -> Bool) -> [ArgMode] -> State Runtime Bool
compare cmp argModes = do
  Runtime {pointer, memory} <- get
  arg1 <- getArg argModes 0
  arg2 <- getArg argModes 1
  let target = memory --> pointer + 3
  writeTarget
    argModes
    2
    (if arg1 `cmp` arg2
       then 1
       else 0)
  movePointer (pointer + 4)
  return False

setRelativeBase :: [ArgMode] -> State Runtime Bool
setRelativeBase argModes = do
  arg1 <- getArg argModes 0
  Runtime {..} <- get
  put (Runtime memory pointer (relativeBase + arg1) inputs outputs)
  movePointer (pointer + 2)
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
    9  -> setRelativeBase
    99 -> exit
    _  -> error $ "Unknown opcode found: " ++ show opcode

runCommand :: State Runtime Bool
runCommand = do
  Runtime {memory, pointer} <- get
  let cmd = memory --> pointer
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
