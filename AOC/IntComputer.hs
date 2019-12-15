{-# LANGUAGE NamedFieldPuns #-}

module AOC.IntComputer
  ( Program
  , Runtime(..)
  , getArg
  , getTarget
  , runCommand
  , runProgram
  , runUntilInput
  , runUntilOutput
  , runUntilFinished
  , parseInput
  , initialize
  , setRelativeBase
  , writeTarget
  ) where

import           AOC.Utils           (splitOn)
import           Control.Monad       (unless)
import           Control.Monad.State
import           Data.Char           (digitToInt)
import           Data.IntMap         (IntMap, alter, findWithDefault, fromList)
import           Prelude             hiding (compare)

type Pointer = Int

type Opcode = Int

type Operation = State Runtime ()

type Program = [Int]

type Memory = IntMap Int

data ArgMode
  = Position
  | Immediate
  | Relative

data Command =
  Command
    { opcode   :: Opcode
    , argModes :: [ArgMode]
    }

data Runtime =
  Runtime
    { memory       :: Memory
    , pointer      :: Pointer
    , relativeBase :: Int
    , finished     :: Bool
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
     Command (read opcode) (fmap charToArgMode [arg1, arg2, arg3])) .
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

movePointer :: Int -> State Runtime ()
movePointer newPointer = get >>= (\rt -> put rt {pointer = newPointer})

updateMemory :: Memory -> State Runtime ()
updateMemory newMemory = get >>= (\rt -> put rt {memory = newMemory})

popInput :: State Runtime Int
popInput = do
  rt <- get
  let (ip:ips) = inputs rt
  put rt {inputs = ips}
  return ip

setInput :: [Int] -> State Runtime ()
setInput ip = get >>= (\rt -> put $ rt {inputs = ip})

addOutput :: Int -> State Runtime ()
addOutput o = get >>= (\rt -> put rt {outputs = o : outputs rt})

mathOperation :: (Int -> Int -> Int) -> Operation
mathOperation op = do
  Runtime {pointer} <- get
  arg1 <- getArg 0
  arg2 <- getArg 1
  target <- getTarget 2
  writeTarget target (arg1 `op` arg2)
  movePointer (pointer + 4)

exit :: Operation
exit = get >>= (\rt -> put rt {finished = True})

input :: Operation
input = do
  Runtime {pointer, memory} <- get
  ip <- popInput
  target <- getTarget 0
  writeTarget target ip
  movePointer (pointer + 2)

output :: Operation
output = do
  Runtime {pointer} <- get
  value <- getArg 0
  addOutput value
  movePointer (pointer + 2)

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

setRelativeBase :: Operation
setRelativeBase = do
  arg1 <- getArg 0
  rt <- get
  put rt {relativeBase = relativeBase rt + arg1}
  movePointer (pointer rt + 2)

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

runCommand :: Operation
runCommand = do
  Runtime {memory, pointer} <- get
  let cmd = memory --> pointer
  let Command {opcode} = parseCommand cmd
  let operation = opcodeCommand opcode
  operation

runUntilFinished :: State Runtime [Int]
runUntilFinished = do
  runCommand
  Runtime {finished, outputs} <- get
  if finished
    then return outputs
    else runUntilFinished

runUntilOutput :: State Runtime (Int, Bool)
runUntilOutput = do
  get >>= (\rt -> put rt {outputs = []})
  runCommand
  Runtime {outputs, finished} <- get
  if finished
    then return (0, True)
    else if null outputs
           then runUntilOutput
           else return (head outputs, False)

runUntilInput :: State Runtime ()
runUntilInput = do
  Command opcode _ <- getCommand
  Runtime {inputs, finished} <- get
  unless
    ((opcode == 3 && null inputs) || finished)
    (runCommand >> runUntilInput)

initialize :: Program -> Runtime
initialize prg = Runtime (fromList $ zip [0 ..] prg) 0 0 False [] []

runProgram :: [Int] -> Program -> [Int]
runProgram inputs prg =
  evalState (setInput inputs >> runUntilFinished) (initialize prg)

parseInput :: String -> Program
parseInput = fmap read . splitOn ','
