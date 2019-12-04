{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Day2 where

import Parser ( Parser, number )
import Solver
import AOC.Prelude

main :: IO ()
main = printProblem =<< runProblem 2 [part1, part2]

part1 :: Machine :~> Maybe Int
part1 = Solution
  { parser = updateMachine 2 2 . updateMachine 1 12 <$> parseInitialMachine
  , solver = runProgram 0
  }

part2 :: Machine :~> Maybe Int
part2 = Solution
  { parser = parseInitialMachine
  , solver = runPart2
  }

newtype Memory = Memory { machine :: NESeq Int }

data Machine = Machine
  { position :: Int
  , memory :: Memory
  }

data Op = Add | Mult | Halt | Err

data Instruction = Instruction
  { op :: Op
  , args :: [Int]
  , target :: Int
  }

runProgram :: Int -> Machine -> Maybe Int
runProgram i = readMachine i . until ((==0) . position) program . program

runPart2 :: Machine -> Maybe Int
runPart2 m@Machine{..} = fmap (uncurry (+)) . listToMaybe $ do
  noun <- [0..99]
  verb <- [0..99]
  let m' = updateMachine 1 noun . updateMachine 2 verb $ m
  guard $ runProgram 0 m' == Just 19690720
  return (100 * noun, verb)

program :: Machine -> Machine
program m@Machine{..} = processInstruction (Instruction op' args' target') m
  where
   op' = maybe Err getOp $ readMemory position memory
   target' = maybe 0 id $ readMemory (position + 3) memory
   argPos = catMaybes . for [position + 1, position + 2] readMemory $ memory
   args' = catMaybes . for argPos readMemory $ memory

processInstruction :: Instruction -> Machine -> Machine
processInstruction Instruction{..} = step
  where
    step = case op of
      Add -> process sum args
      Mult -> process product args
      Halt -> reset
      Err -> error "This shouldn't happen"
    process = fmap (fmap stepMachine . updateMachine target)

parseMemory :: Parser Memory
parseMemory = Memory . fromList . toNonEmpty <$> number `sepBy` char ','

parseInitialMachine :: Parser Machine
parseInitialMachine = Machine 0 <$> parseMemory

updateMemory :: Int -> Int -> Memory -> Memory
updateMemory = coerce (update @Int)

readMemory :: Int -> Memory -> Maybe Int
readMemory = coerce (lookup @Int)

readMachine :: Int -> Machine -> Maybe Int
readMachine i = readMemory i . memory

updateMachine :: Int -> Int -> Machine -> Machine
updateMachine target value m@Machine{..} = m { memory = updateMemory target value memory }

stepMachine :: Machine -> Machine
stepMachine m@Machine{..} = m { position = position + 4 }

reset :: Machine -> Machine
reset m@Machine{..} = m { position = 0 }

getOp :: Int -> Op
getOp = \case
  1 -> Add
  2 -> Mult
  99 -> Halt
  _ -> Err

   
