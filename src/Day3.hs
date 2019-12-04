module Day3 where

import AOC.Prelude
{-
import Parser ( Parser, number )
import Solver

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
-}

data Move = U | D | L | R

data Wire = Wire Move Int
