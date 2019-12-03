{-# LANGUAGE OverloadedStrings, LambdaCase, NoImplicitPrelude, TypeOperators #-}
module Day1 where

import Parser ( Parser, number )
import Solver
import AOC.Prelude

main :: IO ()
main = printProblem =<< runProblem 1 [part1, part2]

part1 :: [Int] :~> Int
part1 = Solution
  { parser  = many parsePart1
  , solver  = sum
  }

part2 :: [Int] :~> Int
part2 = Solution
  { parser  = many parsePart2
  , solver  = sum
  }

parsePart1 :: Parser Int
parsePart1 = calculateFuel <$> number

parsePart2 :: Parser Int
parsePart2 = totalFuel <$> number

calculateFuel :: Int -> Int
calculateFuel = subtract 2 . (`quot` 3)

totalFuel :: Int -> Int
totalFuel = hylo sum' fuel
  where
    sum' = \case
      Nil -> 0
      Cons x xs -> x + xs
    fuel n = case calculateFuel n of
      f | f > 0 -> join Cons f
      _ -> Nil  

