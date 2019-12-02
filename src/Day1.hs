{-# LANGUAGE OverloadedStrings, LambdaCase, NoImplicitPrelude #-}
module Day1 where

import Common ( Parser, getInput, printOutput, lexeme )
import Prelude ( IO, Int, (.), (>), (+), quot, (<$>), sum, subtract )
import Data.Text.IO ( putStrLn )
import Control.Monad ( join )
import Text.Megaparsec ( many )
import Text.Megaparsec.Char.Lexer ( decimal )
import Data.Functor.Foldable ( hylo, ListF(..) )

main :: IO ()
main = do
  putStrLn "Day 1"
  putStrLn "Part 1:"
  day1 <- getInput 1
  printOutput (many parsePart1) sum day1
  putStrLn "Part 2:"
  printOutput (many parsePart2) sum day1

parsePart1 :: Parser Int
parsePart1 = calculateFuel <$> entry

parsePart2 :: Parser Int
parsePart2 = totalFuel <$> entry

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

entry :: Parser Int
entry = lexeme decimal
