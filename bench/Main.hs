module Main where

import qualified Day1
import qualified Day2
import Solver

import Criterion.Main

main :: IO ()
main = defaultMain [
  bgroup "day1" [ bench "part1" $ nfIO $ runProblem 1 [Day1.part1]
                , bench "part2" $ nfIO $ runProblem 1 [Day1.part2]
                ],
  bgroup "day2" [ bench "part1" $ nfIO $ runProblem 2 [Day2.part1]
                , bench "part2" $ nfIO $ runProblem 2 [Day2.part2]
                ]
  ]
