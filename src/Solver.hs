{-# LANGUAGE TypeOperators, RecordWildCards, OverloadedStrings, NoImplicitPrelude #-}
module Solver where

import AOC.Prelude
import Parser

data a :~> b = Solution
  { parser  :: Parser a
  , solver  :: a -> b
  }

newtype Parts = Parts { unparts :: [Text] }

instance Pretty Parts where
  pretty = align . vsep . fmap pretty . unparts

data Problem = Problem
  { day :: Int
  , parts :: Parts
  }

instance Pretty Problem where
  pretty Problem{..} = pretty ("Day " :: Text) <>
                       pretty day <>
                       pretty (":" :: Text) <>
                       hardline <>
                       pretty parts

data SolutionError = ParseError | SolveError

instance Pretty SolutionError where
  pretty ParseError = "ParseError"
  pretty SolveError = "SolveError"

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right

renderText :: Doc ann -> Text
renderText = renderStrict . layoutPretty defaultLayoutOptions

render :: Doc ann -> String
render = renderString . layoutPretty defaultLayoutOptions

renderSolution :: Pretty b => a :~> b -> Text -> Text
renderSolution sol = either (error . render . pretty) id <$> withSolution sol

withSolution :: Pretty b => a :~> b -> Text -> Either SolutionError Text
withSolution Solution{..} s = do
  x <- maybeToEither ParseError . parseMaybe parser $ s
  y <- maybeToEither SolveError . Just . solver $ x
  pure . renderText $ pretty y
  
getInput :: Int -> IO Text
getInput = readFile . ("./inputs/day" <>) . show

runProblem :: Pretty b => Int -> [a :~> b] -> IO Problem
runProblem d p = do
  day <- getInput d
  return $ Problem d . Parts $ mapM renderSolution p day

renderProblem :: Problem -> Text
renderProblem = renderText . pretty

printProblem :: Problem -> IO ()
printProblem = putStrLn . renderProblem
