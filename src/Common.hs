{-# LANGUAGE NoImplicitPrelude, RankNTypes #-}
module Common where

import System.IO ( IO )
import Data.Monoid ( (<>) )
import Data.Text ( Text )
import Data.Text.IO ( readFile )
import Data.Either ( Either(..) )
import Prelude ( print, Show(..), error, Int )
import Text.Megaparsec as M ( parse, errorBundlePretty, Parsec, empty )
import Data.Void ( Void )
import Data.Function ( (.), ($) )
import qualified Text.Megaparsec.Char as M ( space1 )
import qualified Text.Megaparsec.Char.Lexer as L
    ( lexeme, space )

type Parser = Parsec Void Text

getInput :: Int -> IO Text
getInput = readFile . ("./inputs/day" <>) . show

processInput :: Parser a -> (a -> b) -> Text -> b
processInput parser consumer input = 
  case parse parser "" input of
    Left bundle -> error $ errorBundlePretty bundle
    Right output -> consumer output


(.::) :: (b -> c) -> (a1 -> a2 -> a3 -> b) -> a1 -> a2 -> a3 -> c
(.::) = (.) . (.) . (.)

printOutput :: Show b => Parser a -> (a -> b) -> Text -> IO ()
printOutput = print .:: processInput

space :: Parser ()
space = L.space M.space1 M.empty M.empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

