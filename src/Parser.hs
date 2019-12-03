{-# LANGUAGE NoImplicitPrelude #-}
module Parser where

import Data.Text ( Text )
import Prelude ( Int )
import Text.Megaparsec ( Parsec, empty )
import Data.Void ( Void )
import Text.Megaparsec.Char ( space1 )
import qualified Text.Megaparsec.Char.Lexer as L
    ( lexeme, space, decimal )

type Parser = Parsec Void Text

space :: Parser ()
space = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

number :: Parser Int
number = lexeme L.decimal
