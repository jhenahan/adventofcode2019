{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module AOC.Prelude (
  module Exports,
  toNonEmpty
) where

import Control.Monad as Exports ( join, (=<<), return, mapM, guard )
import Data.Foldable as Exports
import Data.Function as Exports ( ($), (.), id, flip )
import Data.Functor as Exports
import Data.Functor.Foldable as Exports ( hylo, ListF(..) )
import Data.Int as Exports
import Data.Ord as Exports ( (>) )
import Data.Text as Exports (Text)
import Data.Text.IO as Exports (putStrLn, readFile)
import Data.Text.Prettyprint.Doc as Exports hiding ( space )
import Data.Text.Prettyprint.Doc.Render.Text as Exports
import Data.Text.Prettyprint.Doc.Render.String as Exports
import Data.Void as Exports ( Void )
import Data.Eq as Exports
import GHC.Base as Exports hiding ( some, many, mapM, foldr )
import GHC.Num as Exports
import GHC.Real as Exports
import GHC.Show as Exports
import System.IO as Exports ( IO )
import Text.Megaparsec as Exports hiding ( getInput )
import Text.Megaparsec.Char as Exports hiding ( space )
import Data.Either as Exports
import Data.Maybe as Exports
import Data.String as Exports
import GHC.Err as Exports
import Control.Applicative as Exports hiding ( some, many )
import Data.Sequence.NonEmpty as Exports hiding ( length )
import Data.Coerce as Exports
import Data.Traversable as Exports ( for )
import Data.Tuple as Exports
import qualified Data.List.NonEmpty as NE

toNonEmpty :: [a] -> NE.NonEmpty a
toNonEmpty = NE.fromList
