{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module AOC.Prelude (
  module Exports
) where

import Control.Monad as Exports ( join, (=<<), return, mapM )
import Data.Foldable as Exports
import Data.Function as Exports ( ($), (.), id )
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
import GHC.Num as Exports
import GHC.Real as Exports
import GHC.Show as Exports
import System.IO as Exports ( IO )
import Text.Megaparsec as Exports ( many, parseMaybe )
import Data.Either as Exports
import Data.Maybe as Exports
import Data.String as Exports
import GHC.Err as Exports
import Control.Applicative as Exports hiding ( many )

