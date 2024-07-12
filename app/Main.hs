-- |
-- Module      :  Main
-- Description :  Main
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Jul 12 13:47:44 2024.
module Main
  ( main,
  )
where

import Data.Attoparsec.Text (parseOnly)
import Data.Text.IO qualified as T
import Parse (pFilesWithReplacements)
import System.Environment (getArgs)

main :: IO ()
main = do
  [fn] <- getArgs
  d <- T.readFile fn
  print $ parseOnly pFilesWithReplacements d
