{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Replace
-- Description :  Perform replacement
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Jul 12 14:52:41 2024.
module Replace
  ( replaceAll,
  )
where

import Data.Foldable (Foldable (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Parse (R (..), RF (..))

replaceOne :: Text -> R -> Text
replaceOne input (R from ext) = T.replace (quote from) (quote to) input
  where
    quote x = "\"" <> x <> "\""
    to = from <> "." <> ext

replaceOneFile :: RF -> IO ()
replaceOneFile (RF file rs) = do
  let fp = T.unpack file
  input <- T.readFile fp
  let input' = foldl' replaceOne input rs
  T.writeFile fp input'

replaceAll :: [RF] -> IO ()
replaceAll = mapM_ replaceOneFile
