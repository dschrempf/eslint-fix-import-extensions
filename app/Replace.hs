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
  (
  )
where

import Data.Text
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Parse (R (..))

replaceOne :: R -> Text -> Text
replaceOne (R from ext) = T.replace from to
  where
    to = from <> "." <> ext

replaceOneFile :: RF -> IO ()
replaceOneFile (RF file rs) = undefined

-- dat <- T.readFile (T.unpack file)
