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

import Data.Foldable (foldlM)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Imports (breakImports)
import Parse (R (..), RF (..))
import System.Directory (doesFileExist, withCurrentDirectory)
import System.FilePath (takeDirectory)

replaceOne :: FilePath -> Text -> R -> IO Text
replaceOne directory input (R from ext) = withCurrentDirectory directory $ do
  fileExists <- doesFileExist $ T.unpack to
  if fileExists
    then pure $ T.replace (quote from) (quote to) input
    else do
      indexFileExists <- doesFileExist $ T.unpack toIndex
      if indexFileExists
        then pure $ T.replace (quote from) (quote toIndex) input
        else do
          T.putStrLn $ "warning: could not add " <> ext <> " to " <> from
          pure input
  where
    quote x = "\"" <> x <> "\""
    to = from <> "." <> ext
    toIndex = from <> "/index." <> ext

replaceOneFile :: RF -> IO ()
replaceOneFile (RF file rs) = do
  let fp = T.unpack file
  input <- T.readFile fp
  let (imports, rest) = breakImports input
      path = takeDirectory $ T.unpack file
  imports' <- foldlM (replaceOne path) imports rs
  T.writeFile fp (imports' <> rest)

replaceAll :: [RF] -> IO ()
replaceAll = mapM_ replaceOneFile
