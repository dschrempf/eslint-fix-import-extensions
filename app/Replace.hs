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
import Header (breakHeader)
import Parse (R (..), RF (..))
import System.Directory (doesFileExist, withCurrentDirectory)
import System.FilePath (takeDirectory)

replaceOne :: FilePath -> Text -> R -> IO Text
replaceOne directory input (R from ext) = withCurrentDirectory directory $ do
  fileExists <- doesFileExist $ T.unpack to
  if fileExists
    then pure $ replaceWith to
    else do
      indexFileExists <- doesFileExist $ T.unpack toIndex
      if indexFileExists
        then pure $ replaceWith toIndex
        else do
          T.putStrLn $ "warning: could not add " <> ext <> " to " <> from
          pure input
  where
    quote x = "\"" <> x <> "\""
    to = from <> "." <> ext
    toIndex = from <> "/index." <> ext
    replaceWith x = T.replace (quote from) (quote x) input

replaceOneFile :: RF -> IO ()
replaceOneFile (RF file rs) = do
  let fp = T.unpack file
  input <- T.readFile fp
  let (header, rest) = breakHeader input
      path = takeDirectory $ T.unpack file
  imports' <- foldlM (replaceOne path) header rs
  T.writeFile fp (imports' <> rest)

replaceAll :: [RF] -> IO ()
replaceAll = mapM_ replaceOneFile
