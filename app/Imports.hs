{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Imports
-- Description :  Separate imports from rest of file
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Jul 15 09:44:16 2024.
module Imports
  ( TsFile (..),
    breakImports,
  )
where

import Control.Applicative (Alternative (..))
import Data.Attoparsec.Text
  ( Parser,
    endOfInput,
    endOfLine,
    isEndOfLine,
    parseOnly,
    string,
    takeTill,
    takeWhile,
    takeWhile1,
  )
import Data.Char (isSpace)
import Data.Text (Text)
import Data.Text qualified as T
import Prelude hiding (takeWhile)

data TsFile = TsFile {imports :: Text, code :: Text}

whitespace :: Parser Text
whitespace = takeWhile1 isSpace

pComment :: Parser Text
pComment = do
  w <- whitespace
  c <- string "//"
  r <- takeTill isEndOfLine
  endOfLine
  pure $ w <> c <> r <> "\n"

pImport :: Parser Text
pImport = do
  i <- string "import"
  w1 <- takeTill (== ';')
  c <- string ";"
  endOfLine
  pure $ i <> w1 <> c <> "\n"

pBreakImports :: Parser (Text, Text)
pBreakImports = do
  is <- many $ pImport <|> pComment
  re <- takeWhile (const True)
  endOfInput
  pure (T.concat is, re)

breakImports :: Text -> (Text, Text)
breakImports = either error id . parseOnly pBreakImports
