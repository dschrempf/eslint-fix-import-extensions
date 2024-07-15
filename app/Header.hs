{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Header
-- Description :  Separate imports from rest of file
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Jul 15 09:44:16 2024.
module Header
  ( TsFile (..),
    breakHeader,
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

pHeader :: Parser Text
pHeader = do
  i <- string "import" <|> string "export"
  w1 <- takeTill (== ';')
  c <- string ";"
  endOfLine
  pure $ i <> w1 <> c <> "\n"

emptyLine :: Parser Text
emptyLine = string "\n"

pBreakHeader :: Parser (Text, Text)
pBreakHeader = do
  is <- many $ pHeader <|> pComment <|> emptyLine
  re <- takeWhile (const True)
  endOfInput
  pure (T.concat is, re)

breakHeader :: Text -> (Text, Text)
breakHeader = either error id . parseOnly pBreakHeader
