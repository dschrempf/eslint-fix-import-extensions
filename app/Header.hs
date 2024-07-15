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
    manyTill,
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
whitespace = takeWhile isSpace

commentChunks :: Parser [Text]
commentChunks = manyTill (takeWhile1 (/= '*')) (string "*/")

pCommentMultiLine :: Parser Text
pCommentMultiLine = do
  wsS <- whitespace
  cmtS <- string "/*"
  cmts <- commentChunks
  wsE <- whitespace
  pure $ wsS <> cmtS <> T.concat cmts <> "*/" <> wsE

pCommentLine :: Parser Text
pCommentLine = do
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
  is <- many $ pHeader <|> pCommentLine <|> pCommentMultiLine <|> emptyLine
  re <- takeWhile (const True)
  endOfInput
  pure (T.concat is, re)

breakHeader :: Text -> (Text, Text)
breakHeader = either error id . parseOnly pBreakHeader
