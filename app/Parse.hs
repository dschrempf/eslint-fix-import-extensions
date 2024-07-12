{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Parse
-- Description :  Parse ESLint output
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Jul 12 13:48:18 2024.
module Parse
  ( R (..),
    RF (..),
    pFilesWithReplacements,
  )
where

import Control.Applicative (Alternative (..))
import Data.Attoparsec.Text
  ( Parser,
    char,
    decimal,
    endOfInput,
    endOfLine,
    isEndOfLine,
    many1',
    sepBy1',
    skipSpace,
    string,
    takeTill,
    takeWhile1,
    (<?>),
  )
import Data.Char (isSpace)
import Data.Text (Text, singleton)

data R = R {from :: !Text, ext :: !Text}
  deriving (Show)

data RF = RF {file :: !Text, replacements :: ![R]}
  deriving (Show)

pFile :: Parser Text
pFile = takeTill isSpace

skipLocation :: Parser ()
skipLocation = do
  _ <- decimal :: Parser Int
  _ <- char ':'
  _ <- decimal :: Parser Int
  pure () <?> "location"

pQuoted :: Parser Text
pQuoted = do
  _ <- char '"'
  t <- takeWhile1 (\x -> not (isSpace x) && (x /= '"'))
  _ <- char '"'
  pure t

pReplacement :: Parser R
pReplacement = do
  skipSpace
  skipLocation
  skipSpace
  _ <- string "error"
  skipSpace
  _ <- string "Missing"
  skipSpace
  _ <- string "file"
  skipSpace
  _ <- string "extension"
  skipSpace
  ext <- pQuoted <?> "extension"
  skipSpace
  _ <- string "for"
  skipSpace
  from <- pQuoted <?> "from"
  skipSpace
  _ <- string "import/extensions"
  pure $ R from ext

pReplacements :: Parser [R]
pReplacements = pReplacement `sepBy1'` endOfLine

pReplacementsWithFile :: Parser RF
pReplacementsWithFile = do
  file <- pFile
  _ <- endOfLine
  RF file <$> pReplacements

lineWith :: Text -> Parser Text
lineWith txt = do
  _ <- string txt
  ln <- takeTill isEndOfLine
  endOfLine
  pure $ txt <> ln

pFilesWithReplacements :: Parser [RF]
pFilesWithReplacements = do
  skipSpace
  _ <- many (lineWith ">")
  skipSpace
  rs <- pReplacementsWithFile `sepBy1'` skipSpace
  skipSpace
  _ <- many1' (lineWith "✖")
  skipSpace
  _ <- many1' (lineWith "ELIFECYCLE")
  skipSpace
  endOfInput
  pure rs
