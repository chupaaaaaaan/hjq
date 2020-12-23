{-# LANGUAGE OverloadedStrings #-}

module Data.Hjq.Parser where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Text

data JqFilter
  = JqField Text JqFilter
  | JqIndex Int JqFilter
  | JqNil
  deriving (Show, Read, Eq)

data JqQuery
  = JqQueryObject [(Text, JqQuery)]
  | JqQueryArray [JqQuery]
  | JqQueryFilter JqFilter
  deriving (Show, Read, Eq)

parseJqFilter :: Text -> Either Text JqFilter
parseJqFilter s = showParseResult $ parse (jqFilterParser <* endOfInput) s `feed` ""

jqFilterParser :: Parser JqFilter
jqFilterParser = schar '.' *> (jqField <|> jqIndex <|> pure JqNil)
  where
    jqFilter :: Parser JqFilter
    jqFilter = (schar '.' *> jqField) <|> jqIndex <|> pure JqNil

    jqField :: Parser JqFilter
    jqField = JqField <$> (word <* skipSpace) <*> jqFilter

    jqIndex :: Parser JqFilter
    jqIndex = JqIndex <$> (schar '[' *> decimal <* schar ']') <*> jqFilter

parseJqQuery :: Text -> Either Text JqQuery
parseJqQuery s = showParseResult $ parse (jqQueryParser <* endOfInput) s `feed` ""

jqQueryParser :: Parser JqQuery
jqQueryParser = jqQueryArray <|> jqQueryObject <|> jqQueryFilter
  where
    jqQueryArray :: Parser JqQuery
    jqQueryArray = JqQueryArray <$> (schar '[' *> jqQueryParser `sepBy` schar ',' <* schar ']')

    jqQueryObject :: Parser JqQuery
    jqQueryObject = JqQueryObject <$> (schar '{' *> qObj `sepBy` schar ',' <* schar '}')

    qObj :: Parser (Text, JqQuery)
    qObj = (,) <$> (schar '"' *> word <* schar '"') <*> (schar ':' *> jqQueryParser)

    jqQueryFilter :: Parser JqQuery
    jqQueryFilter = JqQueryFilter <$> jqFilterParser

showParseResult :: Show a => Result a -> Either Text a
showParseResult (Done _ r) = Right r
showParseResult r = Left . pack $ show r

word :: Parser Text
word = pack <$> many1 (letter <|> char '-' <|> char '_' <|> digit)

schar :: Char -> Parser Char
schar c = skipSpace *> char c <* skipSpace
