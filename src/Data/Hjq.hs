{-# LANGUAGE OverloadedStrings #-}

module Data.Hjq (hjq) where

import Control.Error.Util
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy
import Data.Functor
import Data.Hjq.Parser
import Data.Hjq.Query
import Data.Text

hjq :: ByteString -> Text -> Either Text ByteString
hjq jsonString queryString = do
  value <- note "Invalid json format." $ decode jsonString
  query <- parseJqQuery queryString
  executeQuery query value <&> encodePretty
