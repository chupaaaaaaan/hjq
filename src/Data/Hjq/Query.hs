{-# LANGUAGE OverloadedStrings #-}

module Data.Hjq.Query where 

import Control.Monad
import Control.Lens.Operators
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
import Data.Hjq.Parser
import Data.Aeson
import Data.Aeson.Lens
import Data.Text

applyFilter :: JqFilter -> Value -> Either Text Value
applyFilter (JqField fieldName n) obj@(Object _) = join $ noteNotFoundError fieldName (fmap (applyFilter n) (obj ^? key fieldName))
applyFilter (JqIndex index n) array@(Array _) = join $ noteOutOfRangeError index (fmap (applyFilter n) (array ^? nth index))

noteNotFoundError :: Text -> Maybe a -> Either Text a
noteNotFoundError _ (Just x) = Right x
noteNotFoundError s Nothing = Left $ "field name not found :" <> s

noteOutOfRangeError :: Int -> Maybe a -> Either Text a
noteOutOfRangeError _ (Just x) = Right x
noteOutOfRangeError s Nothing = Left $ "out of range :" <> tshow s

executeQuery :: JqQuery -> Value -> Either Text Value
-- executeQuery (JqQueryObject o) v = Object . H.fromList <$> mapM (mapM (`executeQuery` v)) o
executeQuery (JqQueryObject o) v = fmap (Object . H.fromList) . sequence . fmap sequence $ fmap (fmap $ flip executeQuery v) o
-- executeQuery (JqQueryArray l) v = Array . V.fromList <$> mapM (`executeQuery` v) l
executeQuery (JqQueryArray l) v = fmap (Array . V.fromList) . sequence $ fmap (flip executeQuery v) l
executeQuery (JqQueryFilter f) v = applyFilter f v

tshow :: Show a => a -> Text
tshow = pack . show

