{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}


module Hilt.JSON where

import Data.Char               (toLower)
import Data.Text               (Text)
import Data.Text.Lazy          as TL  (toStrict)
import Data.Text.Lazy.Encoding as TLE (decodeUtf8)
import Data.Text.Encoding      as TE  (encodeUtf8)
import Data.ByteString.Lazy    as BL  (fromStrict)
import Data.Time               (UTCTime, parseTimeM, defaultTimeLocale)

import Data.Aeson       (encode, eitherDecode)
import Data.Aeson.Types (ToJSON, FromJSON, Options, defaultOptions, fieldLabelModifier, Parser)


toJson :: ToJSON a => a -> Text
toJson = TL.toStrict . TLE.decodeUtf8 . encode

fromJson :: FromJSON a => Text -> Either String a
fromJson =  eitherDecode . BL.fromStrict . TE.encodeUtf8

dropPrefix :: String -> Options
dropPrefix s = defaultOptions { fieldLabelModifier = (\(x:xs) -> map toLower [x] ++ xs) . drop (length s) }

-- (Just "%H:%M:%S")  == "%Y-%m-%dT%H:%M:%S"  -- i.e. YYYY-MM-DDTHH:MM:SS
parseUtcTime :: String -> Maybe UTCTime
parseUtcTime = parseTimeM False defaultTimeLocale "%FT%T%QZ"

utcTimeParser :: String -> Parser UTCTime
utcTimeParser expires = case parseUtcTime expires of
  Nothing -> fail "utcTime value has to be of the following format: YYYY-MM-DDTHH:MM:SS.SZ"
  Just e  -> return e
