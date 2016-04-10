{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Aeson
import Data.Char (toLower)
import Data.Time.Format
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import System.Exit (exitFailure)

import qualified Data.Text.IO as T

import Text.Toml.Parser
import Text.Toml.Types.Toml

import qualified Data.HashMap.Strict as Map

instance ToJSON Toml where
    toJSON (Toml hm) = Object $ Map.map toJSON hm

instance ToJSON TNamable where
    toJSON (TTable _ t) = toJSON t
    toJSON (TArray _ x) = object
        [ "type" .= ("array" :: String)
        , "value" .= toJSON x
        ]
    toJSON (TString t) = object
        [ "type" .= ("string" :: String)
        , "value" .= t
        ]
    toJSON (TInteger i) = object
        [ "type" .= ("integer" :: String)
        , "value" .= show i
        ]
    toJSON (TDouble f) = object
        [ "type" .= ("float" :: String)
        , "value" .= show f
        ]
    toJSON (TBoolean b) = object
        [ "type" .= ("bool" :: String)
        , "value" .= (map toLower $ show b)
        ]
    toJSON (TDatetime t) = object
        [ "type" .= ("datetime" :: String)
        , "value" .= format t
        ]
      where
        -- 1987-07-05T17:45:00Z
        format = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

main :: IO ()
main = either
    (\err -> putStrLn err >> exitFailure)
    (T.putStrLn . toStrict . decodeUtf8 . encode)
    . parseToml "<stdin>" =<< T.getContents
