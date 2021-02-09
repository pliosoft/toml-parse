{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SpecHelper where

import Data.Aeson
import Data.Char (toLower)
import qualified Data.HashMap.Strict as Map
import Data.Time.Format
import Text.Toml.Types.Toml

instance ToJSON Toml where
  toJSON (Toml hm) = Object $ Map.map toJSON hm

instance ToJSON TNamable where
  toJSON (TTable _ t) = toJSON t
  toJSON (TArray ImplicitOutline x) = toJSON x
  toJSON (TArray _ x) =
    object
      [ "type" .= ("array" :: String),
        "value" .= toJSON x
      ]
  toJSON (TString t) =
    object
      [ "type" .= ("string" :: String),
        "value" .= t
      ]
  toJSON (TInteger i) =
    object
      [ "type" .= ("integer" :: String),
        "value" .= show i
      ]
  toJSON (TDouble f) =
    object
      [ "type" .= ("float" :: String),
        "value" .= show f
      ]
  toJSON (TBoolean b) =
    object
      [ "type" .= ("bool" :: String),
        "value" .= (map toLower $ show b)
      ]
  toJSON (TDatetime t) =
    object
      [ "type" .= ("datetime" :: String),
        "value" .= format t
      ]
    where
      -- 1987-07-05T17:45:00Z
      format = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
