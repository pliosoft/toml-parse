{-# LANGUAGE PackageImports #-}
module Text.Toml.Parser2
    ( parseToml
    ) where

import "htoml" Text.Toml
import "htoml" Text.Toml.Types

import Text.Toml.Types.Toml

import Data.Bifunctor (bimap)
import Data.Text (Text)

import qualified Data.HashMap.Strict as Map

parseToml :: String -> Text -> Either String Toml
parseToml src = bimap show tableToToml . parseTomlDoc src

tableToToml :: Table -> Toml
tableToToml = Toml . Map.map nodeToNamable

nodeToNamable :: Node -> TNamable
nodeToNamable (NTValue value) = valueToNamable value
nodeToNamable (NTable table) = TTable undefined $ tableToToml table
nodeToNamable (NTArray tables) = TArray undefined $ map (TTable undefined . tableToToml) tables

valueToNamable :: TValue -> TNamable
valueToNamable (VString s) = TString s
valueToNamable (VInteger i) = TInteger i
valueToNamable (VFloat d) = TDouble d
valueToNamable (VBoolean b) = TBoolean b
valueToNamable (VDatetime t) = TDatetime t
valueToNamable (VArray values) = TArray undefined $ map valueToNamable values
