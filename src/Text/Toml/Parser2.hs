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
nodeToNamable (VTable table) = TTable undefined $ tableToToml table
nodeToNamable (VTArray tables) = TTableArray undefined $ map tableToToml tables
nodeToNamable (VString s) = TString s
nodeToNamable (VInteger i) = TInteger i
nodeToNamable (VFloat d) = TDouble d
nodeToNamable (VBoolean b) = TBoolean b
nodeToNamable (VDatetime t) = TDatetime t
nodeToNamable (VArray nodes) = TArray undefined $ map nodeToNamable nodes
