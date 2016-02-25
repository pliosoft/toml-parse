{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Toml.Query
    ( TomlPath(..)
    , TomlPathElement(..)
    , rootPath
    , pathFromParts
    , joinPaths
    , indexArray
    , indexArrayAt
    , lookupStr
    , lookupInt
    , lookupBool
    , lookupDouble
    , lookupDate
    ) where


import Text.Toml.Types.Toml
import Data.Text(Text)
import qualified Data.Text as T
import Data.Time.Clock(UTCTime)
import Data.Int(Int64)
import qualified Data.HashMap.Strict as Map
import Data.Foldable
import Data.String(IsString(..))
import qualified Control.Lens as L
import Prelude hiding (lookup)


-- | a path element is either a table name or an array index
data TomlPathElement = TableLookup Text | ArrayLookup Int
   deriving (Eq,Show)

-- | A path into a toml document
newtype TomlPath = TomlPath {unPath :: [TomlPathElement]}
   deriving (Eq,Show)


-- | TomlPath can be specifed with a single dot-notation string using this instance.
-- e.g., lookupTable "a.b.c"
instance IsString TomlPath where
   fromString = TomlPath . map TableLookup . cleanup
      where cleanup = filter ("" /=) . map T.strip . T.splitOn "." . T.pack


-- | Path to the root of a toml document
rootPath :: TomlPath
rootPath = TomlPath []


-- | Index a path at current path
indexArray :: Int -> TomlPath
indexArray i = TomlPath [ArrayLookup i]

-- | Index the array at a path
indexArrayAt :: TomlPath -> Int -> TomlPath
indexArrayAt p i = joinPaths p (indexArray i)


-- | Provide a list of path parts, and this will contruct a toml path
pathFromParts :: [Text] -> TomlPath
pathFromParts p = TomlPath (map TableLookup p)


-- | Given two paths, join them
joinPaths :: TomlPath -> TomlPath -> TomlPath
joinPaths a b = TomlPath (unPath a ++ unPath b)


-- | Given a key and a toml document, maybe produce the raw node contained at that location. This is
-- considered a primitive operation. Please use specific lookup functions to get access to the raw data.
lookup :: Text -> Toml -> Maybe TNamable
lookup key doc = Map.lookup key (unToml doc)


-- | how to index any given level of a namable structure
lookupSinglePath :: Maybe TNamable -> TomlPathElement -> Maybe TNamable
lookupSinglePath (Just (TTable _ n)) (TableLookup key) = lookup key n
lookupSinglePath (Just (TArray _ n)) (ArrayLookup idx) = n L.^? L.element idx
lookupSinglePath _ _ = Nothing


-- | Look up the named object at a path
lookupPath :: TomlPath -> Toml -> Maybe TNamable
lookupPath path doc = case unPath path of
                         (TableLookup k : rest) ->  foldl' lookupSinglePath (lookup k doc) rest
                         _ -> Nothing

-- | Given some path, will produce any table at the specified location.
lookupArray :: TomlPath -> Toml -> Maybe [TNamable]
lookupArray path doc =
   case lookupPath path doc of
      Just (TArray _ tn) -> return tn
      _ -> Nothing


-- | Given some path, will produce any table at the specified location.
lookupTable :: TomlPath -> Toml -> Maybe Toml
lookupTable path doc =
   case lookupPath path doc of
      Just (TTable _ tn) -> return tn
      _ -> Nothing


-- | If you have a path and you expect a string, this will return said string
lookupStr :: TomlPath -> Toml -> Maybe Text
lookupStr path doc =
   case lookupPath path doc of
      Just (TString t) -> return t
      _ -> Nothing

-- | If you have a path and you expect a integer, this will return said value
lookupInt :: TomlPath -> Toml -> Maybe Int64
lookupInt path doc =
   case lookupPath path doc of
      Just (TInteger t) -> return t
      _ -> Nothing


-- | If you have a path and you expect a bool, this will return said value
lookupBool :: TomlPath -> Toml -> Maybe Bool
lookupBool path doc =
   case lookupPath path doc of
      Just (TBoolean t) -> return t
      _ -> Nothing


-- | If you have a path and you expect a double, this will return said value
lookupDouble :: TomlPath -> Toml -> Maybe Double
lookupDouble path doc =
   case lookupPath path doc of
      Just (TDouble t) -> return t
      _ -> Nothing


-- | If you have a path and you expect a date, this will return said value
lookupDate :: TomlPath -> Toml -> Maybe UTCTime
lookupDate path doc =
   case lookupPath path doc of
      Just (TDatetime t) -> return t
      _ -> Nothing
