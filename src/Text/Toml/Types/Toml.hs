{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Toml.Types.Toml
    ( Toml(..)
    , fromList
    , insert
    , empty
    , Inlined(..)
    , TNamable(..)
    ) where


import Data.Text(Text)
import Data.Time.Clock(UTCTime)
import Data.Int(Int64)
import qualified Data.HashMap.Strict as Map
import Prelude hiding (lookup)


-- | Certain TOML constructs can be defined inline or outline. This mostly matters when we round-trip toml
-- through the parser and pretty-printer.
-- Also relevant is the ImplicitOutline case, which is used for tables defined implicitly, such as a here: [a.b]
data Inlined = Inline | Outline | ImplicitOutline

-- | Anything that can be named in a toml document.
data TNamable = TTable       Inlined  Toml
              | TTableArray  Inlined [Toml]
              | TArray       Inlined [TNamable]
              | TString               Text
              | TInteger              Int64
              | TDouble               Double
              | TBoolean              Bool
              | TDatetime             UTCTime


-- | A Toml document or sub-document
newtype Toml = Toml {unToml :: Map.HashMap Text TNamable }

fromList :: [(Text, TNamable)] -> Toml
fromList = Toml . Map.fromList

-- | Given some name/value pair, will insert into a toml document, producing a new document with the specified
-- namable nested under the provided name.
insert :: Text -> TNamable -> Toml -> Toml
insert t n a = Toml (Map.insert t n (unToml a))


-- | Create an empty toml document
empty :: Toml
empty = Toml Map.empty
