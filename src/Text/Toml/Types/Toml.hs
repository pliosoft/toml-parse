{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Toml.Types.Toml
    ( Toml(..)
    , insert
    , empty
    , Inlined(..)
    , TNamable(..)
    ) where


import Data.Text(Text)
import qualified Data.Text as T
import Data.Time.Clock(UTCTime)
import Data.Int(Int64)
import qualified Data.HashMap.Strict as Map
import Data.Foldable
import Data.String(IsString(..))
import qualified Control.Lens as L
import Prelude hiding (lookup)


-- | Certain TOML constructs can be defined inline or outline. This mostly matters when we round-trip toml
-- through the parser and pretty-printer.
-- Also relevant is the ImplicitOutline case, which is used for tables defined implicitly, such as a here: [a.b]
data Inlined = Inline | Outline | ImplicitOutline

-- | Anything that can be named in a toml document.
data TNamable = TTable       Inlined  Toml
              | TArray       Inlined [TNamable]
              | TString               Text
              | TInteger              Int64
              | TDouble               Double
              | TBoolean              Bool
              | TDatetime             UTCTime


-- | A Toml document or sub-document
newtype Toml = Toml {unToml :: Map.HashMap Text TNamable }


-- | Given some name/value pair, will insert into a toml document, producing a new document with the specified
-- namable nested under the provided name.
insert :: Text -> TNamable -> Toml -> Toml
insert t n a = Toml (Map.insert t n (unToml a))


-- | Create an empty toml document
empty :: Toml
empty = Toml Map.empty

