{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Toml.Types.Toml
    ( Toml(..)
    , fromList
    , insert
    , empty
    , insertChildren
    , lookup
    , appendChildren
    , Inlined(..)
    , TNamable(..)
    ) where


import Prelude hiding (lookup)

import Control.Monad ((<=<))
import Data.Int(Int64)
import Data.Text(Text)
import Data.Time.Clock(UTCTime)

import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T


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

fromList :: [(Text, TNamable)] -> Toml
fromList = Toml . Map.fromList

-- | Given some name/value pair, will insert into a toml document, producing a new document with the specified
-- namable nested under the provided name.
insert :: Text -> TNamable -> Toml -> Toml
insert t n a = Toml (Map.insert t n (unToml a))

-- | Given a key and a toml document, maybe produce the raw node contained at that location. This is
-- considered a primitive operation. Please use specific lookup functions to get access to the raw data.
lookup :: Text -> Toml -> Maybe TNamable
lookup key doc = Map.lookup key (unToml doc)

lookupDefault :: TNamable -> Text -> Toml -> TNamable
lookupDefault def key doc = Map.lookupDefault def key (unToml doc)

-- | Like Map.insertWith but taking a Monadic action to resolve conflicts
insertWithM :: Monad m => (TNamable -> TNamable -> m TNamable) -> Text -> TNamable -> Toml -> m Toml
insertWithM f k v (Toml m) = maybe
    (return $ Toml $ Map.insert k v m)
    (return . Toml . flip (Map.insert k) m <=< f v)
    $ Map.lookup k m

-- Takes apart a toml doc, runs a transformation at that point, and then puts it back together
inContext :: Monad m => [Text] -> (TNamable -> TNamable -> m TNamable) -> TNamable -> Toml -> m Toml
inContext (t:[]) f v d = insertWithM f t v d
inContext (t:ts) f v d =
    case lookupDefault (TTable Inline empty) t d of
        (TTable _ tml) -> do
            inner <- TTable Inline <$> inContext ts f v tml
            insertWithM comb t inner d
        _ -> fail "Not a table"
  where
    comb x y = do
        m <- Map.union
            <$> nested x
            <*> nested y

        return $ TTable Inline $ Toml m

    nested (TTable _ (Toml t1)) = return t1
    nested _ = fail "not a table"

-- TODO: what does this case represent?
inContext _ _ _ _ = fail "inContext was non-exhaustive"

-- | Insert a whole set of values under a certain key, appending where it is an array and failing otherwise
appendChildren :: Monad m => [Text] -> [(Text, TNamable)] -> Toml -> m Toml
appendChildren path n a = inContext path arrayAppend (TArray Inline [TTable Inline (fromList n)]) a
   where arrayAppend (TArray _ a1) (TArray _ a2) = return $ TArray Inline $ a1 ++ a2
         arrayAppend _ _ = fail "Not arrays"

-- | Insert a whole set of values under a certain key
insertChildren :: [Text] -> [(Text, TNamable)] -> Toml -> Toml
insertChildren [] n a = Toml ((unToml a) `Map.union` (Map.fromList n))
insertChildren (root:rt) n a =
      insert root (TTable ImplicitOutline
                     (insertChildren rt n (nestedToml root a))) a
   where nestedToml x p =
            case lookup x p of
               Just (TTable _ t) -> t
               _ -> empty


-- | Create an empty toml document
empty :: Toml
empty = Toml Map.empty
