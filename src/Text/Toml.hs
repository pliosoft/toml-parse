{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Toml
    ( readToml
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
import Text.Toml.Types.Toml


-- | Given a toml document, will read it completely, producing a Toml document as a response
readToml :: FilePath -> IO (Either Text Toml)
readToml a = do putStrLn "readToml"
                return $ Right (insert "four" (TInteger 4) empty)


