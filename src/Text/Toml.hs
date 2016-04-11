module Text.Toml
    ( readToml
    ) where

import Text.Toml.Parser
import Text.Toml.Types.Toml

import qualified Data.Text.IO as T

-- | Given a toml document, will read it completely, producing a Toml document as a response
readToml :: FilePath -> IO (Either String Toml)
readToml a = parseToml a <$> T.readFile a
