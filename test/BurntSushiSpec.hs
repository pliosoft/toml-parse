module BurntSushiSpec
  ( main,
    spec,
  )
where

import Control.Monad (forM_)
import Data.Aeson hiding (json)
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Text as T
import qualified Data.Text.IO as T
import SpecHelper ()
import System.FilePath
import System.FilePath.Glob (compile, globDir1)
import Test.Hspec
import Text.Toml.Parser

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let tests = "toml-test/tests"

  validPaths <- runIO $ globDir1 (compile "*.toml") $ tests </> "valid"
  invalidPaths <- runIO $ globDir1 (compile "*.toml") $ tests </> "invalid"

  forM_ validPaths $ \fp -> do
    toml <- runIO $ T.readFile fp
    json <- runIO $ BS.readFile $ asJSON fp

    it (makeRelative tests fp) $ do
      let Right value = eitherDecode json :: Either String Value
          eparsed = toJSON <$> parseToml fp toml

      case eparsed of
        Right parsed
          | parsed /= value ->
            expectationFailure $
              unlines
                [ "Decoding mismatch",
                  "Input TOML:",
                  T.unpack toml,
                  "",
                  "Expected JSON:",
                  C8.unpack $ encodePretty value,
                  "",
                  "Actual JSON:",
                  C8.unpack $ encodePretty parsed
                ]
        Left err ->
          expectationFailure $
            unlines
              [ "Decoding error:",
                "Input TOML:",
                T.unpack toml,
                "",
                "Expected JSON:",
                C8.unpack $ encodePretty value,
                "",
                "Error:",
                err
              ]
        _ -> return () -- success
  forM_ invalidPaths $ \fp -> do
    toml <- runIO $ T.readFile fp

    it (makeRelative tests fp) $ do
      let eparsed = toJSON <$> parseToml fp toml

      case eparsed of
        Right parsed ->
          expectationFailure $
            unlines
              [ "Decoding success, expected failure",
                "Input TOML:",
                T.unpack toml,
                "",
                "Unexpected JSON:",
                C8.unpack $ encodePretty parsed
              ]
        Left _ -> return () -- success

asJSON :: FilePath -> FilePath
asJSON = (<.> "json") . dropExtension
