{-# LANGUAGE OverloadedStrings #-}

module Text.Toml.QuerySpec (main, spec) where

import Test.Hspec
import Text.Toml.Query
import Text.Toml.Types.Toml

testDocument :: Toml
testDocument =
  insert
    "A"
    ( TTable
        Outline
        ( insert
            "B"
            (TString "Hello World")
            ( insert
                "D"
                (TDouble 2.0)
                ( insert
                    "X"
                    (TInteger 42)
                    ( insert
                        "Y"
                        (TBoolean True)
                        empty
                    )
                )
            )
        )
    )
    (insert "Arr" (TArray Inline [TDouble 3.14, TDouble 3]) empty)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "TomlPath" $ do
    it "can parse and join paths" $ do
      joinPaths "A.b" "C.d"
        `shouldBe` (TomlPath (map TableLookup ["A", "b", "C", "d"]))
    it "can join an index to a path" $ do
      joinPaths "A" (indexArray 3)
        `shouldBe` (TomlPath [TableLookup "A", ArrayLookup 3])
    it "indexArrayAt can index to a path" $ do
      indexArrayAt "A" 3
        `shouldBe` (TomlPath [TableLookup "A", ArrayLookup 3])
    it "parses the empty path to the root path" $ do
      joinPaths "" "" `shouldBe` rootPath
    it "will build a path manually from parts" $ do
      pathFromParts ["a", "b", "c"]
        `shouldBe` (TomlPath [TableLookup "a", TableLookup "b", TableLookup "c"])
  describe "Toml Querying" $ do
    it "should find a string" $ do
      lookupStr "A.B" testDocument
        `shouldBe` (Just "Hello World")
    it "should find an integer" $ do
      lookupInt "A.X" testDocument
        `shouldBe` (Just 42)
    it "should find a boolean" $ do
      lookupBool "A.Y" testDocument
        `shouldBe` (Just True)
    it "should find a double" $ do
      lookupDouble "A.D" testDocument
        `shouldBe` (Just 2.0)
    it "should index to find a double" $ do
      lookupDouble (indexArrayAt "Arr" 0) testDocument
        `shouldBe` (Just 3.14)
    it "should index to find another double" $ do
      lookupDouble (indexArrayAt "Arr" 1) testDocument
        `shouldBe` (Just 3)
    it "out of bounds indexing should produce nothing" $ do
      lookupDouble (indexArrayAt "Arr" 120) testDocument
        `shouldBe` Nothing
    it "out of bounds indexing should produce nothing (Negative)" $ do
      lookupDouble (indexArrayAt "Arr" (-1)) testDocument
        `shouldBe` Nothing
