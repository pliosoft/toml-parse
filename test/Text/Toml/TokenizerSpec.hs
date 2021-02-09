{-# LANGUAGE OverloadedStrings #-}

module Text.Toml.TokenizerSpec (main, spec) where

import Data.Text (Text)
import Data.Time
import Test.Hspec
import Text.Toml.Tokenizer
import Text.Toml.Types.Tokens

main :: IO ()
main = hspec spec

tokIs :: Text -> [Token] -> Expectation
tokIs s expected = tokenize s `shouldBe` Right expected

spec :: Spec
spec = do
  describe "Basic Tokenization" $ do
    it "true" $ do
      tokIs
        "true"
        [KeywordTrueT (1, 1)]
    it "case sensitive true" $ do
      tokIs
        "TrUe"
        [IdentifierT (1, 1) "TrUe"]
    it "false" $ do
      tokIs
        "false"
        [KeywordFalseT (1, 1)]
    it "case sensitive false" $ do
      tokIs
        "FaLsE"
        [IdentifierT (1, 1) "FaLsE"]
    it "comma period equal brackets braces" $ do
      tokIs
        ", . = [ ] { }"
        [CommaT (1, 1), PeriodT (1, 3), EqualT (1, 5), LeftBracketT (1, 7), RightBracketT (1, 9), LeftBraceT (1, 11), RightBraceT (1, 13)]

  describe "Integers & Floats" $ do
    it "toml integers" $ do
      tokIs
        "123 -10 +12"
        [IntegerT (1, 1) 123 "123", IntegerT (1, 5) (-10) "-10", IntegerT (1, 9) 12 "+12"]
    it "toml integers with underscores" $ do
      tokIs
        "1_2_3 12_3 _13 13_"
        [IntegerT (1, 1) 123 "1_2_3", IntegerT (1, 7) 123 "12_3", IdentifierT (1, 12) "_13", IntegerT (1, 16) 13 "13", IdentifierT (1, 18) "_"]
    it "toml floats" $ do
      tokIs
        "123.456"
        [FloatT (1, 1) 123.456 "123.456"]
    it "toml floats with underscores" $ do
      tokIs
        "1_2_3.4_56"
        [FloatT (1, 1) 123.456 "1_2_3.4_56"]
    it "toml floats with exp and underscores" $ do
      tokIs
        "1_0e2 1e2_2"
        [FloatT (1, 1) 1000.0 "1_0e2", FloatT (1, 7) 1.0e22 "1e2_2"]
    it "toml floats with exp and frac" $ do
      tokIs
        "0e0 0.0 1.123e3 -2.1e2"
        [FloatT (1, 1) 0.0 "0e0", FloatT (1, 5) 0.0 "0.0", FloatT (1, 9) 1123.0 "1.123e3", FloatT (1, 17) (-210.0) "-2.1e2"]

  describe "String Tokenization" $ do
    it "simple quoted string" $ do
      tokIs
        "\"hello\" 'single quote'"
        [QuotedStringT (1, 1) "hello", QuotedStringT (1, 9) "single quote"]
    it "multiline dquoted string" $ do
      tokIs
        "\"\"\"hello\n   world\"\"\""
        [QuotedStringT (1, 1) "hello\n   world"]
    it "multiline squoted string" $ do
      tokIs
        "'''hello\n   world'''"
        [QuotedStringT (1, 1) "hello\n   world"]
    it "multiline squoted string with inner escaped whitespace" $ do
      tokIs
        "'''hello \\\n   world'''"
        [QuotedStringT (1, 1) "hello world"]
    it "multiline squoted string with leading newline" $ do
      tokIs
        "'''\nhello\n   world'''"
        [QuotedStringT (1, 1) "hello\n   world"]

  describe "Date Tokenization" $ do
    it "simple toml dates" $ do
      tokIs
        "1955-11-12"
        [LocalDateT (1, 1) (LocalTime (ModifiedJulianDay 35423) midnight)]
    it "toml dates with time and no zone" $ do
      tokIs
        "1955-11-12T09:28:00"
        [LocalDateT (1, 1) (LocalTime (ModifiedJulianDay 35423) (timeToTimeOfDay (secondsToDiffTime 34080)))]
    it "RFC 3339 utc dates" $ do
      tokIs
        "1985-04-12T23:20:50.52Z"
        [DateT (1, 1) (ZonedTime (LocalTime (ModifiedJulianDay 46167) (TimeOfDay 23 20 (fromRational 50.52))) utc)]
    it "RFC 3339 zoned dates" $ do
      tokIs
        "1985-04-12T23:20:50.52-07:30"
        [DateT (1, 1) (ZonedTime (LocalTime (ModifiedJulianDay 46167) (TimeOfDay 23 20 (fromRational 50.52))) (minutesToTimeZone (-7 * 60 -30)))]
