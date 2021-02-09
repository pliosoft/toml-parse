{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Tokens for Toml filetype
module Text.Toml.Types.Tokens (Token (..), tokenPos) where

import Data.Text (Text)
import Data.Time.LocalTime

-- | All of the tokens we will parse from a TOML file.
data Token
  = IdentifierT !(Int, Int) !Text
  | QuotedStringT !(Int, Int) !Text
  | KeywordTrueT !(Int, Int)
  | KeywordFalseT !(Int, Int)
  | LeftBracketT !(Int, Int)
  | RightBracketT !(Int, Int)
  | LeftBraceT !(Int, Int)
  | RightBraceT !(Int, Int)
  | CommaT !(Int, Int)
  | EqualT !(Int, Int)
  | PeriodT !(Int, Int)
  | IntegerT !(Int, Int) !Integer !Text
  | FloatT !(Int, Int) !Double !Text
  | DateT !(Int, Int) !ZonedTime
  | LocalDateT !(Int, Int) !LocalTime
  | NewlineT !(Int, Int)
  deriving (Eq, Show)

instance Eq ZonedTime where
  a == b = (zonedTimeToUTC a) == (zonedTimeToUTC b)

tokenPos :: Token -> (Int, Int)
tokenPos t = case t of
  IdentifierT pos _ -> pos
  QuotedStringT pos _ -> pos
  KeywordTrueT pos -> pos
  KeywordFalseT pos -> pos
  LeftBracketT pos -> pos
  RightBracketT pos -> pos
  LeftBraceT pos -> pos
  RightBraceT pos -> pos
  CommaT pos -> pos
  EqualT pos -> pos
  PeriodT pos -> pos
  IntegerT pos _ _ -> pos
  FloatT pos _ _ -> pos
  DateT pos _ -> pos
  LocalDateT pos _ -> pos
  NewlineT pos -> pos
