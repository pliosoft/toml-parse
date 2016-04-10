module Text.Toml.Parser
    ( parseToml
    ) where

import Text.Toml.Combinators
import Text.Toml.Tokenizer
import Text.Toml.Types.Toml

import Control.Monad ((<=<))
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Time.LocalTime
import Text.Parsec

parseToml :: String -> Text -> Either String Toml
parseToml src = first show . parse parser src <=< tokenize

parser :: Parser Toml
parser = fromList <$> manyTill assignment eof

assignment :: Parser (Text, TNamable)
assignment = (,) <$> (identifier <* equal) <*> value

value :: Parser TNamable
value = choice
    [ array
    , TBoolean <$> bool
    , TString <$> quoted
    , TInteger . fromIntegral <$> integer
    , TDouble <$> float
    , TDatetime . zonedTimeToUTC <$> date
    , TDatetime . localTimeToUTC tz <$> localDate
    ]

  where
    tz :: TimeZone
    tz = error "required IO poses a small problem..."

array :: Parser TNamable
array = TArray Inline
    <$> between lbracket rbracket
    (try (endBy value comma) <|> sepBy value comma)
