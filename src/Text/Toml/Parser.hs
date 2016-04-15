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

-- Internal types for converting from parsed data to final Toml datatype
type SectionKey = Text
data Section = Section SectionKey [(Text, TNamable)]

parseToml :: String -> Text -> Either String Toml
parseToml src = first show . parse parser src <=< tokenize

-- TODO: Handle subsections, instead of fst'ing them away
parser :: Parser Toml
parser = (fromList . fst) <$> sections <* eof

sections :: Parser ([(Text, TNamable)], [Section])
sections = (,) <$> many assignment <*> many section

-- TODO: qualified section keys will probably need another SectionKey entry
sectionKey :: Parser SectionKey
sectionKey = choice [identifier, quoted]

section :: Parser Section
section = Section <$> (bracketed sectionKey) <*> many assignment

assignment :: Parser (Text, TNamable)
assignment = (,) <$> ((identifier <|> quoted) <* equal) <*> value

value :: Parser TNamable
value = choice
    [ array
    , TBoolean <$> bool
    , TString <$> quoted
    , TInteger . fromIntegral <$> integer
    , TDouble <$> float
    , TDatetime . zonedTimeToUTC <$> date
    , TDatetime . localTimeToUTC tz <$> localDate
    ] <?> "value"

  where
    tz :: TimeZone
    tz = error "required IO poses a small problem..."

array :: Parser TNamable
array = TArray Inline
    <$> between lbracket rbracket
    (try (endBy value comma) <|> sepBy value comma)
