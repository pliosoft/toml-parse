module Text.Toml.Parser
    ( parseToml
    ) where

import Text.Toml.Combinators
import Text.Toml.Tokenizer
import Text.Toml.Types.Toml

import Control.Monad ((<=<))
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.List (foldl')
import Data.Time.LocalTime
import Text.Parsec

-- Internal types for converting from parsed data to final Toml datatype
type SectionKey = Text
data Section = Section SectionKey [(Text, TNamable)]

firstDup :: Ord a => [a] -> Maybe a
firstDup xs = dup xs Set.empty
  where dup [] _ = Nothing
        dup (a:as) s = if Set.member a s
                       then Just a
                       else dup as (Set.insert a s)

parseToml :: String -> Text -> Either String Toml
parseToml src = first show . parse parser src <=< tokenize

mergeSection :: Toml -> Section -> Toml
mergeSection t (Section key lst) = insertChildren key Inline lst t

parser :: Parser Toml
parser = do (intro, rest) <- sections <* eof
            case firstDup (map (\(Section k _) -> k) rest) of
               Nothing -> return $ foldl' mergeSection (fromList intro) rest
               Just x -> unexpected $ "Duplicate key " ++ (T.unpack x)

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

allSame, allArray, allBoolean, allInteger, allString, allDouble, allDatetime :: [TNamable] -> Bool
allSame [] = True
allSame ((TArray{}):xs) = allArray xs
allSame ((TBoolean{}):xs) = allBoolean xs
allSame ((TString{}):xs) = allString xs
allSame ((TInteger{}):xs) = allInteger xs
allSame ((TDouble{}):xs) = allDouble xs
allSame ((TDatetime{}):xs) = allDatetime xs

allArray [] = True
allArray ((TArray{}):as) = allArray as
allArray _ = False

allBoolean [] = True
allBoolean ((TBoolean{}):as) = allBoolean as
allBoolean _ = False

allInteger [] = True
allInteger ((TInteger{}):as) = allInteger as
allInteger _ = False

allString [] = True
allString ((TString{}):as) = allString as
allString _ = False

allDouble [] = True
allDouble ((TDouble{}):as) = allDouble as
allDouble _ = False

allDatetime [] = True
allDatetime ((TDatetime{}):as) = allDatetime as
allDatetime _ = False

array :: Parser TNamable
array = do elms <- p
           if not $ allSame elms
           then unexpected "Types do not match"
           else return $ TArray Inline elms
    where p = between lbracket rbracket (try (endBy value comma) <|> sepBy value comma)

