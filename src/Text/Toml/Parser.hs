{-# LANGUAGE OverloadedStrings #-}

module Text.Toml.Parser
    ( parseToml
    ) where

import Text.Toml.Combinators
import Text.Toml.Tokenizer
import Text.Toml.Types.Toml

import Control.Monad ((<=<), foldM, forM_, unless, void)
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Time.LocalTime
import Text.Parsec hiding (newline)

import qualified Data.Set as Set
import qualified Data.Text as T

-- Internal types for converting from parsed data to final Toml datatype
type SectionKey = [Text]

data Section
    = ObjSection SectionKey [(Text, TNamable)]
    | ArraySection SectionKey [(Text, TNamable)]

skey :: Section -> SectionKey
skey (ObjSection k _) = k
skey (ArraySection k _) = k

-- | Parse the given value to a @'Toml'@ document
parseToml
    :: String -- ^ Input name, e.g. @\"<stdin>\"@
    -> Text   -- ^ Toml content
    -> Either String Toml
parseToml src = first show . parse parser src <=< tokenize

parser :: Parser Toml
parser = do
    (intro, rest) <- between linebreaks linebreaks sections <* eof

    forM_ (firstDup $ map fst intro) $ \x ->
        unexpected $ "duplicate assignment " ++ T.unpack x

    forM_ (firstDup $ map skey rest) $ \x ->
        unexpected $ "duplicate section " ++ T.unpack (T.intercalate "." x)

    foldM mergeSection (fromList intro) $ rest

  where
    firstDup :: Ord a => [a] -> Maybe a
    firstDup = dup Set.empty

    dup :: Ord a => Set.Set a -> [a] -> Maybe a
    dup _ [] = Nothing
    dup s (a:as) =
        if Set.member a s
            then Just a
            else dup (Set.insert a s) as

    mergeSection :: Toml -> Section -> Parser Toml
    mergeSection t (ObjSection key lst) = insertChildren key lst t
    mergeSection t (ArraySection key lst) = appendChildren key lst t

sections :: Parser ([(Text, TNamable)], [Section])
sections = (,) <$> many assignment <*> many section

section :: Parser Section
section = arraySection <|> objSection
  where
    arraySection :: Parser Section
    arraySection = ArraySection
        <$> try (doubleBracketed sectionKey <* linebreaks)
        <*> many assignment

    objSection :: Parser Section
    objSection = ObjSection
        <$> (bracketed sectionKey <* linebreaks)
        <*> many assignment

    sectionKey :: Parser SectionKey
    sectionKey = choice [identifier, quoted] `sepBy1` period

assignment :: Parser (Text, TNamable)
assignment = assignment' <* linebreaks
  where
    assignment' :: Parser (Text, TNamable)
    assignment' = (,) <$> ((identifier <|> quoted) <* equal) <*> value

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
array = do
    elms <- between
        (lbracket >> linebreaks)
        (rbracket >> linebreaks)
        (try (endBy value (comma <* linebreaks))
          <|> sepBy value (comma <* linebreaks))

    unless (allSame elms) $
        unexpected "Element types do not match"

    return $ TArray Inline elms

linebreaks :: Parser ()
linebreaks = void $ many newline

allSame, allArray, allBoolean, allInteger, allString, allDouble, allDatetime, allTable :: [TNamable] -> Bool
allSame [] = True
allSame ((TArray{}):xs) = allArray xs
allSame ((TBoolean{}):xs) = allBoolean xs
allSame ((TString{}):xs) = allString xs
allSame ((TInteger{}):xs) = allInteger xs
allSame ((TDouble{}):xs) = allDouble xs
allSame ((TDatetime{}):xs) = allDatetime xs
allSame ((TTable{}):xs) = allTable xs

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

allTable [] = True
allTable ((TTable{}):as) = allTable as
allTable _ = False
