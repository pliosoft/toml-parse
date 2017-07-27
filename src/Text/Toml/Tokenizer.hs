{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}

module Text.Toml.Tokenizer
    ( tokenize, tokenizer
    ) where


   import           Prelude hiding (takeWhile)
   import           Text.Toml.Types.Tokens
   import           Control.Applicative
   import           Control.Arrow
   import           Control.Monad
   import           Data.Attoparsec.Text hiding (number)
   import           Data.Char
   import qualified Data.ByteString as B
   import           Data.Text (Text)
   import qualified Data.Text as T
   import           Data.Ratio
   import Data.Maybe (fromMaybe)
   import Data.Time
   import Numeric (readHex)
   import System.IO.Unsafe (unsafePerformIO)

   type Position = (Int, Int)

   tokenize :: Text -> Either String [Token]
   tokenize t = parseOnly (fmap fst tokenizer <* spaces (0,1) <* endOfInput) t

   tokenizer :: Parser ([Token], Position)
   tokenizer = manyWithPos (spaces >=> token) (1,1)

   -- | Parse a token.
   token :: Position -> Parser (Token, Position)
   token pos = dMultilineString pos
           <|> sMultilineString pos
           <|> dQuotedString pos
           <|> sQuotedString pos
           <|> date          pos
           <|> float         pos
           <|> integer       pos
           <|> period        pos
           <|> comma         pos
           <|> equals        pos
           <|> lbrace        pos
           <|> rbrace        pos
           <|> lbracket      pos
           <|> rbracket      pos
           <|> true          pos
           <|> false         pos
           <|> identifier    pos

   -- | A sequence of digits, interpreted as a decimal fraction  "123" => 0.123
   pfdecimal :: Parser Rational
   pfdecimal = do int <- many1 (digit `sepBy1` (optional (char '_')))
                  return ((foldl step 0 (reverse (concat int)))/10)
      where
         step :: Rational -> Char -> Rational
         step a c = (a / 10) + fromIntegral (ord c - 48)

   -- | A sequence of digits, interpreted to an integer token
   pdecimal :: Integral i => Parser i
   pdecimal = do int <- many1 (digit `sepBy1` (optional (char '_')))
                 return (foldl step 0 (concat int))
      where step a c = (a * 10) + fromIntegral (ord c - 48)

   -- | A sequence of digits, interpreted to an integer token
   psdecimal :: Integral i => Parser i
   psdecimal = do sign <- option '+' (char '+' <|> char '-')
                  v <- pdecimal
                  return $ case sign of
                              '-' -> negate v
                              '+' -> v

   pfloat :: Parser Rational
   pfloat = do v <- psdecimal
               (frac,exp) <- ((,) <$> (option 0 (char '.' *> pfdecimal)) <*> ((char 'e' <|> char 'E') *> psdecimal))
                         <|> ((,) <$> (char '.' *> pfdecimal) <*> (option 0 ((char 'e' <|> char 'E') *> psdecimal)))
               return $ if v < 0 then ((toRational v) - frac) * 10 ^^ exp
                                 else ((toRational v) + frac) * 10 ^^ exp

   -- | Parse a TOML integer
   integer :: Position -> Parser (Token, Position)
   integer pos = do (raw, v) <- match psdecimal
                    return (IntegerT pos v raw, second (+ (T.length raw)) pos)

   -- | Parse a TOML float
   float :: Position -> Parser (Token, Position)
   float pos = do (raw, v) <- match pfloat
                  return (FloatT pos (fromRational v) raw, second (+ (T.length raw)) pos)



   decimal1 :: Integral i => Parser i
   decimal1 = do dig <- satisfy isDigit
                 return (fromIntegral ((ord dig) - 48))

   decimal4 = do d1 <- decimal1
                 d2 <- decimal1
                 d3 <- decimal1
                 d4 <- decimal1
                 return (1000 * d1 + 100*d2 + 10*d3 + d4)

   decimal2 = do d1 <- decimal1
                 d2 <- decimal1
                 return (10 * d1 + d2)

   -- Quick and Dirty RFC 3339 parser, with extra rules for toml extensions
   pdateFullyear   = decimal4
   pdateMonth      = decimal2
   pdateMday       = decimal2
   ptimeHour       = decimal2
   ptimeMinute     = decimal2
   ptimeSecond     = decimal2
   ptimeSecfrac    = option 0 (char '.' *> pfdecimal)
   ptimeNumoffset  =  (,,) <$> (char '+' <|> char '-') <*> ptimeHour <*> (char ':' *> ptimeMinute)
   ptimeOffset     = (Nothing <$ (char 'Z' <|> char 'z')) <|> (Just <$> ptimeNumoffset)
   ppartialTime    = (,,,) <$> (ptimeHour <* char ':') <*> (ptimeMinute <* char ':') <*> ptimeSecond <*> optional ptimeSecfrac
   pfullDate       =  (,,) <$> (pdateFullyear <* char '-') <*> (pdateMonth <* char '-') <*> pdateMday
   pfullTime       =   (,) <$> ppartialTime <*> ptimeOffset
   pdateTime       =   (,) <$> pfullDate <*> ((char 't' <|> char 'T') *> pfullTime)
   pdatePartialTime=   (,) <$> pfullDate <*> ((char 't' <|> char 'T') *> ppartialTime)



   convertDT :: (ZonedTime -> Token) -> ((Integer, Int, Int),
                 ((Int, Int, Int, Maybe Rational), Maybe (Char, Int, Int))) -> Maybe Token
   convertDT f ((y,m,d), ((h, mi, sec, r), os)) =
      do fd <- fromGregorianValid y m d
         tod <- makeTimeOfDayValid h mi (fromRational ((toRational sec) + (fromMaybe 0 r)))
         case os of
            Just (sign, tzh, tzm) -> do
               let ustz = tzh * 60 + tzm
               let tz = if sign == '-' then (-1) * ustz else ustz
               return (f (ZonedTime LocalTime{localDay=fd, localTimeOfDay=tod} (minutesToTimeZone tz)))

            Nothing  -> do
               return (f (ZonedTime LocalTime{localDay=fd, localTimeOfDay=tod} utc))

   convertPDT :: (LocalTime -> Token) -> ((Integer, Int, Int), (Int, Int, Int, Maybe Rational)) -> Maybe Token
   convertPDT f ((y,m,d), (h,mi,sec,r)) =
      do fd <- fromGregorianValid y m d
         tod <- makeTimeOfDayValid h mi (fromRational ((toRational sec) + (fromMaybe 0 r)))
         return (f LocalTime { localDay = fd, localTimeOfDay =  tod})

   convertFD :: (LocalTime -> Token) -> (Integer, Int, Int) -> Maybe Token
   convertFD f (y,m,d) =
      do fd <- fromGregorianValid y m d
         return (f LocalTime { localDay = fd, localTimeOfDay = timeToTimeOfDay (secondsToDiffTime 0) })

   -- | Parse a TOML date
   date :: Position -> Parser (Token, Position)
   date pos = do (raw, v) <- match ((convertDT       (DateT pos) <$> pdateTime)
                               <|>  (convertPDT (LocalDateT pos) <$> pdatePartialTime)
                               <|>  (convertFD  (LocalDateT pos) <$> pfullDate))
                 case v of
                     Just jv -> return (jv, second (+ (T.length raw)) pos)
                     Nothing -> fail "invalid date"

   dMultilineToken = string "\"\"\""
   sMultilineToken = string "\'\'\'"
   multilineWhitespace = char '\\' >> char '\n' >> takeWhile (\x -> x == ' ' || x == '\n' || x == '\t')

   -- | Parse a multiline single quoted string, @'''fizbuz'''@.
   sMultilineString :: Position -> Parser (Token, Position)
   sMultilineString pos = do (raw, v) <- match mlparse
                             return (QuotedStringT pos v, second (+ (T.length raw)) pos)
     where mlparse = do void $ sMultilineToken
                        skipWhile (\c -> c == '\n')
                        skipMany multilineWhitespace
                        body <- manyTill chunk sMultilineToken
                        return . T.pack $ body
           chunk = (skipMany multilineWhitespace) *> anyChar



   -- | Parse a multiline double quoted string, @\"\"\"fizbuz\"\"\"@.
   dMultilineString :: Position -> Parser (Token, Position)
   dMultilineString pos = do (raw, v) <- match mlparse
                             return (QuotedStringT pos v, second (+ (T.length raw)) pos)
     where mlparse = do void $ dMultilineToken
                        skipWhile (\c -> c == '\n')
                        skipMany multilineWhitespace
                        body <- manyTill chunk dMultilineToken
                        return . T.pack $ body
           chunk = anyChar <* (skipMany multilineWhitespace)

   -- | Parse a quoted string, @\"fizbuz\"@.
   dQuotedString :: Position -> Parser (Token, Position)
   dQuotedString pos = stringWithPosition pos . T.pack
       <$> between (char dQuote) (char dQuote) (many strChar)
     where
       strChar = try escSeq <|> try (satisfy (\c -> c /= dQuote && c /= '\\'))
       dQuote  = '\"'

   -- | Parse a singly quoted string, @\'fizbuz\'@.
   sQuotedString :: Position -> Parser (Token, Position)
   sQuotedString pos = stringWithPosition pos . T.pack
       <$> between (char sQuote) (char sQuote) (many (satisfy (/= sQuote)))
     where
       sQuote = '\''

   escSeq = char '\\' *> escSeqChar
     where
       escSeqChar =
               try (char '"')  *> return '"'
           <|> try (char '\\') *> return '\\'
           <|> try (char '/')  *> return '/'
           <|> try (char 'b')  *> return '\b'
           <|> try (char 't')  *> return '\t'
           <|> try (char 'n')  *> return '\n'
           <|> try (char 'f')  *> return '\f'
           <|> try (char 'r')  *> return '\r'
           <|> try (char 'u')  *> unicodeHex 4
           <|> try (char 'U')  *> unicodeHex 8
           <?> "escape character"

   unicodeHex :: Int -> Parser Char
   unicodeHex n = do
       h <- count n (satisfy isHex)
       let v = fst . head . readHex $ h
       return $ if v <= maxChar then toEnum v else '_'
    where
      isHex c = (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
      maxChar = fromEnum (maxBound :: Char)

   stringWithPosition pos s = (QuotedStringT pos s, second (+ (fromIntegral $ T.length s + 2)) pos)

   between begin end parser = begin *> parser <* end

   -- | Helper that takes a simple constructor and a position, and constructs the appropriate result
   pret :: (Position -> Token) -> Position -> Parser (Token, Position)
   pret x pos = pure (x pos, second (+1) pos)

   -- | Parse a period \".\".
   period :: Position -> Parser (Token, Position)
   period pos = char '.' *> pret PeriodT pos

   -- | Parse 'true'
   true :: Position -> Parser (Token, Position)
   true pos = string "true" *> pret KeywordTrueT pos

   -- | Parse 'false'
   false :: Position -> Parser (Token, Position)
   false pos = string "false" *> pret KeywordFalseT pos

   -- | Parse a equals \"=\".
   equals :: Position -> Parser (Token, Position)
   equals pos = char '=' *> pret EqualT pos

   -- | Parse a comma \",\".
   comma :: Position -> Parser (Token, Position)
   comma pos = char ',' *> pret CommaT pos

   -- | Parse a left bracket \"[\".
   lbracket :: Position -> Parser (Token, Position)
   lbracket pos = char '[' *> pret LeftBracketT pos

   -- | Parse a right bracket \"]\".
   rbracket :: Position -> Parser (Token, Position)
   rbracket pos = char ']' *> pret RightBracketT pos

   -- | Parse a left brace \"{\".
   lbrace :: Position -> Parser (Token, Position)
   lbrace pos = char '{' *> pret LeftBraceT pos

   -- | Parse a right brace \"}\".
   rbrace :: Position -> Parser (Token, Position)
   rbrace pos = char '}' *> pret RightBraceT pos

   -- | An identifier in toml is a bare string of alphanumeric, plus _ and -
   identifier :: Position -> Parser (Token, Position)
   identifier pos = cons <$> takeWhile1 wordChar
     where
      cons w = (IdentifierT pos w, second (+ (fromIntegral $ T.length w)) pos)
      wordChar = inClass "-a-zA-Z0-9_"


   -- | Like 'many', but retains the current source position
   manyWithPos :: (Position -> Parser ( a,  Position))
               ->  Position -> Parser ([a], Position)
   manyWithPos p pos = do r <- fmap (first Just) (p pos) <|> pure (Nothing, pos)
                          case r of
                           (Nothing, _) ->
                              return ([], pos)
                           (Just x, newpos@(!_, !_)) ->
                                    do (xs, finalpos) <- manyWithPos p newpos
                                       return (x:xs, finalpos)



   -- | Skip spaces (space, newline, tab (=4 spaces)) and keep
   -- positioning information up to date.
   -- TODO: Support windows style line endings
   spaces :: Position -> Parser Position
   spaces (sline, scol) = go sline scol
    where
      go line col =
         do c <- peekChar
            case c of
               Just '\n' -> anyChar *> go (line+1) 1
               Just ' '  -> anyChar *> go line (col+1)
               Just '#'  -> anyChar *> takeWhile (/='\n') *> go line (col+1)
               Just '\t' -> anyChar *> go line (col+4)
               _ -> return (line, col)
