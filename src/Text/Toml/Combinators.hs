{-# LANGUAGE FlexibleContexts #-}

-- | Parser combinators.
module Text.Toml.Combinators
    ( Parser
    , identifier
    , quoted
    , bool
    , lbracket
    , rbracket
    , bracketed
    , doubleBracketed
    , lbrace
    , rbrace
    , comma
    , equal
    , period
    , integer
    , float
    , date
    , localDate
    , satisfy
    , module Text.Parsec.Combinator
    ) where

   import           Text.Toml.Types.Tokens
   import           Data.Text (Text)
   import qualified Data.Text as T
   import           Data.Time.LocalTime
   import           Text.Parsec.Combinator
   import           Text.Parsec.Pos
   import           Text.Parsec.Prim


   type Parser = Parsec [Token] ()

   -- | Match a word with the given string.
   identifier :: Parser Text
   identifier = satisfy it
      where it (IdentifierT _ t) = Just t
            it _ = Nothing

   -- | Quoted string.
   quoted :: Parser Text
   quoted = satisfy it
      where it (QuotedStringT _ t) = Just t
            it _ = Nothing

   -- | A comma.
   comma :: Parser ()
   comma = satisfy it
      where it (CommaT _) = Just ()
            it _ = Nothing

   -- | A period.
   period :: Parser ()
   period = satisfy it
      where it (PeriodT _) = Just ()
            it _ = Nothing

   -- | right bracket.
   rbracket :: Parser ()
   rbracket = satisfy it
      where it (RightBracketT _) = Just ()
            it _ = Nothing

   -- | left bracket..
   lbracket :: Parser ()
   lbracket = satisfy it
      where it (LeftBracketT _) = Just ()
            it _ = Nothing

   -- | right brace.
   rbrace :: Parser ()
   rbrace = satisfy it
      where it (RightBraceT _) = Just ()
            it _ = Nothing

   -- | left brace.
   lbrace :: Parser ()
   lbrace = satisfy it
      where it (LeftBraceT _) = Just ()
            it _ = Nothing

   bool :: Parser Bool
   bool = satisfy it
      where it (KeywordTrueT _) = Just True
            it (KeywordFalseT _) = Just False
            it _ = Nothing

   equal :: Parser ()
   equal = satisfy it
      where it (EqualT _) = Just ()
            it _ = Nothing

   integer :: Parser Integer
   integer = satisfy it
       where it (IntegerT _ i _) = Just i
             it _ = Nothing

   float :: Parser Double
   float = satisfy it
       where it (FloatT _ f _) = Just f
             it _ = Nothing

   date :: Parser ZonedTime
   date = satisfy it
       where it (DateT _ z) = Just z
             it _ = Nothing

   localDate :: Parser LocalTime
   localDate = satisfy it
       where it (LocalDateT _ l) = Just l
             it _ = Nothing

   doubleBracketed :: Parser a -> Parser a
   doubleBracketed = between (lbracket >> lbracket) (rbracket >> rbracket)

   bracketed :: Parser a -> Parser a
   bracketed = between lbracket rbracket

   -- | Satisfy the given predicate from the token stream.
   satisfy :: (Token -> Maybe a) -> Parser a
   satisfy = tokenPrim tokenString tokenPosition

   -- | Make a string out of the token, for error message purposes.
   tokenString :: Token -> String
   tokenString t = case t of
                        IdentifierT _ w -> "identifier \"" ++ T.unpack w ++ "\""
                        QuotedStringT _ s -> "quotation \"" ++ T.unpack s ++ "\""
                        KeywordTrueT{} -> "true"
                        KeywordFalseT{} -> "false"
                        LeftBracketT{} -> "left bracket"
                        RightBracketT{} -> "end of section"
                        LeftBraceT{} -> "left brace"
                        RightBraceT{} -> "right brace"
                        CommaT{} -> "comma"
                        EqualT{} -> "equals"
                        PeriodT{} -> "period"
                        IntegerT _ i _ -> "integer \"" ++ show i ++ "\""
                        FloatT _ d _ -> "float \"" ++ show d ++ "\""
                        DateT _ d -> "date \"" ++ show d ++ "\""
                        LocalDateT _ d -> "date \"" ++ show d ++ "\""

   -- | Update the source position to that of the next token in the stream
   tokenPosition :: SourcePos -> Token -> [Token] -> SourcePos
   tokenPosition pos _ [] = pos
   tokenPosition pos _ (t:_) = setSourceColumn (setSourceLine pos line) col
      where (line,col) = tokenPos t
