{-# LANGUAGE FlexibleContexts #-}

-- | Parser combinators.
module Text.Toml.Combinators where

   import           Text.Toml.Types.Tokens
   import           Data.Text (Text)
   import qualified Data.Text as T
   import           Text.Parsec.Pos
   import           Text.Parsec.Prim

   -- | Match a word with the given string.
   string :: Stream s m Token => Text -> ParsecT s u m Text
   string s = satisfy it
      where it (IdentifierT _ t) | t == s = Just t
            it _ = Nothing

   -- | Match a word with the given string.
   identifier :: Stream s m Token => ParsecT s u m Text
   identifier = satisfy it
      where it (IdentifierT _ t) = Just t
            it _ = Nothing

   -- | Quoted string.
   quoted :: Stream s m Token => ParsecT s u m Text
   quoted = satisfy it
      where it (QuotedStringT _ t) = Just t
            it _ = Nothing

   -- | A comma.
   comma :: Stream s m Token => ParsecT s u m ()
   comma = satisfy it
      where it (CommaT _) = Just ()
            it _ = Nothing

   -- | A period.
   period :: Stream s m Token => ParsecT s u m ()
   period = satisfy it
      where it (PeriodT _) = Just ()
            it _ = Nothing

   -- | right bracket.
   rbracket :: Stream s m Token => ParsecT s u m ()
   rbracket = satisfy
      where it (RightBracketT _) = Just ()
            it _ = Nothing

   -- | left bracket..
   lbracket :: Stream s m Token => ParsecT s u m ()
   lbracket = satisfy it
      where it (LeftBracketT _) = Just ()
            it _ = Nothing

   -- | right brace.
   rbrace :: Stream s m Token => ParsecT s u m ()
   rbrace = satisfy it
      where it (RightBraceT _) = Just ()
            it _ = Nothing

   -- | left brace.
   lbrace :: Stream s m Token => ParsecT s u m ()
   lbrace = satisfy it
      where it (LeftBraceT _) = Just ()
            it _ = Nothing

   -- | angle bracket.
   rangle :: Stream s m Token => ParsecT s u m ()
   rangle = satisfy it
      where it (RightAngleT _) = Just ()
            it _ = Nothing

   -- | angle bracket.
   langle :: Stream s m Token => ParsecT s u m ()
   langle = satisfy it
      where it (LeftAngleT _) = Just ()
            it _ = Nothing


   -- | @between open close p@ parses @open@, followed by @p@ and @close@.
   -- Returns the value returned by @p@.
   --
   -- >  braces  = between (symbol "{") (symbol "}")
   between :: (Stream s m t)
           => ParsecT s u m open
           -> ParsecT s u m close
           -> ParsecT s u m a
           -> ParsecT s u m a
   between open close p =
      do _ <- open
         x <- p
         _ <- close
         return x


   -- | Try to match all the given strings, or none at all.
   strings :: Stream s m Token => [Text] -> ParsecT s u m ()
   strings ss = try (mapM_ string ss)

   -- | Satisfy the given predicate from the token stream.
   satisfy :: Stream s m Token => (Token -> Maybe a) -> ParsecT s u m a
   satisfy = tokenPrim tokenString tokenPosition

   -- | The parser @anyToken@ accepts any kind of token. It is for example
   -- used to implement 'eof'. Returns the accepted token.
   anyToken :: (Stream s m Token) => ParsecT s u m Token
   anyToken = satisfy Just

   -- | Make a string out of the token, for error message purposes.
   tokenString :: Token -> String
   tokenString t = case t of
                        IdentifierT _ w -> "identifier \"" ++ T.unpack w ++ "\""
                        QuotedStringT _ s -> "quotation \"" ++ T.unpack s ++ "\""
                        IntegerT _ i -> "integer \"" ++ show s ++ "\""
                        FloatT _ d -> "float \"" ++ show d ++ "\""
                        DateT _ d -> "date \"" ++ show d ++ "\""
                        PeriodT{} -> "period"
                        CommaT{} -> "comma"
                        KeywordTrueT{} -> "true"
                        KeywordFalseT{} -> "false"
                        EqualT{} -> "equals"
                        LeftBracketT{} -> "left bracket"
                        RightBracketT{} -> "right bracket"
                        LeftBraceT{} -> "left brace"
                        RightBraceT{} -> "right brace"
                        LeftAngleT{} -> "left angle bracket"
                        RightAngleT{} -> "right angle bracket"

   -- | Update the position by the token.
   tokenPosition :: SourcePos -> Token -> t -> SourcePos
   tokenPosition pos t _ = setSourceColumn (setSourceLine pos line) col
      where (line,col) = tokenPos t

   -- | @notFollowedBy p@ only succeeds when parser @p@ fails. This parser
   -- does not consume any input. This parser can be used to implement the
   -- \'longest match\' rule. For example, when recognizing keywords (for
   -- example @let@), we want to make sure that a keyword is not followed
   -- by a legal identifier character, in which case the keyword is
   -- actually an identifier (for example @lets@). We can program this
   -- behaviour as follows:
   --
   -- >  keywordLet  = try (do{ string "let"
   -- >                       ; notFollowedBy alphaNum
   -- >                       })
   notFollowedBy :: (Stream s m Token) => ParsecT s u m Token -> ParsecT s u m ()
   notFollowedBy p = try ((do c <- try p
                              unexpected (tokenString c)) <|>
                           return ())

   many1 :: (Stream s m Token) => ParsecT s u m a -> ParsecT s u m [a]
   many1 p = do
      x <- p
      xs <- many p
      return (x:xs)

   -- | This parser only succeeds at the end of the input. This is not a
   -- primitive parser but it is defined using 'notFollowedBy'.
   eof :: (Stream s m Token) => ParsecT s u m ()
   eof = notFollowedBy anyToken <?> "end of input"

