module PostgresqlSyntax.Ast.Ident where

import qualified Data.Text as Text
import HeadedMegaparsec hiding (string)
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Extras.TextBuilder (char7)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, try)
import qualified PostgresqlSyntax.Predicate as Predicate
import Test.QuickCheck (frequency, suchThat)
import TextBuilder (text)

-- |
-- ==== References
-- @
-- IDENT
-- @
data Ident = QuotedIdent Text | UnquotedIdent Text
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Ident where
  toTextBuilder = \case
    QuotedIdent a -> char7 '"' <> text (Text.replace "\"" "\"\"" a) <> char7 '"'
    UnquotedIdent a -> text a
  parser = quotedName <|> keywordNameByPredicate UnquotedIdent (not . Predicate.keyword)
    where
      quotedName = filter (const "Empty name") (not . Text.null) (quotedString '"') & fmap QuotedIdent

instance Arbitrary Ident where
  arbitrary =
    frequency
      [ (95, UnquotedIdent <$> unquotedIdentText),
        (5, QuotedIdent <$> quotedIdentText)
      ]
    where
      unquotedIdentText =
        ( do
            firstChar <- elements startChars
            restLength <- choose (0, 29)
            rest <- vectorOf restLength (elements contChars)
            pure (Text.pack (firstChar : rest))
        )
          `suchThat` (not . Predicate.keyword)
      startChars = ['a' .. 'z'] <> ['_']
      contChars = startChars <> ['0' .. '9'] <> ['$']
      quotedIdentText = do
        len <- choose (1, 30)
        Text.pack <$> vectorOf len (arbitrary `suchThat` (not . isControl))
