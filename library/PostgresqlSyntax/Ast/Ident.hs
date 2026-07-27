module PostgresqlSyntax.Ast.Ident where

import qualified Data.Text as Text
import HeadedMegaparsec
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Extras.TextBuilder (char7)
import PostgresqlSyntax.IsAst
import qualified PostgresqlSyntax.KeywordSet as KeywordSet
import qualified PostgresqlSyntax.Predicate as Predicate
import PostgresqlSyntax.Prelude hiding (filter, try)
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

-- |
-- ==== References
-- @
-- ColId:
--   |  IDENT
--   |  unreserved_keyword
--   |  col_name_keyword
-- @
--
-- Most grammar positions that hold an identifier (column\/table\/alias
-- names, ...) are actually @ColId@, not bare @IDENT@ — this is the
-- permissive variant that most 'Ident'-typed fields elsewhere in
-- "PostgresqlSyntax.Ast" should parse with, since 'Ident'\'s own generic
-- 'parser' only accepts the strict @IDENT@ token (no keyword fallback).
colId :: Parser Ident
colId =
  label "identifier"
    $ parser
    <|> keywordNameFromSet UnquotedIdent (KeywordSet.unreservedKeyword <> KeywordSet.colNameKeyword)

-- |
-- ==== References
-- @
-- ColLabel:
--   |  IDENT
--   |  unreserved_keyword
--   |  col_name_keyword
--   |  type_func_name_keyword
--   |  reserved_keyword
-- @
colLabel :: Parser Ident
colLabel =
  label "column label"
    $ keywordNameFromSet UnquotedIdent KeywordSet.keyword
    <|> parser

-- |
-- ==== References
-- @
-- type_function_name:
--   | IDENT
--   | unreserved_keyword
--   | type_func_name_keyword
-- @
typeFunctionName :: Parser Ident
typeFunctionName =
  keywordNameFromSet UnquotedIdent KeywordSet.typeFunctionName
    <|> parser

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
