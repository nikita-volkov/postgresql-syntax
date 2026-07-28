module PostgresqlSyntax.Ast.Ident where

import qualified Data.Text as Text
import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.TextBuilder as TextBuilder
import PostgresqlSyntax.IsAst
import qualified PostgresqlSyntax.KeywordSet as KeywordSet
import qualified PostgresqlSyntax.Predicate as Predicate
import PostgresqlSyntax.Prelude hiding (filter, try)
import qualified Test.QuickCheck as Qc
import qualified TextBuilder

-- |
-- ==== References
-- @
-- IDENT
-- @
data Ident = QuotedIdent Text | UnquotedIdent Text
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst Ident where
  toTextBuilder = \case
    QuotedIdent a -> TextBuilder.char7 '"' <> TextBuilder.text (Text.replace "\"" "\"\"" a) <> TextBuilder.char7 '"'
    UnquotedIdent a -> TextBuilder.text a
  parser = quotedName <|> keywordNameByPredicate UnquotedIdent (not . Predicate.keyword)
    where
      quotedName = Parser.filter (const "Empty name") (not . Text.null) (quotedString '"') & fmap QuotedIdent

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
  Parser.label "identifier" $
    parser
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
  Parser.label "column label" $
    keywordNameFromSet UnquotedIdent KeywordSet.keyword
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

instance Qc.Arbitrary Ident where
  arbitrary =
    Qc.frequency
      [ (95, UnquotedIdent <$> unquotedIdentText),
        (5, QuotedIdent <$> quotedIdentText)
      ]
    where
      unquotedIdentText =
        ( do
            firstChar <- Qc.elements startChars
            restLength <- Qc.choose (0, 29)
            rest <- Qc.vectorOf restLength (Qc.elements contChars)
            pure (Text.pack (firstChar : rest))
        )
          `Qc.suchThat` (not . Predicate.keyword)
      startChars = ['a' .. 'z'] <> ['_']
      contChars = startChars <> ['0' .. '9'] <> ['$']
      quotedIdentText = do
        len <- Qc.choose (1, 30)
        Text.pack <$> Qc.vectorOf len (Qc.arbitrary `Qc.suchThat` (not . isControl))
