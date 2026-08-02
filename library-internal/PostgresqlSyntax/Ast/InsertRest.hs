module PostgresqlSyntax.Ast.InsertRest where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.InsertColumnList
import PostgresqlSyntax.Ast.OverrideKind
import PostgresqlSyntax.Ast.SelectStmt
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- insert_rest:
--   | SelectStmt
--   | OVERRIDING override_kind VALUE_P SelectStmt
--   | '(' insert_column_list ')' SelectStmt
--   | '(' insert_column_list ')' OVERRIDING override_kind VALUE_P SelectStmt
--   | DEFAULT VALUES
-- @
data InsertRest
  = SelectInsertRest (Maybe InsertColumnList) (Maybe OverrideKind) SelectStmt
  | DefaultValuesInsertRest
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst InsertRest where
  toTextBuilder settings = \case
    SelectInsertRest a b c ->
      TextBuilders.optLexemes
        [ fmap (TextBuilders.renderInParens . toTextBuilder settings) a,
          fmap insertRestOverriding b,
          Just (toTextBuilder settings c)
        ]
    DefaultValuesInsertRest -> "DEFAULT VALUES"
    where
      insertRestOverriding a = "OVERRIDING " <> toTextBuilder settings a <> " VALUE"
  parser settings =
    asum
      [ DefaultValuesInsertRest <$ (Parsers.keyword "default" *> Parsers.space1 *> Parser.endHead *> Parsers.keyword "values"),
        do
          -- 'Parser.wrapToHead' makes the whole parenthesized column list
          -- (including any 'Parser.endHead' calls inside 'InsertColumnItem')
          -- backtrackable as one unit. Without it, parsing e.g. @(VALUES ...)@
          -- would commit to treating @VALUES@ as a single-column
          -- 'InsertColumnList' entry (since it's a valid 'ColId') right after
          -- reading it, and fail instead of backtracking into the correct
          -- 'SelectStmt' (@select_with_parens@ wrapping a bare @VALUES@
          -- clause) parse below. See gram.y's @insert_rest@.
          a <- optional (Parser.wrapToHead (Parsers.inParens (parser settings) <* Parsers.space1))
          b <- optional $ do
            Parsers.keyword "overriding"
            Parsers.space1
            Parser.endHead
            b <- parser settings
            Parsers.space1
            Parsers.keyword "value"
            Parsers.space1
            return b
          c <- parser settings
          return (SelectInsertRest a b c)
      ]

instance Qc.Arbitrary InsertRest where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ SelectInsertRest <$> Gens.terminatingMaybe Qc.arbitrary <*> Qc.arbitrary <*> Qc.arbitrary,
        pure DefaultValuesInsertRest
      ]
