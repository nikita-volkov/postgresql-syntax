module PostgresqlSyntax.Ast.InsertRest where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.InsertColumnList
import PostgresqlSyntax.Ast.OverrideKind
import PostgresqlSyntax.Ast.SelectStmt
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
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
  toTextBuilder = \case
    SelectInsertRest a b c ->
      TextBuilders.optLexemes
        [ fmap (TextBuilders.renderInParens . toTextBuilder) a,
          fmap insertRestOverriding b,
          Just (toTextBuilder c)
        ]
    DefaultValuesInsertRest -> "DEFAULT VALUES"
    where
      insertRestOverriding a = "OVERRIDING " <> toTextBuilder a <> " VALUE"
  parser =
    asum
      [ DefaultValuesInsertRest <$ (Parsers.keyword "default" *> Parsers.space1 *> Parser.endHead *> Parsers.keyword "values"),
        do
          a <- optional (Parsers.inParens parser <* Parsers.space1)
          b <- optional $ do
            Parsers.keyword "overriding"
            Parsers.space1
            Parser.endHead
            b <- parser
            Parsers.space1
            Parsers.keyword "value"
            Parsers.space1
            return b
          c <- parser
          return (SelectInsertRest a b c)
      ]

instance Qc.Arbitrary InsertRest where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ SelectInsertRest <$> Qc.terminatingMaybe Qc.arbitrary <*> Qc.arbitrary <*> Qc.scale (`div` 2) Qc.arbitrary,
        pure DefaultValuesInsertRest
      ]
