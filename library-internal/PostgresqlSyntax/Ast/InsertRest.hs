module PostgresqlSyntax.Ast.InsertRest where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.InsertColumnList
import PostgresqlSyntax.Ast.OverrideKind
import PostgresqlSyntax.Ast.SelectStmt
import qualified PostgresqlSyntax.Helpers.Gens as Gens
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
          a <- optional (Parsers.inParens (parser settings) <* Parsers.space1)
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
