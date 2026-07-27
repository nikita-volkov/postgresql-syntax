module PostgresqlSyntax.Ast.InsertRest where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.InsertColumnList
import PostgresqlSyntax.Ast.OverrideKind
import PostgresqlSyntax.Ast.SelectStmt
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

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
      optLexemes
        [ fmap (renderInParens . toTextBuilder) a,
          fmap insertRestOverriding b,
          Just (toTextBuilder c)
        ]
    DefaultValuesInsertRest -> "DEFAULT VALUES"
    where
      insertRestOverriding a = "OVERRIDING " <> toTextBuilder a <> " VALUE"
  parser =
    asum
      [ DefaultValuesInsertRest <$ (keyword "default" *> space1 *> endHead *> keyword "values"),
        do
          a <- optional (inParens parser <* space1)
          b <- optional $ do
            keyword "overriding"
            space1
            endHead
            b <- parser
            space1
            keyword "value"
            space1
            return b
          c <- parser
          return (SelectInsertRest a b c)
      ]

instance Arbitrary InsertRest where
  arbitrary =
    oneof
      [ SelectInsertRest <$> scale (`div` 2) arbitrary <*> arbitrary <*> scale (`div` 2) arbitrary,
        pure DefaultValuesInsertRest
      ]
