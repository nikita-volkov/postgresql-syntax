module PostgresqlSyntax.Ast.CommonTableExpr where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.PreparableStmt
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- common_table_expr:
--   |  name opt_name_list AS opt_materialized '(' PreparableStmt ')'
-- opt_materialized:
--   | MATERIALIZED
--   | NOT MATERIALIZED
--   | EMPTY
-- @
data CommonTableExpr = CommonTableExpr Ident (Maybe (NonEmpty Ident)) (Maybe Bool) PreparableStmt
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst CommonTableExpr where
  toTextBuilder (CommonTableExpr a b c d) =
    optLexemes
      [ Just (toTextBuilder a),
        fmap (renderInParens . commaNonEmpty toTextBuilder) b,
        Just "AS",
        fmap materialization c,
        Just (renderInParens (toTextBuilder d))
      ]
    where
      materialization = bool "NOT MATERIALIZED" "MATERIALIZED"
  parser = label "common table expression" $ do
    name <- colId <* space <* endHead
    nameList <- optional (inParens (sep1 commaSeparator colId) <* space1)
    keyword "as"
    space1
    materialized <- optional (materialized <* space1)
    stmt <- inParens parser
    return (CommonTableExpr name nameList materialized stmt)
    where
      materialized =
        True <$ keyword "materialized"
          <|> False <$ keyphrase "not materialized"

instance Arbitrary CommonTableExpr where
  arbitrary =
    CommonTableExpr
      <$> arbitrary
      <*> scale (`div` 2) arbitrary
      <*> arbitrary
      <*> scale (`div` 2) arbitrary
