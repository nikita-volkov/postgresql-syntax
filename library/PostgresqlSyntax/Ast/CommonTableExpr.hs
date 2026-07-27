module PostgresqlSyntax.Ast.CommonTableExpr where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.PreparableStmt
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

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
  parser = Parser.label "common table expression" $ do
    name <- colId <* Parser.space <* Parser.endHead
    nameList <- optional (inParens (Parser.sep1 commaSeparator colId) <* Parser.space1)
    keyword "as"
    Parser.space1
    materialized <- optional (materialized <* Parser.space1)
    stmt <- inParens parser
    return (CommonTableExpr name nameList materialized stmt)
    where
      materialized =
        True <$ keyword "materialized"
          <|> False <$ keyphrase "not materialized"

instance Qc.Arbitrary CommonTableExpr where
  arbitrary =
    CommonTableExpr
      <$> Qc.arbitrary
      <*> Qc.scale (`div` 2) Qc.arbitrary
      <*> Qc.arbitrary
      <*> Qc.scale (`div` 2) Qc.arbitrary
