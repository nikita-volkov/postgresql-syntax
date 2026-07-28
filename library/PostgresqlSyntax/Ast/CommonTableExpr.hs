module PostgresqlSyntax.Ast.CommonTableExpr where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.PreparableStmt
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.Helpers.Parsers
import PostgresqlSyntax.Helpers.TextBuilders
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
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
        True
          <$ keyword "materialized"
            <|> False
          <$ keyphrase "not materialized"

instance Qc.Arbitrary CommonTableExpr where
  shrink = Qc.genericShrink
  arbitrary =
    CommonTableExpr
      <$> Qc.arbitrary
      <*> Qc.terminatingMaybe Qc.arbitrary
      <*> Qc.terminatingMaybe Qc.arbitrary
      <*> Qc.arbitrary
