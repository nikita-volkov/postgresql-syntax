module PostgresqlSyntax.Ast.CommonTableExpr where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Ident
import PostgresqlSyntax.Ast.PreparableStmt
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
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
    TextBuilders.optLexemes
      [ Just (toTextBuilder a),
        fmap (TextBuilders.renderInParens . TextBuilders.commaNonEmpty toTextBuilder) b,
        Just "AS",
        fmap materialization c,
        Just (TextBuilders.renderInParens (toTextBuilder d))
      ]
    where
      materialization = bool "NOT MATERIALIZED" "MATERIALIZED"
  parser = Parser.label "common table expression" $ do
    name <- colId <* Parsers.space <* Parser.endHead
    nameList <- optional (Parsers.inParens (Parsers.sep1 Parsers.commaSeparator colId) <* Parsers.space1)
    Parsers.keyword "as"
    Parsers.space1
    materialized <- optional (materialized <* Parsers.space1)
    stmt <- Parsers.inParens parser
    return (CommonTableExpr name nameList materialized stmt)
    where
      materialized =
        True
          <$ Parsers.keyword "materialized"
            <|> False
          <$ Parsers.keyphrase "not materialized"

instance Qc.Arbitrary CommonTableExpr where
  shrink = Qc.genericShrink
  arbitrary =
    CommonTableExpr
      <$> Qc.arbitrary
      <*> Gens.terminatingMaybe (Gens.nonEmptyUpTo 6 Qc.arbitrary)
      <*> Gens.terminatingMaybe Qc.arbitrary
      <*> Qc.arbitrary
