module PostgresqlSyntax.Ast.WithClause where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.CommonTableExpr (CommonTableExpr)
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude 
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- with_clause:
--   |  WITH cte_list
--   |  WITH_LA cte_list
--   |  WITH RECURSIVE cte_list
-- @
data WithClause = WithClause Bool (NonEmpty CommonTableExpr)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst WithClause where
  toTextBuilder settings (WithClause a b) =
    "WITH " <> bool "" "RECURSIVE " a <> TextBuilders.commaNonEmpty (toTextBuilder settings) b
  parser settings = Parser.label "with clause" $ do
    Parsers.keyword "with"
    Parsers.space1
    Parser.endHead
    recursive <- option False (True <$ Parsers.keyword "recursive" <* Parsers.space1)
    cteList <- Parsers.sep1 Parsers.commaSeparator (parser settings)
    return (WithClause recursive cteList)

instance Qc.Arbitrary WithClause where
  shrink = Qc.genericShrink
  arbitrary = WithClause <$> arbitrary <*> Gens.nonEmptyUpTo 6 Qc.arbitrary
