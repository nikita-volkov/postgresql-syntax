module PostgresqlSyntax.Ast.WithClause where

import Control.Applicative.Combinators (option)
import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.CommonTableExpr
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
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
  toTextBuilder (WithClause a b) =
    "WITH " <> bool "" "RECURSIVE " a <> TextBuilders.commaNonEmpty toTextBuilder b
  parser = Parser.label "with clause" $ do
    Parsers.keyword "with"
    Parsers.space1
    Parser.endHead
    recursive <- option False (True <$ Parsers.keyword "recursive" <* Parsers.space1)
    cteList <- Parsers.sep1 Parsers.commaSeparator parser
    return (WithClause recursive cteList)

instance Qc.Arbitrary WithClause where
  shrink = Qc.genericShrink
  arbitrary = WithClause <$> arbitrary <*> Gens.nonEmptyUpTo 6 Qc.arbitrary
