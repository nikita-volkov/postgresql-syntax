module PostgresqlSyntax.Ast.WithClause where

import Control.Applicative.Combinators (option)
import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.CommonTableExpr
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
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
    "WITH " <> bool "" "RECURSIVE " a <> commaNonEmpty toTextBuilder b
  parser = Parser.label "with clause" $ do
    keyword "with"
    Parser.space1
    Parser.endHead
    recursive <- option False (True <$ keyword "recursive" <* Parser.space1)
    cteList <- Parser.sep1 commaSeparator parser
    return (WithClause recursive cteList)

instance Qc.Arbitrary WithClause where
  shrink = Qc.genericShrink
  arbitrary = WithClause <$> arbitrary <*> Qc.nonEmptyUpTo 6 Qc.arbitrary
