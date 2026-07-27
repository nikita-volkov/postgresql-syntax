module PostgresqlSyntax.Ast.WithClause where

import Control.Applicative.Combinators (option)
import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.CommonTableExpr
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
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
  arbitrary = WithClause <$> arbitrary <*> do
    len <- Qc.choose (0, 6)
    x <- Qc.scale (`div` 2) Qc.arbitrary
    xs <- Qc.vectorOf len (Qc.scale (`div` 2) Qc.arbitrary)
    pure (x :| xs)
