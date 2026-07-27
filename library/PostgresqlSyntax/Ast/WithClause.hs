module PostgresqlSyntax.Ast.WithClause where

import Control.Applicative.Combinators (option)
import HeadedMegaparsec
import PostgresqlSyntax.Ast.CommonTableExpr
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

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
  parser = label "with clause" $ do
    keyword "with"
    space1
    endHead
    recursive <- option False (True <$ keyword "recursive" <* space1)
    cteList <- sep1 commaSeparator parser
    return (WithClause recursive cteList)

instance Arbitrary WithClause where
  arbitrary = WithClause <$> arbitrary <*> do
    len <- choose (0, 6)
    x <- scale (`div` 2) arbitrary
    xs <- vectorOf len (scale (`div` 2) arbitrary)
    pure (x :| xs)
