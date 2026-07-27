module PostgresqlSyntax.Ast.SortClause where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.SortBy
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, sortBy, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- sort_clause:
--   |  ORDER BY sortby_list
--
-- sortby_list:
--   |  sortby
--   |  sortby_list ',' sortby
-- @
newtype SortClause = SortClause (NonEmpty SortBy)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst SortClause where
  toTextBuilder (SortClause a) = "ORDER BY " <> commaNonEmpty toTextBuilder a
  parser = do
    keyphrase "order by"
    endHead
    space1
    SortClause <$> sep1 commaSeparator parser

instance Arbitrary SortClause where
  arbitrary = do
    len <- choose (0, 7)
    x <- scale (`div` 2) arbitrary
    xs <- vectorOf len (scale (`div` 2) arbitrary)
    pure (SortClause (x :| xs))
