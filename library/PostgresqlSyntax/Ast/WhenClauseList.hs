module PostgresqlSyntax.Ast.WhenClauseList where

import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.WhenClause
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- when_clause_list:
--   | when_clause
--   | when_clause_list when_clause
-- @
newtype WhenClauseList = WhenClauseList (NonEmpty WhenClause)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst WhenClauseList where
  toTextBuilder (WhenClauseList a) = spaceNonEmpty toTextBuilder a
  parser = WhenClauseList <$> sep1 space1 parser

instance Arbitrary WhenClauseList where
  arbitrary = do
    len <- choose (0, 6)
    x <- scale (`div` 2) arbitrary
    xs <- vectorOf len (scale (`div` 2) arbitrary)
    pure (WhenClauseList (x :| xs))
