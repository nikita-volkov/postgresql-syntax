module PostgresqlSyntax.Ast.SetClauseList where

import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.SetClause
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- set_clause_list:
--   | set_clause
--   | set_clause_list ',' set_clause
-- @
newtype SetClauseList = SetClauseList (NonEmpty SetClause)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst SetClauseList where
  toTextBuilder (SetClauseList a) = commaNonEmpty toTextBuilder a
  parser = SetClauseList <$> sep1 commaSeparator parser

instance Arbitrary SetClauseList where
  arbitrary = do
    len <- choose (0, 9)
    x <- scale (`div` 2) arbitrary
    xs <- vectorOf len (scale (`div` 2) arbitrary)
    pure (SetClauseList (x :| xs))
