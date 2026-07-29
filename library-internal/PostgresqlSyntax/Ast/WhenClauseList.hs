module PostgresqlSyntax.Ast.WhenClauseList where

import PostgresqlSyntax.Ast.WhenClause
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

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
  toTextBuilder (WhenClauseList a) = TextBuilders.spaceNonEmpty toTextBuilder a
  parser = WhenClauseList <$> Parsers.sep1 Parsers.space1 parser

instance Qc.Arbitrary WhenClauseList where
  shrink = Qc.genericShrink
  arbitrary = WhenClauseList <$> Gens.nonEmptyUpTo 6 Qc.arbitrary
