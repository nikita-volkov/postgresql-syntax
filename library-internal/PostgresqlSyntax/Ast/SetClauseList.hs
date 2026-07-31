module PostgresqlSyntax.Ast.SetClauseList where

import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.SetClause
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

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
  toTextBuilder settings (SetClauseList a) = TextBuilders.commaNonEmpty (toTextBuilder settings) a
  parser settings = SetClauseList <$> Parsers.sep1 Parsers.commaSeparator (parser settings)

instance Qc.Arbitrary SetClauseList where
  shrink = Qc.genericShrink
  arbitrary = SetClauseList <$> Gens.nonEmptyUpTo 9 Qc.arbitrary
