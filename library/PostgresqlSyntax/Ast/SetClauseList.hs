module PostgresqlSyntax.Ast.SetClauseList where

import PostgresqlSyntax.Ast.SetClause
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import PostgresqlSyntax.Helpers.Parsers
import PostgresqlSyntax.Helpers.TextBuilders
import PostgresqlSyntax.IsAst
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
  toTextBuilder (SetClauseList a) = commaNonEmpty toTextBuilder a
  parser = SetClauseList <$> Parser.sep1 commaSeparator parser

instance Qc.Arbitrary SetClauseList where
  shrink = Qc.genericShrink
  arbitrary = SetClauseList <$> Qc.nonEmptyUpTo 9 Qc.arbitrary
