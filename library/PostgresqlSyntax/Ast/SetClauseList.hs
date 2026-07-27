module PostgresqlSyntax.Ast.SetClauseList where

import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.SetClause
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
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
  arbitrary = do
    len <- Qc.choose (0, 9)
    x <- Qc.scale (`div` 2) Qc.arbitrary
    xs <- Qc.vectorOf len (Qc.scale (`div` 2) Qc.arbitrary)
    pure (SetClauseList (x :| xs))
