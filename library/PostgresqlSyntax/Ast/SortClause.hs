module PostgresqlSyntax.Ast.SortClause where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.SortBy
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.QuickCheck as Qc
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, sortBy, try)
import qualified Test.QuickCheck as Qc

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
    Parser.endHead
    Parser.space1
    SortClause <$> Parser.sep1 commaSeparator parser

instance Qc.Arbitrary SortClause where
  arbitrary = SortClause <$> Qc.nonEmptyUpTo 7 Qc.arbitrary
