module PostgresqlSyntax.Ast.SortClause where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.SortBy
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
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
  toTextBuilder settings (SortClause a) = "ORDER BY " <> TextBuilders.commaNonEmpty (toTextBuilder settings) a
  parser settings = do
    Parsers.keyphrase "order by"
    Parser.endHead
    Parsers.space1
    SortClause <$> Parsers.sep1 Parsers.commaSeparator (parser settings)

instance Qc.Arbitrary SortClause where
  shrink = Qc.genericShrink
  arbitrary = SortClause <$> Gens.nonEmptyUpTo 7 Qc.arbitrary
