module PostgresqlSyntax.Ast.GroupClause where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.GroupByItem
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- group_clause:
--   |  GROUP_P BY set_quantifier group_by_list
--   |  /*EMPTY*/
-- @
--
-- @set_quantifier@ (@DISTINCT@\/@ALL@) is not modeled here — this
-- codebase's grammar subset doesn't support @GROUP BY DISTINCT@.
newtype GroupClause = GroupClause (NonEmpty GroupByItem)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst GroupClause where
  toTextBuilder settings (GroupClause a) = "GROUP BY " <> TextBuilders.commaNonEmpty (toTextBuilder settings) a
  parser settings = do
    Parsers.keyphrase "group by"
    Parser.endHead
    Parsers.space1
    GroupClause <$> Parsers.sep1 Parsers.commaSeparator (parser settings)

instance Qc.Arbitrary GroupClause where
  shrink = Qc.genericShrink
  arbitrary = GroupClause <$> Gens.nonEmptyUpTo 6 Qc.arbitrary
