module PostgresqlSyntax.Ast.FromClause where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.FromList
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- from_clause:
--   |  FROM from_list
--   |  /*EMPTY*/
-- @
newtype FromClause = FromClause FromList
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst FromClause where
  toTextBuilder settings (FromClause a) = "FROM " <> toTextBuilder settings a
  parser settings = do
    Parsers.keyword "from"
    Parser.endHead
    Parsers.space1
    FromClause <$> parser settings

instance Qc.Arbitrary FromClause where
  shrink = Qc.genericShrink
  arbitrary = FromClause <$> Qc.arbitrary
