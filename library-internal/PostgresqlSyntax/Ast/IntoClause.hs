module PostgresqlSyntax.Ast.IntoClause where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.OptTempTableName
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- into_clause:
--   |  INTO OptTempTableName
--   |  /*EMPTY*/
-- @
newtype IntoClause = IntoClause OptTempTableName
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst IntoClause where
  toTextBuilder settings (IntoClause a) = "INTO " <> toTextBuilder settings a
  parser settings = do
    Parsers.keyword "into"
    Parser.endHead
    Parsers.space1
    IntoClause <$> parser settings

instance Qc.Arbitrary IntoClause where
  shrink = Qc.genericShrink
  arbitrary = IntoClause <$> Qc.arbitrary
