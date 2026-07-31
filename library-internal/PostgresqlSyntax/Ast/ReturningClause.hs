module PostgresqlSyntax.Ast.ReturningClause where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.TargetList
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- returning_clause:
--   |  RETURNING returning_with_clause target_list
--   |  /*EMPTY*/
-- @
--
-- @returning_with_clause@ (the @WITH (...)@ modifier) is not modeled
-- here — not supported by this codebase's grammar subset.
newtype ReturningClause = ReturningClause TargetList
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst ReturningClause where
  toTextBuilder settings (ReturningClause a) = "RETURNING " <> toTextBuilder settings a
  parser settings = do
    Parsers.keyword "returning"
    Parsers.space1
    Parser.endHead
    ReturningClause <$> parser settings

instance Qc.Arbitrary ReturningClause where
  shrink = Qc.genericShrink
  arbitrary = ReturningClause <$> Qc.arbitrary
