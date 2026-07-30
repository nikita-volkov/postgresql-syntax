module PostgresqlSyntax.Ast.UsingClause where

import PostgresqlSyntax.Ast.FromList
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- using_clause:
--   |  USING from_list
--   |  /*EMPTY*/
-- @
newtype UsingClause = UsingClause FromList
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst UsingClause where
  toTextBuilder settings (UsingClause a) = "USING " <> toTextBuilder settings a
  parser settings = do
    Parsers.keyword "using"
    Parsers.space1
    UsingClause <$> parser settings

instance Qc.Arbitrary UsingClause where
  shrink = Qc.genericShrink
  arbitrary = UsingClause <$> Qc.arbitrary
