module PostgresqlSyntax.Ast.AscDesc where

import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude

-- |
-- ==== References
-- @
-- opt_asc_desc:
--   | ASC
--   | DESC
--   | EMPTY
-- @
data AscDesc = AscAscDesc | DescAscDesc
  deriving (Show, Generic, Eq, Ord, Data, Enum, Bounded)

instance IsAst AscDesc where
  toTextBuilder = \case
    AscAscDesc -> "ASC"
    DescAscDesc -> "DESC"
  parser = keyword "asc" $> AscAscDesc <|> keyword "desc" $> DescAscDesc

instance Arbitrary AscDesc where
  arbitrary = elements [minBound .. maxBound]
