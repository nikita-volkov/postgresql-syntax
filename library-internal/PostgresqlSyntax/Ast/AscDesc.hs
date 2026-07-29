module PostgresqlSyntax.Ast.AscDesc where

import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

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
  parser = Parsers.keyword "asc" $> AscAscDesc <|> Parsers.keyword "desc" $> DescAscDesc

instance Qc.Arbitrary AscDesc where
  shrink = Qc.genericShrink
  arbitrary = Qc.elements [minBound .. maxBound]
