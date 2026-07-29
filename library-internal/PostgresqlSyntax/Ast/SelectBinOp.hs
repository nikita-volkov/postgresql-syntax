module PostgresqlSyntax.Ast.SelectBinOp where

import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
--   |  select_clause UNION all_or_distinct select_clause
--   |  select_clause INTERSECT all_or_distinct select_clause
--   |  select_clause EXCEPT all_or_distinct select_clause
-- @
data SelectBinOp = UnionSelectBinOp | IntersectSelectBinOp | ExceptSelectBinOp
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst SelectBinOp where
  toTextBuilder settings = \case
    UnionSelectBinOp -> "UNION"
    IntersectSelectBinOp -> "INTERSECT"
    ExceptSelectBinOp -> "EXCEPT"
  parser settings =
    asum
      [ Parsers.keyword "union" $> UnionSelectBinOp,
        Parsers.keyword "intersect" $> IntersectSelectBinOp,
        Parsers.keyword "except" $> ExceptSelectBinOp
      ]

instance Qc.Arbitrary SelectBinOp where
  shrink = Qc.genericShrink
  arbitrary = Qc.elements [UnionSelectBinOp, IntersectSelectBinOp, ExceptSelectBinOp]
