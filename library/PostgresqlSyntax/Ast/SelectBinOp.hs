module PostgresqlSyntax.Ast.SelectBinOp where

import PostgresqlSyntax.Ast.Internal
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
  toTextBuilder = \case
    UnionSelectBinOp -> "UNION"
    IntersectSelectBinOp -> "INTERSECT"
    ExceptSelectBinOp -> "EXCEPT"
  parser =
    asum
      [ keyword "union" $> UnionSelectBinOp,
        keyword "intersect" $> IntersectSelectBinOp,
        keyword "except" $> ExceptSelectBinOp
      ]

instance Qc.Arbitrary SelectBinOp where
  shrink = Qc.genericShrink
  arbitrary = Qc.elements [UnionSelectBinOp, IntersectSelectBinOp, ExceptSelectBinOp]
