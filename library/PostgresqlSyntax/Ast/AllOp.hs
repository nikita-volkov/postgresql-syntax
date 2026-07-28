module PostgresqlSyntax.Ast.AllOp where

import PostgresqlSyntax.Ast.MathOp
import PostgresqlSyntax.Ast.Op
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- all_Op:
--   | Op
--   | MathOp
-- @
data AllOp
  = OpAllOp Op
  | MathAllOp MathOp
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst AllOp where
  toTextBuilder = \case
    OpAllOp a -> toTextBuilder a
    MathAllOp a -> toTextBuilder a
  parser =
    asum
      [ OpAllOp <$> parser,
        MathAllOp <$> parser
      ]

instance Qc.Arbitrary AllOp where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ OpAllOp <$> Qc.arbitrary,
        MathAllOp <$> Qc.arbitrary
      ]
