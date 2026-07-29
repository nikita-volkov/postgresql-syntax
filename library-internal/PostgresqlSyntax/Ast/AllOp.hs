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
  toTextBuilder settings = \case
    OpAllOp a -> toTextBuilder settings a
    MathAllOp a -> toTextBuilder settings a
  parser settings =
    asum
      [ OpAllOp <$> parser settings,
        MathAllOp <$> parser settings
      ]

instance Qc.Arbitrary AllOp where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ OpAllOp <$> Qc.arbitrary,
        MathAllOp <$> Qc.arbitrary
      ]
