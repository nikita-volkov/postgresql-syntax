module PostgresqlSyntax.Ast.QualOp where

import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.AnyOperator
import PostgresqlSyntax.Ast.Op
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

-- |
-- ==== References
-- @
-- qual_Op:
--   | Op
--   | OPERATOR '(' any_operator ')'
-- @
data QualOp
  = OpQualOp Op
  | OperatorQualOp AnyOperator
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst QualOp where
  toTextBuilder settings = \case
    OpQualOp a -> toTextBuilder settings a
    OperatorQualOp a -> "OPERATOR (" <> toTextBuilder settings a <> ")"
  parser settings =
    asum
      [ OpQualOp <$> parser settings,
        OperatorQualOp <$> Parsers.inParensWithClause (Parsers.keyword "operator") (parser settings)
      ]

instance Qc.Arbitrary QualOp where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ OpQualOp <$> Qc.arbitrary,
        OperatorQualOp <$> Qc.arbitrary
      ]
