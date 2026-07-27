module PostgresqlSyntax.Ast.QualOp where

import PostgresqlSyntax.Ast.AnyOperator
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.Op
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude

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
  toTextBuilder = \case
    OpQualOp a -> toTextBuilder a
    OperatorQualOp a -> "OPERATOR (" <> toTextBuilder a <> ")"
  parser =
    asum
      [ OpQualOp <$> parser,
        OperatorQualOp <$> inParensWithClause (keyword "operator") parser
      ]

instance Arbitrary QualOp where
  arbitrary =
    oneof
      [ OpQualOp <$> arbitrary,
        OperatorQualOp <$> arbitrary
      ]
