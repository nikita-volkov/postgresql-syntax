module PostgresqlSyntax.Ast.QualOp where

import PostgresqlSyntax.Ast.AnyOperator
import PostgresqlSyntax.Ast.Op
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
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
  toTextBuilder = \case
    OpQualOp a -> toTextBuilder a
    OperatorQualOp a -> "OPERATOR (" <> toTextBuilder a <> ")"
  parser =
    asum
      [ OpQualOp <$> parser,
        OperatorQualOp <$> Parsers.inParensWithClause (Parsers.keyword "operator") parser
      ]

instance Qc.Arbitrary QualOp where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ OpQualOp <$> Qc.arbitrary,
        OperatorQualOp <$> Qc.arbitrary
      ]
