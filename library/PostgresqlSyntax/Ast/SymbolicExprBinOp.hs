module PostgresqlSyntax.Ast.SymbolicExprBinOp where

import PostgresqlSyntax.Ast.MathOp
import PostgresqlSyntax.Ast.QualOp
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude

data SymbolicExprBinOp
  = MathSymbolicExprBinOp MathOp
  | QualSymbolicExprBinOp QualOp
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst SymbolicExprBinOp where
  toTextBuilder = \case
    MathSymbolicExprBinOp a -> toTextBuilder a
    QualSymbolicExprBinOp a -> toTextBuilder a
  parser =
    QualSymbolicExprBinOp <$> parser
      <|> MathSymbolicExprBinOp <$> parser

instance Arbitrary SymbolicExprBinOp where
  arbitrary =
    oneof
      [ MathSymbolicExprBinOp <$> arbitrary,
        QualSymbolicExprBinOp <$> arbitrary
      ]
