module PostgresqlSyntax.Ast.SymbolicExprBinOp where

import PostgresqlSyntax.Ast.MathOp
import PostgresqlSyntax.Ast.QualOp
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

data SymbolicExprBinOp
  = MathSymbolicExprBinOp MathOp
  | QualSymbolicExprBinOp QualOp
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst SymbolicExprBinOp where
  toTextBuilder = \case
    MathSymbolicExprBinOp a -> toTextBuilder a
    QualSymbolicExprBinOp a -> toTextBuilder a
  parser =
    QualSymbolicExprBinOp
      <$> parser
        <|> MathSymbolicExprBinOp
      <$> parser

instance Qc.Arbitrary SymbolicExprBinOp where
  arbitrary =
    Qc.oneof
      [ MathSymbolicExprBinOp <$> Qc.arbitrary,
        QualSymbolicExprBinOp <$> Qc.arbitrary
      ]
