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
  toTextBuilder settings = \case
    MathSymbolicExprBinOp a -> toTextBuilder settings a
    QualSymbolicExprBinOp a -> toTextBuilder settings a
  parser settings =
    QualSymbolicExprBinOp
      <$> parser settings
        <|> MathSymbolicExprBinOp
      <$> parser settings

instance Qc.Arbitrary SymbolicExprBinOp where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ MathSymbolicExprBinOp <$> Qc.arbitrary,
        QualSymbolicExprBinOp <$> Qc.arbitrary
      ]
