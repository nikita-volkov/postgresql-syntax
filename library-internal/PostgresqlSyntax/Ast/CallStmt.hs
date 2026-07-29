module PostgresqlSyntax.Ast.CallStmt where

import PostgresqlSyntax.Ast.FuncApplication
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

newtype CallStmt
  = CallStmt FuncApplication
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst CallStmt where
  toTextBuilder (CallStmt a) = "CALL " <> toTextBuilder a
  parser = do
    Parsers.keyword "call"
    Parsers.space1
    CallStmt <$> parser

instance Qc.Arbitrary CallStmt where
  shrink = Qc.genericShrink
  arbitrary = CallStmt <$> arbitrary
