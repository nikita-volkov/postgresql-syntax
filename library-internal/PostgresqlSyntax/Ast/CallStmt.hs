module PostgresqlSyntax.Ast.CallStmt where

import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.FuncApplication
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

newtype CallStmt
  = CallStmt FuncApplication
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst CallStmt where
  toTextBuilder settings (CallStmt a) = "CALL " <> toTextBuilder settings a
  parser settings = do
    Parsers.keyword "call"
    Parsers.space1
    CallStmt <$> parser settings

instance Qc.Arbitrary CallStmt where
  shrink = Qc.genericShrink
  arbitrary = CallStmt <$> arbitrary
