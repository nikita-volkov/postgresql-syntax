module PostgresqlSyntax.Ast.CallStmt where

import PostgresqlSyntax.Ast.FuncApplication
import PostgresqlSyntax.Ast.Internal
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import qualified Test.QuickCheck as Qc

newtype CallStmt
  = CallStmt FuncApplication
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst CallStmt where
  toTextBuilder (CallStmt a) = "CALL " <> toTextBuilder a
  parser = do
    keyword "call"
    Parser.space1
    CallStmt <$> parser

instance Qc.Arbitrary CallStmt where
  arbitrary = CallStmt <$> Qc.scale (`div` 2) arbitrary
