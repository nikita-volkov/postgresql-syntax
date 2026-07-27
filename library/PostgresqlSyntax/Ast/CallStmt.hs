module PostgresqlSyntax.Ast.CallStmt where

import PostgresqlSyntax.Ast.FuncApplication
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude
import Test.QuickCheck (scale)

newtype CallStmt
  = CallStmt FuncApplication
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst CallStmt where
  toTextBuilder (CallStmt a) = "CALL " <> toTextBuilder a
  parser = do
    keyword "call"
    space1
    CallStmt <$> parser

instance Arbitrary CallStmt where
  arbitrary = CallStmt <$> scale (`div` 2) arbitrary
