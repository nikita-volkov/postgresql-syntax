module PostgresqlSyntax.Ast.WithClause where

import PostgresqlSyntax.IsAst (IsAst)
import PostgresqlSyntax.Prelude (Data, Eq, Ord, Show)
import Test.QuickCheck (Arbitrary)

data WithClause

instance Show WithClause

instance Eq WithClause

instance Ord WithClause

instance Data WithClause

instance IsAst WithClause

instance Arbitrary WithClause
