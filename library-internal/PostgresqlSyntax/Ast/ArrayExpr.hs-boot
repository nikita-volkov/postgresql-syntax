module PostgresqlSyntax.Ast.ArrayExpr where

import PostgresqlSyntax.IsAst (IsAst)
import PostgresqlSyntax.Prelude (Data, Eq, Ord, Show)
import Test.QuickCheck (Arbitrary)

data ArrayExpr

instance Show ArrayExpr

instance Eq ArrayExpr

instance Ord ArrayExpr

instance Data ArrayExpr

instance IsAst ArrayExpr

instance Arbitrary ArrayExpr
