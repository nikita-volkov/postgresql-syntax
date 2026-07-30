module PostgresqlSyntax.Ast.BExpr where

import PostgresqlSyntax.IsAst (IsAst)
import PostgresqlSyntax.Prelude (Data, Eq, Ord, Show)
import Test.QuickCheck (Arbitrary)

data BExpr

instance Show BExpr

instance Eq BExpr

instance Ord BExpr

instance Data BExpr

instance IsAst BExpr

instance Arbitrary BExpr
