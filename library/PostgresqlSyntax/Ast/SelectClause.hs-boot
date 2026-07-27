module PostgresqlSyntax.Ast.SelectClause where

import PostgresqlSyntax.IsAst (IsAst)
import PostgresqlSyntax.Prelude (Data, Eq, Ord, Show)
import Test.QuickCheck (Arbitrary)

data SelectClause

instance Show SelectClause

instance Eq SelectClause

instance Ord SelectClause

instance Data SelectClause

instance IsAst SelectClause

instance Arbitrary SelectClause
