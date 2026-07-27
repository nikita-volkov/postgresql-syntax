module PostgresqlSyntax.Ast.SelectWithParens where

import PostgresqlSyntax.IsAst (IsAst)
import PostgresqlSyntax.Prelude (Data, Eq, Ord, Show)
import Test.QuickCheck (Arbitrary)

data SelectWithParens

instance Show SelectWithParens

instance Eq SelectWithParens

instance Ord SelectWithParens

instance Data SelectWithParens

instance IsAst SelectWithParens

instance Arbitrary SelectWithParens
