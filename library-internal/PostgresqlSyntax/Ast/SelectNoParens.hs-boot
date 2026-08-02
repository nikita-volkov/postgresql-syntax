module PostgresqlSyntax.Ast.SelectNoParens where

import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectWithParens (SelectWithParens)
import PostgresqlSyntax.Algebra (IsAst, Refines)
import PostgresqlSyntax.Prelude (Data, Eq, Ord, Show)
import Test.QuickCheck (Arbitrary)

data SelectNoParens

instance Show SelectNoParens

instance Eq SelectNoParens

instance Ord SelectNoParens

instance Data SelectNoParens

instance IsAst SelectNoParens

instance Arbitrary SelectNoParens

instance Refines SelectWithParens SelectNoParens
