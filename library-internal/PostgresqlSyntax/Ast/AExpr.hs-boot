module PostgresqlSyntax.Ast.AExpr where

import PostgresqlSyntax.Algebra (IsAst, Refines)
import PostgresqlSyntax.Prelude (Data, Eq, Ord, Show)
import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectWithParens (SelectWithParens)
import Test.QuickCheck (Arbitrary)

data AExpr

instance Show AExpr

instance Eq AExpr

instance Ord AExpr

instance Data AExpr

instance IsAst AExpr

instance Arbitrary AExpr

instance Refines SelectWithParens AExpr
