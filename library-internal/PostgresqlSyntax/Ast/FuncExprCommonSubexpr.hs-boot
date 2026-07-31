module PostgresqlSyntax.Ast.FuncExprCommonSubexpr where

import PostgresqlSyntax.Algebra (IsAst)
import PostgresqlSyntax.Prelude (Data, Eq, Ord, Show)
import Test.QuickCheck (Arbitrary)

data FuncExprCommonSubexpr

instance Show FuncExprCommonSubexpr

instance Eq FuncExprCommonSubexpr

instance Ord FuncExprCommonSubexpr

instance Data FuncExprCommonSubexpr

instance IsAst FuncExprCommonSubexpr

instance Arbitrary FuncExprCommonSubexpr
