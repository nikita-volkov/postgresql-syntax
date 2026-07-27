module PostgresqlSyntax.Ast.SelectNoParens where

import PostgresqlSyntax.IsAst (IsAst)

data SelectNoParens

instance IsAst SelectNoParens
