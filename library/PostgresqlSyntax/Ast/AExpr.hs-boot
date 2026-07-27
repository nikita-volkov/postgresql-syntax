module PostgresqlSyntax.Ast.AExpr where

import PostgresqlSyntax.IsAst (IsAst, Parser)
import PostgresqlSyntax.Prelude (Data, Eq, Ord, Show, Text)
import Test.QuickCheck (Arbitrary)

data AExpr

instance Show AExpr

instance Eq AExpr

instance Ord AExpr

instance Data AExpr

instance IsAst AExpr

instance Arbitrary AExpr

-- | 'parser', but rejecting the given words when they'd otherwise be
-- accepted as a trailing bare column-reference identifier. Needed by
-- "PostgresqlSyntax.Ast.SortBy", which must not let @a_expr@ swallow a
-- keyword (@USING@\/@ASC@\/@DESC@\/@NULLS@) that is meant to terminate it.
filteredParser :: [Text] -> Parser AExpr
