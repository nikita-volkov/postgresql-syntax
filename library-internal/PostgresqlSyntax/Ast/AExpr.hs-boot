module PostgresqlSyntax.Ast.AExpr where

import {-# SOURCE #-} PostgresqlSyntax.Ast.SelectWithParens (SelectWithParens)
import PostgresqlSyntax.IsAst (IsAst)
import PostgresqlSyntax.Prelude (Bool, Data, Eq, Ord, Parser, Show, Text)
import PostgresqlSyntax.Settings (Settings)
import Test.QuickCheck (Arbitrary, Gen)

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
filteredParser :: Settings -> [Text] -> Parser AExpr

-- | See "PostgresqlSyntax.Ast.AExpr" for the full documentation.
isBoundedAExprOperand :: AExpr -> Bool

-- | See "PostgresqlSyntax.Ast.AExpr" for the full documentation.
safeAExprOperand :: Gen AExpr -> Gen AExpr

-- | See "PostgresqlSyntax.Ast.AExpr" for the full documentation.
selectWithParensAExpr :: SelectWithParens -> AExpr
