module PostgresqlSyntax.Ast.TablesampleClause where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.ExprList
import PostgresqlSyntax.Ast.FuncName
import PostgresqlSyntax.Ast.Internal
import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- tablesample_clause:
--   | TABLESAMPLE func_name '(' expr_list ')' opt_repeatable_clause
-- @
--
-- @opt_repeatable_clause@ is a bare alias to 'PostgresqlSyntax.Ast.AExpr'.
data TablesampleClause = TablesampleClause FuncName ExprList (Maybe AExpr)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst TablesampleClause where
  toTextBuilder (TablesampleClause a b c) =
    "TABLESAMPLE " <> toTextBuilder a <> " (" <> toTextBuilder b <> ")" <> suffixMaybe repeatableClause c
    where
      repeatableClause a' = "REPEATABLE (" <> toTextBuilder a' <> ")"
  parser = do
    keyword "tablesample"
    space1
    endHead
    a <- parser
    space
    b <- inParens parser
    c <- optional (space *> repeatableClause)
    return (TablesampleClause a b c)
    where
      repeatableClause = do
        keyword "repeatable"
        space
        inParens (endHead *> parser)

instance Arbitrary TablesampleClause where
  arbitrary = TablesampleClause <$> arbitrary <*> scale (`div` 2) arbitrary <*> scale (`div` 2) arbitrary
