module PostgresqlSyntax.Ast.RowsfromItem where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.FuncExprWindowless
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.TableFuncElementList
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- rowsfrom_item:
--   | func_expr_windowless opt_col_def_list
-- @
--
-- @opt_col_def_list@ is a bare alias to
-- 'PostgresqlSyntax.Ast.TableFuncElementList'.
data RowsfromItem = RowsfromItem FuncExprWindowless (Maybe TableFuncElementList)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst RowsfromItem where
  toTextBuilder (RowsfromItem a b) = toTextBuilder a <> suffixMaybe colDefList b
    where
      colDefList a' = "AS (" <> toTextBuilder a' <> ")"
  parser = do
    a <- parser
    endHead
    b <- optional (space1 *> colDefList)
    return (RowsfromItem a b)
    where
      colDefList = keyword "as" *> space *> inParens (endHead *> parser)

instance Arbitrary RowsfromItem where
  arbitrary = RowsfromItem <$> scale (`div` 2) arbitrary <*> scale (`div` 2) arbitrary
