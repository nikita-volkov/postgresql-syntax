module PostgresqlSyntax.Ast.RowsfromItem where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.FuncExprWindowless
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.TableFuncElementList
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

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
    Parser.endHead
    b <- optional (Parser.space1 *> colDefList)
    return (RowsfromItem a b)
    where
      colDefList = keyword "as" *> Parser.space *> inParens (Parser.endHead *> parser)

instance Qc.Arbitrary RowsfromItem where
  arbitrary = RowsfromItem <$> Qc.scale (`div` 2) arbitrary <*> Qc.scale (`div` 2) arbitrary
