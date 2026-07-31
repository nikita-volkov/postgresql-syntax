module PostgresqlSyntax.Ast.RowsfromItem where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Algebra
import PostgresqlSyntax.Ast.FuncExprWindowless
import PostgresqlSyntax.Ast.TableFuncElementList
import qualified PostgresqlSyntax.Helpers.Gens as Gens
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
import qualified PostgresqlSyntax.Helpers.TextBuilders as TextBuilders
import PostgresqlSyntax.Prelude
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
  toTextBuilder settings (RowsfromItem a b) = toTextBuilder settings a <> TextBuilders.suffixMaybe colDefList b
    where
      colDefList a' = "AS (" <> toTextBuilder settings a' <> ")"
  parser settings = do
    a <- parser settings
    Parser.endHead
    b <- optional (Parsers.space1 *> colDefList)
    return (RowsfromItem a b)
    where
      colDefList = Parsers.keyword "as" *> Parsers.space *> Parsers.inParens (Parser.endHead *> parser settings)

instance Qc.Arbitrary RowsfromItem where
  shrink = Qc.genericShrink
  arbitrary = RowsfromItem <$> arbitrary <*> Gens.terminatingMaybe arbitrary
