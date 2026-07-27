module PostgresqlSyntax.Ast.FuncTable where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.FuncExprWindowless
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.OptOrdinality
import PostgresqlSyntax.Ast.RowsfromList
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- func_table:
--   | func_expr_windowless opt_ordinality
--   | ROWS FROM '(' rowsfrom_list ')' opt_ordinality
-- @
data FuncTable
  = FuncExprFuncTable FuncExprWindowless OptOrdinality
  | RowsFromFuncTable RowsfromList OptOrdinality
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst FuncTable where
  toTextBuilder = \case
    FuncExprFuncTable a (OptOrdinality b) -> toTextBuilder a <> bool "" " WITH ORDINALITY" b
    RowsFromFuncTable a (OptOrdinality b) -> "ROWS FROM (" <> toTextBuilder a <> ")" <> bool "" " WITH ORDINALITY" b
  parser =
    asum
      [ do
          keyword "rows"
          space1
          keyword "from"
          space
          a <- inParens (endHead *> parser)
          b <- OptOrdinality <$> trueIfPresent (space *> keyword "with" *> space1 *> keyword "ordinality")
          return (RowsFromFuncTable a b),
        do
          a <- parser
          b <- OptOrdinality <$> trueIfPresent (space1 *> keyword "with" *> space1 *> keyword "ordinality")
          return (FuncExprFuncTable a b)
      ]

instance Arbitrary FuncTable where
  arbitrary =
    oneof
      [ FuncExprFuncTable <$> scale (`div` 2) arbitrary <*> arbitrary,
        RowsFromFuncTable <$> scale (`div` 2) arbitrary <*> arbitrary
      ]
