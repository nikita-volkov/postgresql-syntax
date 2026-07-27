module PostgresqlSyntax.Ast.FuncTable where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.FuncExprWindowless
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.Ast.OptOrdinality
import PostgresqlSyntax.Ast.RowsfromList
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

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
          Parser.space1
          keyword "from"
          Parser.space
          a <- inParens (Parser.endHead *> parser)
          b <- OptOrdinality <$> trueIfPresent (Parser.space *> keyword "with" *> Parser.space1 *> keyword "ordinality")
          return (RowsFromFuncTable a b),
        do
          a <- parser
          b <- OptOrdinality <$> trueIfPresent (Parser.space1 *> keyword "with" *> Parser.space1 *> keyword "ordinality")
          return (FuncExprFuncTable a b)
      ]

instance Qc.Arbitrary FuncTable where
  arbitrary =
    Qc.oneof
      [ FuncExprFuncTable <$> Qc.scale (`div` 2) Qc.arbitrary <*> Qc.arbitrary,
        RowsFromFuncTable <$> Qc.scale (`div` 2) Qc.arbitrary <*> Qc.arbitrary
      ]
