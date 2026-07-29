module PostgresqlSyntax.Ast.FuncTable where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.FuncExprWindowless
import PostgresqlSyntax.Ast.OptOrdinality
import PostgresqlSyntax.Ast.RowsfromList
import qualified PostgresqlSyntax.Helpers.Parsers as Parsers
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
  toTextBuilder settings = \case
    FuncExprFuncTable a (OptOrdinality b) -> toTextBuilder settings a <> bool "" " WITH ORDINALITY" b
    RowsFromFuncTable a (OptOrdinality b) -> "ROWS FROM (" <> toTextBuilder settings a <> ")" <> bool "" " WITH ORDINALITY" b
  parser settings =
    asum
      [ do
          Parsers.keyword "rows"
          Parsers.space1
          Parsers.keyword "from"
          Parsers.space
          a <- Parsers.inParens (Parser.endHead *> parser settings)
          b <- OptOrdinality <$> Parsers.trueIfPresent (Parsers.space *> Parsers.keyword "with" *> Parsers.space1 *> Parsers.keyword "ordinality")
          return (RowsFromFuncTable a b),
        do
          a <- parser settings
          b <- OptOrdinality <$> Parsers.trueIfPresent (Parsers.space1 *> Parsers.keyword "with" *> Parsers.space1 *> Parsers.keyword "ordinality")
          return (FuncExprFuncTable a b)
      ]

instance Qc.Arbitrary FuncTable where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ FuncExprFuncTable <$> Qc.arbitrary <*> Qc.arbitrary,
        RowsFromFuncTable <$> Qc.arbitrary <*> Qc.arbitrary
      ]
