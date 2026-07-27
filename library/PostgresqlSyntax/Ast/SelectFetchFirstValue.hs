module PostgresqlSyntax.Ast.SelectFetchFirstValue where

import HeadedMegaparsec
import PostgresqlSyntax.Ast.CExpr
import PostgresqlSyntax.Ast.Fconst
import PostgresqlSyntax.Extras.HeadedMegaparsec hiding (run)
import PostgresqlSyntax.Extras.TextBuilder (doubleDec, int64Dec)
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- select_fetch_first_value:
--   | c_expr
--   | '+' I_or_F_const
--   | '-' I_or_F_const
-- @
data SelectFetchFirstValue
  = ExprSelectFetchFirstValue CExpr
  | NumSelectFetchFirstValue Bool (Either Int64 Double)
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst SelectFetchFirstValue where
  toTextBuilder = \case
    ExprSelectFetchFirstValue a -> toTextBuilder a
    NumSelectFetchFirstValue a b -> bool "+" "-" a <> intOrFloat b
    where
      intOrFloat = either int64Dec doubleDec
  parser =
    ExprSelectFetchFirstValue
      <$> parser
      <|> NumSelectFetchFirstValue
      <$> (plusOrMinus <* endHead <* space)
      <*> iconstOrFconst
    where
      plusOrMinus = False <$ char '+' <|> True <$ char '-'
      iconstOrFconst = Right <$> (coerce <$> (parser :: Parser Fconst)) <|> Left <$> decimal

instance Arbitrary SelectFetchFirstValue where
  arbitrary =
    oneof
      [ ExprSelectFetchFirstValue <$> scale (`div` 2) arbitrary,
        NumSelectFetchFirstValue <$> arbitrary <*> arbitrary
      ]
