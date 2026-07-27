module PostgresqlSyntax.Ast.SelectFetchFirstValue where

import qualified HeadedMegaparsec as Parser
import PostgresqlSyntax.Ast.CExpr
import PostgresqlSyntax.Ast.Fconst
import qualified PostgresqlSyntax.Extras.HeadedMegaparsec as Parser
import qualified PostgresqlSyntax.Extras.TextBuilder as TextBuilder
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

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
      intOrFloat = either TextBuilder.int64Dec TextBuilder.doubleDec
  parser =
    ExprSelectFetchFirstValue
      <$> parser
      <|> NumSelectFetchFirstValue
      <$> (plusOrMinus <* Parser.endHead <* Parser.space)
      <*> iconstOrFconst
    where
      plusOrMinus = False <$ Parser.char '+' <|> True <$ Parser.char '-'
      iconstOrFconst = Right <$> (coerce <$> (parser :: Parser Fconst)) <|> Left <$> Parser.decimal

instance Qc.Arbitrary SelectFetchFirstValue where
  arbitrary =
    Qc.oneof
      [ ExprSelectFetchFirstValue <$> Qc.scale (`div` 2) Qc.arbitrary,
        NumSelectFetchFirstValue <$> Qc.arbitrary <*> Qc.arbitrary
      ]
