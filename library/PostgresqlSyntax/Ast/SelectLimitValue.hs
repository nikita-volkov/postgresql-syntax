module PostgresqlSyntax.Ast.SelectLimitValue where

import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Ast.Internal
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import Test.QuickCheck (scale)

-- |
-- ==== References
-- @
-- select_limit_value:
--   | a_expr
--   | ALL
-- @
data SelectLimitValue
  = ExprSelectLimitValue AExpr
  | AllSelectLimitValue
  deriving (Show, Generic, Eq, Ord, Data)

instance IsAst SelectLimitValue where
  toTextBuilder = \case
    ExprSelectLimitValue a -> toTextBuilder a
    AllSelectLimitValue -> "ALL"
  parser =
    AllSelectLimitValue
      <$ keyword "all"
      <|> ExprSelectLimitValue
      <$> parser

instance Arbitrary SelectLimitValue where
  arbitrary =
    oneof
      [ ExprSelectLimitValue <$> scale (`div` 2) arbitrary,
        pure AllSelectLimitValue
      ]
