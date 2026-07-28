module PostgresqlSyntax.Ast.SelectLimitValue where

import {-# SOURCE #-} PostgresqlSyntax.Ast.AExpr (AExpr)
import PostgresqlSyntax.Helpers.Parsers
import PostgresqlSyntax.IsAst
import PostgresqlSyntax.Prelude hiding (filter, many, some, try)
import qualified Test.QuickCheck as Qc

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

instance Qc.Arbitrary SelectLimitValue where
  shrink = Qc.genericShrink
  arbitrary =
    Qc.oneof
      [ ExprSelectLimitValue <$> Qc.scale (`div` 2) Qc.arbitrary,
        pure AllSelectLimitValue
      ]
